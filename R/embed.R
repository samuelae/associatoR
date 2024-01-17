#' Embed associations
#'
#' \code{ar_embed} generates target embeddings.
#'
#' @param associations an \code{associatoR} object including targets.
#' @param type a \code{character} specifying the type of embedding. One of \code{c("counts","ppmi","ppmi-svd","huggingface")}. Default is \code{"ppmi-svd"}.
#' @param min_count an \code{integer} value specifying the minimum response count for responses to be considered in the embedding for \code{type = c("counts","ppmi","ppmi-svd")}. Default is \code{5}.
#' @param n_dim an \code{integer} value specifying the number of dimensions generated in \code{type = "ppmi-svd"}. Default is \code{300}.
#' @param model a \code{character} specifying the model label. Must match the name on \href{https://huggingface.co/models}{huggingface.co/models}.
#' @param token a \code{character} string specifying the access token for the hugging face API. Must be obtained from \href{https://huggingface.co/inference-api}{huggingface.co/inference-api}.
#' @param context an optional \code{character} string specifying a common lead text that may help the language model interpret the associations. Defaults to \code{"Free association: "}
#'
#' @return The function returns the \code{associatoR} object including a new
#'   \code{matrix} element called \code{target_embeddings} containing the target embeddings.
#'
#' @references Aeschbach, S., Mata, R., Wulff, D. U. (2024). associatoR. psyArXiv
#'
#' @examples
#'
#' ar_import(risk_asso, participant = "id", cue = "cue", response = "response",
#'           response_vars = "trial", participant_vars = c("gender", "age", "age_group")) %>%
#'   ar_normalize() %>%
#'   ar_set_targets(target_set = "responses") %>%
#'   ar_embed()
#'
#' @export

ar_embed <- function(associations,
                     type = "ppmi-svd",
                     min_count = 5,
                     n_dim = 300,
                     model = NULL,
                     token = NULL,
                     context = NULL) {

  # checks
  chk::chk_s3_class(associations, "associatoR")
  chk::chk_subset("targets", names(associations))
  chk::chk_subset(type, c("counts","ppmi","ppmi-svd","huggingface"))
  chk::chk_true(any(associations$targets$target %in% associations$cues$cue))

  if(type != "huggingface"){

    # get counts
    counts = associations$responses %>%
      dplyr::filter(cue %in% associations$targets$target) %>%
      dplyr::mutate(response = paste0("resp_",response)) %>%
      dplyr::group_by(cue, response) %>%
      dplyr::summarize(n = dplyr::n()) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(names_from = response,
                  values_from = n)

    # get count embed
    embed = as.matrix(counts %>% dplyr::select(-cue))
    rownames(embed) = counts$cue
    embed[is.na(embed)] = 0

    # remove lower than min_count
    embed = embed[,colSums(embed) >= min_count]

    # remove cues with zero row sum
    row_sums = rowSums(embed)
    if(any(row_sums == 0)){
      embed = embed[row_sums >= min_count,]
      warning(paste0(sum(row_sums < min_count)," cues with < min_count responses >= min_count were dropped from embedding."))
      }


    if(type %in% c("ppmi","ppmi-svd")){

      # do ppmi
      embed = embed / sum(embed)
      norm = rowSums(embed) %*% t(colSums(embed))
      embed = log2(embed / norm)
      embed[embed < 0] = 0

      if(type == "ppmi-svd"){
        n_dim = min(n_dim, ncol(embed), nrow(embed))
        svd = RSpectra::svds(embed, n_dim)
        rownames(svd$u) = rownames(embed)
        embed = svd$u %*% diag(svd$d)
        }
      }
    }

  # api
  if(type == "huggingface"){

    # check if token exists
    chk::chk_not_null(token)

    # set model
    if(is.null(model)) {
      model = "sentence-transformers/all-mpnet-base-v2"
      } else {
      chk::chk_character(model)
      }

    # set context
    if(is.null(context)) {
      context = "Free association: "
      } else {
      chk::chk_character(context)
      }

    # set targets
    targets = associations$targets$target

    # set api and token
    api = glue::glue("https://api-inference.huggingface.co/pipeline/feature-extraction/{model}")

    # split to undercut limit
    if(length(targets) > 500) {
      ind = seq(0, ceiling(length(targets) / 500) - .00001, length = length(targets)) %>% floor()
      texts = split(targets, ind)
      } else {
      texts = list(targets)
      }

    # setup progress bar
    bar = progress::progress_bar$new(format = "Generating embeddings [:bar] :percent eta: :eta", total = 100, clear = FALSE, width= 60)
    bar$tick(0)

    # post
    embed = lapply(1:length(texts), function(i){

      # process text
      text_processed = texts[[i]] %>% paste0(context, .)

      # do post
      post = httr::POST(url = api,
                        httr::add_headers(Authorization = glue::glue("Bearer {token}"),
                                          "Content-Type" = "application/json"),
                        body = jsonlite::toJSON(text_processed))

      # show error
      if(!post$status_code %in% c(200, 503)) stop(paste0("Error:", post$status_code))

      if(post$status_code == 503){

        # handle error
        error = httr::content(post, as = "text", encoding = "UTF-8") %>% jsonlite::fromJSON()
        time = error$estimated
        message(paste0(error$error,glue::glue(". Waiting {time} seconds.")))

        # sleep
        Sys.sleep(time + 5)

        # rerun
        post = httr::POST(url = api,
                          httr::add_headers(Authorization = glue::glue("Bearer {token}"),
                                            "Content-Type" = "application/json"),
                          body = jsonlite::toJSON(texts[[i]]))
        }


      if(post$status_code == 200){

        # extract embedding
        emb_raw = httr::content(post, as = "text", encoding = "UTF-8") %>%
          jsonlite::fromJSON()

        # handle multi-token embeddings
        if(class(emb_raw)[1] == "list"){
          emb = sapply(emb_raw, function(x) x[1,1,]) %>% t()
          } else {
          emb = emb_raw
          }
        }

      # names
      rownames(emb) = texts[[i]]

      # update bar
      bar$update(i/length(texts))

      # out
      emb

    }) %>% do.call(what = rbind)


    }


  # out ----

  # colnames
  colnames(embed) = paste0("dim_",1:ncol(embed))
  embed = embed %>%
    tibble::as_tibble() %>%
    dplyr::mutate(target = rownames(embed)) %>%
    dplyr::select(target, dplyr::everything())

  # add
  associations$target_embedding = embed

  # out
  associations

}


#' Project embeddings
#'
#' \code{ar_project} generates a 2D projection of the target embedding.
#'
#' @param associations an \code{associatoR} object including targets.
#' @param type a \code{character} specifying the type of projection. One of \code{c("mds", "umap")}. Default is \code{"umap"}.
#' @param ... additional parguments passed to the projection method.
#'
#' @return The function returns the \code{associatoR} object with the
#'   \code{target_embeddings} overwritten by the projected embeddings.
#'
#' @references Aeschbach, S., Mata, R., Wulff, D. U. (2024). associatoR. psyArXiv
#'
#' @examples
#'
#' ar_import(risk_asso, participant = "id", cue = "cue", response = "response",
#'           response_vars = "trial", participant_vars = c("gender", "age", "age_group")) %>%
#'   ar_normalize() %>%
#'   ar_set_targets(target_set = "responses") %>%
#'   ar_embed() %>%
#'   ar_project()
#'
#' @export

ar_project <- function(associations,
                       type = "umap",
                       ...) {

  # checks
  chk::chk_s3_class(associations, "associatoR")
  chk::chk_subset("target_embedding", names(associations))
  chk::chk_subset(type, c("mds","umap"))

  # get embed
  embed = associations$target_embedding %>%
    dplyr::select(-target) %>%
    as.matrix()

  # mds
  if(type == "mds"){

    # run mds
    embed_2d = stats::cmdscale(embed, k = 2, ...)

  }

  # umap
  if(type == "umap"){

    embed_2d = umap::umap(embed, ...)$layout

  }

  # names
  colnames(embed_2d) = c("x","y")

  # out
  embed_2d = embed_2d %>%
    tibble::as_tibble() %>%
    dplyr::mutate(target = associations$target_embedding$target) %>%
    dplyr::select(target, dplyr::everything())

  # over write
  associations$target_embedding = embed_2d

  # out
  associations
  }
