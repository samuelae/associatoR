#' Embed associations
#'
#' \code{ar_embed_targets} generates target embeddings.
#'
#' @param associations an \code{associatoR} object including targets.
#' @param method a \code{character} specifying the type of embedding. One of \code{c("counts","ppmi","ppmi-svd","huggingface")}. Default is \code{"ppmi-svd"}.
#' @param min_count an \code{integer} value specifying the minimum response count for responses to be considered in the embedding for \code{method = c("counts","ppmi","ppmi-svd")}. Default is \code{5}.
#' @param n_dim an \code{integer} value specifying the number of dimensions generated in \code{method = "ppmi-svd"}. Default is \code{300}.
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
#' ar_import(intelligence,
#'           participant = participant_id,
#'           cue = cue,
#'           response = response,
#'           participant_vars = c(gender, education),
#'           response_vars = c(response_position, response_level)) %>%
#'   ar_set_targets(targets = "cues") %>%
#'   ar_embed_targets()
#'
#' @export

ar_embed_targets <- function(associations,
                             method = "ppmi-svd",
                             min_count = 5,
                             n_dim = 300,
                             model = NULL,
                             token = NULL,
                             context = NULL) {

  # checks
  check_object(associations)
  check_targets(associations)
  chk::chk_subset(method, c("counts", "ppmi", "ppmi-svd", "huggingface"))
  chk::chk_true(any(associations$targets$target %in% associations$cues$cue))

  if(method != "huggingface") {

    # get counts
    counts = associations$responses %>%
      dplyr::filter(cue %in% associations$targets$target) %>%
      dplyr::mutate(response = paste0("resp_", response)) %>%
      dplyr::group_by(cue, response) %>%
      dplyr::summarize(n = dplyr::n()) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(names_from = response, values_from = n)

    # get count embed
    embed = as.matrix(counts %>% dplyr::select(-cue))
    rownames(embed) = counts$cue
    embed[is.na(embed)] = 0

    # remove lower than min_count
    embed = embed[, colSums(embed) >= min_count]

    # remove cues with zero row sum
    row_sums = rowSums(embed)
    if(any(row_sums == 0)) {
      embed = embed[row_sums >= min_count, ]
      warning(paste0(sum(row_sums < min_count), " targets with count < min_count were dropped from embedding."))
    }


    if(method %in% c("ppmi", "ppmi-svd")) {

      # do ppmi
      embed = embed / sum(embed)
      norm = rowSums(embed) %*% t(colSums(embed))
      embed = log2(embed / norm)
      embed[embed < 0] = 0

      if(method == "ppmi-svd"){
        n_dim = min(n_dim, ncol(embed), nrow(embed))
        svd = RSpectra::svds(embed, n_dim)
        rownames(svd$u) = rownames(embed)
        embed = svd$u %*% diag(svd$d)
      }
    }
  }

  # api
  if(method == "huggingface") {

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
    bar = progress::progress_bar$new(format = "Generating embeddings [:bar] :percent eta: :eta",
                                     total = 100, clear = FALSE, width= 60)
    bar$tick(0)

    # post
    embed = lapply(1:length(texts), function(i) {

      # process text
      text_processed = texts[[i]] %>% paste0(context, .)

      # do post
      post = httr::POST(url = api,
                        httr::add_headers(Authorization = glue::glue("Bearer {token}"),
                                          "Content-Type" = "application/json"),
                        body = jsonlite::toJSON(text_processed))

      # show error
      if(!post$status_code %in% c(200, 503)) stop(paste0("Error:", post$status_code))

      if(post$status_code == 503) { # Service Unavailable

        # handle error
        error = httr::content(post, as = "text", encoding = "UTF-8") %>% jsonlite::fromJSON()
        time = error$estimated
        message(paste0(error$error, glue::glue(". Waiting {time} seconds.")))

        # sleep
        Sys.sleep(time + 5)

        # rerun
        post = httr::POST(url = api,
                          httr::add_headers(Authorization = glue::glue("Bearer {token}"),
                                            "Content-Type" = "application/json"),
                          body = jsonlite::toJSON(text_processed))
      }

      if(post$status_code == 200) { # OK

        # extract embedding
        emb_raw = httr::content(post, as = "text", encoding = "UTF-8") %>%
          jsonlite::fromJSON()

        # handle multi-token embeddings
        if(class(emb_raw)[1] == "list") {
          emb = sapply(emb_raw, function(x) x[1, 1, ]) %>% t()
        } else {
          emb = emb_raw
        }

      } else { # not OK
        # catch error message of potential second POST attempt and stop
        stop(paste0("Error:", post$status_code))
      }

      # names
      rownames(emb) = texts[[i]]

      # update bar
      bar$update(i / length(texts))

      # out
      emb

    }) %>% do.call(what = rbind) # end embed = lapply

  } # end if method == huggingface


  # out ----

  # colnames
  colnames(embed) = paste0("dim_",1:ncol(embed))

  # restore order of targets
  embed = embed[associations$targets$target[associations$targets$target %in% rownames(embed)], ]

  # convert to tibble
  embed = embed %>%
    tibble::as_tibble() %>%
    dplyr::mutate(target = rownames(embed)) %>%
    dplyr::select(target, dplyr::everything())

  # add embedding
  associations$target_embedding = embed

  # add attribute
  attr(associations$target_embedding, "embedding_settings") = list(method = method,
                                                                   min_count = min_count,
                                                                   n_dim = n_dim,
                                                                   model = model,
                                                                   token = token,
                                                                   context = context)

  # out
  associations

}


#' Project embeddings
#'
#' \code{ar_project} generates a 2D projection of the target embedding.
#'
#' @param associations an \code{associatoR} object including targets.
#' @param method a \code{character} specifying the type of projection. One of \code{c("mds", "umap")}. Default is \code{"umap"}.
#' @param n_dim an \code{integer} specifying the number of projection dimensions. Default is \code{2}.
#' @param ... additional parguments passed to the projection method.
#'
#' @return The function returns the \code{associatoR} object with the
#'   \code{target_embeddings} overwritten by the projected embeddings.
#'
#' @references Aeschbach, S., Mata, R., Wulff, D. U. (2024). associatoR. psyArXiv
#'
#' @examples
#' ar_import(intelligence,
#'           participant = participant_id,
#'           cue = cue,
#'           response = response,
#'           participant_vars = c(gender, education),
#'           response_vars = c(response_position, response_level)) %>%
#'   ar_set_targets(targets = "cues") %>%
#'   ar_embed_targets() %>%
#'   ar_project_embedding()
#'
#' @export

ar_project_embedding <- function(associations,
                                 method = "umap",
                                 n_dim = 2,
                                 ...) {

  # checks
  check_object(associations)
  check_embeddings(associations)
  chk::chk_subset(method, c("mds","umap"))
  chk::chk_whole_number(n_dim)
  if(n_dim < 1) stop("Argument n_dim must be larger than 0.")

  # get embed
  embed = associations$target_embedding %>%
    dplyr::select(-target) %>%
    as.matrix()

  # pca
  if(method == "pca"){

    # run pca
    embed = stats::princomp(embed, ...)$scores[,1:n_dim]

  }

  # mds
  if(method == "mds"){

    # run mds
    embed = stats::cmdscale(dist(embed), k = n_dim, ...)

  }

  # umap
  if(method == "umap"){

    args = list(...)
    if(!"config" %in% args){
      args$config = umap::umap.defaults
      args$config$n_components = 2
    }
    embed = umap::umap(embed, config = args$config, ...)$layout

  }

  # names
  colnames(embed) = paste0("dim_",1:ncol(embed))

  # out
  embed = embed %>%
    tibble::as_tibble() %>%
    dplyr::mutate(target = associations$target_embedding$target) %>%
    dplyr::select(target, dplyr::everything())

  # over write
  associations$target_embedding = embed

  # out
  associations

}
