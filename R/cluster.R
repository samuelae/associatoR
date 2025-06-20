#' Cluster targets
#'
#' \code{ar_cluster_targets} produces a target clustering based on the associatoR object's target embedding.
#'
#' @param associations an \code{associatoR} object including target_embeddings.
#' @param method a \code{character} specifying the clustering method. One of \code{c("louvain", "hclust", "kmeans", "dbscan")}. Default is \code{"louvain"}.
#' @param similarity a \code{character} specifying the similarity metric. One of \code{c("arccos", "cosine", "euclidean")}. Default is \code{"arccos"}.
#' @param k an \code{integer} specifying the number of clusters for \code{method = c("hclust", "kmeans")}.
#' @param linkage a \code{character} specifying the linkage criterion for \code{method = c("hclust")}. Default is \code{"complete"}.
#' @param eps a \code{numeric} specifying the point distance used in \code{method = c("dbscan")}.
#' @param ... additional attributes passed on to the clustering method.
#'
#' @return The function returns the \code{associatoR} object including a new
#'   \code{clusters} column in \code{targets} containing the target clusters.
#' @export
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
#'   ar_cluster_targets(method = "louvain")
#'
ar_cluster_targets <- function(associations,
                               method = "louvain",
                               similarity = "arccos",
                               k = NULL,
                               linkage = NULL,
                               eps = NULL,
                               ...) {

  # check inputs
  check_object(associations)
  check_targets(associations)
  check_embeddings(associations)
  chk::chk_subset(method, c("louvain", "hclust", "kmeans", "dbscan"))
  chk::chk_subset(similarity, c("arccos", "cosine", "euclidean"))

  # get embedding
  if("cluster" %in% names(associations$targets)){
    associations$targets = associations$targets %>% dplyr::select(-cluster)
    warnings("Existing clustering overwritten.")
  }

  # get embedding
  emb = associations$target_embedding[, -1] %>% as.matrix()
  rownames(emb) = associations$target_embedding %>% dplyr::pull(target)

  # check k
  if(!(is.null(k) | is.numeric(k))){
    if(k > nrow(emb)) stop("k must be smaller than nrow($target_embeddings).")
    }

  # get sim
  if(similarity %in% c("cosine","arccos")){
    sim = cosine(emb)
    if(similarity == "cosine"){
      sim[sim<0] = 0
      } else {
      sim = arccos_sim(sim)
      }
    }

  if(similarity == "euclidean"){
    sim = dist(emb)
    }


  # louvain
  if(method == "louvain"){

    # reverse sim
    if(similarity == "euclidean") sim = as.matrix(1/sim)

    # convert to graph and run louvain clustering
    g = igraph::graph_from_adjacency_matrix(sim,
                                            mode = "undirected",
                                            weighted = TRUE,
                                            diag = FALSE,
                                            add.colnames = NULL)

    # get clusters
    clusters = igraph::cluster_louvain(g, ...)
    targets_clusters = tibble::tibble(target = clusters$names,
                                      cluster = clusters$membership)

    }

  # hclust
  if(method == "hclust"){

    # reverse sim
    if(similarity != "euclidean") {
      sim = 1/(sim+.01)
      diag(sim) = 0
      sim = as.dist(sim)
      }

    if(is.null(linkage)) linkage = "complete"

    # get clusters
    clustering = fastcluster::hclust(sim, method = linkage, ...)
    clusters = stats::cutree(clustering, k = k)
    targets_clusters <- tibble::tibble(target = names(clusters),
                                       cluster = clusters)

  }

  # kmeans
  if(method == "kmeans"){

    # get clusters
    clusters = stats::kmeans(emb, centers = k, ...)
    targets_clusters <- tibble::tibble(target = names(clusters$cluster),
                                       cluster = clusters$cluster)

  }

  # dbscan
  if(method == "dbscan"){

    # reverse sim
    if(similarity != "euclidean") {
      sim = 1/(sim+.01)
      diag(sim) = 0
      sim = as.dist(sim)
    }

    # set eps
    if(is.null(eps)) eps = quantile(c(sim), .01)

    # get clusters
    clusters = dbscan::dbscan(sim, eps = eps, ...)
    targets_clusters <- tibble::tibble(target = rownames(emb),
                                       cluster = clusters$cluster)

  }

  # return raw clusters if in bootstrapping mode
  if("boot" %in% names(associations)) return(targets_clusters)

  # improve cluster name readability
  targets_clusters <- targets_clusters %>%
    dplyr::mutate(cluster = paste0("cluster_", cluster))

  # add clusters to targets tibble, rename NA clusters
  associations$targets <- associations$targets %>%
    dplyr::left_join(targets_clusters, by = "target") %>%
    tidyr::replace_na(list(cluster = "no_cluster"))

  # add attribute
  cluster_settings = list(method = method,
                          similarity = similarity,
                          k = k,
                          linkage = linkage,
                          eps = eps)
  cluster_settings = c(cluster_settings, list(...))
  attr(associations$targets, "cluster_settings") = cluster_settings

  # out
  associations

}


#' Cluster stability
#'
#' \code{ar_cluster_stability} evaluates the stability of a clustering generated by ar_cluster_targets()
#'
#' Clustering stability is calculated with the identical clustering settings (e.g., clustering algorithm, similarity function, etc.) used to generate the original cluster assignment. The information is stored as a attribute to the \code{targets} table with name \code{"cluster_settings"}.
#'
#' @param associations an \code{associatoR} object including target_embeddings.
#' @param n_boot an \code{integer} specifying the number of bootstrap samples to draw. Default is \code{1000}.
#' @param unique a \code{logical} specifying whether targets duplicates in bootstrap samples should be ignored. Default is \code{TRUE}.
#'
#' @return The function returns a list containing two matrices with the target- (\code{target_stability}) and cluster-wise (\code{cluster_stability}) probabilities of being assigned to the same cluster under random pertubation.
#'
#' @examples
#' ar_obj = ar_import(intelligence,
#'                    participant = participant_id,
#'                    cue = cue,
#'                    response = response,
#'                    participant_vars = c(gender, education),
#'                    response_vars = c(response_position, response_level)) %>%
#'   ar_set_targets(targets = "cues") %>%
#'   ar_embed_targets() %>%
#'   ar_cluster_targets(method = "louvain")
#'
#' # in productive use, n_boot should be set higher, defaults to 1000
#' ar_cluster_stability(ar_obj, n_boot = 10)
#'
#' @export
ar_cluster_stability <- function(associations, n_boot = 1000, unique = TRUE) {

  # check inputs
  check_object(associations)
  check_targets(associations)
  check_embeddings(associations)

  # get embedding
  emb = associations$target_embedding[, -1] %>% as.matrix()
  rownames(emb) = associations$target_embedding %>% dplyr::pull(target)

  # get settings
  cluster_setting = attr(associations$targets, "cluster_settings")

  # setup container
  results = expand.grid(i = 1:nrow(emb), j = 1:nrow(emb)) %>%
    tibble::as_tibble() %>%
    dplyr::filter(i < j) %>%
    dplyr::mutate(id = get_id(rownames(emb)[i], rownames(emb)[j]),
                  count = 0,
                  stab = 0) %>%
    dplyr::select(id, count, stab)

  # mutable object
  associations_boot = associations
  associations_boot$boot = TRUE

  # setup progress bar
  bar = progress::progress_bar$new(format = "Running bootstrap [:bar] :percent eta: :eta", total = 100, clear = FALSE, width= 60)
  bar$tick(0)

  for(i in 1:n_boot){

    # sample
    if(unique){
      size = round(nrow(emb) * (1-1/exp(1)))
      targets = sample(rownames(emb), size = size, replace = FALSE)
    } else {
      targets = sample(rownames(emb), replace = TRUE)
    }

    # overwrite_embeddings
    associations_boot$target_embedding = tibble::tibble(target = targets) %>% dplyr::bind_cols(tibble::as_tibble(emb[targets,]))

    # run clustering
    clustering = do.call(ar_cluster_targets,
                         c(list(associations = associations_boot), cluster_setting))

    # evaluate sameness
    equal = cstab:::equal(clustering$cluster)

    # evaluate sameness
    equal_tbl = expand.grid(i = 1:length(targets), j = 1:length(targets)) %>%
      tibble::as_tibble() %>%
      dplyr::filter(i < j) %>%
      dplyr::arrange(i, j) %>%
      dplyr::mutate(i = targets[i],
                    j = targets[j],
                    id = get_id(i, j),
                    equal = equal) %>%
      dplyr::group_by(id) %>%
      dplyr::summarize(count = dplyr::n(),
                       stab = sum(equal))

    # add to counts
    results = results %>%
      dplyr::left_join(equal_tbl, by = c("id"), suffix = c("","_new")) %>%
      dplyr::mutate(count = count + ifelse(is.na(count_new), 0, count_new),
                    stab = stab + ifelse(is.na(stab_new), 0, stab_new)) %>%
      dplyr::select(-count_new, -stab_new)


    # update bar
    bar$update(i/n_boot)

  }

  # get original clusters from data
  clusters = associations$targets %>% dplyr::pull(cluster, target)

  # evaluate
  results = results %>%
    dplyr::mutate(i = stringr::str_split(id, "_") %>% sapply(`[`, 1),
                  j = stringr::str_split(id, "_") %>% sapply(`[`, 2)) %>%
    dplyr::select(i, j, count, stab) %>%
    dplyr::mutate(i_cluster = clusters[i],
                  j_cluster = clusters[j],
                  stability = stab / count) %>%
    dplyr::select(i, j, i_cluster, j_cluster, count, stability)

  # expand
  results = results %>%
    dplyr::bind_rows(tibble::tibble(i = results$j,
                                    j = results$i,
                                    i_cluster = results$j_cluster,
                                    j_cluster = results$i_cluster,
                                    count = results$count,
                                    stability = results$stability))

  # aggregate within clusters
  results_cluster = results %>%
    dplyr::group_by(i_cluster, j_cluster) %>%
    dplyr::summarize(stability = mean(stability)) %>%
    dplyr::ungroup()

  # output matrix target
  target_unique = names(clusters)[!is.na(clusters)]
  target_stability = matrix(nrow = length(target_unique),
                            ncol = length(target_unique),
                            dimnames = list(target_unique, target_unique))
  target_stability[cbind(results$i,results$j)] = results$stability
  diag(target_stability) = 1

  # output matrix cluster
  cluster_unique = na.omit(unique(clusters))
  cluster_stability = matrix(nrow = length(cluster_unique),
                             ncol = length(cluster_unique),
                             dimnames = list(cluster_unique, cluster_unique))
  cluster_stability[cbind(results_cluster$i_cluster,results_cluster$j_cluster)] = results_cluster$stability

  # out
  list(target_stability = target_stability,
       cluster_stability = cluster_stability)

}


