#' Cluster targets
#'
#' @param associations an \code{associatoR} object including target_embeddings.
#' @param method a \code{character} specifying the clustering method. One of \code{c("louvain","hclust","kmeans","dbscan")}. Default is \code{"louvain"}.
#' @param similarity a \code{character} specifying the similarity metric. One of \code{c("arccos","cosine","euclidean")}. Default is \code{"arccos"}.
#' @param k an \code{integer} specifying the number of clusters for \code{method = c("hclust","kmeans")}.
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
ar_cluster_targets <- function(associations, method = c("louvain"), similarity = "arccos", k = NULL, linkage = NULL, eps = NULL, ...) {

  # check inputs
  check_object(associations)
  check_targets(associations)
  check_embeddings(associations)
  chk::chk_subset(method, c("louvain","hclust","kmeans","dbscan"))
  chk::chk_subset(similarity, c("arccos","cosine","euclidean"))

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

  # hclust
  if(method == "kmeans"){

    # get clusters
    clusters = stats::kmeans(emb, centers = k, ...)
    targets_clusters <- tibble::tibble(target = names(clusters$cluster),
                                       cluster = clusters$cluster)

  }

  # hclust
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
    clusters = dbscan::kmeans(sim, eps = eps, ...)
    targets_clusters <- tibble::tibble(target = rownames(emb),
                                       cluster = clusters$cluster)

  }

  # add clusters to targets tibble
  associations$targets <- associations$targets %>%
    dplyr::left_join(targets_clusters, by = "target")

  # out
  associations

}
