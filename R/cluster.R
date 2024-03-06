#' Cluster targets
#'
#' @param associations an \code{associatoR} object including target_embeddings.
#' @param method a \code{character} specifying the clustering method. One of \code{c("louvain")}. Default is \code{"louvain"}.
#' @param ... additional attributes passed on to the clustering function.
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
ar_cluster_targets <- function(associations, method = c("louvain"), ...) {

  # check inputs
  chk::chk_s3_class(associations, "associatoR")
  chk::chk_subset("target_embedding", names(associations))
  chk::chk_subset(method, c("louvain"))

  # select clustering algorithm
  switch (method,
    "louvain" = {

      # calculate cosine similarity matrix
      emb <- associations$target_embedding[, -1] %>% as.matrix()
      emb <- t(emb)
      colnames(emb) <- associations$target_embedding[, 1] %>% dplyr::pull(target)
      sim <- lsa::cosine(emb)

      # convert to graph and run louvain clustering
      g <- igraph::graph_from_adjacency_matrix(sim,
                                               mode = "undirected",
                                               weighted = TRUE,
                                               diag = FALSE,
                                               add.colnames = NULL)

      # set negative cos to 0
      igraph::edge_attr(g, "weight")[igraph::edge_attr(g, "weight") < 0] <- 0

      # detect clusters using louvain algorithm
      clusters <- igraph::cluster_louvain(g, ...)

      # extract membership to clusters
      targets_clusters <- tibble::tibble(target = clusters$names,
                                         cluster = clusters$membership)

    }
  )

  # add clusters to targets tibble
  associations$targets <- associations$targets %>%
    dplyr::left_join(targets_clusters, by = "target")

  # out
  associations

}
