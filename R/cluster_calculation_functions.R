# CHOOSE CLUSTER

#' Aggregate results from cluster calls
#'
#' @param x              data.frame: dataset of variables to perform k-means
#' @param k_range        numeric: vector of candidate amount of centers
#' @param idCluster      character: how should be the iteration called?
#' @param methods        character: methods to call (currently only 'kmeans'&'hclust' and BOTH have to be called)
#' @param linkages       character: possible linkages to call for hclust
#' @param seed           numeric: seed for initial random assignment (if NULL, randomly chosen)
#'
#' @return tibble
#'
#' @export
#'
#'
wrapper_clusters <- function(x, k_range = 2:5,
                             idCluster = NULL,
                             methods = c('kmeans', 'hclust'),
                             linkages = NULL, seed = NULL) {
  # set seed
  if (is.null(seed)) {
    seed <- sample(x = seq(1000), size = 1)
  }
  # choose linkages
  if (is.null(linkages)) {
    linkages <- c('complete', 'average', 'ward.D')
  }
  # calculate distance
  distance_matrix <- dist(x, method = 'euclidean')

  # run methods
  # # run k-means
  if ('kmeans' %in% methods) {
    kmean_cluster <- choose_kmeans(x = x, k_range = k_range, cluster_seed = seed,
                                   diss_matrix = distance_matrix)
  }
  # # run hcluster
  if ('hclust' %in% methods) {
    hc_cluster <- vector('list', length(linkages))
    for (w in seq_along(hc_cluster)) {
      hc_cluster[[w]] <- choose_hc(x = x, k_range = k_range,
                                   linkage = linkages[w],
                                   diss_matrix = distance_matrix)
    }
    hc_cluster <- bind_rows(hc_cluster)
  }

  # # wrap methods into a single tibble (must be improved)
  clust <- bind_rows(kmean_cluster, hc_cluster)

  # adjust output
  # # id if null
  if (is.null(idCluster)) { idCluster <- NA }

  # # include variables
  result <- clust %>%
    mutate(
      id = idCluster,
      n_variables = ncol(x)
    ) %>%
    relocate(id, .before = k)
  result$variables <- list(colnames(x))

return(result)
}


#' Choose k-means cluster by maximal silhouette
#'
#' @param x              data.frame: dataset of variables to perform k-means
#' @param diss_matrix    distance matrix of the x (if null, it is computed)
#' @param k_range        numeric: vector of candidate amount of centers
#' @param cluster_seed    numeric: seed for initial random assignment (if NULL, randomly chosen)
#'
#' @return tibble with k, k-means object with amount of centers that maximise silhouette and mean silhouette
#'
#' @export
#'
#'
choose_kmeans <- function(x, k_range = 2:8, diss_matrix = NULL, cluster_seed = NULL) {
  # set seed to guarantee reproducibility
  if (is.null(cluster_seed)) {cluster_seed <- sample(x = seq(100), size = 1) }

  # calculate distance matrix / dissimilarity matrix
  if (is.null(diss_matrix)) {
    diss_matrix <- dist(x, method = 'euclidean')
  }
  # calculate metric
  k_clusters <- tibble(k = k_range) %>%
    mutate(
      cluster = map(k, ~kmeans(x, centers = .x)),
      silh = map(cluster, ~silhouette(x = .x$cluster, dist = diss_matrix)),
      mean_silh = map(silh, ~mean(.x[, 3]))
    )
  # choose best
  result <- k_clusters %>%
    unnest(cols = c(mean_silh)) %>%
    arrange(desc(mean_silh)) %>%
    slice(1) %>%
    select(-silh) %>%
    mutate(method = 'k-Means')

  # if returning only the best, generate kmeans with a seed
  set.seed(cluster_seed)
  result$cluster <- kmeans(x, centers = result$k)$cluster %>% list()

return(result)
}

#' Choose hierarchical cluster by maximal silhouette
#'
#' @param x              data.frame: dataset of variables to perform hierarchical clustering
#' @param diss_matrix    distance matrix of the x (if null, it is computed)
#' @param linkage        the agglomeration method of hclust (if null, complete linkage)
#' @param k_range        numeric: vector of candidate amount of centers
#'
#' @return tibble with k, hc clusters with amount of centers that maximise silhouette and mean silhouette
#'
#' @export
#'
#'
choose_hc <- function(x, k_range = 2:8, linkage = NULL, diss_matrix = NULL) {
  # not necessary to have a seed (no random initialization)
  # calculate distance matrix / dissimilarity matrix
  if (is.null(diss_matrix)) {
    diss_matrix <- dist(x, method = 'euclidean')
  }
  # linkage
  if (is.null(linkage)) {
    linkage <- 'complete'
  }
  # calculate metric
  hc_clusters <- tibble(k = k_range) %>%
    mutate(
      cluster = map(k, ~cutree(tree = hclust(d = diss_matrix, method = linkage), k = .x)),
      silh = map(cluster, ~silhouette(x = .x, dist = diss_matrix)),
      mean_silh = map(silh, ~mean(.x[, 3]))
    )
  # choose best
  result <- hc_clusters %>%
    unnest(cols = c(mean_silh)) %>%
    arrange(desc(mean_silh)) %>%
    slice(1) %>%
    select(-silh) %>%
    mutate(method = paste('HC', stringr::str_remove(linkage, pattern = '\\.'), sep = '-'))

return(result)
}

