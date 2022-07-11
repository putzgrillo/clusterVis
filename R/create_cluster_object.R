#' Create object of calculated and selected clusters (input for app creation)
#'
#' @param df                  data.frame: dataset with all variables (id and cluster variables)
#' @param k_test              numeric: vector of candidate amount of centers
#' @param fixed_variables     character: names of variables that should always be used
#' @param optional_variables  character: names of variables that could be included
#' @param best_n_by_variables numeric: choose best n best clusters by the amount of variables (if set to a value greater than available, bring all without repeating)
#' @param seed_value          numeric: seed for initial random assignment (if NULL, set to 193827)
#' @param nchar_id            numeric: n first digits to employ in the id variable
#' @param ...                 args to be passed to the wrapper_clusters function
#'
#' @return list with ...
#'
#' @export
#'
create_cluster_object <- function(df, k_test, fixed_variables, optional_variables,
                                  best_n_by_variables = 3, is_parallel = FALSE, n_cores = 3,
                                  seed_value = NULL, nchar_id = 4, ...) {
  # adjust
  if (is.null(k_test)) {k_test <- 2:5}
  if (is.null(seed_value)) {seed_value <- 193827}
  if (is.null(fixed_variables) | is.null(optional_variables)) {
    stop('fixed and optional variables should always be supplied')
  }

  # generate unique combinations
  combination_variables <- lapply(seq_along(optional_variables), function (x) {
    combinat::combn(optional_variables, x, simplify = FALSE)
  }) %>%
    unlist(., recursive = FALSE) %>%
    lapply(., function (x) {c(fixed_variables, x)}) %>%
    append(., list(fixed_variables))

  # apply
  if (is_parallel) {
    library(parallel)
    clusters <- mclapply(combination_variables, function(x) {
      wrapper_clusters(x = df %>% select(all_of(x)),
                       k_range = k_test,
                       idCluster = paste0(substr(x, 1, nchar_id), collapse = "|"),
                       seed = seed_value)
    }, mc.cores = n_cores)
  } else {
    clusters <- lapply(combination_variables, function(x)
      wrapper_clusters(x = df %>% select(all_of(x)),
                       k_range = k_test,
                       idCluster = paste0(substr(x, 1, nchar_id), collapse = "|"),
                       seed = seed_value)
    )
  }

  # bind the combinations
  clusters <- bind_rows(clusters) %>%
    arrange(desc(mean_silh))

  # best combinations
  best_clusters <- clusters %>%
    group_by(n_variables) %>%
    slice(seq(best_n_by_variables)) %>%
    mutate(
      selected = TRUE,
      id_key = paste(round(mean_silh, 3), method, id, sep = '<>') # key that will be displayed on shiny selector
    ) %>% ungroup()

  # summary table with all candidates
  df_all_candidates <- clusters %>%
    select(-cluster, -variables) %>%
    left_join({best_clusters %>% select(-cluster, -variables)},
              by = c("id", "k", "mean_silh", "method", "n_variables")) %>%
    mutate(selected = ifelse(is.na(selected), FALSE, selected))

  # output
  result <- list(
    original_df = df,
    cluster = best_clusters,
    summary_all_candidates = df_all_candidates
  )
return(result)
}
