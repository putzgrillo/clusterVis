#' Bind transformed version of data
#'
#' @param x              data.frame: dataset variables to be transformed and appended
#' @param variableNames  character: variables that must be transformed. if NULL, all variables are transformed
#' @param logTransform   logical: should the variables be log transformed?
#' @param rangeTransform logical: should the variables be range tranformed?
#' @param suffix         character: what should be suffix of transformed variables
#'
#' @return original data.frame with transformed variables binded
#'
#' @export
#'
transform_data <- function(x, variableNames, logTransform = T, rangeTransform = T, suffix = "_transf") {
  # create data.frame with all variables to be transformed
  if (is.null(variableNames)) {
    x_transform <- x
  } else {
    x_transform <- x %>% select(all_of(variableNames))
  }
  # transform and bind
  nomes <- paste(colnames(x_transform), suffix, sep = "")
  if (logTransform) { x_transform <- lapply(x_transform, function(y) { log(y + 1) }) }
  if (rangeTransform) { x_transform <- lapply(x_transform, function(y) { (y-min(y)) / (max(y)-min(y)) }) }
  names(x_transform) <- nomes
  x_transform <- x_transform %>% bind_cols()

  # result
  resultado <- bind_cols(x, x_transform)
return(resultado)
}
