# app helpers

#' generate a table summary with statistics of numeric variables
#'
#' @param df_tbl          data.frame: dataset to be summarised
#' @param grp             character: name of the variable used to group by (size 1 only)
#'
#' @return a summarised table
#'
#' @export
app_helper_summary_table <- function(df_tbl, grp) {
  # if it needs to use multiple variables in grp, use unite() to create a grouped variable

  # generate summary statistics
  df_tbl %>%
    dplyr::rename(grp = all_of(grp)) %>%
    dplyr::group_by(grp) %>%
    dplyr::summarise(
      freq = dplyr::n(),
      dplyr::across(.cols = where(is.numeric),
                    .fns = list(mean = ~mean(.x, na.rm = TRUE),
                                std = ~sd(.x, na.rm = TRUE),
                                median = ~median(.x, na.rm = TRUE),
                                q1 = ~quantile(.x, probs = 0.25),
                                q3 = ~quantile(.x, probs = 0.75)),
                    .names = '{.col}<>{.fn}'
      )
    ) %>%
    dplyr::mutate(rel_freq = freq / sum(freq)) %>%
    dplyr::relocate(rel_freq, .after = freq) %>%
    tidyr::pivot_longer(-grp, names_to = 'name', values_to = 'value') %>%
    # round numeric variables
    dplyr::mutate(
      dplyr::across(.cols = where(is.numeric),
                    .fns = ~round(.x, digits = 4))
    ) %>%
    # separate name from function
    dplyr::mutate(
      strings = stringr::str_extract_all(name, "[^<>]+"),
      variable = map(.x = strings, .f = pluck, 1) %>% unlist(),
      fn = map(.x = strings, .f = pluck, last) %>% unlist()
    ) %>% dplyr::select(grp, variable, fn, value) %>%
    tidyr::pivot_wider(names_from = grp, values_from = value)

}


#' generate a 3d interactive plot with hover
#'
#' @param df_plot         data.frame: dataset of variables to plot
#' @param x_axis          character: name of numerical variable to plot on x axis
#' @param y_axis          character: name of numerical variable to plot on y axis
#' @param z_axis          character: name of numerical variable to plot on z axis
#' @param class_variable  character: categorical variable to plot on the colour parameter
#' @param size_variable   character: name of numerical variable to use as size, if NULL, an empty class is created.
#' @param range_sizes     numeric: a vector of size two with minimum and maximum size
#'
#' @return a plotly object
#'
#' @export
# input <- NULL
# input$cluster <- clusters_obj$cluster$id_key[3]
# input$varPlotX <- 'valence'
# input$varPlotY <- 'energy'
# input$varPlotZ <- 'danceability'
# input$clusterX <- 'valence'
# input$clusterY <- 'energy'
# input$clusterZ <- 'danceability'
# input$varPlotBox <- 'valence'
app_helper_3d_scatter <- function(df_plot, x_axis, y_axis, z_axis, class_variable,
                                  size_variable = NULL, range_sizes = c(200, 300)) {
  if (is.null(size_variable)) {
    size_variable <- 'weight'
    df_plot <- df_plot %>%
      dplyr::mutate(weight = 1)
  }
  # adjust the data for the plot
  df_plot <- df_plot %>%
    dplyr::select(all_of(c(y_axis, x_axis, z_axis, class_variable, size_variable))) %>%  # selected in order
    purrr::set_names(nm = c('.Y', '.X', '.Z', '.C', '.S')) %>%
    dplyr::mutate(.C = as_factor(.C))

  # generate the plot
  plot_ly(df_plot,
          x = ~.X,
          y = ~.Y,
          z = ~.Z,
          type = "scatter3d", mode = "markers", color = ~.C, size = ~.S, sizes = range_sizes,
          hoverinfo = 'text',
          text = ~paste(
            '</br>', class_variable, ': ', .C,
            '</br>', x_axis, ': ', round(.X, 2),
            '</br>', y_axis, ': ', round(.Y, 2),
            '</br>', z_axis, ': ', round(.Z, 2),
            '</br>', size_variable, ': ', round(.S, 3), sep = ''
          )
  ) %>%
    layout(
      title = "",
      scene = list(
        xaxis = list(title = paste('x: ', x_axis, sep = '')),
        yaxis = list(title = paste('y: ', y_axis, sep = '')),
        zaxis = list(title = paste('z: ', z_axis, sep = ''))
      ))
}



#' generate a interactive box-plot with hover
#'
#' @param df_plot         data.frame: dataset of variables to plot
#' @param y_axis          character: name of numerical variable to plot on y axis
#' @param class_variable  character: categorical variable to plot on the colour parameter
#'
#' @return a plotly object
#'
#' @export
# input <- NULL
# input$cluster <- clusters_obj$cluster$id_key[3]
# input$varPlotX <- 'valence'
# input$varPlotY <- 'energy'
# input$varPlotZ <- 'danceability'
# input$clusterX <- 'valence'
# input$clusterY <- 'energy'
# input$clusterZ <- 'danceability'
# input$varPlotBox <- 'valence'
app_helper_boxplot <- function(df_plot, y_axis, class_variable) {
  y_axis = 'energy'
  class_variable = 'Cluster'
  # adjust the data for the plot
  df_plot <- df_plot %>%
    select(all_of(c(y_axis, class_variable))) %>%  # selected in order
    set_names(nm = c('.Y', '.C')) %>%
    mutate(.C = as_factor(.C))

  # generate the ggplot
  ggplotBox <- ggplot(df_plot, aes_string(x = ".C", y = ".Y", fill = ".C")) +
    geom_boxplot() +
    labs(x = class_variable, y = y_axis) +
    theme_bw()

  # make the plot interactive
  ggplotly(ggplotBox)
}


#' generate a interactive 3d scatter plot with surface calculated by nnet
#'
#' @param df_plot         data.frame: dataset of variables to plot
#' @param z_axis          character: name of numerical variable to plot on z axis and to be explained (dependent variable)
#' @param y_axis          character: name of numerical variable to plot on y axis (independent variable)
#' @param x_axis          character: name of numerical variable to plot on z axis (independent variable)
#' @param class_variable  character: categorical variable to plot on the colour parameter
#' @param shape_variable   character: name of numerical variable to use as size, if NULL, an empty class is created.
#'
#' @return a plotly object
#'
#' @export
app_helper_surface_3d_scatter <- function(df_plot, z_axis, x_axis, y_axis,
                                          class_variable, shape_variable) {
  # adjust the data for the plot
  df_plot <- df_plot %>%
    select(all_of(c(y_axis, x_axis, z_axis, class_variable, shape_variable))) %>%  # selected in order
    set_names(nm = c('.Y', '.X', '.Z', '.C', '.S')) %>%
    mutate(.C = as_factor(.C))

  # train a neural network to evaluate
  model_surface <- nnet::nnet.formula(.Z ~ .X + .Y,
                                      size = 10,
                                      data = df_plot,
                                      trace = FALSE)

  # predict all possible combinations
  comb_x <- seq(min(df_plot$.X), max(df_plot$.X), by = 1)
  comb_y <- seq(min(df_plot$.Y), max(df_plot$.Y), by = 1)
  scatter_surface <- expand.grid(.X = comb_x, .Y = comb_y, KEEP.OUT.ATTRS = F)
  scatter_surface$.Z <- predict(model_surface, newdata = scatter_surface)

  scatter_surface <- scatter_surface %>%
    pivot_wider(names_from = .X, values_from = .Z) %>%  #
    column_to_rownames('.Y') %>% as.matrix()

  # generate the plot
  plot_ly(df_plot,
          x = ~.X,
          y = ~.Y,
          z = ~.Z,
          type = "scatter3d", mode = "markers", color = ~.C,
          symbol = ~.S, symbols = c('circle', 'diamond'),
          hoverinfo = 'text',
          text = ~paste(
            '</br>', class_variable, ': ', .C,
            '</br>', x_axis, ': ', round(.X, 2),
            '</br>', y_axis, ': ', round(.Y, 2),
            '</br>', z_axis, ': ', round(.Z, 2),
            '</br>', shape_variable, ': ', .S, sep = ''
          )
  ) %>%
    layout(
      title = "",
      legend = list(orientation = 'h'),
      scene = list(
        xaxis = list(title = paste('x: ', x_axis, sep = '')),
        yaxis = list(title = paste('y: ', y_axis, sep = '')),
        zaxis = list(title = paste('z: ', z_axis, sep = ''))
      )) %>%
    add_trace(
      z = ~scatter_surface,
      x = ~comb_x,
      y = ~comb_y,
      type = 'surface',
      alpha = 0.2)
}
