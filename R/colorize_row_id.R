#' Colorizes Row ID
#'
#' @param df a data_frame
#' @param n_colors the number of colors
#' @param key_col the key column
#'
#' @return a data_frame
#'
#' @examples
#' NULL
colorize_row_id <- function(df, n_colors, key_col = "id") {
  # Assumes that key_col is integer
  colors <- scales::brewer_pal(type = "qual", "Set1")(n_colors)
  df$color <- colors[df[[key_col]]]
  df
}
