#' Colorizes Row ID
#'
#' @param df
#' @param n_colors
#' @param key_col
#'
#' @return
#'
#' @examples
colorize_row_id <- function(df, n_colors, key_col = "id") {
  # Assumes that key_col is integer
  colors <- scales::brewer_pal(type = "qual", "Set1")(n_colors)
  df$color <- colors[df[[key_col]]]
  df
}
