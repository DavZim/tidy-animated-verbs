#' Colorize Keys
#'
#' @param df a data_frame
#' @param n_colors number of colors
#' @param key_col the key for the colors
#' @param color_other color for other
#' @param color_missing color for missing
#'
#' @return a data_frame
#'
#' @examples
#' NULL
colorize_keys <- function(df, n_colors, key_col = "id", color_other = "#d0d0d0", color_missing = "#ffffff") {
  # Assumes that key_col is integer
  colors <- scales::brewer_pal(type = "qual", "Set1")(n_colors)
  mutate(
    df,
    color = ifelse(label == key_col, value, n_colors + 1),
    color = colors[as.integer(color)],
    color = ifelse(is.na(color), "#d0d0d0", color),
    color = ifelse(is.na(value), "#ffffff", color)
  )
}
