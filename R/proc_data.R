#' Processes the data
#'
#' @param x a data_frame
#' @param .id the id column
#' @param color_fun the function to colorize the data
#' @param color_when when should the coloring be applied
#' @param ... further arguments passed to color_fun
#'
#' @return a data_frame
#'
#' @examples
#' NULL
proc_data <- function(x, .id = "x", color_fun = colorize_keys,
                      color_when = c("after", "before"), ...) {
  color_when <- match.arg(color_when)
  n_colors <- max(x$id)

  if (color_when == "before") x <- color_fun(x, n_colors, ...)

  x <- x %>%
    mutate(.y = -row_number()) %>%
    tidyr::gather("label", "value", setdiff(colnames(x), c(".y", "color"))) %>%
    mutate(value = as.character(value)) %>%
    group_by(.y) %>%
    mutate(
      .x = 1:n(),
      .id = .id,
      .width = 1
    ) %>%
    ungroup(.y)

  if (color_when == "after") x <- color_fun(x, n_colors, ...)
  x
}
