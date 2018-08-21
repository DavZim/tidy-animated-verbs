#' Processes the data
#'
#' @param x
#' @param .id
#' @param color_fun
#' @param color_when
#' @param ...
#'
#' @return
#'
#' @examples
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
