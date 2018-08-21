#' Plots a join of data
#'
#' @param x
#' @param title
#' @param xlims
#' @param ylims
#'
#' @return
#'
#' @examples
plot_data_join <- function(x, title = "", xlims = xlim(0.5, 5.5), ylims = ylim(-3.5, -0.5)) {
  x %>%
    filter(!is.na(value)) %>%
    plot_data(title) +
    xlims + ylims
}
