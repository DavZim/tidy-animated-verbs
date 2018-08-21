#' Plots a set operation
#'
#' @param x
#' @param title
#' @param xlims
#' @param ylims
#'
#' @return
#'
#' @examples
plot_data_set <- function(x, title = "", xlims = xlim(1.5, 6.5), ylims = ylim(-3.5, -0.5), ...) {
  filter(x, label != "id") %>%
    plot_data(title, ...) +
    xlims + ylims
}
