#' Plots a join of data
#'
#' @param x
#' @param title
#' @param xlims
#' @param ylims
#'
#' @return
#' @export
#'
#' @examples
plot_data_join <- function(x, title = "", xlims = xlim(0.5, 5.5), ylims = ylim(-3.5, -0.5)) {
  plot_data(x, title) +
    xlims + ylims
}
