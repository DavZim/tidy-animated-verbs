#' Plots a set operation
#'
#' @param x a data_frame
#' @param title the title of the plot
#' @param xlims the xlimits
#' @param ylims the ylimits
#' @param ... further argument passed to plot_data
#'
#' @return a ggplot object
#'
#' @examples
#' NULL
plot_data_set <- function(x, title = "", xlims = xlim(1.5, 6.5), ylims = ylim(-3.5, -0.5), ...) {
  filter(x, label != "id") %>%
    plot_data(title, ...) +
    xlims + ylims
}
