#' Plots a join of data
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
plot_data_join <- function(x, title = "", xlims = xlim(0.5, 5.5), ylims = ylim(-3.5, -0.5), ...) {
  x %>%
    filter(!is.na(value)) %>%
    plot_data(title, ...) +
    xlims + ylims
}
