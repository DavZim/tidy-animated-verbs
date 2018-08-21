#' Saves a static plot into different folders (see \code{\link{static_plot_filename}})
#'
#' @param g a ggplot object as returned by \code{\link{plot_data}}
#' @param filename the filename
#' @param formats the format for the files
#' @param width the width of the plot
#' @param height the height of the plot
#'
#' @return NULL
#' @export
#'
#' @examples
#' NULL
save_static_plot <- function(g, filename, formats = c("png", "svg"), width = 7,
                             height = 7) {
  filenames <- formats %>%
    purrr::set_names() %>%
    purrr::map_chr(tidyAnimatedVerbs:::static_plot_filename, x = filename) %>%
    purrr::iwalk(
      ~ ggsave(filename = .x, plot = g, dev = .y, height = height, width = width)
    )
}
