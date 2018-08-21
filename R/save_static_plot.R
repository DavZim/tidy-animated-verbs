#' Saves a static plot
#'
#' @param g
#' @param filename
#' @param formats
#'
#' @return
#'
#' @examples
save_static_plot <- function(g, filename, formats = c("png", "svg")) {
  filenames <- formats %>%
    purrr::set_names() %>%
    purrr::map_chr(static_plot_filename, x = filename) %>%
    purrr::iwalk(
      ~ ggsave(filename = .x, plot = g, dev = .y)
    )
}
