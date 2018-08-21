#' Constructs the filename for a file and its extension
#'
#' @param x a file
#' @param ext the extension
#'
#' @return
#' @export
#'
#' @examples
#' static_plot_filename("my-file", "png")
#' static_plot_filename("my-file", "gif")
static_plot_filename <- function(x, ext) {
  here::here("images", "static", ext, paste0(x, ".", ext))
}
