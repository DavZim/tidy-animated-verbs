
#' Plots the Data
#'
#' @param x
#' @param title
#'
#' @return
#' @export
#'
#' @examples
plot_data <- function(x, title = "") {
  if (!"alpha" %in% colnames(x)) x$alpha <- 1
  if (!".width" %in% colnames(x)) x$`.width` <- 1

  ggplot(x) +
    aes(.x, .y, fill = color, label = value) +
    geom_tile(aes(width = .width, alpha = alpha), color = "white", size = 3) +
    geom_text(data = x %>% filter(!is.na(value)),
              aes(x = .x), hjust = 0.5, size = 12, family = "Fira Sans", color = "white") +
    scale_fill_identity() +
    scale_alpha_identity() +
    coord_equal() +
    ggtitle(title) +
    theme_void() +
    theme(plot.title = element_text(family = "Fira Mono", hjust = 0.5, size = 24)) +
    guides(fill = FALSE)
}
