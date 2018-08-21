#' Plots a dataset
#'
#' @param x a dataset as prepared by \code{\link{proc_data}}
#' @param title a title for the plot
#' @param ... additional arguments such as title_family and text_family
#'
#' @return
#'
#' @examples
#' x <- data_frame(
#'   id = 1:3,
#'   x = paste0("x", 1:3)
#' )
#'
#' x %>%
#'   tidyAnimatedVerbs:::proc_data() %>%
#'   tidyAnimatedVerbs:::plot_data(title = "sample data",
#'                                 text_family = "Times New Roman",
#'                                 title_family = "Times New Roman")
plot_data <- function(x, title = "", ...) {

  dots <- list(...)

  if ("text_family" %in% names(dots)) {
    text_family <- dots$text_family
  } else {
    text_family <- "Fira Sans"
  }

  if ("title_family" %in% names(dots)) {
    title_family <- dots$title_family
  } else {
    title_family <- "Fira Mono"
  }

  if (!"alpha" %in% colnames(x)) x$alpha <- 1
  if (!".width" %in% colnames(x)) x$`.width` <- 1

  ggplot(x) +
    aes(.x, .y, fill = color, label = value) +
    geom_tile(aes(width = .width, alpha = alpha), color = "white", size = 3) +
    geom_text(data = x %>% filter(!is.na(value)),
              aes(x = .x), hjust = 0.5, size = 12, family = text_family, color = "white") +
    scale_fill_identity() +
    scale_alpha_identity() +
    coord_equal() +
    ggtitle(title) +
    theme_void() +
    theme(plot.title = element_text(family = title_family, hjust = 0.5, size = 24)) +
    guides(fill = FALSE)
}
