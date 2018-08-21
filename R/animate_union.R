#' Animates a union between two datasets
#'
#' @param x the left dataset
#' @param y the right dataset
#' @param result gif or static, if static, only the last frame is shown.
#'
#' @return a gif or a ggplot image
#' @export
#'
#' @examples
#' x <- data_frame(
#'   id = 1:3,
#'   x = c(1, 1, 2),
#'   y = c("a", "b", "a")
#' )
#'
#' y <- data_frame(
#'   id = c(1, 4),
#'   x = c(1, 2),
#'   y = c("a", "b")
#' )
#'
#' # render the static image
#' animate_union(x, y, "static")
#'
#' \donttest{
#'   # to save a png of the static image, use
#'   un <- animate_union(x, y, "static")
#'   ggsave("union.png", un)
#'
#'   # render a gif
#'   animate_union(x, y)
#'
#'   # to save the gif, use
#'   un <- animate_union(x, y)
#'   anim_save("union.gif", un)
#' }
animate_union <- function(x, y, result = "gif") {

  tidyAnimatedVerbs:::check_xy_format(x, y)

  initial_set_dfs <- bind_rows(
    tidyAnimatedVerbs:::proc_data_set(x, "x"),
    tidyAnimatedVerbs:::proc_data_set(y, "y") %>% mutate(.x = .x + 3)
  ) %>%
    mutate(frame = 1)

  if (result == "gif") {
    un <- bind_rows(
      initial_set_dfs,
      union(x, y) %>%
        tidyAnimatedVerbs:::proc_data_set("xy") %>%
        mutate(frame = 2, .x = .x + 1.5),
      dplyr::intersect(x, y) %>%
        tidyAnimatedVerbs:::proc_data_set("xy") %>%
        mutate(frame = 2, .y = -4, .x = .x + 1.5)
    ) %>%
      tidyAnimatedVerbs:::plot_data_set("union(x, y)",
                                        ylims = ylim(-4.5, -0.5)) %>%
      tidyAnimatedVerbs:::animate_plot()

    res <- animate(un)

  } else if (result == "static") {

    res <- dplyr::union(x, y) %>%
      tidyAnimatedVerbs:::proc_data_set() %>%
      mutate(.x = .x + 1.5) %>%
      tidyAnimatedVerbs:::plot_data_set("union(x, y)", ylims = ylim(-0.5, -4.5))

  }

  return(res)
}
