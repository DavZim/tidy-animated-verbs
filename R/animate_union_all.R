#' Animates a union-all between two datasets
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
#' animate_union_all(x, y, "static")
#'
#' \donttest{
#'   # to save a png of the static image, use
#'   un <- animate_union_all(x, y, "static")
#'   ggsave("union.png", un)
#'
#'   # render a gif
#'   animate_union_all(x, y)
#'
#'   # to save the gif, use
#'   un <- animate_union_all(x, y)
#'   anim_save("union.gif", un)
#' }
animate_union_all <- function(x, y, result = "gif") {

  tidyAnimatedVerbs:::check_xy_format(x, y)

  initial_set_dfs <- bind_rows(
    tidyAnimatedVerbs:::proc_data_set(x, "x"),
    tidyAnimatedVerbs:::proc_data_set(y, "y") %>% mutate(.x = .x + 3)
  ) %>%
    mutate(frame = 1)

  if (result == "gif") {
    ua <- bind_rows(
      initial_set_dfs,
      initial_set_dfs %>%
        mutate(frame = 2, .y = ifelse(.id == "y", .y - 3, .y)), # fly y down
      tidyAnimatedVerbs:::proc_data_set(x, "ux") %>%
        mutate(frame = 3, .x = .x + 1.5),                # merge
      tidyAnimatedVerbs:::proc_data_set(y, "uy") %>%
        mutate(frame = 3, .x = .x + 1.5, .y = .y - 3),   # un-merge
      initial_set_dfs %>%
        mutate(frame = 4, .y = ifelse(.id == "y", .y - 3, .y))  # fly y up
    ) %>%
      arrange(desc(frame)) %>%
      tidyAnimatedVerbs:::plot_data_set("union_all(x, y)",
                                        ylims = ylim(-5.5, -0.5)) +
      transition_states(frame, 1, c(1, 0, 1, 0))

    res <- animate(ua)

  } else if (result == "static") {

    res <- union_all(x, y) %>%
      tidyAnimatedVerbs:::proc_data_set() %>%
      mutate(.x = .x + 1.5) %>%
      tidyAnimatedVerbs:::plot_data_set("union_all(x, y)",
                                        ylims = ylim(-5.5, -0.5))
  }

  return(res)
}
