#' Animates a right join between two datasets
#'
#' @param x the left dataset to join
#' @param y the right dataset to join
#' @param result gif or static, if static, only the last frame is shown.
#' @param ... Further arguments such as \code{text_family} and \code{title_family} to specify the fonts
#'
#' @return a gif or a ggplot image
#' @export
#'
#' @examples
#' x <- data_frame(
#'   id = 1:3,
#'   x = paste0("x", 1:3)
#' )
#'
#' y <- data_frame(
#'   id = (1:4)[-3],
#'   y = paste0("y", (1:4)[-3])
#' )
#'
#' # render the static image
#' animate_right_join(x, y, "static")
#'
#' \donttest{
#'   # to save a png of the static image, use
#'   rj <- animate_right_join(x, y, "static")
#'   ggsave("right-join.png", rj)
#'
#'   # render a gif
#'   animate_right_join(x, y)
#'
#'   # to save the gif, use
#'   rj <- animate_right_join(x, y)
#'   anim_save("right-join.gif", rj)
#' }
animate_right_join <- function(x, y, result = "gif", ...) {

  tidyAnimatedVerbs:::check_xy_format(x)
  tidyAnimatedVerbs:::check_xy_format(y)

  initial_join_dfs <- tidyAnimatedVerbs:::proc_data(x, "x") %>%
    bind_rows(mutate(tidyAnimatedVerbs:::proc_data(y, "y"), .x = .x + 3)) %>%
    mutate(frame = 1)

  rj_joined_dfs <- right_join(x, y, "id") %>%
    tidyAnimatedVerbs:::proc_data("y") %>%
    mutate(frame = 2, .x = .x + 1)

  if (result == "gif") {

    rj_extra_blocks <- inner_join(x, y, "id") %>%
      select(id) %>%
      tidyAnimatedVerbs:::proc_data("x") %>%
      mutate(frame = 2, .x = .x + 1)

    rj <- bind_rows(
      initial_join_dfs,
      rj_joined_dfs,
      rj_extra_blocks
    ) %>%
      filter(!is.na(value)) %>%
      mutate(
        .id = ifelse(label == "x", label, .id),
        removed = as.integer(grepl("3", value))
      ) %>%
      arrange(removed, value, .id, frame) %>%
      tidyAnimatedVerbs:::plot_data("right_join(x, y)", ...) %>%
      tidyAnimatedVerbs:::animate_plot()

    res <- animate(rj)

  } else if (result == "static") {

    res <- tidyAnimatedVerbs:::plot_data(rj_joined_dfs, "right_join(x, y)", ...)

  }

  return(res)
}
