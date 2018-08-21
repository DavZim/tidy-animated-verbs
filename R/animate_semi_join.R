#' Animates a semi join between two datasets
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
#' animate_semi_join(x, y, "static")
#'
#' \donttest{
#'   # to save a png of the static image, use
#'   sj <- animate_semi_join(x, y, "static")
#'   ggsave("semi-join.png", rj)
#'
#'   # render a gif
#'   animate_semi_join(x, y)
#'
#'   # to save the gif, use
#'   sj <- animate_semi_join(x, y)
#'   anim_save("semi-join.gif", sj)
#' }
animate_semi_join <- function(x, y, result = "gif", ...) {

  tidyAnimatedVerbs:::check_xy_format(x)
  tidyAnimatedVerbs:::check_xy_format(y)

  initial_join_dfs <- tidyAnimatedVerbs:::proc_data(x, "x") %>%
    bind_rows(mutate(tidyAnimatedVerbs:::proc_data(y, "y"), .x = .x + 3)) %>%
    mutate(frame = 1)

  if (result == "gif") {

    sj_joined_df <- semi_join(x, y, "id") %>%
      tidyAnimatedVerbs:::proc_data("x") %>%
      mutate(frame = 2, .x = .x + 1.5)

    sj_extra_blocks <- inner_join(x, y, "id") %>%
      select(id) %>%
      tidyAnimatedVerbs:::proc_data("y") %>%
      mutate(frame = 2, .x = .x + 1.5)

    sj <- bind_rows(
      initial_join_dfs,
      sj_joined_df,
      sj_extra_blocks
    ) %>%
      arrange(value) %>%
      tidyAnimatedVerbs:::plot_data("semi_join(x, y)", ...) %>%
      tidyAnimatedVerbs:::animate_plot()

    res <- animate(sj)

  } else if (result == "static") {

    res <- semi_join(x, y, "id") %>%
      tidyAnimatedVerbs:::proc_data() %>%
      mutate(.x = .x + 1.5) %>%
      tidyAnimatedVerbs:::plot_data_join("semi_join(x, y)", ...)

  }

  return(res)
}
