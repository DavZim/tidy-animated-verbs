#' Animates an intersect between two datasets
#'
#' @param x the left dataset
#' @param y the right dataset
#' @param result gif or static, if static, only the last frame is shown.
#' @param title the title for the graphic
#' @param ... Further arguments such as \code{text_family} and \code{title_family} to specify the fonts
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
#' animate_intersect(x, y, "static")
#'
#' \donttest{
#'   # to save a png of the static image, use
#'   ins <- animate_intersect(x, y, "static")
#'   ggsave("intersect.png", ins)
#'
#'   # render a gif
#'   animate_intersect(x, y)
#'
#'   # to save the gif, use
#'   ins <- animate_intersect(x, y)
#'   anim_save("intersect.gif", ins)
#' }
animate_intersect <- function(x, y, result = "gif", title = "intersect(x, y)", ...) {

  tidyAnimatedVerbs:::check_xy_format(x, y)

  initial_set_dfs <- bind_rows(
    tidyAnimatedVerbs:::proc_data_set(x, "x"),
    tidyAnimatedVerbs:::proc_data_set(y, "y") %>% mutate(.x = .x + 3)
  ) %>%
    mutate(frame = 1)

  if (result == "gif") {
    ins_df <- dplyr::intersect(x, y)

    ins_step2 <- bind_rows(
        tidyAnimatedVerbs:::proc_data_set(ins_df, "x"),
        tidyAnimatedVerbs:::proc_data_set(ins_df, "y")
      ) %>%
      filter(.y == -1) %>%
      mutate(frame = 2, .x = .x + 1.5)

    ins <- initial_set_dfs %>%
      bind_rows(ins_step2) %>%
      arrange(desc(frame)) %>%
      tidyAnimatedVerbs:::plot_data_set(title, ...) %>%
      tidyAnimatedVerbs:::animate_plot()

    res <- animate(ins)

  } else if (result == "static") {

    res <- dplyr::intersect(x, y) %>%
      tidyAnimatedVerbs:::proc_data_set() %>%
      mutate(.x = .x + 1.5) %>%
      tidyAnimatedVerbs:::plot_data_set(title, ...)

  }

  return(res)
}
