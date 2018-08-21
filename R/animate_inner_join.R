#' Animates an inner join between two datasets
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
#' animate_inner_join(x, y, "static")
#'
#' \donttest{
#'   # to save a png of the static image, use
#'   ij <- animate_inner_join(x, y, "static")
#'   ggsave("inner-join.png", ij)
#'
#'   # render a gif
#'   animate_inner_join(x, y)
#'
#'   # to save the gif, use use
#'   ij <- animate_inner_join(x, y)
#'   anim_save("inner-join.gif", ij)
#' }
animate_inner_join <- function(x, y, result = "gif", ...) {

  tidyAnimatedVerbs:::check_xy_format(x)
  tidyAnimatedVerbs:::check_xy_format(y)

  initial_join_dfs <- tidyAnimatedVerbs:::proc_data(x, "x") %>%
    bind_rows(mutate(tidyAnimatedVerbs:::proc_data(y, "y"), .x = .x + 3)) %>%
    mutate(frame = 1)

  if (result == "gif") {
    ij_joined_df <- inner_join(x, y, "id")

    ij_joined_df <- bind_rows(
      tidyAnimatedVerbs:::proc_data(ij_joined_df, "x"),
      tidyAnimatedVerbs:::proc_data(ij_joined_df, "y")
    ) %>%
      filter(!(label == "x" & .id == "y") & !(label == "y" & .id == "x")) %>%
      mutate(frame = 2, .x = .x + 1)

    ij <- bind_rows(
      initial_join_dfs,
      ij_joined_df
    ) %>%
      mutate(removed = value %in% c("3", "4", "x3", "y4"),
             removed = as.integer(removed)) %>%
      arrange(desc(frame), removed, desc(.id)) %>%
      tidyAnimatedVerbs:::plot_data("inner_join(x, y)", ...) %>%
      tidyAnimatedVerbs:::animate_plot()

    res <- animate(ij)

  } else if (result == "static") {

    res <- inner_join(x, y, by = "id") %>%
      tidyAnimatedVerbs:::proc_data() %>%
      mutate(.x = .x + 1) %>%
      tidyAnimatedVerbs:::plot_data_join("inner_join(x, y)", ...)
  }

  return(res)
}
