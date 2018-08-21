#' Animates a full join between two datasets
#'
#' @param x the left dataset to join
#' @param y the right dataset to join
#' @param result gif or static, if static, only the last frame is shown.
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
#' animate_full_join(x, y, "static")
#'
#' \donttest{
#' to save a png of the static image, use
#' fj <- animate_full_join(x, y, "static")
#' ggsave("full-join.png", fj)
#'
#'  # render a gif
#'  animate_full_join(x, y)
#'
#'  # to save the gif, use
#'  fj <- animate_full_join(x, y)
#'  anim_save("full-join.gif", fj)
#' }
animate_full_join <- function(x, y, result = "gif") {

  tidyAnimatedVerbs:::check_xy_format(x)
  tidyAnimatedVerbs:::check_xy_format(y)

  initial_join_dfs <- tidyAnimatedVerbs:::proc_data(x, "x") %>%
    bind_rows(mutate(tidyAnimatedVerbs:::proc_data(y, "y"), .x = .x + 3)) %>%
    mutate(frame = 1)

  if (result == "gif") {
    fj_joined_df <- full_join(x, y, "id") %>%
      tidyAnimatedVerbs:::proc_data("x") %>%
      mutate(.id = ifelse(value %in% c("4", "y4"), "y", .id)) %>%
      mutate(frame = 2, .x = .x + 1)

    fj_extra_blocks <- inner_join(x, y, "id") %>%
      select(id) %>%
      tidyAnimatedVerbs:::proc_data("y") %>%
      mutate(frame = 2, .x = .x + 1)

    fj <- initial_join_dfs %>%
      bind_rows(fj_joined_df, fj_extra_blocks) %>%
      tidyAnimatedVerbs:::plot_data("full_join(x, y)") +
      transition_states(frame, transition_length = 2, state_length = 1) +
      enter_appear() +
      exit_disappear(early = TRUE) +
      ease_aes("sine-in-out")

    res <- animate(fj)

  } else if (result == "static") {

    res <- full_join(x, y, "id") %>%
      tidyAnimatedVerbs:::proc_data() %>%
      mutate(.x = .x + 1) %>%
      tidyAnimatedVerbs:::plot_data_join("full_join(x, y)",
                                         ylims = ylim(-4.5, -0.5))
  }

  return(res)
}
