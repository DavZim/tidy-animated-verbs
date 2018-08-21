#' Title
#'
#' @param x
#' @param y
#' @param result gif or static, if static, only the last frame is shown.
#'
#' @return
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
#'  # render a gif
#'  animate_full_join(x, y)
#'
#'  # to save the gif you can use
#'  fj <- animate_full_join(x, y)
#'  anim_save(here::here("images", "full-join.gif"), fj)
#' }
animate_full_join <- function(x, y, result = "gif") {

  check_xy_format(x)
  check_xy_format(y)

  initial_join_dfs <- proc_data(x, "x") %>%
    bind_rows(mutate(proc_data(y, "y"), .x = .x + 3)) %>%
    mutate(frame = 1)

  if (result == "gif") {
    fj_joined_df <- full_join(x, y, "id") %>%
      proc_data("x") %>%
      mutate(.id = ifelse(value %in% c("4", "y4"), "y", .id)) %>%
      mutate(frame = 2, .x = .x + 1)

    fj_extra_blocks <- inner_join(x, y, "id") %>%
      select(id) %>%
      proc_data("y") %>%
      mutate(frame = 2, .x = .x + 1)

    fj <- initial_join_dfs %>%
      bind_rows(fj_joined_df, fj_extra_blocks) %>%
      plot_data("full_join(x, y)") +
      transition_states(frame, transition_length = 2, state_length = 1) +
      enter_appear() +
      exit_disappear(early = TRUE) +
      ease_aes("sine-in-out")

    res <- animate(fj)
    # anim_save(here::here("images", "full-join.gif"), fj)

  } else if (result == "static") {

    res <- full_join(x, y, "id") %>%
      proc_data() %>%
      mutate(.x = .x + 1) %>%
      plot_data_join("full_join(x, y)", ylims = ylim(-4.5, -0.5))
  }

  return(res)
}
