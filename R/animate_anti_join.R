#' Animates an anti join between two datasets
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
#' animate_anti_join(x, y, "static")
#'
#' \donttest{
#'   # to save a png of the static image, use
#'   aj <- animate_anti_join(x, y, "static")
#'   ggsave("anti-join.png", aj)
#'
#'   # render a gif
#'   animate_anti_join(x, y)
#'
#'   # to save the gif, use
#'   aj <- animate_anti_join(x, y)
#'   anim_save("anti-join.gif", aj)
#' }
animate_anti_join <- function(x, y, result = "gif") {

  tidyAnimatedVerbs:::check_xy_format(x)
  tidyAnimatedVerbs:::check_xy_format(y)

  initial_join_dfs <- tidyAnimatedVerbs:::proc_data(x, "x") %>%
    bind_rows(mutate(tidyAnimatedVerbs:::proc_data(y, "y"), .x = .x + 3)) %>%
    mutate(frame = 1)

  initial_join_dfs <- initial_join_dfs %>%
    arrange(.x, .y) %>%
    mutate(.obj = row_number(), .obj = .obj + 90 * as.integer(.id == "y"))

  if (result == "gif") {

    aj_step2 <- initial_join_dfs %>%
      filter(.id == "x" | value %in% paste(1:2)) %>%
      mutate(frame = 2,
             .x = ifelse(.id == "y", 2.5, .x + 1.5),
             alpha = case_when(
               .x > 3 && .id == "x" ~ 0.5,
               .y > -2.5 ~ 0.25,
               TRUE ~ 1
             ))

    aj_step3 <- aj_step2 %>%
      filter(alpha == 1) %>%
      mutate(frame = 3)

    aj_step4 <- aj_step2 %>%
      filter(alpha == 1) %>%
      mutate(frame = 4, .y = -1)

    aj <- bind_rows(
      initial_join_dfs,
      aj_step2,
      aj_step3,
      aj_step4
    ) %>%
      mutate(
        alpha = ifelse(is.na(alpha), 1, alpha),
        .obj = ifelse(value == 4, 0, .obj)
      ) %>%
      arrange(.obj, frame) %>%
      tidyAnimatedVerbs:::plot_data("anti_join(x, y)") %>%
      tidyAnimatedVerbs:::animate_plot(transition_length = c(2, 1, 2),
                                       state_length = c(1, 0, 0, 1))

    res <- animate(aj)

  } else if (result == "static") {

    res <- anti_join(x, y, by = "id") %>%
      tidyAnimatedVerbs:::proc_data() %>%
      mutate(.x = .x + 1.5) %>%
      plot_data_join("anti_join(x, y)")

  }

  return(res)
}
