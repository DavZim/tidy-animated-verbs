#' Animates a setdiff operation between two datasets
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
#' animate_setdiff(x, y, "static")
#'
#' \donttest{
#'   # to save a png of the static image, use
#'   setd <- animate_setdiff(x, y, "static")
#'   ggsave("setdiff.png", setd)
#'
#'   # render a gif
#'   animate_setdiff(x, y)
#'
#'   # to save the gif, use
#'   setd <- animate_setdiff(x, y)
#'   anim_save("setdiff.gif", setd)
#' }
animate_setdiff <- function(x, y, result = "gif") {

  tidyAnimatedVerbs:::check_xy_format(x, y)

  initial_set_dfs <- bind_rows(
    tidyAnimatedVerbs:::proc_data_set(x, "x"),
    tidyAnimatedVerbs:::proc_data_set(y, "y") %>% mutate(.x = .x + 3)
  ) %>%
    mutate(frame = 1)

  if (result == "gif") {

    setd_step2 <- initial_set_dfs %>%
      mutate(
        frame = 2,
        alpha = case_when(
          .y == -1 ~ 0.55,
          .id == "y" ~ 0.15,
          TRUE ~ 1
        )
      )

    # Merge, dim overlapping elements
    setd_step3 <- initial_set_dfs %>%
      filter(!(.id == "y" & .y == -2)) %>%
      mutate(
        frame = 3,
        alpha = ifelse(.y == -1, 0.25, 1),
        .x = ifelse(.id == "y", .x - 3, .x),
        .x = .x + 1.5
      )

    # Result of setdiff
    setd_step4 <- setdiff(x, y) %>%
      tidyAnimatedVerbs:::proc_data_set("xy") %>%
      mutate(frame = 4, .x = .x + 1.5)

    setd <- bind_rows(
      initial_set_dfs,
      setd_step2,
      setd_step3,
      setd_step4
    ) %>%
      mutate(alpha = ifelse(is.na(alpha), 1, alpha)) %>%
      arrange(frame, desc(.y), desc(.id)) %>%
      tidyAnimatedVerbs:::plot_data_set(., "setdiff(x, y)") %>%
      tidyAnimatedVerbs:::animate_plot()

    res <- animate(setd)

  } else if (result == "static") {

    res <- dplyr::setdiff(x, y) %>%
      tidyAnimatedVerbs:::proc_data_set() %>%
      mutate(.x = .x + 1.5) %>%
      tidyAnimatedVerbs:::plot_data_set("setdiff(x, y)")

  }

  return(res)
}
