#' Animates a Plot
#'
#' @param x
#' @param transition_length
#' @param state_length
#'
#' @return
#'
#' @examples
animate_plot <- function(x, transition_length = 2, state_length = 1) {
  x +
    transition_states(frame, transition_length, state_length) +
    enter_fade() +
    exit_fade() +
    ease_aes("sine-in-out")
}
