#' Animates a Plot
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
animate_plot <- function(x) {
  x +
    transition_states(frame, transition_length, state_length) +
    enter_fade() +
    exit_fade() +
    ease_aes("sine-in-out")
}
