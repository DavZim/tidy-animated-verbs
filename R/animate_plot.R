#' Animates a Plot
#'
#' @param x an object as returned by \code{\link{plot_data}}
#' @param transition_length the length of the transition
#' @param state_length the length of the states
#'
#' @return an animated gif
#'
#' @examples
#' NULL
animate_plot <- function(x, transition_length = 2, state_length = 1) {
  x +
    transition_states(frame, transition_length, state_length) +
    enter_fade() +
    exit_fade() +
    ease_aes("sine-in-out")
}
