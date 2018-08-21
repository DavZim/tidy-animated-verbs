#' Processes a dataset for set operations
#'
#' @param x
#' @param .id
#'
#' @return
#'
#' @examples
proc_data_set <- function(x, .id = "x") {
  proc_data(x, .id, colorize_row_id, "before")
}
