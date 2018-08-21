#' Processes a dataset for set operations
#'
#' @param x a data_frame
#' @param .id an id
#'
#' @return a data_frame
#'
#' @examples
#' NULL
proc_data_set <- function(x, .id = "x") {
  proc_data(x, .id, colorize_row_id, "before")
}
