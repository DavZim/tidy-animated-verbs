check_xy_format <- function(x) {
  if (!(all(c("id", "x") %in% names(x)) ||
        all(c("id", "y") %in% names(x)))) {
    stop("x and y must have the names 'id' and 'x' or 'x'")
  }
}
