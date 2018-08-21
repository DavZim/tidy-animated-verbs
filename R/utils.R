check_xy_format <- function(x, y = NULL) {
  if (is.null(y)) {
    # y only provided if x and y have to have the same names, thus set-operations!
    if (!(all(c("id", "x") %in% names(x)) ||
          all(c("id", "y") %in% names(x)))) {
      stop("x and y must have the names 'id' and 'x' or 'x'")
    }
  } else {
    if (!any(names(x) %in% names(y)))
      stop("x and y must have the same names in set-operations")
  }
}
