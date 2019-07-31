#' Insert a data.frame into another data.frame
#'
#' @param x A \link[base]{data.frame}
#' @param y A [base::data.frame()]
#' @param where The place where data y will be inserted into data x
#'
#' @return combined data
#' @export
#'
#' @examples
#' x <- data.frame(a = "abc")
#' y <- data.frame(b = "def")
#' insert_into(x,y)
#' insert_into(x,y,where = 2)
#' @seealso [add_col()]
#' @family xyz
insert_into <- function(x, y, where = 1) {
  if (where == 1) {
    cbind(y, x)
  } else if (where > ncol(x)) {
    cbind(x, y)
  } else {
    lhs <- 1:(where - 1)
    cbind(x[lhs], y, x[-lhs])
  }
}

#' Verifies the parameter where
#'
#' Verifies that parameter 'where' is a valid class before running 'insert_into'
#'
#' @param x
check_where <- function(x) {
  if (length(x) != 1 || !is.numeric(x)) {
    stop("`where` must be a length one numeric vector.",
      call. = FALSE)
  }
  x <- as.integer(x)

  if (x == 0 || is.na(x)) {
    stop("`where` must not be zero or missing",
      call. = FALSE)
  } else {
    x
  }
}
