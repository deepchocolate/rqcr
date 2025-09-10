#' Test if x is a date
#' @param x Anything
#' @param format If any other format than specified by as.Date
isDate <- function (x, format) {
  x <- as.Date(x, format=format, optional=TRUE)
  ifelse(is.na(x), F, T)
}
