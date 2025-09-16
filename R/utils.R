#' Test if x is a date
#' @param x Anything
#' @param format If any other format than specified by as.Date
isDate <- function (x, format) {
  x <- as.Date(x, format=format, optional=TRUE)
  ifelse(is.na(x), F, T)
}

#' Connect to an SQLite database.
#' @param db The database to connect to.
#' @param foreignKeys Whether to use foreign keys.
connectSQLite(db, foreginKeys=T) {
  require(RSQLite)
  dbCon <- dbConnect(RSQLite::SQLite(), db)
  fk <- paste0('PRAGMA foreign_keys=', ifelse(foreignKeys, 1, 0))
  dbExecute(dbCon, fk)
}
