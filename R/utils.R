#' Test if x is a date
#' @param x Anything
#' @param format If any other format than specified by as.Date
isDate <- function (x, format) {
  x <- as.Date(x, format=format, optional=TRUE)
  ifelse(is.na(x), F, T)
}

#' Remove database tables.
#' @param con A database connection.
#' @param tables A vector of tables to remove.
#' @importFrom duckdb dbRemoveTable
#' @export
dbRemoveTables <- function(con, tables) {
  for (tb in tables) duckdb::dbRemoveTable(con, tb, fail_if_missing=F)
}

#' Create a database ENUM type.
#' @param con A database connections.
#' @param name The name of the ENUM type.
#' @param values A vector of values in the ENUM.
#' @importFrom stringi %s+%
#' @importFrom DBI dbExecute
#' @export
dbCreateTypeEnum <- function (con, name, values) {
  values <- unique(values)
  sql <- 'CREATE TYPE ' %s+% name %s+% " AS ENUM ('" %s+% paste0(values, collapse="','") %s+% "')"
  DBI::dbExecute(con, sql)
}

#' Disconnect from a DuckDB.
#' @param con A database connection.
#' @param checkpoint Whether to issue a CHECKPOINT prior to exiting.
#' @importFrom duckdb dbDisconnect
#' @importFrom DBI dbExecute
#' @export
dbDuckDisconnect <- function(con, checkpoint=T) {
  if (checkpoint) DBI::dbExecute(con, 'CHECKPOINT;')
  duckdb::dbDisconnect(con, shutdown=T)
}
#' Recode values to missing.
#' @param x A vector of inputs.
#' @param cases Cases in x to convert to NA.
#' @importFrom dplyr case_match
#' @export
recodeToNA <- function (x, cases) {
  if (all(is.na(x))) return(x)
  dplyr::case_match(x, cases~NA, .default=x)
}

#' Recode individual sex to M(ale)/F(emale).
#' @param x A vector of inputs.
#' @param default Default value of x if no match is made.
#' @importFrom dplyr case_match
#' @export
recodeSex <- function (x, default=NA) {
  x <- as.character(x)
  dplyr::case_match(x,
                    c('1','M')~'M',
                    c('2', 'K')~'F',
                    .default=default
  )
}
