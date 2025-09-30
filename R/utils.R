#' Count frequencies of discrete values.
#' @param dta Any data accepted by dplyr.
#' @param col Column in dta to count values.
#' @param ... Columns to select in data.
#' @details
#' Count frequencies
#' Without any columns passed in (...) this function simply returns the distinct
#' values, counts and percent of these values. By passing other
#' columns through ... the function performs this operation over these columns so
#' that each value is only counted once for each group.
#' @import dplyr
#' @export
frequencyCountDistinct <- function (dta, col, ...) {
  dta <- dta %>% select(..., all_of(col)) %>% distinct() %>% select(all_of(col)) %>% collect() %>% count(pick(all_of(col)), name='n') %>%
    arrange(!!as.name(col))
  colnames(dta) <- c(col, 'n')
  n <- sum(dta$n)
  dta$Percent <-  100*dta$n/n
  colnames(dta) <- c(col, 'N', 'Percent')
  dta
}

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
