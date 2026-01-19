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

#' Connect to a DuckDB database
#' @export
#' @details
#' The main purpose of this function is to provide a default with read_only=T
#' with a database stored in a file. For in-memory databases, you might aswell
#' just use `DBI::dbConnect(duckdb::duckdb()).
#'
#' @param dbFile The database file.
#' @param read_only Connect without writing permission?
#' @param ... Further arguments passed to `DBI::dbConnect()`.
dbDuckConnect <- function(dbFile, read_only=T, ...) {
  DBI::dbConnect(duckdb::duckdb(dbdir=dbFile, read_only=read_only), ...)
}

#' Connect multiple DuckDB databases
#' @export
#' @details
#' This function sets up a database from multiple DuckDB database files by creating
#' a "main" in-memory database. Each argument to `...` must be named to give
#' each database a name.
#'
#' @importFrom DBI dbConnect dbExecute
#' @param ... Named arguments for other databases, `database-name = "database-file"`
#' @param read_only Whether connections should be in read only mode.
dbDuckConnectMany <- function (..., read_only=T) {
  callExp <- match.call()
  argList <- as.list(callExp[-1])
  argListNames <- names(argList)
  if (is.null(argListNames)) stop('Arguments ust be named')
  rd <- ifelse(read_only, ' (READ_ONLY)', '')
  dbCon <- dbConnect(duckdb:::duckdb())
  for (nme in argListNames) {
    fle <- eval(argList[[nme]])
    dbExecute(dbCon, glue::glue("ATTACH '{fle}' AS {nme}{rd}"))
  }
  dbCon
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
