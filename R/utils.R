#' Create age/positive number intervals from a variable.
#' @param x A vector of numbers.
#' @param by Width of intervals.
#' @param from Interval start.
#' @param to Interval end.
#' @param sep Separator text between intervals.
#' @details
#' Create age intervals
#' Each number in x will be placed in one of the intervals between arguments from
#' and to where by specifies the range of each interval. The right end of the interval
#' is by default set to Inf, i.e. oldest will be "anyone above 90" if by=90.
#' @export
createIntervalsAge <- function (x, by, from=0, to=90, sep='-') {
  ageL <- seq(from=from, to=to, by=by)
  ageU <- ageL + by - 1
  ageU <- ageU[-length(ageU)]
  lab <- paste(ageL, ageU, sep=sep)
  lab[length(lab)] <- substr(lab[length(lab)], 1, 3)
  cut(x, breaks=c(ageL, Inf), labels=lab, include.lowest=T, right=F)
}

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

#' Get data from an excelfile by searching through all sheets.
#' @export
#' @param file The excel file path.
#' @param col The column to search.
#' @param val The value to look for in col.
#' @param skip Number of lines to skip in each sheet.
#' @param colsSelect Which columns to select.
getExcelData <- function (file, col, val, skip, colsSelect) {
  sheets <- readxl::excel_sheets(file)
  col <- tolower(col)
  colsSelect <- tolower(colsSelect)
  dta <- NULL
  for (sheet in sheets) {
    tmp <- readxl::read_xlsx(file, sheet, skip=skip)
    cnames <- tolower(colnames(tmp))
    colnames(tmp) <- cnames
    if (col %in% cnames) {
      tmp$sheet <- sheet
      dta <- rbind(dta,
                   tmp[tmp[,col]==val, c('sheet',colsSelect)])
    }
  }
  dta
}

#' Test if a vector is equal to the intersection of other vectors.
#' @param x A vector.
#' @param ... Vectors to intersect.
intersectEquals <- function (x, ...) {
  ints <- Reduce(intersect, list(...))
  setequal(x, ints)
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
