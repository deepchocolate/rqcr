#' Add the preceding value of a sequence as a column in data
#' @export
#' @import dplyr
#' @param .data Any tabular data accepted by dplyr
#' @param colStates The column indicating current state.
#' @param colName Name of column for the origin state. Defaults to "from`colStates`".
addPredecessor <- function (.data, colStates, colName=NULL) {
  colName <- substitute(colName)
  if (is.null(colName)) colName <- paste0('from', substitute(colStates))
  else colName <- deparse(colName)
  .data %>% mutate({{ colName }} := lag({{ colStates }}))
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

#' Indicate novel elements in ordered data
#' @name indicateNovel
#' @export
#' @import dplyr
setGeneric('indicateNovel', function (.data, ...) standardGeneric('indicateNovel'))
#' @rdname indicateNovel
setMethod('indicateNovel', signature('factor'),
          function (.data) {
            indicateNovel(as.character(.data))
          })
#' @rdname indicateNovel
setMethod('indicateNovel', signature('ANY'),
          function (.data) {
            o <- rep(0, length(.data))
            vs <- match(unique(.data), .data)
            o[vs] <- 1
            o
          })
#' @rdname indicateNovel
#' @param .data Any data object accepted by dplyr
#' @param times Column referring to the temporal ordering.
#' @param values Column referring to values to mark as new.
#' @param ... Grouping factors.
#' @param .name Name of the column indicating novelty.
setMethod('indicateNovel', signature('data.frame'),
          function (.data, times, values, ..., .name='novel') {
            .data %>% group_by(...) %>% arrange({{ times }}) %>%
              mutate("{.name}" :=indicateNovel({{ values }})) %>% ungroup()
            })

#' Split data by rows that are equal to some value.
#' @details
#' The motivation for this function comes from spreadsheet in which multiple tables
#' are present in a sheet. If such tables are separated by some character or an entire
#' empty row this function can split such data into its constituents. If using
#' `header=TRUE` it is currently required that tables have the same columns.
#' @seealso [mergeDataByRow()]
#' @export
#' @param dta Usually a data.frame.
#' @param splitIndicator A value to delineate rows that separate tables in data.
#' @param header If TRUE, the first row in each dataset are used as a header for the data.
splitDataByRow <- function (dta, splitIndicator=NA, header=F) {
  if (is.na(splitIndicator)) whch <- which(apply(is.na(dta),1, all))
  else whch <- which(apply(dta == splitIndicator, 1, all))
  out <- list()
  i <- 1
  j <- i
  for (split in whch) {
    if (split - i > 0) {
      d <- dta[i:(split-1),]
      if (header) {
        colnames(d) <- d[1,]
        d <- d[-1,]
      }
      out[[j]] <- d
    }
    i <- split + 1
    j <- j + 1
  }
  n <- nrow(dta)
  if (n - i >= 0) {
    d <- dta[i:n,]
    if (header) {
      colnames(d) <- d[1,]
      d <- d[-1,]
    }
    out[[j]] <- d
  }
  rmWhich <- which(sapply(out, FUN=is.null))
  if (length(rmWhich) > 0) out <- out[-rmWhich]
  out
}

#' Merge data by row like rbind, but with options to separate data.
#' @export
#' @details
#' Like the motivation for `splitDataByRow`, the motivation for this function
#' comes from spreadsheets where several tables may be present in a sheet.
#' @seealso [splitDataByRow()]
#' @param ... Tabular objects like data frames.
#' @param insertColnames Insert the column names of each table as the first row in the data.
#' @param separator Separate each table with a row of this value (use NA for whitespace).
#' @return A data.frame.
mergeDataByRow <- function(..., insertColnames=T, separator=NA) {
  tables <- list(...)
  out <- NULL
  for (i in 1:length(tables)) {
    tb <- tables[[i]]
    if (insertColnames) out <- rbind(out, colnames(tb))
    out <- rbind(out, as.matrix(tb), dimnames=NULL)
    if (!isFALSE(separator)) out <- rbind(out, rep(NA, ncol(tb)))
  }
  if (!isFALSE(separator)) out <- out[-nrow(out),]
  data.frame(out,row.names = NULL)
}

#' Merge time periods
#' @export
#' @param .date Period starting date (YYYY-MM-DD)
#' @param .days Period length in days
mergePeriods <- function (.date, .days) {
  times <- diffDays(.date, first(.date))
  timesEnd <- times + .days
  pos <- which(times[-1] > timesEnd[-length(timesEnd)])
  splts <- splitVector(.days, pos+1)
  date <- splitVector(.date, pos+1)
  .date <- unlist(lapply(date, FUN=first))
  ids <- 1:length(splts)
  reps <- lapply(splts, FUN=length)
  ids <- rep(ids, unlist(reps))
  .days <- c(tapply(.days, factor(ids), FUN=sum), use.names = F)
  tibble(date=.date, days=.days)
}

#' Update values conditionally in tabular data
#' @details
#' This function uses dplyr::case_when to perform conditional updates in data, but
#' by default preserving existing values for rows that do not match.
#'
#' @seealso [dplyr::case_when()]
#' @export
#' @import dplyr formula.tools
#' @importFrom rlang := enquos quo_get_expr
#' @param .data Anything accepted by dplyr (can be piped).
#' @param col The column to update
#' @param ... Conditions for updates in the form `Column == "value" ~ Replacement`.
#' @param .warnIfMissing Throws a warning if the update condition is not identified in .data
updateCases <- function (.data, col, ..., .warnIfMissing=FALSE) {
  frms <- list()
    for (frm in enquos(...)) {
      frm <- quo_get_expr(frm)
      if (typeof(.data %>% pull({{col}})) == 'character') {
        varRhs <- rhs(frm)
        if (is.numeric(varRhs)) rhs(frm) <- as.character(varRhs)
      }
      if (.warnIfMissing | configRQCR('updateCases', 'warnings')) {
        rows <- lhs(frm)
        nrows <- .data %>% filter(eval(rows)) %>%  nrow()
        if (nrows == 0) warning('No rows found for ', nrows)
      }
    frms <- append(frms, frm)
  }
  .data %>% mutate( "{{col}}" := case_when(!!!frms, .default = {{ col }}))
}
