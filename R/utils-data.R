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
  whch <- ifelse(is.na(splitIndicator), which(apply(is.na(dta),1, all)), which(apply(dta == splitIndicator, 1, all)))
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

#' Update values conditionally in tabular data
#' @details
#' This function uses dplyr::case_when to perform conditional updates in data, but
#' by default preserving existing values for rows that do not match.
#'
#' @seealso [dplyr::case_when()]
#' @export
#' @import dplyr
#' @importFrom rlang := enquos
#' @param .data Anything accepted by dplyr (can be piped).
#' @param col The column to update
#' @param ... Conditions for updates in the form `Column == "value" ~ Replacement`.
updateCases <- function (.data, col, ...) {
  .data %>% mutate( "{{col}}" := case_when(!!!enquos(...), .default = {{ col }}))
}
