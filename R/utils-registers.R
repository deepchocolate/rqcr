#' @import methods
NULL
renameColumnsMap <- function (dta, dtaCols, english=F, verbose=T) {
  cnames <- colnames(dta)
  # Indices in Delivery
  overl <- match(cnames, dtaCols$Delivery)
  # Positions in data columns
  inx <- 1:length(overl)
  colMissing <- is.na(overl)
  # Columns not present in renaming map
  novel <- inx[colMissing]
  # Remove positions that are not present in the mapping as they should not be renamed
  if (any(is.na(overl))) inx <- inx[!colMissing]
  overl <- stats::na.omit(overl)
  # If no overlap exists, just return data
  if (!any(overl)) return(dta)
  # Choose language
  if (english) colsNew <- dtaCols$English
  else colsNew <- dtaCols$Norwegian
  # Replace any empty entries with their default names (mainly for English)
  colsEmpty <- colsNew == ''
  if (any(colsEmpty)) colsNew[colsEmpty] <- dtaCols$Norwegian[colsEmpty]
  listOldNames <- cnames
  cnames[inx] <- colsNew[overl]
  listNewNames <- cnames
  listNewNames[novel] <- ''
  listNewNames <- c('CHANGE', listNewNames)
  listOldNames <- c('COLUMN', listOldNames)
  mx <- max(nchar(listOldNames))
  mx2 <- max(nchar(listNewNames))
  if (verbose) message(stringi::stri_sprintf(paste0('%',mx,'s \t %-',mx2,'s\n'), listOldNames, listNewNames))
  colnames(dta) <- cnames
  dta
}
#' Rename columns from delivery to new names
#' @name renameColumns
#' @export
#' @param dta A table with column names, e.g. data.frame.
#' @param dtaCols A data frame of column names.
#' @param english Whether to translate column names to English.
#' @param verbose Whether to print how columns have been renamed.
setGeneric('renameColumns', function (dta, dtaCols, english=F, verbose=T) standardGeneric('renameColumns'))
#' @rdname renameColumns
setMethod('renameColumns', signature('ANY', 'data.frame', 'logical', 'logical'), renameColumnsMap)
#' @rdname renameColumns
setMethod('renameColumns', signature('ANY', 'data.frame', 'missing', 'missing'), renameColumnsMap)
#' @rdname renameColumns
setMethod('renameColumns', signature('ANY', 'data.frame', 'logical', 'missing'), renameColumnsMap)
#' @rdname renameColumns
setMethod('renameColumns', signature('ANY', 'data.frame', 'missing', 'logical'), renameColumnsMap)
