renameColumnsMap <- function (dta, dtaCols, english=F) {
  cnames <- colnames(dta)
  # Indices in Delivery
  overl <- match(cnames, dtaCols$Delivery)
  # Positions in data columns
  inx <- 1:length(overl)
  # Remove positions that are not present in the mapping as they should not be renamed
  if (any(is.na(overl))) inx <- inx[!is.na(overl)]
  overl <- na.omit(overl)
  # If no overlap exists, just return data
  if (!any(overl)) return(dta)
  # Choose language
  if (english) colsNew <- dtaCols$English
  else colsNew <- dtaCols$Norwegian
  # Replace any empty entries with their default names (mainly for English)
  colsEmpty <- colsNew == ''
  if (any(colsEmpty)) colsNew[colsEmpty] <- dtaCols$Norwegian[colsEmpty]
  cnames[inx] <- colsNew[overl]
  colnames(dta) <- cnames
  dta
}
#' Rename columns from delivery to new names
#' @name renameColumns
#' @export
#' @param dta A table with column names, e.g. data.frame.
#' @param dtaCols A data frame of column names
#' @param english Whether to translate column names to English
setGeneric('renameColumns', function (dta, dtaCols, english=F) standardGeneric('renameColumns'))
#' @rdname renameColumns
setMethod('renameColumns', signature('ANY', 'data.frame', 'logical'), renameColumnsMap)
#' @rdname renameColumns
setMethod('renameColumns', signature('ANY', 'data.frame', 'missing'), renameColumnsMap)
