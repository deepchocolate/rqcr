#' Perform QC on the drug register
#' @param fileInput The drug register.
#' @param fileOutput The output file.
#' @param formatInput File format of input.
#' @param formatOutput File format of output.
#' @importFrom utils read.csv write.csv
processDrugRegister <- function(fileInput, fileOutput, formatInput='csv', formatOutput='csv') {
  dtaIn <- read.csv(fileInput)
  nr <- nrow(dtaIn)
  datesBad <- !isDate(dtaIn$Date)
  datesAnyBad <- any(datesBad)
  if (datesAnyBad) stop('Bad dates detected at rows: ', paste0(which(datesBad), collapse=','))
  write.csv(dtaIn, file=fileOutput)
}
