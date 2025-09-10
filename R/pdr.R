#' Perform QC on the drug register
processDrugRegister <- function(fileInput, fileOutput, formatInput='csv', formatOutput='csv') {
  dtaIn <- read.csv(fileInput)
  nr <- nrow(dtaIn)
  datesBad <- !isDate(dtaIn$Date)
  datesAnyBad <- any(datesBad)
  if (datesAnyBad) stop('Bad dates detected at rows: ', paste0(which(datesBad), collapse=','))
  write.csv(dtaIn, file=fileOutput)
}
