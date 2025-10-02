#' Perform QC on the drug register
#' @param fileInput The drug register.
#' @param fileOutput The output file.
#' @param formatInput File format of input.
#' @param formatOutput File format of output.
#' @importFrom utils read.csv write.csv
#' @export
processDrugRegister <- function(fileInput, fileOutput, formatInput='csv', formatOutput='csv') {
  dtaIn <- read.csv(fileInput)
  nr <- nrow(dtaIn)
  datesBad <- !isDate(dtaIn$Date)
  datesAnyBad <- any(datesBad)
  if (datesAnyBad) stop('Bad dates detected at rows: ', paste0(which(datesBad), collapse=','))
  write.csv(dtaIn, file=fileOutput)
}

#' Compare statistics between the Norwegian drug register and public data.
#'
#' @details
#' Additional details...
#' Calculate and compare frequencies by ATC codes in the Norweigan drug register
#' with aggregated data available through the Norwegian Institute of Public Health's
#' public API available at statistikk.fhi.no. This data can be downloaded through
#' R using the package getStatisticsFHI on which this function depends. See
#' https://github.com/deepchocolate/get-statistics-fhi for details.
#' @export
#' @param df.local Drug register data at an individual level
#' @param df.public Raw data downloaded from statistikk.fhi.no.
#' @return A dataframe with ATC code, age group, year, sex, and columns Public,
#' Local, and their difference (Public - Local).
compareDrugFrequencies <- function (df.local, df.public) {
  cols <- c('atc', 'age','year', 'sex')
  if (!requireNamespace("getStatisticsFHI", quietly = TRUE)) {
    remotes::install_github('deepchocolate/get-statistics-fhi')
  }
  suggests::need('getStatisticsFHI>=1.0.1',
                 install_cmd = quote(remotes::install_github('deepchocolate/get-statistics-fhi')),
                 msg='This function requires the R-package "getStatisticsFHI". Please visit https://github.com/deepchocolate/get-statistics-fhi for installation options.')
  df.public <- getStatisticsFHI::recodeLMR(df.public)
  if (!intersectEquals(cols, colnames(df.local), colnames(df.public)))
    stop('Required columns not present in input data: ',paste(cols, collapse=','))
  df.local$age <- createIntervalsAge(df.local$age, by=5, sep=' - ')
  atc.local <- frequencyCountDistinct(df.local, cols, 'lopenr')
  dtaComp <- merge(df.public, atc.local, by.x=cols, by.y=cols)
  dtaComp <- dtaComp[,c('atc','age','year','sex','individuals', 'N')]
  colnames(dtaComp) <- c('ATC', 'Age', 'Year', 'Sex', 'Public', 'Local')
  dtaComp$Difference <- with(dtaComp, Public - Local)
  dtaComp
}
