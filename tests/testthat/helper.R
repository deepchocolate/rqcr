FILE_NO_DR <- 'data/norwegian_drug_register.csv'

#' Create a temporary PDR dataset for use in testing
#' @param error Any value in the column ERROR that should indicate an ERROR
#' @return The temporary filename
dataPDRCreate <- function (error=F) {
  require(stringi)
  dta <- utils::read.csv(FILE_NO_DR)
  fileNew <- withr::local_tempfile(.local_envir = .GlobalEnv)
  if (!isFALSE(error)) dta <- subset(dta, ERROR %in% error)
  utils::write.csv(dta, file=fileNew)
  arrow::write_parquet(dta, sink=fileNew %s+% '.parquet')
  fileNew
}
