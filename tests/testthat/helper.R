#' Create a temporary PDR dataset for use in testing
#' @param error Any value in the column ERROR that should indicate an ERROR
#' @return The temporary filename
dataPDRCreate <- function (error=F) {
  dta <- read.csv('data/norwegian_drug_register.csv')
  fileNew <- withr::local_tempfile(.local_envir = .GlobalEnv)
  if (!isFALSE(error)) dta <- subset(dta, ERROR %in% error)
  write.csv(dta, file=fileNew)
  fileNew
}
