test_that('Compare drug frequencies', {
  require(dplyr)
  # Raw data from API
  dtaApi <- read.csv('data/fhi-api-lmr-sample.csv')
  # Adapt local input data
  fileIn <- dataPDRCreate()
  dtaLoc <- read.csv(fileIn)
  dtaExt <- dtaLoc
  dtaExt$ATC <- c(rep('A01AA', 3), 'A01AA01')
  dtaExt$Date <- c(2016, 2020,2009,2006)
  dtaLoc <- rbind(dtaLoc,dtaExt)
  dtaLoc <- dtaLoc[dtaLoc$Date != 'x',]
  colnames(dtaLoc) <- tolower(colnames(dtaLoc))
  #dtaLoc$Date <- c(2016, 2020,2009,2006)
  #dtaLoc$atc <- c(rep('A01AA', 3), 'A01AA01')
  dtaLoc$year <- with(dtaLoc, as.integer(substr(dtaLoc$date, 1, 4)))
  dtaLoc$age <- with(dtaLoc, year - faar)
  # Gives error, need to have sex as a column
  expect_error(
    compareDrugFrequencies(dtaLoc, dtaApi),
    regexp='Required columns not present in input data: atc,age,year,sex'
  )
  dtaLoc$sex <- 'Females'
  # Compare
  res <- compareDrugFrequencies(dtaLoc, dtaApi)
  resExp <- data.frame(ATC=c("A01AA", "A01AA", "A01AA", "A01AA01", "N06BA01", "N06BA01", "N06BA02"),
                       Age=c("10 - 14", "25 - 29", "35 - 39", "25 - 29", "15 - 19", "40 - 44", "40 - 44"),
                       Year=c(2016, 2009, 2020, 2006, 2024, 2025, 2025),
                       Sex=rep('Females',7),
                       Public=c(106, rep(NA,6)),
                       Local=1, Difference=c(105,rep(NA,6)),
                       Percent=c(10500, rep(NA,6)))
  expect_equal(res, resExp)
})

test_that('Test process norwegian drug register', {
  fileOut <- withr::local_tempfile()
  fileIn <- dataPDRCreate('')
  processDrugRegister(fileIn, fileOut)
  expect_true(file.exists(fileOut))
})

test_that('Test exception on bad date', {
  fileOut <- withr::local_tempfile()
  fileIn <- dataPDRCreate()
  expect_error(
    processDrugRegister(fileIn, fileOut),
    regexp='Bad dates detected at rows: ([1-9])+'
  )
})
