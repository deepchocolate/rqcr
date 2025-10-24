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
  resExp <- data.frame(ATC=c("A01AA", "A01AA", "A01AA","A01AA","A01AA","A01AA", "A01AA01", "A01AA01", "A01AA01", "A01AA01"),
                       Age=c("10 - 14", "40 - 44","5 - 9","50 - 54","60 - 64","80 - 84","40 - 44","45 - 49","45 - 49","65 - 69"),
                       Year=c(2016,2013,2006,2006,2016,2013,2009,2015,2020,2011),
                       Sex=c( "Females","Males","Females","Both","Both","Both","Females","Females","Females","Both"),
                       Public=c(106,379,0,38,2094,959,27,956,1999,390),
                       Local=c(1, rep(NA,9)), Difference=c(105,rep(NA,9)),
                       Percent=c(10500, rep(NA,9)))
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
