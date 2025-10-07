test_that('Compare drug frequencies', {
  # Raw data from API
  dtaApi <- read.csv('data/fhi-api-lmr-sample.csv')
  # Adapt local input data
  fileIn <- dataPDRCreate()
  dtaLoc <- read.csv(fileIn)
  dtaLoc$Date <- c(2016, 2020,2009,2006)
  dtaLoc$atc <- c(rep('A01AA', 3), 'A01AA01')
  dtaLoc$year <- with(dtaLoc, as.integer(substr(dtaLoc$Date, 1, 4)))
  dtaLoc$age <- with(dtaLoc, year - faar)
  # Gives error, need to have sex as a column
  expect_error(
    compareDrugFrequencies(dtaLoc, dtaApi),
    regexp='Required columns not present in input data: atc,age,year,sex'
  )
  dtaLoc$sex <- 'Females'
  # Compare
  res <- compareDrugFrequencies(dtaLoc, dtaApi)
  resExp <- data.frame(ATC='A01AA',Age='10 - 14', Year=2016, Sex='Females', Public=106,Local=1, Difference=105, Percent=10500)
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
