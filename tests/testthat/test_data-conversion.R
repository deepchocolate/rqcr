require(duckdb)
test_that('Test parquet', {
  dta <- read.csv(FILE_NO_DR)
  dta$year <- substr(dta$Date, 1, 4)
  tmpdb <- withr::local_tempfile()
  con <- dbConnect(duckdb())
  dbWriteTable(con, 'temp', dta)
  # Convert to parquet
  filePar <- withr::local_tempfile()
  convertDBTableToParquet(con, 'temp', filePar)
  dtaPar <- arrow::open_dataset(filePar) |> collect()
  expect_equal(dta, as.data.frame(dtaPar))
  # Test partitioning
  filePar <- withr::local_tempfile()
  convertDBTableToParquet(con, 'temp', filePar, partition='year')
  dtaPar <- arrow::open_dataset(filePar) |> collect()
  expect_equal(dta, as.data.frame(dtaPar))
})

test_that('Test convertToCSV', {
  require(stringi)
  pathFile <- dataPDRCreate()
  dta1 <- read.csv(pathFile)
  fileOut <- withr::local_tempfile()
  fle <- convertToCSV(pathFile %s+% '.parquet', fileOut)
  dta2 <- read.csv(fle)
  expect_equal(dta1, dta2)
  # Test lowering columns
  fle <- convertToCSV(pathFile %s+% '.parquet', fileOut, lowerColumns = T)
  dta2 <- read.csv(fle)
  colnames(dta1) <- tolower(colnames(dta1))
  expect_equal(dta1, dta2)
  # Test removing a column
  fle <- convertToCSV(pathFile %s+% '.parquet', fileOut, lowerColumns = T, dropColumns = 'lopenr')
  dta2 <- read.csv(fle)
  dta1 <- dta1[,-1]
  expect_equal(dta1, dta2)
})

test_that('readBigCSV', {
  pathFile <- dataPDRCreate()
  # There is a warning issued due to a bad date, it does not, however affect
  # the resulting output.
  suppressWarnings ({
    dta <- readBigCSV(pathFile, nlines=2)
  })
  expect_type(dta, 'list')
  expect_equal(nrow(dta), 4)
})

test_that('convertBigCSV', {
  pathFile <- dataPDRCreate()
  pathOutput <- pathFile %s+% '.dta'
  fileSpss <- convertBigCSV(pathFile, pathOutput)
  expect_true(file.exists(fileSpss))
  dta <- haven::read_dta(fileSpss)
  expect_equal(nrow(dta), 4)
})
