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
