test_that('Test createIntervals', {
  ints <-  c(0, 10, 15, 99)
  intsFac <- createIntervalsAge(ints, 5, 0, 90)
  expect_equal(as.character(intsFac), c('0-4','10-14', '15-19', '90-'))
  intsFac <- createIntervalsAge(c(ints,NA), 5, 0, 90)
  expect_equal(as.character(intsFac), c('0-4','10-14', '15-19', '90-', NA))
  # Negative values leads to missing values
  intsFac <- createIntervalsAge(c(ints,-1), 5, 0, 90)
  expect_equal(as.character(intsFac), c('0-4','10-14', '15-19', '90-', NA))
})

test_that('Test recodeToNA', {
  expect_equal(c('1','2',NA,NA), recodeToNA(c(1,2, NA, ''), c('')))
  # Cannot include numbers as NA codes
  expect_error(recodeToNA(c('1',2, NA, ''), c(2)))
  expect_equal(c('1',NA,NA,NA), recodeToNA(c(1,2, NA, ''), c('2','', NA)))
})

test_that('Test recodeSex', {
  expect_equal(c('M', 'F', NA), recodeSex(c('1','2', '3')))
  expect_equal(c('M', 'F', NA), recodeSex(c(1,2, '3')))
  expect_equal(c('M', 'F', NA), recodeSex(c('M','K', NA)))

})

test_that('Test frequencyCount', {
  fileIn <- dataPDRCreate()
  dta <- arrow::open_dataset(fileIn %s+% '.parquet')
  tmp <- frequencyCountDistinct(dta, 'ATC', 'lopenr')
  expDta <- data.frame(ATC=c('N06BA01', 'N06BA02'), N=c(2, 1), Percent=c(100*2/3, 100/3))
  expect_equal(as.data.frame(tmp), expDta)
  tmp <- frequencyCountDistinct(dta, c('ATC', 'Date'), 'lopenr')
  expDta <- data.frame(
    ATC=c(rep('N06BA01', 3), 'N06BA02'),
    Date=c('2024-12-31','2025-01-03', 'x','2025-01-03'),
    N=rep(1,4),
    Percent=rep(25,4)
  )
  expect_equal(as.data.frame(tmp), expDta)
})

test_that('Test intersectEquals', {
  v <- c('a', 'b')
  expect_true(intersectEquals(v, c(v, 'c'), c(v, 'A','C')))
  expect_false(intersectEquals(c(v, 'C'), c(v, 'c'), c(v, 'A','C')))
})

test_that('renameColumns', {
  fileIn <- dataPDRCreate()
  dta <- read.csv(FILE_NO_DR)
  dta2 <- renameColumns(dta, NO_NAMES$DRUG_REGISTER)
  expect_equal(dta2$lopenr, dta$Lopenummer_NPR)
  # No overlap should keep everything as is
  dta3 <- data.frame(A=1,B=2)
  expect_equal(renameColumns(dta3, NO_NAMES$DRUG_REGISTER), dta3)
})
