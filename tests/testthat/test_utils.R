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
  print(arrow::schema(dta))
  tmp <- frequencyCountDistinct(dta, 'ATC', 'IID')
  expDta <- data.frame(ATC=c('N06BA01', 'N06BA02'), N=c(2, 1), Percent=c(100*2/3, 100/3))
  expect_equal(as.data.frame(tmp), expDta)
  tmp <- frequencyCountDistinct(dta, c('ATC', 'Date'), 'IID')
  expDta <- data.frame(
    ATC=c(rep('N06BA01', 3), 'N06BA02'),
    Date=c('2024-12-31','2025-01-03', 'x','2025-01-03'),
    N=rep(1,4),
    Percent=rep(25,4)
  )
  expect_equal(as.data.frame(tmp), expDta)
})
