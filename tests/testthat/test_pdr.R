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
