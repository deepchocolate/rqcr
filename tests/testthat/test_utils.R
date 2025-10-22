test_that('closest', {
  expect_equal(closest(1:3), 1)
  expect_equal(closest(c(-2,-1,1,3)), c(-1,1))
  vec <- -1:3
  expect_equal(closest(vec), 0)
  expect_equal(minPositive(vec), 0)
  expect_equal(maxNegative(vec), -1)
  expect_equal(maxNegative(1:3), NA)
  expect_equal(minPositive(-1:-3), NA)
})

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

test_that('diffYears', {
  d <- diffYears('2024-01-01', '2025-01-01')
  expect_equal(d, 1)
  d <- diffYears('20240101', '20250101')
  expect_equal(d, 1)
  d <- diffYears(c('20240101', '20250101','20260210'), '20250101')
  expect_equal(d, c(1,0,-1.11), tolerance=0.01)
  d <- diffYears(c('20240101', '20250101','20260210'), c('20240101', '20250101','20260210'))
  expect_equal(d, c(0,0,0))
})

test_that('labelStrings', {
  dta <- data.frame(id=c(1,1,2,2,3),
                    str=c('A','B', 'AA', 'BB', 'CA'),
                    labExp=c('LabA','LabB','LabA','LabB', ''))
  dta$out <- labelStrings(dta$str, c('A', 'B'), c('LabA', 'LabB'))
  expect_equal(dta$out, dta$labExp)
  # Using exclusion across all sets
  dta <- data.frame(str=c('A','B', 'AA', 'BB', 'CA', 'BBc'),
                    labExp=c('LabA','LabB','LabA','', '', ''))
  dta$out <- labelStrings(dta$str, c('A', 'B'), c('LabA', 'LabB'), c('BB'))
  expect_equal(dta$out, dta$labExp)
  # Using exclusion per set
  dta <- data.frame(str=c('A','B', 'AA', 'BB', 'CA', 'BBc'),
                    labExp=c('LabA','LabB','','LabB', '', ''))
  dta$out <- labelStrings(dta$str, c('A', 'B'), c('LabA', 'LabB'), c('AA', 'BBc'))
  expect_equal(dta$out, dta$labExp)
  expect_error(labelStrings('A', c('A', 'B'), 'LabA'), 'sets and labels need to be of equal length')
  expect_equal(labelStrings(c('A', 'C'), c('A', 'B'), c('LabA', 'LabB'), c('B', 'C', 'D')), c('LabA', ''))
  expect_equal(labelStrings(c('A', 'C'), c('A', 'B'), c('LabA', 'LabB'), c(NA, 'C', NA)), c('LabA', ''))
  expect_error(labelStrings(c('A', 'C'), c('A','A', 'B'), c('LabA', 'Lab A', 'LabB'), c('B','BB', 'C', 'D')),
               'sets contain duplicate patterns')
  expect_error(labelStrings(c('A', 'Bb', 'D'), c('A', 'B', 'F'), c('LabA', 'LabB', 'LabF'), c('Bb', 'C')),
               'exclude needs to be length 1 or equal to sets')
  # Using a list
  grps <- list(LabA=list(sets=c('A', 'aa'), exclude=c('Ab')),
               LabB=list(sets=c('B', 'BB')))
  expect_equal(labelStrings(c('A', 'aa', 'Ab', 'B'), grps), c('LabA', 'LabA', '', 'LabB'))
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
