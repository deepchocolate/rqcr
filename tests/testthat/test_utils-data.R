test_that('addPredecessor', {
  df <- data.frame(A=3:1)
  out <- df %>% addPredecessor(A)
  df$fromA <- c(NA,3,2)
  expect_equal(df, out)
  df <- data.frame(A=3:1)
  out <- df %>% addPredecessor(A, B)
  df$B <- c(NA,3,2)
  expect_equal(out, df)
})

test_that('getExcelData', {
  dta <- getExcelData(FILE_EXCEL, 'C', 'B', 1, c('A','B'))
  expect_equal(dta, tibble(sheet=c('Sheet A', 'Sheet B'), a=c(3,7), b=c(4,8)))
})

test_that('indicateNovel', {
  dta <- data.frame(subject=c(rep(1, 4),2,2,2), time=c(1,4,3,2, 1,3, 2), values=c('a', 'c', 'b','b', 'a', 'b', 'a'), stringsAsFactors = F)
  o <- dta %>% indicateNovel(time, values, subject)
  expect_equal(o$novel, c(1,1,1,0,0,1,1))
  dta$values <- factor(dta$values)
  o <- dta %>% indicateNovel(time, values, subject)
  expect_equal(o$novel, c(1,1,1,0,0,1,1))
})

test_that('splitDataByRow, mergeDataByRow', {
  require(data.table)
  dta <- data.frame(a=c(1,NA,2,NA,NA,3), b=c(1,NA,2, NA,NA, 3))
  dtaExp <- list(data.frame(a=1,b=1), data.frame(a=2,b=2, row.names=3), data.frame(a=3,b=3, row.names=3))
  dtaOut <- splitDataByRow(dta)
  rownames(dtaOut[[2]]) <- 3 # Seems impossible to get the type of row.names right when constructing the expected dataframe as above
  rownames(dtaOut[[3]]) <- 3
  expect_equal(dtaOut, dtaExp)
  # Merge again
  dtaMerged <- mergeDataByRow(dtaOut[[1]], dtaOut[[2]], dtaOut[[3]], insertColnames=F)
  dtaExp2 <- dta[-4,]
  rownames(dtaExp2) <- 1:nrow(dtaExp2)
  expect_equal(dtaMerged, dtaExp2)
  # Split/Merge with colnames
  dtaMerged <- mergeDataByRow(dtaOut[[1]], dtaOut[[2]], dtaOut[[3]], insertColnames=T)
  # Test with data.table
  dtaOut <- splitDataByRow(as.data.table(dta))
  rownames(dtaOut[[2]]) <- 1
  rownames(dtaOut[[3]]) <- 1
  dtaExp <- lapply(dtaExp, as.data.table)
  rownames(dtaExp[[2]]) <- 1
  rownames(dtaExp[[3]]) <- 1
  expect_equal(dtaOut, dtaExp)
})

test_that('mergePeriods', {
  dta <- tibble(date=c('2000-01-01', '2000-01-05', '2000-01-10', '2000-02-20','2000-02-22', '2000-04-01','2000-04-01'),
                days=c(10, 10, 10, 10, 2, 4, 3))
  dtaExp <- tibble(date=c('2000-01-01', '2000-02-20', '2000-04-01'),
                days=c(30, 12, 7))
  expect_equal(mergePeriods(dta$date, dta$days), dtaExp)
  # With option reset=T
  dtaExp <- tibble(date=c('2000-01-01', '2000-02-20', '2000-04-01'), days=c(19, 4, 3.5))
  expect_equal(mergePeriods(dta$date, dta$days, reset=T), dtaExp)
  dta <- tibble(date=c('2000-01-01', '2000-01-05', '2000-01-10'),
                days=c(4, 4, 4))
  dtaExp <- tibble(date=c('2000-01-01'),
                   days=c(12))
  expect_equal(mergePeriods(dta$date, dta$days, maxDistance=4), dtaExp)
  # More complicated
  dta <- tibble(date=c('2023-09-07','2023-09-29','2023-10-27','2023-12-12','2024-01-23','2024-03-05','2024-04-23','2024-06-11','2024-07-30','2024-09-24','2024-11-19','2025-01-14','2025-03-11'),
                days=c(60,30,30,60,60,60,30,30,30,30,30,30,30))
  dta$date <- as.Date(dta$date)
  dtaExp <- tibble(date=c('2023-09-07','2024-11-19','2025-01-14','2025-03-11'),
                   days=c(420,30,30,30))
  dtaExp$date <- as.Date(dtaExp$date)
  expect_equal(mergePeriods(dta$date, dta$days), dtaExp)
})

test_that('standardizeColumns', {
  df <- data.frame(A=1:3, B=1:3, C=4:6)
  o <- standardizeColumns(df, A, B, C)
  expect_equal(o, data.frame(A=-1:1, B=-1:1, C=-1:1))
  df <- standardizeColumns(df, B, C)
  expect_equal(df, data.frame(A=1:3, B=-1:1, C=-1:1))
})

test_that('updateCases', {
  df <- data.frame(colA=c('A', 'B'), colB=c(1,3))
  # Update colB to 2 where colA equals "B"
  df2 <- updateCases(df, colB, colA == 'B' ~ 2)
  df$colB <- 1:2
  expect_equal(df2, df)
  df2 <- updateCases(df, colB, colA == 'B' ~ 2, colA =='A'~ 1, .warnIfMissing = T)
  expect_equal(df2, df)
  expect_warning(updateCases(df, colB, colA == 'C' ~ 2, .warnIfMissing=T))
  # Do it but provide the optional warning through a configuration
  # Providing the option argument will be ignored
  configRQCR('updateCases', 'warnings', T)
  expect_warning(updateCases(df, colB, colA == 'C' ~ 2, .warnIfMissing=F))
  # Updating with a number in a text field
  df2 <- updateCases(df, colA, colA == 'B' ~ 2)
})
