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

test_that('updateCases', {
  df <- data.frame(colA=c('A', 'B'), colB=c(1,3))
  # Update colB to 2 where colA equals "B"
  df2 <- updateCases(df, colB, colA == 'B' ~ 2)
  df$colB <- 1:2
  expect_equal(df2, df)
})
