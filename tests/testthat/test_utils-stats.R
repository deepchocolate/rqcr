test_that('countUnique', {
  df <- data.frame(a=c(1,1,2))
  expect_equal(countUnique(df,a), 2)
  df <- data.frame(a=c('1','1','2'), b=c('1','1','2'))
  expect_equal(countUnique(df), 2)
  df$c <- 1
  expect_equal(countUnique(df, c), 1)
  expect_equal(countUnique(1:3), 3)
  expect_equal(tibble(a=c(1,1,2)) %>% countUnique(a), 2)
  expect_equal(countUnique(c(1,1,2)), 2)
  expect_equal(c('a',1, 1) %>% countUnique(), 2)
})

test_that('Test frequencyCount', {
  fileIn <- dataPDRCreate()
  dta <- arrow::open_dataset(fileIn %s+% '.parquet')
  tmp <- frequencyCountDistinct(dta, 'ATC')
  expect_equal(tmp, tibble(ATC=c('N06BA01', 'N06BA02'), N=c(1,1), Percent=c(50,50)))
  tmp <- frequencyCountDiscrete(dta, ATC)
  expect_equal(tmp, tibble(ATC=c('N06BA01', 'N06BA02'), N=c(3,1), Percent=c(75,25)))
  tmp <- dta %>% frequencyCountDiscrete(ATC)
  expect_equal(tmp, tibble(ATC=c('N06BA01', 'N06BA02'), N=c(3,1), Percent=c(75,25)))
  # This will just be a simple rowcount
  expect_equal(frequencyCountDiscrete(dta), tibble(N=4, Percent=100))
  # Count over PIN
  tmp <- frequencyCountDistinct(dta, 'ATC', 'lopenr')
  expDta <- data.frame(ATC=c('N06BA01', 'N06BA02'), N=c(2, 1), Percent=c(100*2/3, 100/3))
  expect_equal(as.data.frame(tmp), expDta)
  tmp <- frequencyCountDiscrete(dta, lopenr, ATC)
  expect_equal(tmp, tibble(lopenr=c('I1','I2','I3'), ATC=c('N06BA01','N06BA02','N06BA01'), N=c(1,1,2), Percent=c(25,25,50)))
  # Counts over two distinct values
  tmp <- frequencyCountDistinct(dta, c('ATC', 'Date'), 'lopenr')
  expDta <- data.frame(
    ATC=c(rep('N06BA01', 3), 'N06BA02'),
    Date=c('2024-12-31','2025-01-03', 'x','2025-01-03'),
    N=rep(1,4),
    Percent=rep(25,4)
  )
  expect_equal(as.data.frame(tmp), expDta)
  tmp <- frequencyCountDiscrete(dta, ATC, Date, lopenr)
  expDta <- collect(dta)[, c('ATC', 'Date', 'lopenr')]
  expDta$N <- 1
  expDta$Percent <- 25
  expect_equal(tmp, expDta %>% arrange(ATC))
  df <- data.frame(id=c(1,1,1,3,3,3), b=c('a','a','b','c','c','a'))
  expect_equal(frequencyCountDistinct(df, 'b', id), tibble(b=c('a','b','c'), N=c(2,1,1), Percent=c(50,25, 25)))
})

test_that('percent', {
  df <- data.frame(A=c('A','A','B'),N=c(1,1,2))
  o <- df %>% percent()
  expect_equal(o$Percent, c(25,25,50))
  o <- df %>% percent(A)
  expect_equal(o$Percent, c(50,50,100))
})

test_that('standardize', {
  x <- 1:3
  expect_equal(standardize(x), -1:1)
})
