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
  # Check with NA
  expect_equal(c('a',1,NA) %>% countUnique(), 2)
  dta <- tibble(A=c('a',1,NA), B=c(2,3,4))
  expect_equal(countUnique(dta), 3)
  expect_equal(countUnique(dta, na.rm=T), 2)
  expect_equal(dta %>% countUnique(A), 2)
})

test_that('Test frequencyCount', {
  fileIn <- dataPDRCreate()
  dta <- arrow::open_dataset(fileIn %s+% '.parquet')
  tmp <- frequencyCountDistinct(dta, 'ATC')
  expect_equal(tmp, tibble(ATC=c('N06BA01', 'N06BA02','N06BA12'), N=c(1,1,1), Percent=rep(100/3,3)), tolerance=.01)
  tmp <- frequencyCountDiscrete(dta, ATC)
  expect_equal(tmp, tibble(ATC=c('N06BA01', 'N06BA02','N06BA12'), N=c(4,1,2), Percent=c(57.14, 14.29, 28.57)), tolerance=0.01)
  tmp <- dta %>% frequencyCountDiscrete(ATC)
  expect_equal(tmp, tibble(ATC=c('N06BA01', 'N06BA02','N06BA12'), N=c(4,1,2), Percent=c(57.14,14.29,28.57)),tolerance = 0.01)
  # This will just be a simple rowcount
  expect_equal(frequencyCountDiscrete(dta), tibble(N=7, Percent=100))
  # Count over PIN
  tmp <- frequencyCountDistinct(dta, 'ATC', 'lopenr')
  expDta <- data.frame(ATC=c('N06BA01', 'N06BA02','N06BA12'), N=c(3, 1,1), Percent=c(100*3/5, 100/5, 100/5))
  expect_equal(as.data.frame(tmp), expDta)
  tmp <- frequencyCountDiscrete(dta, lopenr, ATC)
  expect_equal(tmp, tibble(lopenr=c('I1','I2','I3','I4','I5'), ATC=c('N06BA01','N06BA02','N06BA01','N06BA01','N06BA12'), N=c(1,1,2,1,2), Percent=c(14.29,14.29,28.57,14.29,28.57)),tolerance=.01)
  # Counts over two distinct values
  tmp <- frequencyCountDistinct(dta, c('ATC', 'Date'), 'lopenr')
  expDta <- data.frame(
    ATC=c(rep('N06BA12',2),rep('N06BA01', 3), 'N06BA02','N06BA01'),
    Date=c('2018-06-30','2018-08-05','2024-12-31','2025-01-03', 'x','2025-01-03','2020-05-30'),
    N=rep(1,7),
    Percent=rep(14.29,7)
  )
  expect_equal(as.data.frame(tmp) %>% arrange(Date), expDta %>% arrange(Date), tolerance=.01)
  tmp <- frequencyCountDiscrete(dta, ATC, Date, lopenr)
  expDta <- collect(dta)[, c('ATC', 'Date', 'lopenr')]
  expDta$N <- 1
  expDta$Percent <- 14.29
  expect_equal(tmp %>% arrange(lopenr), expDta %>% arrange(lopenr), tolerance=.01)
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
