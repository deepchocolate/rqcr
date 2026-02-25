test_that('drugRegister', {
  a <- drugRegister(FILE_NO_DR)
  expect_equal(class(a), c("dataRegister", "data.table", "data.frame"))
  logCheckpoint(a, 'Test', 1, 'Test')
  logReset(a)
  a <- drugRegister(a)
  expect_equal(class(a), c("dataRegister", "data.table", "data.frame"))

  expect_error(getColumn(a, 'IID'))
  a <- configure(a, 'data/no-drug-register.yaml')
  expect_equal(getColumn(a, 'individual'), 'IID')
  # Logging
  expect_equal(getLog(a), NULL)
  a <- logExclusion(a, 'Individual', 1, 'Dislike')
  expect_equal(getLog(a), data.frame(action='Exclusion', what='Individual', statistic=1, description='Dislike'))

  # Exclusions
  expect_equal(getExclusions(a), NULL)
  a <- addExclusion(a, IID='I1', description='Test')
  expect_equal(getExclusions(a), list(list(description='Test', data=tibble(IID='I1'))))
  b <- applyExclusions(a, F)
  expect_equal(class(b), c('dataRegister',"data.table", "data.frame"))
  expect_equal(nrow(b), nrow(a) - 1)
  expect_false('I1' %in% b$IID)
  expect_contains(b$IID, c('I2', 'I3'))

  # Error because the atc code identifier (column) is misset
  a <- setColumn(a, 'code_atc', 'atcCode')
  expect_error(frequencyATC(a))
  a <- setColumn(a, 'code_atc', 'atcCode4')
  r <- frequencyATC(a)
  expect_equal(r$atcCode4, 'N06B')
  expect_equal(r$N, 4)
  expect_equal(r$Percent, 100)

  a <- renameColumns(a, NO_NAMES$DRUG_REGISTER$English, NO_NAMES$DRUG_REGISTER$Delivery, F)
  # Index observations
  a <- a %>% indexObservations()
  expect_equal(a$i, 1:4)
  a <- a %>% indexObservations(IID, .nameMax='I')
  expect_equal(a$i, c(1,1,1,2))
  expect_equal(a$I, c(1,1,2,2))
  ### Merge periods
  a <- subset(a, dateDelivery != 'x')
  expect_equal(getMergedPeriods(a), tibble(IID=c('I1','I2','I3'), date=c('2024-12-31','2025-01-03','2025-01-03'), days=c(1,2,-1)))
  dta <- tibble(id=c(1,1,1,2,2),
                date=c('2000-01-01', '2000-01-05', '2000-01-10', '2020-12-30', '2021-01-02'),
                days=c(4, 4, 4, 4, 2))
  a <- drugRegister(dta)
  a <- setColumn(a, 'individual', 'id')
  a <- setColumn(a, 'dispensation_date', 'date')
  a <- setColumn(a, 'dispensation_days', 'days')
  periods <- getMergedPeriods(a)
  expect_equal(periods, tibble(id=c(1,1,2), date=c('2000-01-01','2000-01-10','2020-12-30'), days=c(8,4,6)))

  ### Illustrate some issues with certain operations, notable change of class when grouping in dplyr
  # Class is preserved with bind_rows
  a <- drugRegister(tibble(A=1:3, B=1))
  expect_equal(class(a), c('dataRegister', 'tbl_df', 'tbl', 'data.frame'))
  a <- a %>% bind_rows(data.frame(A=4,B=2))
  expect_equal(class(a), c('dataRegister', 'tbl_df', 'tbl', 'data.frame'))
  a <- data.frame(A=1:3, B=1)
  b <- drugRegister(a)
  b <- left_join(b, tibble(A=1,C=2), join_by(A))
  expect_equal(class(b), c('dataRegister', 'data.frame'))
  b <- b %>% mutate(D=2)
  expect_equal(class(b), c('dataRegister', 'data.frame'))
  b <- b %>% arrange(desc(A))
  expect_equal(class(b), c('dataRegister', 'data.frame'))
  a <- drugRegister(a)
  a <- logCheckpoint(a, 'Test', 1, 'Test')
  expect_equal(class(a), c('dataRegister', 'data.frame'))
  # Class and its attributes is lost when using group by
  a <- a %>% arrange(A) %>% group_by(A)
  expect_equal(class(a), c('dataRegister', 'grouped_df','tbl_df', 'tbl', 'data.frame'))
  a <- a %>% ungroup()
  expect_equal(class(a), c('dataRegister', 'tbl_df', 'tbl', 'data.frame'))
  expect_error(getLog(a))
})

test_that('renameColumns', {
  fileIn <- dataPDRCreate()
  dta <- read.csv(FILE_NO_DR)
  dta2 <- renameColumns(dta, NO_NAMES$DRUG_REGISTER$Norwegian, NO_NAMES$DRUG_REGISTER$Delivery, verbose=F)
  expect_equal(dta2$lopenr, dta$Lopenummer_NPR)
  # No overlap should keep everything as is
  dta3 <- data.frame(A=1,B=2)
  expect_equal(renameColumns(dta3, NO_NAMES$DRUG_REGISTER$Norwegian, NO_NAMES$DRUG_REGISTER$Delivery), dta3)
})
