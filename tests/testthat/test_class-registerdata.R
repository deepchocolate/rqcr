test_that('drugRegister', {
  a <- drugRegister(FILE_NO_DR)
  expect_equal(attributes(a)$columns, NULL)
  #expect_equal(getColumn('IID', 'Norwegian'), 'lopenr')
  expect_error(getColumn(a, 'IID'))
  a <- configure(a, 'data/no-drug-register.yaml')
  expect_equal(getColumn(a, 'individual'), 'IID')
  # Logging
  expect_equal(getLog(a), NULL)
  a <- logExclusion(a, 'Individual', 1, 'Dislike')
  expect_equal(getLog(a), data.frame(action='Exclusion', what='Individual', statistic=1, description='Dislike'))
  #print(head(a))
  #print(attributes(a))
  # Error because the atc code identifier (column) is not set
  expect_error(frequencyATC(a))
  a <- setColumn(a, 'atcCode4', 'atcCode4')
  r <- frequencyATC(a)
  expect_equal(r$atcCode4, 'N06B')
  expect_equal(r$N, 4)
  expect_equal(r$Percent, 100)
  a <- renameColumns(a, NO_NAMES$DRUG_REGISTER, T, F)
  # Index observations
  a <- a %>% indexObservations()
  expect_equal(a$i, 1:4)
  a <- a %>% indexObservations(IID)
  expect_equal(a$i, c(1,1,1,2))
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

})
