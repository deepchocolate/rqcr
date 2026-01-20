test_that('dbConnectMany', {
  dirDB <- setupDatabases()
  dbA <- dirDB %s+% '/database-a.duckdb'
  dbB <- dirDB %s+% '/database-b.duckdb'
  dbCon <- dbDuckConnectMany(`database_a`=dbA, `database_b`=dbB)
  res <- dbGetQuery(dbCon, 'SHOW DATABASES')
  expect_equal(res$database_name, c('database_a', 'database_b', 'memory'))
  res <- dbGetQuery(dbCon, 'SELECT * FROM database_a.tableA')
  expect_equal(res$Var, 'A')
  res <- dbGetQuery(dbCon, 'SELECT * FROM database_b.tableB')
  expect_equal(res$Var, 'B')
})
