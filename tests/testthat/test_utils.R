test_that('calculateAge', {
  d <- calculateAge(c('20240101', '20260101'), '20250101')
  expect_equal(d, c(1, -1))
  d <- calculateAge(c('20240101', '20260101'), c('20250101','20270101'))
  expect_equal(d, c(1, 1))
  d <- calculateAge(c('20240101', '20260101', NA), '20250101')
  expect_equal(d, c(1, -1, NA))
  expect_error(calculateAge('20240101', c('20240101', '20250101')))
})

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

test_that('collapseUnique', {
  a <- c('C','A','A')
  expect_equal(collapseUnique(a), 'A,C')
  expect_equal(collapseUnique(a, sort=F), 'C,A')
  expect_equal(collapseUnique(a, sep='-'), 'A-C')
  expect_equal(c(), NULL)
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
  expect_equal(d, -1)
  d <- diffYears('20240101', '20250101')
  expect_equal(d, -1)
  d <- diffYears(c('20240101', '20250101','20260210'), '20250101')
  expect_equal(d, c(-1,0,1.11), tolerance=0.01)
  d <- diffYears(c('20240101', '20250101','20260210'), c('20240101', '20250101','20260210'))
  expect_equal(d, c(0,0,0))
})

test_that('diffDays', {
  d <- diffDays('2001-01-01', '2000-12-30')
  expect_equal(d, 2)
  d <- diffDays('2001-01-01', c('2000-12-30', '20010103'))
  expect_equal(d, c(2,-2))
  d <- diffDays(c('2000-12-30', '20010103'), '2001-01-01')
  expect_equal(d, c(-2, 2))
  df <- data.frame(A=c('2000-12-30', '20010103'), B=c('2001-01-01', '2001-01-03'))
  d <- diffDays(df$A, df$B)
  expect_equal(d, c(-2, 0))
  df <- df %>% addDiffDays(A, B)
  expect_equal(colnames(df), c('A','B', 'diffDaysAB'))
  expect_equal(df$diffDaysAB, c(-2, 0))
})

test_that('distanceBetween', {
  times <- c(1,3,7)
  states <- c('a', 'b','c')
  expect_equal(distanceBetween(states, times, 'a', 'c'), c(NA,NA,NA))
  expect_equal(distanceBetween(states, times, 'b', 'c'), c(NA,NA,4))
  times <- c(times, 8,8)
  states <- c(states,'b', 'd')
  # Multiple destinations
  expect_equal(distanceBetween(states, times, 'b', c('c', 'd')), c(NA,NA,4,NA,0))
  # Test with integers
  expect_equal(distanceBetween(1,1,1,1), NA)
  expect_equal(distanceBetween(c(1,2),c(10,14), 1, 2), c(NA,4))
  expect_error(distanceBetween(1:3,1:2, 1, 2), 'times and statest need to have equal length')
})

test_that('elements', {
  df <- data.frame(A=c('A','B','B'), B=c(1,2,2))
  expect_equal(elements(df, B), c(1,2))
  expect_equal(elements(df), unique(df))
  expect_equal(elements(df$A), c('A','B'))
  expect_error(elements(df$C))
})

test_that('expandRange', {
  expect_equal(expandRange(c('A','A1-A3', 'A08','C100-C101')), c('A','A1','A2','A3', 'A08','C100','C101'))
  expect_equal(expandRange(c('A','A01-A03','B00-B02')), c('A','A01','A02','A03','B00','B01','B02'))
  vec <- c('A', 'A1-A3', 'ABC10-ABC11', 'ABC19','B3-B2')
  expect_equal(expandRange(vec, leadZeroes=F), c('A', 'A1','A2','A3', 'ABC10','ABC11', 'ABC19', 'B3', 'B2'))
  expect_equal(expandRange(c('A8#A10', 'A99-A101'), c('#', '-')), c('A08', 'A09', 'A10', 'A099','A100','A101'))
  expect_equal(expandRange('A8#A10', '#', leadZeroes=F), c('A8', 'A9', 'A10'))
  expect_equal(expandRange('A10-A8'), c('A10', 'A09', 'A08'))
  expect_equal(expandRange('A09-A10'), c('A09', 'A10'))
})

test_that('getPrefixMatches', {
  expect_equal(getPrefixMatches('a', c('aa', 'A', 'bA')), c('aa', 'A'))
  expect_equal(getPrefixMatches(c(1, 'a'), c('aa', 'A', 'bA')), list(`1`=character(), a=c('aa', 'A')))
  expect_equal(getPrefixMatches(c(1, 'a'), c(10, 'aa')), c('1'='10', a='aa'))
})

test_that('glueCollapse', {
  expect_equal(glueCollapse('hello {tmp}',tmp=c('a','b'), sep='-'), 'hello a-b')
})

test_that('whichTransitionsInterval', {
  df <- data.frame(A=c('a', 'b', 'a', 'b', 'a', 'c'),
                   B=c(0,1,2,4,5,6))
  out <- whichTransitionsInterval(df$A, df$B, 'a', 'b')
  expect_equal(1:4, out)
  out <- whichTransitionsInterval(df$A, df$B, 'a', 'b', 2,4)
  expect_equal(3:4, out)
  out <- whichTransitionsInterval(df$A, df$B, 'a', 'b', 0,1)
  expect_equal(1:2, out)
  df$subject <- c('G1','G1','G2','G2','G3','G3')
  # Illustrate how to integrate when applying function by groups
  out <- df %>% arrange(subject, B) %>% group_by(subject) %>%
    filter(row_number() %in% whichTransitionsInterval(A, B, 'a', 'b', 0,1))
  expect_equal(c('a','b'), out$A)
  expect_equal(0:1, out$B)
  expect_equal(c('G1','G1'), out$subject)
})

test_that('indexAlong, adjacent, positionsAdjacent', {
  dta <- data.frame(A=c('A','A','B'), B=1:3, C=c('a','b', 'c'))
  expect_equal(indexAlong(dta), 1:3)
  expect_equal(indexAlong(tibble(dta), 'A'), c(1,2,1))
  expect_equal(indexAlong(dta, 'A', 'B'), c(1,1,1))
  # indexAlongUnique
  expect_equal(indexAlongUnique(dta$A), c(1,1,2))
  expect_equal(indexAlongUnique(dta, A), tibble(cbind(dta, i=c(1,1,2))))
  expect_equal(indexAlongUnique(dta, A, .name='N', .nameMax='NMax'), tibble(cbind(dta, N=c(1,1,2), NMax=2)))
  expect_equal(indexAlongUnique(dta, A, A), tibble(cbind(dta, i=c(1,1,1))))
  expect_equal(indexAlongUnique(dta, B, A), tibble(cbind(dta, i=c(1,2,1))))
  # positions ...
  expect_equal(positionsAdjacent(c(1,4,5)), c(2,3))
  expect_equal(positionsAdjacent(4:6), 1:3)
  expect_equal(positionsDistant(c(1,4,5)), c(1,2))
  expect_equal(positionsDistant(4:6), numeric(0))
  expect_equal(adjacent(c(1,2,5)), 1:2)
  expect_equal(adjacent(1:3), 1:3)
  expect_equal(adjacent(c(10, 1,2,5,6)), c(1:2, 5:6))
  expect_equal(indexAlongDistance(c(1,2,3,6,8,9)), c(1,1,1,2,3,3))
  expect_equal(indexAlongDistance(c(1,1,2,3,5)), c(1,1,1,1,2))
})

test_that('labelStrings', {
  dta <- data.frame(id=c(1,1,2,2,3),
                    str=c('A','B', 'AA', 'BB', 'CA'),
                    labExp=c('LabA','LabB','LabA','LabB', ''))
  dta$out <- labelStrings(dta$str, c('A', 'B'), c('LabA', 'LabB'))
  expect_equal(dta$out, dta$labExp)
  # Applying on factor
  dta$str <- factor(dta$str)
  dta$out <- labelStrings(dta$str, c('A','B'), c('LabA', 'LabB'))
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

test_that('Test intersectEquals', {
  v <- c('a', 'b')
  expect_true(intersectEquals(v, c(v, 'c'), c(v, 'A','C')))
  expect_false(intersectEquals(c(v, 'C'), c(v, 'c'), c(v, 'A','C')))
})

test_that('splitVector', {
  expect_equal(splitVector(1:3, 2), list(1, 2:3))
  expect_equal(splitVector(1:4, c(2,3)), list(1, 2, 3:4))
})

test_that('txtNPercent', {
  expect_equal(txtNPercent(1, 10), '1 (10)')
  expect_equal(txtNPercent(1, 3), '1 (33.3)')
})
