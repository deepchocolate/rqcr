#' Count the unique elements in data
#' @export
#' @import dplyr
#' @param dta A data.frame or similar.
#' @param ... Columns in `dta`.
countUnique <- function (dta, ...) {
  dta %>% distinct(...) %>% nrow()
}

#' Create age/positive number intervals from a variable.
#' @param x A vector of numbers.
#' @param by Width of intervals.
#' @param from Interval start.
#' @param to Interval end.
#' @param sep Separator text between intervals.
#' @details
#' Create age intervals
#' Each number in x will be placed in one of the intervals between arguments from
#' and to where by specifies the range of each interval. The right end of the interval
#' is by default set to Inf, i.e. oldest will be "anyone above 90" if by=90.
#' @export
createIntervalsAge <- function (x, by, from=0, to=90, sep='-') {
  ageL <- seq(from=from, to=to, by=by)
  ageU <- ageL + by - 1
  ageU <- ageU[-length(ageU)]
  lab <- paste(ageL, ageU, sep=sep)
  lab[length(lab)] <- substr(lab[length(lab)], 1, 3)
  cut(x, breaks=c(ageL, Inf), labels=lab, include.lowest=T, right=F)
}

#' Calculate age based on YYYY-MM-DD.
#' @export
#' @importFrom lubridate today
#' @param dateBirth Birth date.
#' @param when Date when age is to be calculated.
calculateAge <- function (dateBirth, when=today()) {
  if (length(dateBirth) != length(when) & length(when) > 1) stop('when has to be either a single date or a vector of length equal to dateBirth')
  diffYears(dateBirth, when)
}

#' Collapse unique elements in a vector
#' @export
#' @param x A vector.
#' @param sep A character separating elements.
#' @param sort Whether to sort elements.
collapseUnique <- function (x, sep=',', sorted=T) {
  x <- unique(x)
  if (sorted) x <- sort(x)
  paste0(x, collapse=sep)
}

#' Calculate difference between dates in years.
#' @export
#' @param datesA A date/vector of dates.
#' @param datesB A date/vector of dates.
#' @import lubridate
diffYears <- function (datesA, datesB) {
  interval(datesA, datesB)/years(1)
}

#' Calculate temporal distance between a set of states
#' @details
#' This function calculates the difference between a set of states in time. The
#' origin state is only one, but the destinations can be many.
#'
#' @export
#' @param states States corresponing to each time in `times`.
#' @param times Observation times for `states`
#' @param from The origin state.
#' @param to The destination state(s).
#' @return A sequence of time differences with NA if the origin state is not equal
#' to `from` or the destination is not present in `to`.
distanceBetween <- function (states, times, from, to) {
  len <- length(times)
  if (len != length(states)) stop('times and statest need to have equal length')
  if (len == 1) return(NA)
  stFr <- which(states[-len] %in% from & states[-1] %in% to)
  stTo <- which(states[-1] %in% to)
  if (length(stFr) == 0) return(rep(NA, len))
  dTime <- diff(times)
  o <- rep(NA, length(dTime))
  o[stFr] <- dTime[stFr]
  c(NA, o)
}

#' Find adjacent rows where a state goes from one to the next within a time frame
#' @export
#' @param states States corresponing to each time in `times`.
#' @param times Observation times for `states`
#' @param from The origin state.
#' @param to The destination state(s).
#' @param lower Lower end of interval.
#' @param upper Upper end of interval.
whichTransitionsInterval <- function(states, times, from, to, lower=-Inf, upper=Inf) {
  dTimes <- distanceBetween(states, times, from, to)
  wTimes <- which(dTimes >= lower & dTimes <= upper)
  c(rbind(wTimes-1,wTimes))
}

#' Count frequencies of unique values
#' @import dplyr
#' @export
#' @details
#' Without any columns passed in (...) this function simply returns the distinct
#' values, counts and percent of these values. This behaviour is the same as `unique`,
#' with a count for each value and a parecent. However, by passing other
#' columns through `...` the function performs this operation over these columns so
#' that each value is only counted once for each group defined as the unique combination
#' of values in these columns.
#' @seealso [frequencyCountDiscrete()]
#' @param dta Any data accepted by dplyr.
#' @param col Column in dta to count values.
#' @param ... Columns to select in data.
#' @return A tibble with columns in `col`, and and columns `N` four counts
#' and `Percent` for percentage of values.
frequencyCountDistinct <- function (dta, col, ...) {
  dta <- dta %>% select(..., all_of(col)) %>% distinct() %>% select(all_of(col)) %>% collect() %>% count(pick(all_of(col)), name='n') %>%
    arrange(!!as.name(col))
  colnames(dta) <- c(col, 'n')
  n <- sum(dta$n)
  dta$Percent <-  100*dta$n/n
  colnames(dta) <- c(col, 'N', 'Percent')
  dta
}

#' Count frequencies of discrete values
#' @details
#' Counting is performed by counting rows in data which forms combinations
#' of unique values by the specified columns in `...`. This is what `dplyr::count`
#' does, but adds a percentage column.
#'
#' @export
#' @import dplyr
#' @param .data Any data accepted by dplyr.
#' @param ... Column(s) to form discrete values in `dta`.
#' @return A tibble with columns in `...` and columns for counts (`N`) and percent.
frequencyCountDiscrete <- function (.data, ...) {
  .data %>% select(...) %>% collect() %>% count(..., name='N') %>%
    mutate(Percent=100*.data$N/sum(.data$N))
}

#' Index observations over grouped data
#' @export
#' @details
#' Without passing any groups provided, this function just returns an index
#' (row number) of the data.
#' @seealso [indexAlongUnique()]
#' @importFrom stats ave
#' @param dta A data frame or similar.
#' @param ... Columns in `dta` defining the groups.
indexAlong <- function (dta, ...) {
  grps <- c(...)
  if (length(grps) == 0) return(1:nrow(dta))
  as.integer(ave(dta[,1], dta[,grps], FUN=function (x) 1:length(x)))
}

#' Index unique observations
#' @export
#' @seealso [indexAlong()]
#' @importFrom stats ave
#' @param dta A data frame or similar.
#' @param col The column to index
#' @param ... Columns in `dta` defining
indexAlongUnique <- function (dta, col, ...) {
  grps <- c(...)
  if (length(grps) == 0) {
    o <- as.integer(factor(dta[,col], labels=1:length(unique(dta[,col]))))
    return(o)
  }
  as.integer(ave(dta[,col], dta[,grps], FUN=function (x) {
    o <- factor(x, labels=1:length(unique(x)))
    as.integer(o)
  }))
}

#' Test if a vector is equal to the intersection of other vectors.
#' @export
#' @param x A vector.
#' @param ... Vectors to intersect.
intersectEquals <- function (x, ...) {
  ints <- Reduce(intersect, list(...))
  setequal(x, ints)
}

#' Test if x is a date
#' @export
#' @param x Anything
#' @param format If any other format than specified by as.Date
isDate <- function (x, format) {
  x <- as.Date(x, format=format, optional=TRUE)
  ifelse(is.na(x), F, T)
}

# Label a string based on whether it occurs in a set of strings.
# By default matching is performed by using a case sensitive matching fo the
# first characters in a string. Any function to do matching can be passed through FUN
# as long as it returns TRUE/FALSE for each pattern.
# Parameters are documented by labelStrings below
labelString <- function (string, stringSets, labels, exclude, FUN=startsWith) {
  o <- ''
  hits <- FUN(string, stringSets)
  if (!any(hits)) return(o)
  if (!isFALSE(exclude)) {
    misses <- FUN(string, exclude)
    if (length(exclude) == 1) misses <- rep(misses, length(stringSets))
    if (any(is.na(misses))) misses[is.na(misses)] <- F
    if (any(misses)) hits[which(misses)] <- F
  }
  if (any(hits)) {
    i <- which(hits)
    if (!isFALSE(exclude)) {
      if (!misses[i]) o <- labels[i]
    } else o <- labels[i]
  }
  o
}
#' Match a vector against another character set and label matches.
#' @name labelStrings
#' @export
#' @details
#' `labelStrings` work similar to performing a left join, e.g., using `merge(x,y,all.x=T,...)`
#' if `y` is some data containing a label for a column existing in both `x` and `y`.
#' The difference is that this function allows for flexible matching (currently only
#' the starting characters in string), and allows for excluding patterns
#' For example, if all strings starting with "A" should have some label "Group A", but
#' not strings starting with "AB", "AB could be added as an exclusion pattern
#' (see example).
#' @examples
#' x <- c("A", "ABC", "DEF")
#' labelStrings(x, "A", "Group A", "AB")
#'
#' @param strings A vector of strings.
#' @param sets A vector of strings to match against.
#' @param labels A vector of labels to assign to matches.
#' @param exclude A vector of strings to exclude. Either one for all sets or one per set.
setGeneric('labelStrings', function (strings, sets, labels, exclude) standardGeneric('labelStrings'))
#' @rdname labelStrings
setMethod('labelStrings', signature('factor', 'character','character','missing'),
          function (strings, sets, labels) labelStrings(as.character(strings), sets, labels))
#' @rdname labelStrings
setMethod('labelStrings', signature('character','character','character', 'missing'),
          function (strings, sets, labels) labelStrings(strings,sets,labels,F))
#' @rdname labelStrings
setMethod('labelStrings', signature('character', 'list'),
          function (strings, sets) {
            labels <- names(sets)
            strs <- sapply(sets, FUN=function(x) x$sets)
            labels <- sapply(labels, function (x) rep(x, ncol(strs)))
            strs <- c(strs)
            labels <- c(labels)
            exclude <- sapply(sets, FUN=function(x) x$exclude)
            strs <- unlist(strs, use.names=F)
            exclude <- unlist(exclude, use.names=F)
            labelStrings(strings, strs, labels, exclude)
          })
#' @rdname labelStrings
setMethod('labelStrings', signature('character', 'character', 'character', 'ANY'),
          function (strings, sets, labels, exclude) {
            if (length(sets) != length(unique(sets))) stop('sets contain duplicate patterns')
            if (length(sets) != length(labels)) stop('sets and labels need to be of equal length')
            lenExclude <- length(exclude)
            if (lenExclude < length(sets) & lenExclude > 1) stop('exclude needs to be length 1 or equal to sets')
            sapply(strings, FUN=labelString, stringSets=sets, labels=labels, exclude=exclude, USE.NAMES=F)
          })

#' Get the number(s) in a vector that are closest to another number.
#' @export
#' @param x A vector of numbers.
#' @param y The comparison number.
#' @param na.rm Whether to ignore missing.
#' @return The number(s) in x closest to y.
closest <- function (x, y=0, na.rm=T) {
  dx <- abs(x - y)
  w <- which(dx == min(dx, na.rm=na.rm))
  x[w]
}

#' Maximum of negative numbers in a vector.
#' @export
#' @param x A vector.
#' @param na.rm Whether to ignore missing.
maxNegative <- function (x, na.rm=T) {
  x <- x[x < 0]
  ifelse(length(x) == 0, NA, closest(x, na.rm=na.rm))
}

#' Minimum of positive numbers in a vector.
#' @export
#' @param x A vector.
#' @param na.rm Whether to ignore missing.
minPositive <- function (x, na.rm=T) {
  x <- x[x >= 0]
  ifelse(length(x) == 0, NA, closest(x, na.rm=na.rm))
}

#' Recode values to missing.
#' @param x A vector of inputs.
#' @param cases Cases in x to convert to NA.
#' @importFrom dplyr case_match
#' @export
recodeToNA <- function (x, cases) {
  if (all(is.na(x))) return(x)
  dplyr::case_match(x, cases~NA, .default=x)
}

#' Recode individual sex to M(ale)/F(emale).
#' @param x A vector of inputs.
#' @param default Default value of x if no match is made.
#' @importFrom dplyr case_match
#' @export
recodeSex <- function (x, default=NA) {
  x <- as.character(x)
  dplyr::case_match(x,
                    c('1','M')~'M',
                    c('2', 'K')~'F',
                    .default=default
  )
}

#' Create a string with n and percent
#' @param n A number
#' @param N Another number
#' @param strmask The text to put numbers in. Must contain "\{n\}" and "\{percent\}".
#' @export
txtNPercent <- function(n, N, strmask='{n} ({percent})') {
  percent <- round(100*n/N, 3)
  glue::glue(strmask)
}
