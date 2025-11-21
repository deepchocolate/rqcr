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
  dta <- dta %>% select(..., all_of(col)) %>% distinct() %>% select(all_of(col)) %>% collect() %>% count(pick(all_of(col)), name='N') %>%
    arrange(!!as.name(col))
  dta %>% percent()
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
    percent()
}

#' Calculate percent based on counts
#' @export
#' @param .data Anything accepted by dplyr.
#' @param .colCount Name of column with counts.
#' @param ... Grouping variables.
percent <- function (.data, .colCount='N', ...) {
  .data %>% group_by(...) %>% mutate(Percent=100*!!as.name(.colCount)/sum(!!as.name(.colCount))) %>%
    ungroup()
}
