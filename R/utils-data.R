#' @import dplyr
NULL
#' Add the preceding value of a sequence as a column in data
#' @export
#' @import dplyr
#' @param .data Any tabular data accepted by dplyr
#' @param colStates The column indicating current state.
#' @param colName Name of column for the origin state. Defaults to "from`colStates`".
addPredecessor <- function (.data, colStates, colName=NULL) {
  colName <- substitute(colName)
  if (is.null(colName)) colName <- paste0('from', substitute(colStates))
  else colName <- deparse(colName)
  .data %>% mutate({{ colName }} := lag({{ colStates }}))
}

#' Get data from an excelfile by searching through all sheets.
#' @export
#' @param file The excel file path.
#' @param col The column to search.
#' @param val The value to look for in col.
#' @param skip Number of lines to skip in each sheet.
#' @param colsSelect Which columns to select.
getExcelData <- function (file, col, val, skip, colsSelect) {
  sheets <- readxl::excel_sheets(file)
  col <- tolower(col)
  colsSelect <- tolower(colsSelect)
  dta <- NULL
  for (sheet in sheets) {
    tmp <- readxl::read_xlsx(file, sheet, skip=skip)
    cnames <- tolower(colnames(tmp))
    colnames(tmp) <- cnames
    if (col %in% cnames) {
      tmp$sheet <- sheet
      dta <- rbind(dta,
                   tmp[tmp[,col]==val, c('sheet',colsSelect)])
    }
  }
  dta
}

#' Merge two datasets and indicate falling within a date range
#' @export
#' @name indicateEventsWithin
#' @param .data Anything accepted by dplyr.
#' @param events Data with events.
#' @param dateStart Period start date column in `.data`.
#' @param dateEnd Period end date column in `.data`.
#' @param dateEvent Event date column in `events`.
#' @param .by Grouping columns.
#' @param .slide Integer to shift `dateStart` and `dateEnd` with.
#' @param .ids Ids for joining, adheres to `dplyr::join_by`.
#' @param .name Column name for event indicator.
setGeneric('indicateEventsWithin', function (.data, events, dateStart, dateEnd, dateEvent, .by=NULL, .slide=0, .ids=NULL, .name='event') standardGeneric('indicateEventsWithin'))
#' @rdname indicateEventsWithin
setMethod('indicateEventsWithin', signature('data.frame', 'data.frame'),
          function (.data, events, dateStart, dateEnd, dateEvent, .by, .slide=0, .ids=NULL, .name='event') {
            .data <- .data %>% left_join(events, by=join_by({{ .ids }}))
            .data <- .data %>% mutate(across(c({{dateStart}}, {{dateEnd}}, {{dateEvent}}), lubridate::ymd))
            .data %>% group_by({{ .by }}) %>% mutate('{.name}' := {{dateStart}} + .slide <= {{dateEvent}} & {{dateEnd}} + .slide >= {{dateEvent}}) %>%
              mutate('{.name}' := tidyr::replace_na(.data[[.name]], 0)) %>% ungroup()
          })

#' Indicate novel elements in ordered data
#' @name indicateNovel
#' @export
setGeneric('indicateNovel', function (.data, ...) standardGeneric('indicateNovel'))
#' @rdname indicateNovel
setMethod('indicateNovel', signature('factor'),
          function (.data) {
            indicateNovel(as.character(.data))
          })
#' @rdname indicateNovel
setMethod('indicateNovel', signature('ANY'),
          function (.data) {
            o <- rep(0, length(.data))
            vs <- match(unique(.data), .data)
            o[vs] <- 1
            o
          })
#' @rdname indicateNovel
#' @param .data Any data object accepted by dplyr
#' @param times Column referring to the temporal ordering.
#' @param values Column referring to values to mark as new.
#' @param ... Grouping factors.
#' @param .name Name of the column indicating novelty.
setMethod('indicateNovel', signature('data.frame'),
          function (.data, times, values, ..., .name='novel') {
            .data %>% group_by(...) %>% arrange({{ times }}) %>%
              mutate("{.name}" :=indicateNovel({{ values }})) %>% ungroup()
            })

#' Join data based on column as prefixes
#' @description
#' This function joins data based on prefixes in a column. If `x` is data with
#' a character column, and `y` data with a column of prefixes, `joinLeftPrefix`
#' will perform a join pairs of columns where prefixes in `y` matches prefixes in `x`.
#'
#' @name joinPrefix
#' @aliases joinLeftPrefix
#' @param x Any data object accepted by fuzzyjoin.
#' @param y Data with prefix column(s),
#' @param by Specification of joining, equavalent to `dplyr::join_by`. Note: Only equality joins (`==`) accepted.
#' @param ... Other arguments
#' @export
setGeneric('joinLeftPrefix', function (x, y, by, ...) standardGeneric('joinLeftPrefix'))
setOldClass('dplyr_join_by')
#' @rdname joinPrefix
setMethod('joinLeftPrefix', signature('data.frame','data.frame', 'dplyr_join_by'),
          function (x, y, by) {
            mf <- as.list(stats::setNames(rep('startsWith', length(by$x)), by$x))
            fuzzyjoin::fuzzy_left_join(x, y, by, match_fun=startsWith)
          })

#' Split data by rows that are equal to some value.
#' @details
#' The motivation for this function comes from spreadsheet in which multiple tables
#' are present in a sheet. If such tables are separated by some character or an entire
#' empty row this function can split such data into its constituents. If using
#' `header=TRUE` it is currently required that tables have the same columns.
#' @seealso [mergeDataByRow()]
#' @export
#' @param dta Usually a data.frame.
#' @param splitIndicator A value to delineate rows that separate tables in data.
#' @param header If TRUE, the first row in each dataset are used as a header for the data.
splitDataByRow <- function (dta, splitIndicator=NA, header=F) {
  if (is.na(splitIndicator)) whch <- which(apply(is.na(dta),1, all))
  else whch <- which(apply(dta == splitIndicator, 1, all))
  out <- list()
  i <- 1
  j <- i
  for (split in whch) {
    if (split - i > 0) {
      d <- dta[i:(split-1),]
      if (header) {
        colnames(d) <- d[1,]
        d <- d[-1,]
      }
      out[[j]] <- d
    }
    i <- split + 1
    j <- j + 1
  }
  n <- nrow(dta)
  if (n - i >= 0) {
    d <- dta[i:n,]
    if (header) {
      colnames(d) <- d[1,]
      d <- d[-1,]
    }
    out[[j]] <- d
  }
  rmWhich <- which(sapply(out, FUN=is.null))
  if (length(rmWhich) > 0) out <- out[-rmWhich]
  out
}

#' Merge data by row like rbind, but with options to separate data.
#' @export
#' @details
#' Like the motivation for `splitDataByRow`, the motivation for this function
#' comes from spreadsheets where several tables may be present in a sheet.
#' @seealso [splitDataByRow()]
#' @param ... Tabular objects like data frames.
#' @param insertColnames Insert the column names of each table as the first row in the data.
#' @param separator Separate each table with a row of this value (use NA for whitespace).
#' @return A data.frame.
mergeDataByRow <- function(..., insertColnames=T, separator=NA) {
  tables <- list(...)
  out <- NULL
  for (i in 1:length(tables)) {
    tb <- tables[[i]]
    if (insertColnames) out <- rbind(out, colnames(tb))
    out <- rbind(out, as.matrix(tb), dimnames=NULL)
    if (!isFALSE(separator)) out <- rbind(out, rep(NA, ncol(tb)))
  }
  if (!isFALSE(separator)) out <- out[-nrow(out),]
  data.frame(out,row.names = NULL)
}

#' Merge time periods
#' @export
#' @name mergePeriods
#' @param dates Period starting date (YYYY-MM-DD)
#' @param days Period length in days
#' @param maxDistance Maximum distance between end and start of two periods for merging.
#' @param reset Whether to add the overlapping time between two periods at then end of the merged period.
#' @param resetFun A callable to use for dates with multiple days provided.
mergePeriods <- function (dates, days, maxDistance=0, reset=F, resetFun=mean) {
  if (reset == T) {
    posSameDates <- which(dates[-length(dates)] == dates[-1])
    if (length(posSameDates) > 0) {
      days <- ave(days, dates, FUN=resetFun)
      dates <- dates[-(posSameDates+1)]
      days <- days[-(posSameDates + 1)]
    }
  }
  times <- diffDays(dates, first(dates))
  timesEnd <- times + days
  # Reset days for overlapping periods (except the end): No stockpiling
  if (reset == T) {
    pos <- which(timesEnd[-length(timesEnd)] > times[-1])
    timesEnd[pos] <- diff(times)[pos]
    days[pos] <- diff(times)[pos]
  }
  # Split positions for Overlapping periods: Start before end of the preceeding period
  pos <- which(times[-1] > timesEnd[-length(timesEnd)] + maxDistance)
  # Split positions for non-overlapping periods
  npos <- which(times[-1] <= timesEnd[-length(timesEnd)] + maxDistance)
  # If there are no non-overlapping, stop
  if (length(npos) == 0) return(tibble(date=dates, days=days))
  # Split dates and days where after overlaps
  splts <- splitVector(days, pos+1)
  date <- splitVector(dates, pos+1)
  # Take the first date of the overlapping periods
  dates <- do.call(c, lapply(date, FUN=first))
  # Group and sum the days
  ids <- 1:length(splts)
  reps <- lapply(splts, FUN=length)
  ids <- rep(ids, unlist(reps))
  days <- c(tapply(days, factor(ids), FUN=sum), use.names = F)
  mergePeriods(dates, days, maxDistance, reset, resetFun)
}

#' Standardize columns in data
#' @export
#' @name standardizeColumns
#' @param .data Anything accepted by dplyr.
#' @param ... Columns to standardize.
#' @param .names Use to name the standardize column see `dplyr::across`, E.g `"prefix{.col}"`.
setGeneric('standardizeColumns', function (.data, ..., .names=NULL) standardGeneric('standardizeColumns'))
#' @rdname standardizeColumns
setMethod('standardizeColumns', signature('data.frame'),
          function (.data, ..., .names=NULL) {
            .data %>% mutate(across(.cols=c(...), .fns=standardize, .names=.names))
          })

#' Update values conditionally in tabular data
#' @details
#' This function uses dplyr::case_when to perform conditional updates in data, but
#' by default preserving existing values for rows that do not match.
#'
#' @seealso [dplyr::case_when()]
#' @export
#' @import formula.tools
#' @importFrom rlang := enquos quo_get_expr as_label
#' @param .data Anything accepted by dplyr (can be piped).
#' @param col The column to update
#' @param ... Conditions for updates in the form `Column == "value" ~ Replacement`.
#' @param .warnIfMissing Throws a warning if the update condition is not identified in .data
updateCases <- function (.data, col, ..., .warnIfMissing=FALSE) {
  frms <- list()
    for (frm in enquos(...)) {
      frmQ <- quo_get_expr(frm)
      if (typeof(.data %>% pull({{col}})) == 'character') {
        varRhs <- rhs(frmQ)
        if (is.numeric(varRhs)) rhs(frmQ) <- as_label(varRhs)
      }
      if (.warnIfMissing | configRQCR('updateCases', 'warnings')) {
        rows <- lhs(frmQ)
        nrows <- .data %>% filter(eval(rows)) %>%  nrow()
        if (nrows == 0) warning('No rows found for ', frm)
      }
    frms <- append(frms, frmQ)
  }
  .data %>% mutate( "{{col}}" := case_when(!!!frms, .default = {{ col }}))
}
