#' @import dplyr
NULL
utils::globalVariables(c('English', 'atc', 'lopenr','utlevdato'))
#' Constructor for register class
#' @param x Tabular data.
#' @param register The type of register.
dataRegister <- function (x, register) {
  structure(x, register=register, columns=NULL, logs=NULL,
            class=c('dataRegister', 'data.table', 'data.frame'))
}
setOldClass('dataRegister')

logMessage <- function (action, what, statistic, description, df=NULL) {
  rbind(df,
        data.frame(action=action, what=what, statistic=statistic, description=description))
}

#' Logging
#' @name logging
#' @aliases logExclusion
#' @export
#' @description
#' Functions to log operations on data.
#'
#' @param x A `dataRegister` object.
#' @param what What is excluded?
#' @param statistic Number of exclusions.
#' @param description A description of the exclusion.
setGeneric('logExclusion', function (x, what, statistic, description) standardGeneric('logExclusion'))
#' @rdname logging
setMethod('logExclusion', signature('dataRegister', 'character', 'numeric', 'ANY'),
          function (x, what, statistic, description) {
            attributes(x)$logs <- logMessage('Exclusion', what, statistic, description, attributes(x)$logs)
            x
          })

#' @export
#' @rdname logging
setGeneric('logCheckpoint', function (x, what, statistic, description) standardGeneric('logCheckpoint'))
#' @rdname logging
setMethod('logCheckpoint', signature('dataRegister', 'character', 'numeric', 'ANY'),
          function (x, what, statistic, description) {
            attributes(x)$logs <- logMessage('Checkpoint', what, statistic, description, attributes(x)$logs)
            x
          })

#' @export
#' @rdname logging
setGeneric('getLog', function (x) standardGeneric('getLog'))
#' @rdname logging
setMethod('getLog', signature('dataRegister'),
          function (x) {
            attributes(x)$logs
          })

#' Create object for the drug register
#' @name drugRegister
#' @export
#' @param x Register input: a file or tabular data.
#' @param config A configuration file in YAML format.
setGeneric('drugRegister', function (x, config) standardGeneric('drugRegister'))
#' @rdname drugRegister
setMethod('drugRegister', signature('character', 'character'),
          function (x, config) {
            x <- drugRegister(x)
            configure(x, config)
          })
#' @rdname drugRegister
setMethod('drugRegister', signature('character', 'missing'),
          function (x) {
            x <- data.table::fread(x)
            drugRegister(x)
            })
#' @rdname drugRegister
setMethod('drugRegister', signature('data.frame', 'character'),
          function (x, config) {
            x <- drugRegister(x)
            configure(x, config)
          })
#' @rdname drugRegister
setMethod('drugRegister', signature('data.frame', 'missing'),
          function (x) {
            dataRegister(x, 'drugRegister')
          })

#' Get column name using generic name
#' @import dplyr
#' @param .data A dataRegister object.
#' @param name The generic name.
setGeneric('getColumn', function (.data, name) standardGeneric('getColumn'))
setMethod('getColumn', signature('dataRegister', 'character'),
          function (.data, name) {
            cols <- attributes(.data)$columns
            if (!name %in% names(cols)) stop('Column not found: ', name)
            cols[[name]]
          })
setGeneric('setColumn', function (.data, name, column) standardGeneric('setColumn'))
setMethod('setColumn', signature('dataRegister', 'character', 'character'),
          function (.data, name, column) {
            attributes(.data)$columns[[name]] <- column
            .data
          })

#' Configure a `dataRegister` object
#' @export
#' @name configure
#' @param .data A `dataRegister` object.
#' @param file A yaml file with configuration.
setGeneric('configure', function (.data, file) standardGeneric('configure'))
#' @rdname configure
setMethod('configure', signature('dataRegister', 'character'),
          function (.data, file) {
            cfg <- yaml::read_yaml(file)
            attributes(.data)$columns <- cfg$identifiers
            if ('rename' %in% names(cfg)) {
              .data <- renameColumns(.data, data.frame(Delivery=names(cfg$rename), Norwegian=unlist(cfg$rename), row.names=NULL), verbose=F)
            }
            .data
          })

#' Calculate frequencies of ATC codes
#' @name frequencyATC
#' @export
#' @param x Input data.
#' @param ... Grouping variables.
setGeneric('frequencyATC', function (x, ...) standardGeneric('frequencyATC'))
#' @rdname frequencyATC
setMethod('frequencyATC', signature('dataRegister'),
          function (x, ...) {
            col <- getColumn(x, 'atcCode4')
            x %>% frequencyCountDiscrete( !!as.name(col), ... )
          })

#' Index observations
#' @export
#' @name indexObservations
#' @param .data Any data accepted by dplyr.
#' @param .nameIndex Column name.
#' @param .nameMax Column name for maximum.
#' @param ... Variables to group data by.
setGeneric('indexObservations', function (.data, ...) standardGeneric('indexObservations'))
#' @rdname indexObservations
setMethod('indexObservations', signature('dataRegister'),
          function (.data, ..., .nameIndex='i', .nameMax=NA) {
            .data[,.nameIndex] <- indexAlong(.data, ...)
            .data
          })

#' Merge dispensed drugs into non-overlapping treatment periods
#' @export
#' @aliases getMergedPeriods
#' @name mergePeriods
#' @seealso [mergePeriods()]
#' @param .data Anything accepted by dplyr.
#' @param ... arguments to `mergePeriods`.
setGeneric('getMergedPeriods', function (.data, ...) standardGeneric('getMergedPeriods'))
#' @rdname mergePeriods
setMethod('getMergedPeriods', signature('dataRegister'),
          function (.data, ...) {
            idCol <- getColumn(.data, 'individual')
            dateCol <- getColumn(.data, 'dispensation_date')
            dddCol <- getColumn(.data, 'dispensation_days')
            .data %>% group_by(!!as.name(idCol)) %>% reframe('{idCol}' := first(!!as.name(idCol)), mergePeriods(.data[[dateCol]], .data[[dddCol]], ...))
          })

renameColumnsMap <- function (dta, dtaCols, english=F, verbose=T) {
  cnames <- colnames(dta)
  # Indices in Delivery
  overl <- match(cnames, dtaCols$Delivery)
  # Positions in data columns
  inx <- 1:length(overl)
  colMissing <- is.na(overl)
  # Columns not present in renaming map
  novel <- inx[colMissing]
  # Remove positions that are not present in the mapping as they should not be renamed
  if (any(is.na(overl))) inx <- inx[!colMissing]
  overl <- stats::na.omit(overl)
  # If no overlap exists, just return data
  if (!any(overl)) return(dta)
  # Choose language
  if (english) colsNew <- dtaCols$English
  else colsNew <- dtaCols$Norwegian
  # Replace any empty entries with their default names (mainly for English)
  colsEmpty <- colsNew == ''
  if (any(colsEmpty)) colsNew[colsEmpty] <- dtaCols$Norwegian[colsEmpty]
  listOldNames <- cnames
  cnames[inx] <- colsNew[overl]
  listNewNames <- cnames
  listNewNames[novel] <- ''
  listNewNames <- c('CHANGE', listNewNames)
  listOldNames <- c('COLUMN', listOldNames)
  mx <- max(nchar(listOldNames))
  mx2 <- max(nchar(listNewNames))
  if (verbose) message(stringi::stri_sprintf(paste0('%',mx,'s \t %-',mx2,'s\n'), listOldNames, listNewNames))
  colnames(dta) <- cnames
  dta
}

#' Rename columns in a `dataRegister` object
#' @export
#' @rdname renameColumns
#' @param .data A `dataRegister` object.
#' @param dtaCols A table with column names.
#' @param english Whether to use English names.
#' @param verbose Whether to output information.
setGeneric('renameColumns', function (.data, dtaCols, english=F, verbose=F) standardGeneric('renameColumns'))
#' @rdname renameColumns
setMethod('renameColumns', signature('dataRegister', 'data.frame'),
          function (.data, dtaCols, english=F, verbose=F) {
            renameColumnsMap(.data, dtaCols, english=english, verbose=verbose)
          })
#' @rdname renameColumns
setMethod('renameColumns', signature('data.frame', 'data.frame'),
          function (.data, dtaCols, english=F, verbose=F) {
            renameColumnsMap(.data, dtaCols, english=english, verbose=verbose)
          })

setGeneric('summariseATC', function (x, ...) standardGeneric('summariseATC'))
setMethod('summariseATC', signature('dataRegister'),
          function (x) {
            x %>% group_by(atc) %>% summarise(N=n(), Individuals=length(unique(lopenr)), First=min(utlevdato), Last=max(utlevdato))
          })

#' Save data to file
#' @export
#' @param x A dataRegister class.
#' @param file The filename
#' @param format The file format.
#' @param ... Further parameters passed to `rio::export`
save.dataRegister <- function (x, file, format, ...) {
  rio::export(x, file, format, ...)
}
