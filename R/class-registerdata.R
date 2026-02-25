#' @import dplyr methods
NULL
utils::globalVariables(c('English', 'column'))
.metaVault <- new.env()
#' Constructor for register class
#' @param x Tabular data.
#' @param register The type of register.
dataRegister <- function (x, register) {
  cl <- 'dataRegister'
  cls <- class(x)
  if (!cl %in% cls) cls <- c(cl, cls)
  o <- structure(x, register=register, class=cls)
  meta <- getMeta(o)
  if (!is.list(meta)) setMeta(o, list(identifiers=NULL, logs=NULL, exclusion=NULL))
  o
}
setOldClass(c('dataRegister', 'data.table','data.frame', 'tibble'))

### Functions to interact with the metadata
# Get object stored under name
setGeneric('getMeta', function (.data, name=NULL) standardGeneric('getMeta'))
setMethod('getMeta', signature('dataRegister'),
          function (.data, name=NULL) {
            rg <- attr(.data, 'register')
            if (!is.character(rg)) stop('Register attribute not found')
            o <- get(rg, envir=.metaVault)
            if (!is.null(name)) o <- o[[name]]
            o
          })
setGeneric('updateMeta', function (.data, name, value) standardGeneric('updateMeta'))
setMethod('updateMeta', signature('dataRegister', 'character'),
          function (.data, name, value) {
            o <- getMeta(.data)
            o[[name]] <- value
            setMeta(.data, o)
          })
setGeneric('setMeta', function (.data, value) standardGeneric('setMeta'))
setMethod('setMeta', signature('dataRegister', 'list'),
          function (.data, value) {
            assign(attr(.data, 'register'), value, envir=.metaVault)
          })

logMessage <- function (action, what, statistic, description, df=NULL) {
  rbind(df,
        data.frame(action=action, what=what, statistic=statistic, description=description))
}

#' Add exclusion
#' @export
#' @import tibble
#' @name addExclusion
#'
#' @param .data A data frame.
#' @param ... Column-value pairs.
#' @param description A description of the exclusion
setGeneric('addExclusion', function (.data, ..., description=NULL) standardGeneric('addExclusion'))
#' @rdname addExclusion
setMethod('addExclusion', signature('dataRegister'),
          function (.data, ..., description=NULL) {
            tmp <- getExclusions(.data)
            tmp <- c(tmp, list(list(description=description, data=tibble(...))))
            updateMeta(.data, 'exclusion', tmp)
            invisible(.data)
          })

#' Get exclusions
#'
#' @export
#' @name getExclusions
#' @param .data A data frame.
setGeneric('getExclusions', function (.data) standardGeneric('getExclusions'))
#' @rdname getExclusions
setMethod('getExclusions', signature('dataRegister'),
          function (.data) {
            getMeta(.data, 'exclusion')
          })

#' Remove observations from data marked to be excluded
#'
#' @export
#' @name applyExclusions
#' @seealso [addExclusion()]
#' @seealso [getExclusions()]
#' @param .data Anything accepted by dplyr.
#' @param verbose Informative output.
setGeneric('applyExclusions', function (.data, verbose=T) standardGeneric('applyExclusions'))
#' @rdname applyExclusions
setMethod('applyExclusions', signature('dataRegister'),
          function (.data, verbose=T) {
            tmp <- getExclusions(.data)
            if (length(tmp) == 0) {
              message('Exclusion data is empty.')
              return(.data)
            }
            for (x in tmp) {
              if (verbose) message('Exclusion: ', x$description)
              nBef <- nrow(.data)
              .data <- .data %>% anti_join(x$data)
              if (verbose) message('Dropped rows: ', nBef - nrow(.data))
            }
            .data
          })

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
            logs <- getMeta(x, 'logs')
            logs <- logMessage('Exclusion', what, statistic, description, logs)
            updateMeta(x, 'logs', logs)
            invisible(x)
          })

#' @export
#' @rdname logging
setGeneric('logCheckpoint', function (x, what, statistic, description) standardGeneric('logCheckpoint'))
#' @rdname logging
setMethod('logCheckpoint', signature('dataRegister', 'character', 'numeric', 'ANY'),
          function (x, what, statistic, description) {
            logs <- getMeta(x, 'logs')
            logs <- logMessage('Checkpoint', what, statistic, description, logs)
            updateMeta(x, 'logs', logs)
            invisible(x)
          })

#' Reset/empty log
#'
#' @export
#' @rdname logging
#' @param x A `dataRegister` object.
setGeneric('logReset', function (x) standardGeneric('logReset'))
#' @rdname logging
setMethod('logReset', signature('dataRegister'),
          function (x) {
            updateMeta(x, 'logs', NULL)
            invisible(x)
          })

#' @export
#' @rdname logging
setGeneric('getLog', function (x) standardGeneric('getLog'))
#' @rdname logging
setMethod('getLog', signature('dataRegister'),
          function (x) {
            getMeta(x, 'logs')
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
          function (x) dataRegister(x, 'drugRegister'))

#' Get/set columns for dataRegister methods
#'
#' @description
#' Set and get column identifiers used by `dataRegister` class methods. These identifiers
#' are internal names that refer to columns in the actual data.
#'
#'
#' @export
#' @name rqcr-columns
#' @aliases getColumn
#' @param .data A dataRegister object.
#' @param name Name of the column identifier.
setGeneric('getColumn', function (.data, name) standardGeneric('getColumn'))
#' @rdname rqcr-columns
setMethod('getColumn', signature('dataRegister', 'character'),
          function (.data, name) {
            cols <- getMeta(.data, 'identifiers')
            if (!name %in% names(cols)) stop('Column not found: ', name)
            cols[[name]]
          })

#' @export
#' @rdname rqcr-columns
#' @param column The column in data.
setGeneric('setColumn', function (.data, name, column) standardGeneric('setColumn'))
#' @rdname rqcr-columns
setMethod('setColumn', signature('dataRegister', 'character', 'character'),
          function (.data, name, column) {
            cols <- getMeta(.data, 'identifiers')
            cols[[name]] <- column
            updateMeta(.data, 'identifiers', cols)
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
            updateMeta(.data, 'identifiers', cfg$identifiers)
            if ('rename' %in% names(cfg)) {
              .data <- renameColumns(.data, unlist(cfg$rename, use.names=F), names(cfg$rename), verbose=F)
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
            col <- getColumn(x, 'code_atc')
            x %>% frequencyCountDiscrete( !!as.name(col), ... )
          })

#' Index observations
#' @export
#' @seealso [indexAlong()]
#' @seealso [indexAlongUnique()]
#' @name indexObservations
#' @param .data Any data accepted by dplyr.
#' @param .nameIndex Column name.
#' @param .nameMax Column name for maximum.
#' @param ... Variables to group data by.
setGeneric('indexObservations', function (.data, ...) standardGeneric('indexObservations'))
#' @rdname indexObservations
setMethod('indexObservations', signature('data.frame'),
          function (.data, ..., .nameIndex='i', .nameMax=NULL) {
            rg <- attr(.data, 'register')
            .data[,.nameIndex] <- indexAlong(.data, ...)
            if (!is.null(.nameMax)) .data <- .data %>% mutate(!!.nameMax := max(.data[[.nameIndex]]), .by=c(...))
            if (is.character(rg)) attr(.data, 'register') <- rg
            .data
          })

#' Merge dispensed drugs into non-overlapping treatment periods
#' @export
#' @aliases getMergedPeriods
#' @rdname mergePeriods
#' @param .data Anything accepted by dplyr.
#' @param .maxDistance Maximum distance between end and start of two periods for merging.
#' @param .reset Whether to add the overlapping time between two periods at then end of the merged period.
#' @param .resetFun A callable to use for dates with multiple days provided.
setGeneric('getMergedPeriods', function (.data, .maxDistance=0, .reset=F, .resetFun=mean) standardGeneric('getMergedPeriods'))
#' @rdname mergePeriods
setMethod('getMergedPeriods', signature('dataRegister'),
          function (.data, .maxDistance=0, .reset=F, .resetFun=mean) {
            idCol <- getColumn(.data, 'individual')
            dateCol <- getColumn(.data, 'dispensation_date')
            dddCol <- getColumn(.data, 'dispensation_days')
            .data %>% group_by(!!as.name(idCol)) %>%
              reframe('{idCol}' := first(!!as.name(idCol)), mergePeriods(.data[[dateCol]], .data[[dddCol]], maxDistance=.maxDistance, reset=.reset, resetFun=.resetFun))
          })

#' Rename columns in data
#' @param dta A data.frame.
#' @param colsName New column names.
#' @param colsRename Columns to rename.
#' @param verbose Control information output.
renameColumnsMap <- function (dta, colsName, colsRename=NULL, verbose=T) {
  cnames <- colnames(dta)
  # Indices in Delivery
  overl <- match(cnames, colsRename)
  # Positions in data columns
  inx <- 1:length(overl)
  colMissing <- is.na(overl)
  # Columns not present in renaming map
  novel <- inx[colMissing]
  # Remove positions that are not present in the mapping as they should not be renamed
  if (any(is.na(overl))) inx <- inx[!colMissing]
  overl <- stats::na.omit(overl)
  # If no overlap exists, just return data
  if (!any(overl)) {
    if (verbose) message('No columns to rename provided')
    return(dta)
  }
  colsNew <- colsName
  # Replace any empty entries with their default names (mainly for English)
  colsEmpty <- colsNew == ''
  if (any(colsEmpty)) colsNew[colsEmpty] <- cnames[colsEmpty]
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
#' @param colsNew Vector with new column names.
#' @param colsRename Vector with column names to change.
#' @param verbose Whether to output information.
setGeneric('renameColumns', function (.data, colsNew, colsRename=NULL, verbose=F) standardGeneric('renameColumns'))
#' @rdname renameColumns
setMethod('renameColumns', signature('data.frame', 'character', 'character'),
          function (.data, colsNew, colsRename=NULL, verbose=F) {
            renameColumnsMap(.data, colsNew, colsRename, verbose=verbose)
          })

setGeneric('summariseATC', function (x, ...) standardGeneric('summariseATC'))
setMethod('summariseATC', signature('dataRegister'),
          function (x) {
            idCol <- getColumn(.data, 'individual')
            dateCol <- getColumn(.data, 'dispensation_date')
            atcCol <- getColumn(.data, 'code_atc')
            x %>% group_by(!!as.name(atcCol)) %>% summarise(N=n(), Individuals=length(unique(.data[[idCol]])), First=min(.data[[dateCol]]), Last=max(.data[[dateCol]]))
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
