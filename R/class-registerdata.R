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
#' @export
#' @description
#' Functions to log operations on data.
#'
#' @param x A `dataRegister` object.
#' @param what What is excluded?
#' @param statistic Number of exclusions.
#' @param description A description of the exclusion.
setGeneric('logExclusion', function (x, what, statistic, description) standardGeneric('logExclusion'))
setMethod('logExclusion', signature('dataRegister', 'character', 'numeric', 'ANY'),
          function (x, what, statistic, description) {
            attributes(x)$logs <- logMessage('Exclusion', what, statistic, description, attributes(x)$logs)
            x
          })

#' @export
#' @rdname logging
setGeneric('logCheckpoint', function (x, what, statistic, description) standardGeneric('logCheckpoint'))
setMethod('logCheckpoint', signature('dataRegister', 'character', 'numeric', 'ANY'),
          function (x, what, statistic, description) {
            attributes(x)$logs <- logMessage('Checkpoint', what, statistic, description, attributes(x)$logs)
            x
          })
setGeneric('getLog', function (x) standardGeneric('getLog'))
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
            #cfg <- yaml::read_yaml(config)
            configure(x, config)
          })
#' @rdname drugRegister
setMethod('drugRegister', signature('character', 'missing'),
          function (x) {
            x <- data.table::fread(x)
            dataRegister(x, 'drugRegister');
            })
#' @rdname drugRegister
setMethod('drugRegister', signature('data.frame', 'character'),
          function (x, config) {
            #x <- dataRegister(x, 'drugRegister')
            x <- drugRegister(x)
            configure(x, config)
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

setGeneric('configure', function (.data, file) standardGeneric('configure'))
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

#' @export
setGeneric('getMergedPeriods', function (.data, ...) standardGeneric('getMergedPeriods'))
setMethod('getMergedPeriods', signature('dataRegister'),
          function (.data, ...) {
            idCol <- getColumn(.data, 'IID')
            dateCol <- getColumn(.data, 'dateDelivery')
            dddCol <- getColumn(.data, 'DDD')
            .data %>% group_by(!!as.name(idCol)) %>% reframe('{idCol}' := first(!!as.name(idCol)), mergePeriods(.data[,dateCol], .data[,dddCol]))
          })

setGeneric('renameColumns', function (.data, dtaCols, english, verbose) standardGeneric('renameColumns'))
setMethod('renameColumns', signature('dataRegister', 'data.frame','logical','logical'),
          function (.data, dtaCols, english=F, verbose=F) {
            renameColumnsMap(.data, dtacols, english, verbose=F)
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
