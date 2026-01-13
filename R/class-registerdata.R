utils::globalVariables(c('English', 'atc', 'lopenr','utlevdato'))
#' Constructor for register class
#' @param x Tabular data.
#' @param register The type of register.
#' @param columns Columns in data.
dataRegister <- function (x, register, columns) {
  structure(x, register=register, columns=columns, logs=NULL,
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
#' @param columns Register columns.
setGeneric('drugRegister', function (x, columns) standardGeneric('drugRegister'))
#' @rdname drugRegister
setMethod('drugRegister', signature('character', 'character'),
          function (x, columns) {
            x <- data.table::fread(x)
            dataRegister(x, 'drugRegister', columns)
          })
#' @rdname drugRegister
setMethod('drugRegister', signature('character'), function (x) drugRegister(x, 'Delivery'))
#' @rdname drugRegister
setMethod('drugRegister', signature('data.frame', 'character'),
          function (x, columns) {
            dataRegister(x, 'drugRegister', columns)
          })

#' Get column name using generic name
#' @import dplyr
#' @param name The generic name.
#' @param type Currently "English" or "Norwegian"
setGeneric('getColumn', function (name, type) standardGeneric('getColumn'))
setMethod('getColumn', signature('dataRegister', 'character'),
          function (name, type) getColumn(type, attributes(name)$columns))
setMethod('getColumn', signature('character', 'character'),
          function (name, type) {
            o <- NO_NAMES$DRUG_REGISTER %>% filter(English == name)
            col <- o[,type]
            if (length(col) == 0) stop('Column not found: ', name)
            col
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
