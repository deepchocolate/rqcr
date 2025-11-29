dataRegister <- function (x, register, columns) {
  structure(x, register=register, columns=columns,
            class=c('dataRegister', 'data.table', 'data.frame'))
}
setOldClass('dataRegister')

setGeneric('drugRegister', function (x, columns) standardGeneric('drugRegister'))
setMethod('drugRegister', signature('character', 'character'),
          function (x, columns) {
            x <- data.table::fread(x)
            dataRegister(x, 'drugRegister', columns)
          })
setMethod('drugRegister', signature('character'), function (x) drugRegister(x, 'Delivery'))
setMethod('drugRegister', signature('data.frame', 'character'),
          function (x, columns) {
            dataRegister(x, 'drugRegister', columns)
          })

#' @import dplyr
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

setGeneric('frequencyATC', function (x, ...) standardGeneric('frequencyATC'))
setMethod('frequencyATC', signature('dataRegister'),
          function (x, ...) {
            col <- getColumn(x, 'atcCode4')
            x %>% frequencyCountDiscrete( !!as.name(col), ... )
          })

#' Index observations
#' @export
#' @name indexObservations
#' @param .data Any data accepted by dplyr.
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

save.dataRegister <- function (x, file, format, ...) {
  rio::export(x, file, format, ...)
}
