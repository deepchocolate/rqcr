
.pkg_env <- new.env()
.pkg_env$updateCases = list(warnings=F)
.pkg_env$txtNPercent <- list(maxPercent=F)

#' Configure RQCR
#' @name configRQCR
#' @export
#' @param what What to configure.
#' @param setting Which setting.
#' @param value Setting value.
setGeneric('configRQCR', function (what, setting, value) standardGeneric('configRQCR'))
#' @rdname configRQCR
setMethod('configRQCR', signature('character', 'character', 'ANY'),
          function (what, setting, value) {
            if (!what %in% names(.pkg_env)) stop('Setting not found: ', what)
            if (!setting %in% names(.pkg_env[[what]])) stop('Setting not found: ', setting)
            .pkg_env[[what]][[setting]] <- value
          })
#' @rdname configRQCR
setMethod('configRQCR', signature('character', 'character', 'missing'),
          function (what, setting) {
            .pkg_env[[what]][[setting]]
          })

