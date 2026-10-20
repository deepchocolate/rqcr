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

# If a data.frame is passed
#' @importFrom data.table setattr setDT is.data.table
#' @export
`[.dataRegister` <- function(x, ...) {
  # Save the original custom attribute
  saved_attr <- attr(x, "register")
  xcl <- class(x)

  # Re-apply the class and attribute if the result is still a data.frame
  if (is.data.table(x)) {
    setattr(x, 'class', setdiff(xcl, 'dataRegister'))
    on.exit(setattr(x, "class", xcl), add = TRUE)
    cl <- match.call()
    #cl[[1]] <- quote(data.table:::`[.data.table`)
    cl[[1]] <- quote(`[`)
    res <- eval(cl, envir=parent.frame())
    setattr(x, 'class', xcl)
    setattr(x, 'register', saved_attr)
    if (is.data.frame(res)) {
      setDT(res)
      setattr(res, 'class', xcl)
      setattr(res, "register", saved_attr)
    }
  } else {
    res <- NextMethod('[')
    if (is.data.frame(res)) {
      attr(res, "register") <- saved_attr
      class(res) <- xcl
    }
  }
  return(res)
}

# This is mainly for attributes so has little effect at the moment
#' @importFrom data.table setattr
#' @export
#' @exportS3Method dplyr::dplyr_reconstruct
dplyr_reconstruct.dataRegister <- function(data, template) {
  class(data) <- union(class(template), class(data))
  attr(data, 'register') <- attr(template, 'register')
  if (inherits(template, "data.table")) {
    setattr(data, "register", attr(template, 'register'))
  }
  data
}

#' @importFrom data.table setattr setDT
#' @exportS3Method dplyr::dplyr_col_modify
dplyr_col_modify.dataRegister <- function (data, cols) {
  sattr <- attr(data, 'register')
  isdt <- inherits(data, 'data.table')
  res <- NextMethod('dplyr_col_modify')
  if (isdt) setDT(data)
  setattr(res, 'register', sattr)
  res
}

#' @importFrom data.table setattr setDT
#' @exportS3Method dplyr::dplyr_row_slice
dplyr_row_slice.dataRegister <- function(data, i, ...) {
  # Call standard data.frame row slicing
  sattr <- attr(data, 'register')
  cls <- class(data)
  isdt <- inherits(data, 'data.table')
  res <- NextMethod("dplyr_row_slice")
  if (isdt) {
    setDT(data)
    setattr(data, 'class', cls)
  }
  setattr(res, 'register', sattr)
  res
}


#' @exportS3Method dplyr::group_by
group_by.dataRegister <- function (.data, ..., .add, .drop) {
  .data <- NextMethod()
  class(.data) <- c('dataRegister', class(.data))
  .data
}

#' @exportS3Method dplyr::ungroup
ungroup.dataRegister <- function (x, ...) {
  o <- NextMethod()
  class(o) <- c('dataRegister', class(o))
  o
}

setOldClass(c('dataRegister', 'data.table','data.frame', 'tibble'))

### Functions to interact with the metadata
#' Get register type
#' @name registerType
#' @export
#' @param .data Data of class dataRegister
setGeneric('registerType', function (.data) standardGeneric('registerType'))
#' @rdname registerType
setMethod('registerType', signature('dataRegister'),
          function (.data) {
            attr(.data, 'register')
          })

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

### DRUG REGISTER

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

### PATIENT REGISTER
#' Create object for a patient register
#' @name patientRegister
#' @export
#' @param x Register input: a file or tabular data.
#' @param config A configuration file in YAML format.
setGeneric('patientRegister', function (x, config) standardGeneric('patientRegister'))
#' @rdname patientRegister
setMethod('patientRegister', signature('character', 'character'),
          function (x, config) {
            x <- patientRegister(x)
            configure(x, config)
          })
#' @rdname patientRegister
setMethod('patientRegister', signature('character', 'missing'),
          function (x) {
            x <- data.table::fread(x)
            patientRegister(x)
          })
#' @rdname patientRegister
setMethod('patientRegister', signature('data.frame', 'character'),
          function (x, config) {
            x <- patientRegister(x)
            configure(x, config)
          })
#' @rdname patientRegister
setMethod('patientRegister', signature('data.frame', 'missing'),
          function (x) dataRegister(x, 'patientRegister'))

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
  newNames <- NULL
  cnames <- colnames(dta)
  mp <- tibble(names=cnames) %>% left_join(tibble(names=colsRename, newNames=colsName), join_by(names)) %>%
    mutate(newNames=na_if(newNames, ''), changed=!is.na(newNames), newNames=coalesce(newNames, names))
  colnames(dta) <- mp$newNames
  if (verbose) {
    if (!any(mp$changed)) {
      message('No changes made')
      return(dta)
    }
    pos <- which(mp$changed)
    message(stringi::stri_sprintf(paste0('%', max(nchar(mp$names[pos])),'s \t %-',max(nchar(mp$newNames[pos])),'s\n'), mp$names[pos], mp$newNames[pos]))
  }
  return(dta)
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

#' Get indications for dispensed drug
#' @export
#' @name getDrugIndications
#' @param .data A dataRegister object.
#' @param ... Patient data with diagnoses. Priority will be given in order of appearance
#' @param impute Whether to impute indications from an individuals adjacent dispensations.
setGeneric('getDrugIndications', function (.data, ..., impute=0, days=NA, diagnoses=NULL) standardGeneric('getDrugIndications'))
#' @rdname getDrugIndications
setMethod('getDrugIndications', signature('dataRegister'),
          function (.data, ..., impute=0, days=NA, diagnoses=NULL) {
            dta <- .data
            if (is.data.table(dta)) dta <- data.table:::shallow(.data)
            rtype <- registerType(dta)
            if (rtype != 'drugRegister') stop('The first argument should be of type drugRegister. Received ', rtype)
            tid <- indication <- indicationSource <- NULL
            colsIndications <- getColumn(dta, 'code_indication')
            # Add a temporary row ID
            dta <- dta %>% mutate(tid=row_number())
            # 1. Indication from drug register
            tmp <- dta %>% select(all_of(colsIndications))
            for (col in colsIndications) {
              tmp <- tmp %>% mutate({{ col }} := case_when( dta[[col]]  == '' ~ NA, .default= dta[[col]]))
            }
            dta <- dta %>% mutate(indication=coalesce(tmp[,1] %>% pull(), tmp[,2] %>% pull()), indicationSource=case_when(!is.na(indication) ~ 'drugRegister'))
            # Remove missing indications from drug register and store separately
            tmp <- dta %>% filter(is.na(indication))

            # 2. Impute indications from adjacent dispensations
            colIID <- getColumn(dta, 'individual')
            if (impute > 0) {
              tmp <- tmp %>% select(tid, indication, {{ colIID }})
              tmp <- tmp %>% mutate(indicationSource = case_when(length(unique(indication)) == 1 ~ 'drugRegister dispensations', .default=indicationSource) , .by={{ colIID }})
              tmp <- tmp %>% mutate(indication=case_when(indicationSource == 'drugRegister dispensations' ~ unique(indication), .default=indicationSource ), .by={{ colIID }})
            }
            # Add imputed indications
            dta <- dta %>% filter(!tid %in% subset(tmp, !is.na(indicationSource))$tid)
            dta <- dta %>% bind_rows(tmp %>% filter(!is.na(indicationSource)))
            pdArgs <- list(...)
            if (length(pdArgs) == 0) return(dta)
            tmp <- tmp %>% filter(is.na(indicationSource))
            # 3. Search patient register(s)
            # Individual ID in drug register
            colIID <- getColumn(.data, 'individual')
            colDate <- getColumn(.data, 'dispensation_date')
            for (pd in list(...)) {
              rtype <- registerType(pd)
              if (rtype != 'patientRegister') stop('Data passed in `...` should be of type patientRegister. Received ', rtype)
              colIIDpd <- getColumn(pd, 'individual')
              colDiagsPd <- getColumn(pd, 'diagnosis')
              colDatePd <- getColumn(pd, 'date')
              if (!is.null(diagnoses)) pd <- pd %>% filter({{ colDiagsPd }} %in% diagnoses)
              #print(tmp)
              # Default mode: Identify the diagnosis closest to dispensation
              tmp2 <- tmp %>% inner_join(pd, join_by({{ colIID }} == {{ colIIDpd }}))
              tmp2 <- tmp2 %>% mutate(dateDiff=lubridate::as_date( tmp2[[colDate]]) - lubridate::as_date( tmp2[[colDatePd ]]))
              #print(tmp2)
              tmp2 <- tmp2 %>% filter(abs(dateDiff) == min(dateDiff), .by={{ colIID }})
              if (countUnique(tmp2, tid) != nrow(tmp2)) warning('Multiple potential indications found for at least 1 individual')
              tmp2 <- tmp2 %>% mutate(indication= .data[[colDiagsPd]], indicationSource=registerType(pd))
              dta <- dta %>% filter(!tid %in% tmp2$tid)
              dta <- dta %>% bind_rows(tmp2)
              tmp <- tmp %>% filter(!tid %in% tmp2$tid)
              if (nrow(tmp) == 0) break
              #tmp2 <- tmp %>% indicateEventsWithin(pd, )
              #print(tmp2)
            }
            dta
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
