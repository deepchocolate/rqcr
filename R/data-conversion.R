#' @importFrom glue glue
NULL
#' Convert a database table into a parquet file.
#' @export
#' @param dbCon A database connection (needs to support conversion to parquet).
#' @param table Database table name.
#' @param pathOutput Parquet output path.
#' @param partition Whether to partition data overa column.
#' @param overwrite Whether to overwrite an existing parquet file.
convertDBTableToParquet <- function (dbCon, table, pathOutput, partition=F, overwrite=T) {
  q <- glue('COPY {table} TO "{pathOutput}" (FORMAT "parquet", OVERWRITE {overwrite}, COMPRESSION zstd')
  if (!isFALSE(partition)) q <- glue(q, ', PARTITION_BY {partition}')
  q <- glue(q, ')')
  DBI::dbExecute(dbCon, q)
}

#' Convert a file to CSV.
#' @details
#' Any file format accepted by rio::convert is accepted as input. If directories in the
#' output file path do not exist, they are created.
#' @import rio dplyr
#' @export
#' @param fileInput Path to an input file.
#' @param fileOutput Path to the output CSV.
#' @param formatInput Specify input format if not provided by file extension.
#' @param dropColumns A vector of columns to remove.
#' @param lowerColumns Wheter to lowercase column names.
convertToCSV <- function (fileInput, fileOutput, formatInput, dropColumns=c(), lowerColumns=F) {
  dr <- dirname(fileOutput)
  if (!dir.exists(dr)) dir.create(dr, recursive=T)
  FUN <- ifelse(lowerColumns, tolower, identity)
  import(fileInput, format=formatInput) %>%
    select(-any_of(dropColumns)) %>%
    rename_with(FUN) %>%
    export(fileOutput, format='csv')
}
