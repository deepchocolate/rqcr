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
