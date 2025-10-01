#' @importFrom glue glue
NULL
convertDBTableToParquet <- function (dbCon, table, pathOutput, partition=F, overwrite=T) {
  q <- glue('COPY {table} TO "{pathOutput}" (FORMAT "parquet", OVERWRITE {overwrite}, COMPRESSION zstd')
  if (!isFALSE(partition)) q <- glue(q, ', PARTITION_BY {partition}')
  q <- glue(q, ')')
  DBI::dbExecute(dbCon, q)
}
