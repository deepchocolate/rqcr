## code to prepare `NO_NAMES` dataset goes here
NO_NAMES <- list(
  DRUG_REGISTER=read.csv('data-raw/no-drug-register.tsv')
)
usethis::use_data(NO_NAMES, overwrite = TRUE, internal=T)
