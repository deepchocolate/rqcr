<!-- badges: start -->
  [![R-CMD-check](https://github.com/deepchocolate/rqcr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/deepchocolate/rqcr/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

# Register Quality-Control in R

## Installation

Get latest version from github:
```R
devtools::install_github('https://github.com/deepchocolate/rqcr')
```

Download a release and issue the following in R:
```R
# Dependencies has to be installed manually
deps <- c('arrow', 'DBI', 'dplyr', 'duckdb', 'stringi')
install.packages(deps)
install.packages(pgs='path/rqcr_vX.Y.Z.tar.gz', repos=NULL)
```

### Dependencies
Some functionality depend on R-packages `rio` and `getStatisticsFHI`. `getStatisticsFHI`
and `rio` dependencies are not installed by default. Consult [https://github.com/deepchocolate/get-statistics-fhi](getStatisticsFHI)
github page and `rio::install_formats` for more information.
