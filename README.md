<!-- badges: start -->
  [![R-CMD-check](https://github.com/deepchocolate/rqcr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/deepchocolate/rqcr/actions/workflows/R-CMD-check.yaml)
[![DOI](https://zenodo.org/badge/1054117019.svg)](https://doi.org/10.5281/zenodo.22916814)
<!-- badges: end -->

# Register Quality-Control in R

## Installation

Get latest version from github:
```R
devtools::install_github('https://github.com/deepchocolate/rqcr')
```

Download a release and issue the following in R:
```R
install.packages(pkgs='path/rqcr_vX.Y.Z.tar.gz')
```

### Dependencies
Some functionality depend on R-packages `rio` and `getStatisticsFHI`. `getStatisticsFHI`
and `rio` dependencies are not installed by default. Consult [https://github.com/deepchocolate/get-statistics-fhi](getStatisticsFHI)
github page and `rio::install_formats` for more information.
