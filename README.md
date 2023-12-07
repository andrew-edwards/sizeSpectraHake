
<!--
README.md is generated from README.Rmd. Please edit that file. Build with

load_all()
rmarkdown::render("README.Rmd")

which builds the .html that can be viewed locally (but isn't pushed to GitHub;
GitHub uses README.md to make the page you see on GitHub).
-->

# sizeSpectraHake

<!-- badges: start -->

[![R-CMD-check](https://github.com/andrew-edwards/sizeSpectraHake/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/andrew-edwards/sizeSpectraHake/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/andrew-edwards/sizeSpectraHake/branch/main/graph/badge.svg)](https://app.codecov.io/gh/andrew-edwards/sizeSpectraHake?branch=main)
<!-- badges: end -->

The goal of sizeSpectraHake is to analyse the hake length data. Won’t
work for others as data not included in package.

## Installation

    install.packages("remotes")    # If you do not already have the "remotes" package

    remotes::install_github("andrew-edwards/sizeSpectraHake")

If you get an error like

    Error in utils::download.file(....)

then the connection may be timing out (happens to us on the DFO
network). Try

    options(timeout = 1200)

and then try and install again.
