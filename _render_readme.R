#!/bin/sh

Rscript -e 'rmarkdown::render("README.Rmd")'

cp -r README_files docs
