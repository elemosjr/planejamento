#!/bin/sh

Rscript -e 'roxygen2::roxygenize();
            install.packages(".", repos = NULL);
            rmarkdown::render("README.Rmd")'

cp -r README_files docs

Rscript -e 'pkgdown::build_site()'
