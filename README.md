

# TnT

[![Bioc](http://www.bioconductor.org/shields/years-in-bioc/TnT.svg)](https://www.bioconductor.org/packages/devel/bioc/html/TnT.html#since)
[![Travis-CI Build Status](https://travis-ci.org/Marlin-Na/TnT.svg?branch=master)](https://travis-ci.org/Marlin-Na/TnT)
[![Bioconductor Release](https://www.bioconductor.org/shields/build/release/bioc/TnT.svg)](http://bioconductor.org/checkResults/release/bioc-LATEST/TnT/)
[![Bioconductor Devel](https://www.bioconductor.org/shields/build/devel/bioc/TnT.svg)](http://bioconductor.org/checkResults/devel/bioc-LATEST/TnT/)


TnT is a R package that wraps the [TnT javascript libraries](https://github.com/tntvis)
to provide track-based visulizations from R.
It is useful for displaying genomic features as a simple genome browser, particularly
when working with relevant Bioconductor packages.

To install the stable version from biocondcutor, use

```R
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
BiocManager::install("TnT")
```

Or alternatively, install the latest version from github with

```R
devtools::install_github("Marlin-Na/TnT")
```

To get started, checkout:

- The package vignette [Introduction to TnT](https://tnt.marlin.pub/articles/introduction.html)
- [Examples](https://tnt.marlin.pub/articles/examples) of different track types.

You can also find the [package](http://bioconductor.org/packages/release/bioc/html/TnT.html)
on Bioconductor.


### GSoC 2017

This R package was a Google Summer of Code project in 2017, kindly mentored by
Toby Dylan Hocking and Miguel Pignatelli, under the organization of R project
for statistical computing.

Please see:

- [The project link](https://summerofcode.withgoogle.com/dashboard/project/5521605556961280/overview/)
- [Wiki](https://github.com/rstats-gsoc/gsoc2017/wiki/Interactive-Genome-Browser-in-R) of the project idea
- [My blog post](http://weblog.marlin.pub/post/tnt/tnt-gsoc17/) as a summary of the project.

