#!/usr/bin/env Rscript

library(methods)
library(docopt)
library(servr)

build.doc <- function (path) {
    message("------------  Start running roxygen  ----------------------")
    devtools::document(path)
}

build.install <- function (path) {
    build.doc(path)
    message("------------  Start installing roxygen  -------------------")
    devtools::install(path, quick = FALSE)
}

build.check <- function (path) {
    build.doc(path)
    message("------------  Start checking roxygen  ---------------------")
    devtools::check(path)
}

build.biocheck <- function (path) {
    build.doc(path)
    message("------------  Start Bioc Check  ---------------------------")
    BiocCheck::BiocCheck(path)
}

build.site <- function (path) {
    build.install(path)
    message("------------  Start building documentation site  ----------")
    pkgdown::build_site(pkg = path, path = file.path(path, "gh-pages"), examples = TRUE)
}

build.serve <- function (path) {
    servr::httd(dir = file.path(path, "gh-pages"))
}


doc <- c("

Building Utils for TnT

Usage:
  build.R  [-h | --help]
  build.R  doc       [<path>]
  build.R  install   [<path>]
  build.R  check     [<path>]
  build.R  biocheck  [<path>]
  build.R  site      [<path>]
  build.R  serve     [<path>]

Commands:
  doc        Run roxygen
  install    Run roxygen and install package
  check      Run R CMD check
  biocheck   Run Bioc check
  site       Build documentation site to gh-pages
  serve      Serve documentation site

")

opts <- docopt(doc)

path <- if (is.null(opts$path)) "." else opts$path

main <- function () {
    if (opts$doc)
        build.doc(path)
    else if (opts$install)
        build.install(path)
    else if (opts$check)
        build.check(path)
    else if (opts$biocheck)
        build.biocheck(path)
    else if (opts$site)
        build.site(path)
    else if (opts$serve)
        build.serve(path)
    else
        cat(doc)
}
main()

message("==============================")
