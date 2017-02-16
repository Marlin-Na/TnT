
#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom methods is
NULL

#' @importFrom htmlwidgets JS
#' @export
htmlwidgets::JS

#' @export
print.JS_EVAL <- function (x, ...) {
    #cat(x)
    #invisible(x)
    
    # Print with indentation
    byline <- strsplit(x, "\n", fixed = TRUE)[[1]]
    l_par_cum <- cumsum(lengths(regmatches(byline, gregexpr("(", byline, fixed = TRUE))))
    r_par_cum <- cumsum(lengths(regmatches(byline, gregexpr(")", byline, fixed = TRUE))))
    indent <- l_par_cum - r_par_cum
    spaces <- sapply(indent, FUN = function (n) {
        paste(rep("  ", n), collapse = "")
    })
    out <- paste0(byline, "\n", spaces)
    cat(out)
    invisible(x)
}


