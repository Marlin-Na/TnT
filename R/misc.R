

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @import methods
NULL



#' @import S4Vectors
#' @import IRanges
#' @import GenomicRanges
#' @import GenomeInfoDb
NULL





#' @export
checkArgs <- function (localFunc, ...) {
    # This function is used inside a S4 method to ensure that no extra argument
    #   is passed through "..." to the local function call.
    #   However, we may want to remove these checks later for this package.
    localargs <- {
        la <- names(formals(localFunc))
        la[la != "..."]
    }
    passedargs <- {
        pa <- names(list(...))
        pa <- pa[pa != ""]
        if (length(pa) == 0) return(TRUE)
        pa
    }
    notfitargs <- passedargs[! passedargs %in% localargs]
    if (length(notfitargs) == 0)
        return(TRUE)
    stop("Extra Argument: ", paste(notfitargs, collapse = ","))
}

## An example
if (interactive()) local({
    f <- function (object, ...) {
        .local <- function (object, ext1, ext2, ...) {
            list(object, ext1, ext2)
        }
        checkArgs(.local, ...)
        .local(object, ...)
    }
    f(object = 23, ext1 = 1, ext2 = 2) # No error
    f(object = 23, ext1 = 1, ext2 = 2, ext3 = 3) # Expect error message
})

