
### "JavaScript" is a class to represent literal javascript code.
###
### "JSCascade" is a class used to model a javascript function cascade.
### It can be created with the function "jc" and possibly "ma", then can be
### converted to "JavaScript" with "asJS".
###
### The following is an example of using JSCascade to construct a TnT browser

### EXAMPLE
if (FALSE) local({
    axisTrack <- jc(
        tnt.board.track = ma(),
        height = 20,
        color = "white",
        display = jc(tnt.board.track.feature.axis = ma())
    )
    blockTrack <- jc(
        tnt.board.track = ma(),
        height = 30,
        color = "yellow",
        data = jc(
            tnt.board.track.data.sync = ma(),
            retriever = js("function() {return [{start : 200, end : 350}]}")
        ),
        display = jc(
            tnt.board.track.feature.block = ma(),
            color = "blue",
            index = js("function (d) {return d.start}")
        )
    )
    tntdef <- jc(
        tnt.board = ma(),
        from = 0,
        to = 500,
        min = 50,
        max = 1000,
        add_track = axisTrack,
        add_track = blockTrack
    )
    tntdef
    asJS(tntdef)
    trackWidget(tntdef)
    
    genome <- jc(
        tnt.board.genome = ma(),
        species = "human",
        chr = 10,
        from = 410000,
        to = 420000,
        min_coord = js("new Promise(function (res) {res(-10000)})"),
        max_coord = js("new Promise(function (res) {res(420000)})")
    )
    trackWidget(genome)
})
### EXAMPLE END




## Classes and Generics     ----------------------------------------------------

setClass("JavaScript", contains = "character")
setClass("JSCascade", contains = "SimpleList")
setClass("MultiArgs", contains = "SimpleList")


#JavaScript <-
js <- function (code) {
    new("JavaScript", code)
}

#JSCascade <-
jc <- function (...) {
    lst <- list(...)
    
    # If the element is NULL, then drop the element from the list
    todrop <- vapply(lst, is.null, logical(1))
    ans <- new("JSCascade", listData = lst[!todrop])
    ans
}

#MultiArgs <-
ma <- function (...) {
    new("MultiArgs", listData = list(...))
}

#NoArg <-
na <- ma()



setGeneric("asJS",
    function (object) standardGeneric("asJS")
)

setGeneric("asJC",
    function (object, ...) standardGeneric("asJC")
)



## Validity                 ----------------------------------------------------

setValidity("JavaScript",
    function (object) if (!identical(length(object), 1L))
        "Code must be a length-one character vector." else TRUE
)

setValidity("JSCascade",
    function (object) {
        lst <- as.list(object)
      # Ensure there is no "NULL" value
        isnull <- vapply(lst, is.null, logical(1))
        if (any(isnull))
            return("NULL is not allowed in JSCascade")
      # Ensure the names are specified
        if (length(lst) > 0)
            if (is.null(names(lst)) || any(names(lst) == ""))
                return("Names must be specified.")
      # Only allow certain types of element
        isAllowed <- vapply(lst, inherits, FUN.VALUE = logical(1),
            what = c("JSCascade", "MultiArgs", "JavaScript", "json",
                     "integer", "numeric", "logical", "character", "data.frame")
        )
        if (!all(isAllowed)) {
            classes <- vapply(lst[!isAllowed], FUN.VALUE = character(1),
                function (element) class(element)[1]
            )
            classes <- unique(classes)
            return(paste("Class", classes, "is not allowed."))
        }
        
        TRUE
    }
)

setValidity("MultiArgs",
    function (object) {
        lst <- as.list(object)
      # Only allow position-based JS call
        if (!is.null(names(lst)) && any(names(lst) != ""))
            message("The names will be ignored at present.")
      # Only allow certain types of element
        isAllowed <- vapply(lst, inherits, FUN.VALUE = logical(1),
            what = c("JSCascade", "JavaScript", "json",
                     "integer", "numeric", "logical", "character", "data.frame")
        )
        if (!all(isAllowed)) {
            classes <- vapply(lst[!isAllowed], FUN.VALUE = character(1),
                function (element) class(element)[1]
            )
            classes <- unique(classes)
            return(paste("Class", classes, "is not allowed."))
        }
        
        TRUE
    }
)


## Methods    ------------------------------------------------------------------

setMethod("asJS", signature = "JSCascade",
    function (object) {
        chr <- .convertToJSChar(object)
        js(chr)
    }
)

setMethod("asJC", signature = c(object = "list"),
    function (object)
        do.call(jc, object)
)

setMethod("asJC", signature = c(object = "JSCascade"),
    function (object) object
)



.convertToJSChar <- function (el) {
    # This function is called recursively by itself
    if (inherits(el, "JSCascade")) {
        jscalls <- names(el)
        jsargs <- vapply(el, sys.function(), character(1))
        each <- sprintf("%s(%s)", jscalls, jsargs)
        ans <- paste(each, collapse = "\n.")
        return(ans)
    }
    if (inherits(el, "MultiArgs")) {
        l <- vapply(el, FUN = sys.function(), FUN.VALUE = character(1))
        ans <- paste(l, collapse = ", ")
        return(ans)
    }
    if (inherits(el, c("JavaScript", "json")))
        return(as.character(el))
    
    else {
        ans <- jsonlite::toJSON(el, auto_unbox = TRUE,
                                pretty = if (inherits(el, "data.frame")) 2 else FALSE)
        return(ans)
    }
}





## Show-methods    -------------------------------------------------------------

setMethod("show", signature = "JSCascade",
    function (object) {
        cat("JSCascade Object:\n------\n")
        js <- asJS(object)
        show(js)
        invisible(object)
    }
)


setMethod("show", signature = "JavaScript",
    # Print the javascript code with indentation
    
    function (object) {
        byline <- strsplit(object, "\n", fixed = TRUE)[[1]]
        l_par_cum <- cumsum(lengths(regmatches(byline, gregexpr("(", byline, fixed = TRUE))))
        r_par_cum <- cumsum(lengths(regmatches(byline, gregexpr(")", byline, fixed = TRUE))))
        indent <- l_par_cum - r_par_cum
        spaces <- vapply(indent, FUN.VALUE = character(1L), FUN = function (n) {
            paste(rep("  ", n), collapse = "")
        })
        out <- paste0(byline, "\n", spaces)
        cat(out)
        invisible(object)
    }
)

