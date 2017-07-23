

#' @include tntTracks-compilation.R

#' @export
.mergeScalar <- function (l, name = c("background", "height", "label")) {
    name <- match.arg(name)
    
    l <- unlist(l, recursive = FALSE, use.names = FALSE)
    stopifnot(is.atomic(l))
    
    if (length(l) == 1)
        return(l)
    if (length(l) == 0)
        return(NULL)
    
    l <- l[1]
    msg <- sprintf("Incompatible %s, use %s", name, l)
    warning(msg)
    l
}

# .unmerge <- function (t) {
#     if (!inherits(t, "CompositeTrack"))
#         return(t)
#     li.td <- unname(trackData(t))
#     
# }
# 
# setClass("CompositeTrack", contains = "TnTTrack", slots = c(Data = "list"))
# 
# setMethod("merge", signature = c(x = "TnTTrack", y = "TnTTrack"),
#     function (x, y, ...) {
#         li.t <- unname(list(x, y, ...))
#         stopifnot(all(sapply(li.t, inherits, "TnTTrack")))
#         
#     }
# )

