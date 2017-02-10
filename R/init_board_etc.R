
### This script contains functions to creat "JScascade" objects which can be
### chained up to creat valid TnT definitions. The point is to creat functions
### with fixed arguments which can be further utilized by user-inferencing functions.
### 
### Reference: http://tntvis.github.io/tnt.board/api/index.html






# @examples
# if (interactive()) {
#     board <- jc_tnt_board(from = 1000, to = 1200, min = -1000, max = 5000, width = 1000)
#     blocktrack <- jc_tnt_track(
#         color = "yellow",
#         label = 'test block track',
#         height = 50,
#         data = jc_board_trackdata_sync(
#             retriever = selfRetriever({
#                 asTrackData(data.frame(start = c(1000, 1050), end = c(1100, 1200)))
#             })
#         ),
#         display = jc_board_trackfea_block(color = "green")
#     )
#     loctrack <- jc_tnt_track(height = 0,
#                                display = jc_board_trackfea_location())
#     axistrack <- jc_tnt_track(height = 0,
#                                 display = jc_board_trackfea_axis())
#     tntdef <- c(board, jc(add_track = loctrack,
#                           add_track = blocktrack,
#                           add_track = axistrack))
#     TnT(tntdef)
# }


## utils  ----------------------------------------------------------------------

# Note that this function must be used at the beginning inside another function
# to get non-missing arguments.
# @example
# (function(x, y = 34) {
#     arglist <- getArgList()
#     arglist
# })(x = 1)
getArgList <- function (n = 1) {
    arglist <- as.list(parent.frame(n))
    ## Remove the missing arguments
    arglist <- arglist[!sapply(arglist, is.name)]
    arglist
}

jc_init_with <- function (.list, ...) {
    init <- list(...)
    JCfromlst(append(init, .list))
}

# Also must be used at the beginning
jc_init_args_with <- function(...) {
    arglist <- getArgList(2)
    jc_init_with(arglist, ...)
}

`%addclass%` <- function (x, classes) {
    class(x) <- append(classes, class(x))
    x
}




## -----------------------------------------------------------------------------

# Methods that are not implemented:
#     board.scroll, board.zoom
#     board.tracks, board.add_track, board.find_track
jc_tnt_board <- function (from, to, min, max, width, zoom_out, zoom_in, allow_drag = TRUE) {
    jc_init_args_with(tnt.board = NULL) %addclass% c("board","tnt")
}


# Methods that are not included:
#     track.scale
#     track.id
jc_tnt_track <- function (color, height, label, data, display) {
    # TODO: stopifnot(is(data, ""))
    jc_init_args_with(tnt.board.track = NULL) %addclass% c("track","tnt")
}

jc_add_track <- function (color, height, label, data, display) {
    argv <- getArgList()
    trackdef <- do.call(jc_tnt_track, argv)
    JScascade(add_track = trackdef)
}


## Track data construction      ------------------------------------------------

# Methods not included:
#     data.elements()

jc_board_trackdata_sync <- function (retriever) {
    # Argument "retriever" should be a javascript callback function as a 
    # character string of the class "JScascade".
    jc_init_args_with(tnt.board.track.data.sync = NULL) %addclass% c("track_data", "tnt")
}

jc_board_trackdata_async <- function (retriever) {
    jc_init_args_with(tnt.board.track.data.async = NULL) %addclass% c("track_data", "tnt")
}

jc_board_trackdata_empty <- function () {
    jc_init_args_with(tnt.board.track.data.empty = NULL) %addclass% c("track_data", "tnt")
}


## Display methods for track data    -------------------------------------------

# These are interface methods, there is no need to implement them now.
#     feature.create
#     feature.distribute
#     feature.move
#     feature.fixed
#
# Other interaction methods that may be implemented as shiny functions include:
#     feature.update
#     feature.reset
#
# TODO: I do not really understand why "index" matters,
#       be default, "index" is set to "pos" values in the data.



# This feature expects the data as an array of objects containing
# numerical start and end properties, in R, a dataframe containing the
# "start" and "end" columns can be converted to such a js array by
# jsonlite::toJSON()
jc_board_trackfea_block <- function (color, index) {
    jc_init_args_with(tnt.board.track.feature.block = NULL) %addclass% c("track_display", "tnt")
}

# This features expects the data have "pos" and "val" values.
# The pos refers to its x-position;
# The val refers to the height of the pin and expects a number in
# the "domain", by default, between 0 and 1.
# The "domain" argument takes a javascript array, e.g. JS("[0.3, 1.2]")
jc_board_trackfea_pin <- function (color, index, domain) {
    jc_init_args_with(tnt.board.track.feature.pin = NULL) %addclass% c("track_display", "tnt")
}

# This feature also expects the data contain "pos" and "val" values.
# These points are applied a tension to smooth the connections between the points.
jc_board_trackfea_line <- function (color, index, domain) {
    jc_init_args_with(tnt.board.track.feature.line = NULL) %addclass% c("track_display", "tnt")
}

# Coloring the area behind the curve.
jc_board_trackfea_area <- function (color, index, domain) {
    jc_init_args_with(tnt.board.track.feature.area = NULL) %addclass% c("track_display", "tnt")
}

# It expects the data contain "pos" values
jc_board_trackfea_vline <- function (color, index) {
    jc_init_args_with(tnt.board.track.feature.vline = NULL) %addclass% c("track_display", "tnt")
}


## The following are data-less display methods  ---------------------------

#
jc_board_trackfea_location <- function () {
    jc_init_args_with(tnt.board.track.feature.location = NULL) %addclass% c("track_display", "tnt")
}

# Orientation should be either "top" or "bottom".
jc_board_trackfea_axis <- function (orientation) {
    jc_init_args_with(tnt.board.track.feature.axis = NULL) %addclass% c("track_display", "tnt")
}

