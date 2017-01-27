
### This script contains functions to creat "JScascade" objects which can be
### chained up to creat valid TnT definitions. The point is to creat functions
### with fixed arguments which can be further utilized by user-inferencing functions.
### 
### Reference: http://tntvis.github.io/tnt.board/api/index.html




# Methods that are not implemented:
#     board.scroll, board.zoom
#     board.tracks, board.add_track, board.find_track
init_tnt_board <- function (from, to, min, max, width, zoom_out, zoom_in, allow_drag = TRUE) {
    init <- list(tnt.board = NULL)
    argv <- as.list(match.call())[-1]
    call <- append(init, argv)
    ans <- do.call(JScascade, args = call)
    class(ans) <- append(c("board","tnt"), class(ans))
    ans
}


# Methods that are not included:
#     track.scale
#     track.id
init_tnt_track <- function (color, height, label, data, display) {
    # TODO: stopifnot(is(data, ""))
    init <- list(tnt.board.track = NULL)
    argv <- as.list(match.call())[-1]
    call <- append(init, argv)
    ans  <- do.call(JScascade, args = call)
    class(ans) <- append(c("track","tnt"), class(ans))
    ans
}



## Track data construction      ------------------------------------------------

# Methods not included:
#     data.elements()

init_board_trackdata_sync <- function (retriever) {
    # Argument "retriever" should be a javascript callback function as a 
    # character string of the class "JScascade".
    init <- list(tnt.board.track.data.sync = NULL)
    argv <- as.list(match.call())[-1]
    call <- append(init, argv)
    ans  <- do.call(JScascade, args = call)
    class(ans) <- append(c("track_data", "tnt"), class(ans))
    ans
}

init_board_trackdata_async <- function (retriever) {
    init <- list(tnt.board.track.data.async = NULL)
    argv <- as.list(match.call())[-1]
    call <- append(init, argv)
    ans  <- do.call(JScascade, args = call)
    class(ans) <- append(c("track_data", "tnt"), class(ans))
    ans
}

init_board_trackdata_empty <- function () {
    init <- list(tnt.board.track.data.empty = NULL)
    call <- init
    ans  <- do.call(JScascade, args = call)
    class(ans) <- append(c("track_data", "tnt"), class(ans))
    ans
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
init_board_trackfea_block <- function (color, index) {
    init <- list(tnt.board.track.feature.block = NULL)
    argv <- as.list(match.call())[-1]
    call <- append(init, argv)
    ans  <- do.call(JScascade, args = call)
    class(ans) <- append(c("track_display", "tnt"), class(ans))
    ans
}

# This features expects the data have "pos" and "val" values.
# The pos refers to its x-position;
# The val refers to the height of the pin and expects a number in
# the "domain", by default, between 0 and 1.
# The "domain" argument takes a javascript array, e.g. JS("[0.3, 1.2]")
init_board_trackfea_pin <- function (color, index, domain) {
    init <- list(tnt.board.track.feature.pin = NULL)
    argv <- as.list(match.call())[-1]
    call <- append(init, argv)
    ans  <- do.call(JScascade, args = call)
    class(ans) <- append(c("track_display", "tnt"), class(ans))
    ans
}

# This feature also expects the data contain "pos" and "val" values.
# These points are applied a tension to smooth the connections between the points.
init_board_trackfea_line <- function (color, index, domain) {
    init <- list(tnt.board.track.feature.line = NULL)
    argv <- as.list(match.call())[-1]
    call <- append(init, argv)
    ans  <- do.call(JScascade, args = call)
    class(ans) <- append(c("track_display", "tnt"), class(ans))
    ans
}

# Coloring the area behind the curve.
init_board_trackfea_area <- function (color, index, domain) {
    init <- list(tnt.board.track.feature.area = NULL)
    argv <- as.list(match.call())[-1]
    call <- append(init, argv)
    ans  <- do.call(JScascade, args = call)
    class(ans) <- append(c("track_display", "tnt"), class(ans))
    ans
}

# It expects the data contain "pos" values
init_board_trackfea_vline <- function (color, index) {
    init <- list(tnt.board.track.feature.vline = NULL)
    argv <- as.list(match.call())[-1]
    call <- append(init, argv)
    ans  <- do.call(JScascade, args = call)
    class(ans) <- append(c("track_display", "tnt"), class(ans))
    ans
}


## The following are data-less display methods  ---------------------------

#
init_board_trackfea_location <- function () {
    call <- list(tnt.board.track.feature.location = NULL)
    ans  <- do.call(JScascade, args = call)
    class(ans) <- append(c("track_display", "tnt"), class(ans))
    ans
}

# Orientation should be either "top" or "bottom".
init_board_trackfea_axis <- function (orientation) {
    init <- list(tnt.board.track.feature.axis = NULL)
    argv <- as.list(match.call())[-1]
    call <- append(init, argv)
    ans  <- do.call(JScascade, args = call)
    class(ans) <- append(c("track_display", "tnt"), class(ans))
    ans
}

