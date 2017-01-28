

elementToDrop <- function () {
    ans <- c("elementToDrop")
    class(ans) <- "elementToDrop"
    ans
}

dropE <- function (lst) {
    lst[!sapply(lst, is, class2 = "elementToDrop")]
}



#' @export
tnt_board <- function (from, to, width, min, max, zoom_out, zoom_in, allow_drag = TRUE,
                       with.locinfo = TRUE, axis.at = c("top","bottom","none")) {
    board_args <- {
        dropE(list(
            from = if (missing(from)) elementToDrop() else from,
            to = if (missing(to)) elementToDrop() else to,
            width = if (missing(width)) elementToDrop() else width,
            min = if (missing(min)) elementToDrop() else min,
            max = if (missing(max)) elementToDrop() else max,
            zoom_out = if (missing(zoom_out)) elementToDrop() else zoom_out,
            zoom_in = if (missing(zoom_in)) elementToDrop() else zoom_in,
            allow_drag = allow_drag
        ))
    }
    jc.board <- do.call(jc_tnt_board, args = board_args)
    jc.loc <- {
        if (with.locinfo) {
            feadef <- jc_board_trackfea_location()
            jc_add_track(display = feadef, height = 20, color = "white")
        }
        else NULL
    }
    jc.axis <- {
        axis.at <- match.arg(axis.at)
        if (axis.at == "none") NULL
        else {
            feadef <- jc_board_trackfea_axis(orientation = axis.at)
            jc_add_track(display = feadef, height = 0)
        }
    }
    ans <- c(jc.board, jc.loc, jc.axis)
    class(ans) <- c("tnt_board", "JScascade")
    ans
}


#' @title Add Track With Different Types of Features
#' 
#' @param tnt 
#' @param data 
#' @param label 
#' @param height 
#' @param color.background 
#' @param color.feature 
#'
#' @export
#' @examples
#' if (interactive()) {
#'     mydata1 <- data.frame (
#'         start = c(42, 69, 233),
#'         end = c(54, 99, 250)
#'     )
#'     mydata2 <- data.frame(
#'         start = c(23, 66, 300),
#'         end = c(38, 74, 318)
#'     )
#'     tnt_board(from = 14, to = 114, min = -100, max = 500) %>%
#'         add_track_block(mydata1, label = "My Track 1") %>%
#'         add_track_block(mydata2, label = "My Track 2", color.feature = "green") %>%
#'         TnT()
#' }
add_track_block <- function (tnt, data, label, height = 40,
                             color.background = "white", color.feature = "black") {
    jc.trackdata <- jc_board_trackdata_sync(
        retriever = selfRetriever(asTrackData(data, df_preserved.cols = c("start","end")))
    ) 
    
    jc.display <- jc_board_trackfea_block(color = color.feature)
    trackArgs <- dropE(list(
        color = color.background,
        height = height,
        label = if (missing(label)) elementToDrop() else label,
        data = jc.trackdata,
        display = jc.display
    ))
    jc.addtrack <- do.call(jc_add_track, trackArgs)
    
    ans <- c(tnt, jc.addtrack)
    class(ans) <- class(tnt)
    ans
}





