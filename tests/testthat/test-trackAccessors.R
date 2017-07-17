context("trackAccessors")

test_that("trackData: accessor and setter", {
    gr <- GRanges("chr12", IRanges(1:5, width = 5))
    track_without_tooltip <- BlockTrack(gr)
    track_with_tooltip <- BlockTrack(gr, tooltip = as.data.frame(gr))
    
    trackData(track_without_tooltip)$tooltip <- as.data.frame(gr)
    expect_identical(track_without_tooltip, track_with_tooltip)
    
    # TODO: test other types of track, i.e. non-range track data
})

test_that("trackSpec: accessor and setter", {
    gr <- GRanges("chr11", IRanges(1:6, width = 9))
    track <- BlockTrack(gr)
    
    expect_identical(class(trackSpec(track)$background), "NULL")
    
    # Set Background
    trackSpec(track)$background <- "my background"
    expect_identical(trackSpec(track)$background, "my background")
    
    trackSpec(track) <- list(background = "second background")
    expect_identical(trackSpec(track)$background, "second background")
    
    # Set multiple options
    trackSpec(track) <- list(label = "test label", background = "black", height = 34L)
    expect_identical(trackSpec(track)$label, "test label")
    expect_identical(trackSpec(track)$background, "black")
    
    expect_identical(trackSpec(track)$height, 34L)
    
    trackSpec(track)$height <- 34
    expect_identical(trackSpec(track)$height, 34)
    
    # Warning
    expect_warning_notopt <- function (expr) {
        expect_warning(expr, regexp = "not an available track option")
    }
    expect_warning_notopt(
        trackSpec(track)$something <- 34
    )
    expect_warning_notopt(
        trackSpec(track) <- list(something = "joewf", background = "yellow")
    )
    expect_identical(trackSpec(track)$background, "yellow")
    
    
    expect_error(trackSpec(track)$background <- 3) # class not compatible
    expect_error(trackSpec(track)$height <- "324")
})

