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
    
    # Set single option
    trackSpec(track, "background") <- "my background"
    expect_identical(trackSpec(track, "background"), "my background")
    
    # Set multiple options
    trackSpec(track, c("label", "height")) <- list("my label", 42)
    ts <- trackSpec(track)
    expect_identical(ts[["height"]], 42)
    expect_identical(ts[["label"]], "my label")
    
    # Or
    trackSpec(track)$background <- "yellow"
    expect_identical(trackSpec(track, "background"), "yellow")
    
    # Warning
    expect_warning(regexp = "not an available track option",
        trackSpec(track, c("backg", "label")) <- list("red", "test label")
    )
    
    expect_error(trackSpec(track)$background <- 3) # class not compatible
    expect_error(trackSpec(track)$height <- "324")
})

