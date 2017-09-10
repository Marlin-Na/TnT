
context("Test seqinfo and seqinfo<-")

library(testthat)
test_that("seqinfo of CompositeTrack", {
    
    gr <- GRanges("chr2", IRanges(1,2),
                  seqinfo = Seqinfo(seqnames = c("chr1", "chr2"), seqlengths = c(100, 200)))
    gr2 <- GRanges("4", IRanges(1, 32),
                   seqinfo = Seqinfo(seqnames = c("3", "4"), seqlengths = c(300, 400)))
    bt  <- BlockTrack(gr)
    bt2 <- BlockTrack(gr2)
    
    seqinfo(bt)
    seqinfo(bt2)
    
    expect_warning(
        ct  <- merge(bt, bt2), "no sequence levels in common"
    )
    
    expect_identical(seqinfo(ct), seqinfo(trackData(ct)[[1]]))
    expect_identical(seqinfo(ct), seqinfo(trackData(ct)[[2]]))
    
    seqinfo(ct)
    seqlevels(ct, pruning.mode = "coarse") <- "3"
    seqinfo(ct)
    expect_identical(seqlevels(ct), "3")
    expect_identical(seqinfo(ct), seqinfo(trackData(ct)[[1]]))
    expect_identical(seqinfo(ct), seqinfo(trackData(ct)[[2]]))
})

