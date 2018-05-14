
library(TnT)

library(TxDb.Hsapiens.UCSC.hg19.knownGene)
library(AnnotationHub)
library(org.Hs.eg.db)

# Block Track, etc.

gr <- GRanges("1", IRanges(c(11000, 20000, 60000), width = 2000))
gpos <- GRanges("1", IRanges(c(15000, 50000, 41000), width = 1), value = c(1, 2, 3))

btrack <- TnT::BlockTrack(gr, label = "Block Track",
                          tooltip = as.data.frame(gr), color = "lightblue4", height = 40,
                          background = "beige")
ptrack <- TnT::PinTrack(gpos, label = "Pin Track",
                        tooltip = as.data.frame(gpos), background = "beige",
                        color = c('red', 'red', 'green'), height = 40)


set.seed(2)
pos <- IRanges(seq(1, 100000, by = 50), width = 1)
height <- .5 + cumsum(runif(length(pos), min = -.05, max = .05))
height[height >= 1] <- 1
height[height <= 0] <- 0
pos <- GRanges("1", pos)
pos$value <- height

areatrack <- TnT::AreaTrack(pos, color = 'green', label = "Area Track", background = "beige")
linetrack <- TnT::LineTrack(pos, color = 'red', label = "Line Track", background = "beige")
set.seed(1)
vlinetrack <- TnT::VlineTrack(GRanges("1", IRanges(width = 1, start = sample(1:100000, 20))),
                              color = c("#935116", "blue"), label = "Vline Track", background = "beige")

if (FALSE) {
    txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene
    gtrack <- TnT::GeneTrackFromTxDb(txdb, label = "Gene", color = "burlywood3", height = 130)
    gtrack <- gtrack
    txtrack <- TxTrackFromTxDb(TxDb.Hsapiens.UCSC.hg19.knownGene, seqlevel = 'chr1')
    txtrack <- TxTrackFromTxDb(TxDb.Hsapiens.UCSC.hg19.knownGene, seqlevel = 'chr1', height = 200, label = "Tx Track")
}
library(EnsDb.Hsapiens.v75)
gene <- genes(EnsDb.Hsapiens.v75)
#seqlevelsStyle(gene) <- "UCSC"
#genome(gene) <- "hg19"
gene

ensGeneTrack <- TnT::FeatureTrack(gene, tooltip = as.data.frame(gene),
                                  names = paste(gene$symbol, " (", gene$gene_biotype, ")", sep = ""),
                                  color =
                                      TnT::mapcol(gene$gene_biotype %>% {
                                          . == "lincRNA"
                                      }, palette.fun = grDevices::rainbow), height = 100,
                                  background = "beige", label = "Gene Track"
                                  )
gr.tx <- biovizBase::crunch(EnsDb.Hsapiens.v75, which = AnnotationFilterList(
                           AnnotationFilter::GeneStartFilter(200000, "<"),
                           AnnotationFilter::SeqNameFilter('1')
                           )
)
txtrack <- TxTrackFromGRanges(gr.tx, background = "beige", label = "Tx Track", height = 100)



TnTGenome(list(btrack, ptrack, areatrack, linetrack, vlinetrack, ensGeneTrack, txtrack),
          view.range = GRanges("1", IRanges(3000, 69000)))


TnTGenome(ensGeneTrack, view.range = gene[gene$symbol == "BRCA2"][1] * .7)




TnTGenome(areatrack)

height <- .5 + cumsum(runif(length(pos), min = -.1, max = .1))
height[height >= 1] <- 1
height[height <= 0] <- 0

ltrack <- TnT::LineTrack(pos, value = height, color = "blue")
TnTBoard(ltrack)
atrack <- TnT::AreaTrack(ranges(pos), value = height, color = "blue")
TnTBoard(atrack)

# --- debug areatrack
gr <- GRanges("chr1", IRanges(1:200, width = 1), value = rep(0.4, len = 200))
a <- AreaTrack(gr, domain = c(0, 1))
TnTGenome(a)
TnTGenome(AreaTrack(ranges(gr), value = gr$value, domain = c(0, 1)))
TnTGenome(AreaTrack(IRanges(1:200, width = 1), value = gr$value, domain = c(0, 1)))
ir <- IRanges(1:100, width = 1)
a <- AreaTrack(ir, value = 1, domain = c(0, 2)), view.range = GRanges('UnKnown', IRanges(54,68))
TnTGenome(a)
TnTBoard(a)


TnT:::compileTrack(a)
TnT:::compileBoard(TnTGenome(a))

TnT:::compileBoard(TnTGenome(AreaTrack(ranges())))

pos <- IRanges(seq(1, 1000, by = 10), width = 1)
height <- .5 + cumsum(runif(length(pos), min = -.1, max = .1))
height[height >= 1] <- 1
height[height <= 0] <- 0
ltrack <- TnT::AreaTrack(pos, value = height, color = "blue")
TnTBoard(ltrack)

pos <- IRanges(seq(1, 1000, by = 10), width = 1)
height <- .5 + cumsum(runif(length(pos), min = -.1, max = .1))
height[height >= 1] <- 1
height[height <= 0] <- 0
pos <- GRanges("chr1", pos)
TnTBoard(AreaTrack(pos, height, color = "pink"))
# --- debug areatrack end


# CpG islands and Genes

# m6A hg19
"http://rna.sysu.edu.cn/rmbase/download/human/RMBase_hg19_all_m6A_site.txt"
m6A <- local({
    m6A <- readr::read_tsv('~/msync/projects/TnT_Paper/data/RMBase_hg19_all_m6A_site.txt',
                           col_names = FALSE)
    m6A[c('X4', 'X5', 'X7', 'X8', 'X9', 'X10', 'X11', 'X12', 'X13', 'X14', 'X15', 'X16')] <- NULL
    m6A <- m6A %>% dplyr::rename(seqnames = X1, start = X2, end = X3, strand = X6)
    m6A <- as(m6A, "GRanges")
    end(m6A) <- start(m6A)
    m6A
})
m6A
m6A_track <- TnT::PinTrack(m6A, value = 1, label = "RNA m6A")
TnTGenome(m6A_track, view.range = m6A[1] * 0.001)


library(TxDb.Hsapiens.UCSC.hg19.knownGene)
library(AnnotationHub)
library(org.Hs.eg.db)

txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene
gtrack <- TnT::GeneTrackFromTxDb(txdb, label = "Gene", color = "burlywood3", height = 130)
geneid <- gtrack@Data$id
orgdb <- org.Hs.eg.db
columns(orgdb)

tooltip(gtrack) <- local({
    AnnotationDbi::select(orgdb, keys = tooltip(gtrack)$gene_id, columns = c('GENENAME'))
})

cpg <- local({
    ah <- AnnotationHub()
    ah <- query(ah, "cpgisland")
    cpg <- ah[[names(ah[ah$genome == "hg19"])]]
    cpg
})

cpgtrack <- TnT::BlockTrack(cpg, label = "CpG Islands", tooltip = as.data.frame(cpg),
                            color = "lightblue")
TnTGenome(merge(cpgtrack, gtrack),
          view.range = GRanges("chr1", IRanges(84884317, 85205497)))

