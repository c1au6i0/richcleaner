library(richcleaner)
pheat_test <- rich_pheatmap(gsea, fdr_threshold = 0.01, gs = "GOBPs", value = "n_logp_sign")


test_that("rich_graphs is a pheatmap", {
  expect_identical(
    class(pheat_test), "pheatmap")
})


test_that("rich_graphs", {
  expect_snapshot_value(
    pheat_test, style = "serialize")
})



