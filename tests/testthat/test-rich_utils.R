library(richcleaner)

test_that("rich_wider", {
  expect_snapshot_value(
    rich_wider(gsea, fdr_threshold = 0.01, gs = "GOBPs", value = "n_logp_sign"), style = "serialize")
})

