library(richcleaner)
library(zip)
unzip("GSEA.zip")
set.seed(1502)


df_r <- rich_wider(gsea, fdr_threshold = 0.001, gs = "GOBPs", value = "n_logp_sign")

test_that("rich_wider does not generae warnings", {
  expect_warning(rich_wider(gsea, fdr_threshold = 0.001, gs = "GOBPs", value = "n_logp_sign"), NA)
})


test_that("rich_wider", {
  expect_snapshot_value(
    df_r, style = "serialize")
})


rich_results <- rich_aggregate(path = "./GSEA")
rich_results <- rich_results[sample(nrow(rich_results), 100), ]

# test_that("rich_aggregate", {
#   expect_snapshot_value(
#     rich_results, style = "serialize")
# })


unlink("./GSEA", recursive = TRUE)

