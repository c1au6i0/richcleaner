
<!-- README.md is generated from README.Rmd. Please edit that file -->

# richcleaner

<!-- badges: start -->

[![](https://img.shields.io/badge/devel%20version-0.0.0.9-blue.svg)](https://github.com/c1au6i0/richcleaner)

<!-- badges: end -->

The goal of `{richcleaner}` is to clean up enriched data generated by
[gsea](https://www.gsea-msigdb.org/gsea/index.jsp). Just a couple of
handy functions that we can share in Marchionni lab.

## Installation

``` r
library(devtools)

devtools::install_github("c1au6i0/richcleaner")
```

## Preparation of folders

Organize the folders in contrasts, each containing the folder for each
of the gene set databases used in `gsea`. Put the whole output folder of
`gsea` inside the corresponding folder. Recommanded contrast name
contain a `_` to separate group1 from group2 (`group1_group2`).

    #> root_folder
    #> ├── contrast1
    #> │   ├── BP
    #> │   │   └── gsea_folder
    #> │   │       ├── gsea_report_for_na_neg_1605121516086.tsv
    #> │   │       ├── gsea_report_for_na_pos_1605121516086.tsv
    #> │   │       └── many_other_files.ext
    #> │   ├── CS
    #> │   │   └── gsea_folder
    #> │   │       ├── gsea_report_for_na_neg_1605121516086.tsv
    #> │   │       ├── gsea_report_for_na_pos_1605121516086.tsv
    #> │   │       └── many_other_files.ext
    #> │   └── Reactome
    #> │       └── gsea_folder
    #> │           ├── gsea_report_for_na_neg_1605121516086.tsv
    #> │           ├── gsea_report_for_na_pos_1605121516086.tsv
    #> │           └── many_other_files.ext
    #> └── contrast2
    #>     ├── BP
    #>     │   └── gsea_folder
    #>     │       ├── gsea_report_for_na_neg_1605121516086.tsv
    #>     │       ├── gsea_report_for_na_pos_1605121516086.tsv
    #>     │       └── many_other_files.ext
    #>     ├── CS
    #>     │   └── gsea_folder
    #>     │       ├── gsea_report_for_na_neg_1605121516086.tsv
    #>     │       ├── gsea_report_for_na_pos_1605121516086.tsv
    #>     │       └── many_other_files.ext
    #>     └── Reactome
    #>         └── gsea_folder
    #>             ├── gsea_report_for_na_neg_1605121516086.tsv
    #>             ├── gsea_report_for_na_pos_1605121516086.tsv
    #>             └── many_other_files.ext

## Aggregation

Use the function `rich_aggregate` to look inside that tree folder for
`reports files` and to aggregate them in a single long dataframe.
Running the function with no `path` argument starts an interactive
`{svDialogs}` modal dialog box to choose the folder.

``` r
library(richcleaner)
rich_results <- rich_aggregate()
names(rich_results)
```

    #>  [1] "contrast"     "gs"           "description"  "size"         "es"          
    #>  [6] "nes"          "nom_p_val"    "fdr_q_val"    "fwer_p_val"   "rank_at_max" 
    #> [11] "leading_edge"

Some packages like `{pheatmaps}` takes as an input a matrix, so it is
convenient to select the results of a particular gene set database and
to pivot the dataframe in a wider format with `rich_wider`.

``` r
rich_results_wider <- rich_wider(rich_results, fdr_threshold = 0.001, gs = "GOBPs", value = "n_logp_sign")
```

The argument `fdr_threshold` is used for filtering out gene sets that
have an false discovery rate more than a certain number. We can pivot
wider the dataframe setting `value` to one of the numerical columns of
the dataframe.

## Plotting

The function `rich_pheatmap` is just a wrapper than runs `rich_wider`
and `pheatmap::pheatmap` on the result.
