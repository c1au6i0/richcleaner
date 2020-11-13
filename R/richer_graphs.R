#' rich_pheatmap
#'
#' Just a wrapper that runs \code{\link{rich_wider}} and produces a heatmap of the results using `{pheatmap}`.
#'
#' @param dat Dataframe as returned by script.
#' @param fdr_threshold Minimum `FDR.q.val` to keep.
#' @param gs Gene Set.
#' @param col Palette of heatmap. Default is `rev(RColorBrewer::brewer.pal(11, "RdBu"))`.
#' #' @param ... \link[pheatmap]{pheatmap} arguments .
#' @export
rich_pheatmap <- function(dat,
                          fdr_threshold,
                          gs,
                          value = "n_logp_sign",
                          ...){

  dat_wider<- rich_wider(dat,fdr_threshold = fdr_threshold, gs = gs, value = value)
  pheatmap::pheatmap(as.matrix(dat_wider),
                     col = c("#053061", "#2166AC", "#4393C3", "#92C5DE", "#D1E5F0", "#F7F7F7",
                             "#FDDBC7", "#F4A582", "#D6604D", "#B2182B", "#67001F"),
                     main = gs,
                     ...)

}
