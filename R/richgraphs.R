#' czmo_pheatmap_gsea_
#'
#' Given a dataframe with pathway, one gs, contrast, FDR.q.val and NES, it reshapes it, calculates
#' negative log10(fdr_q_val) * sign of NES and plots it
#' plots heatmaps. This is use internaly
#'
#' @param dat Dataframe
#' @param res Resolution in dpi
#' @param gs The gs to plot
#' @param p_th Threshold for p value filtering
#' @param width Width of the graph
#' @param height Height of the graph
#' @param save_path Where to save the data.
#' @param ... Pheatmap arguments
#' @importFrom rlang .data
#' @importFrom magrittr %>%
rich_pheatmap_ <- function(dat,
                                gs,
                                p_th,
                                res,
                                width,
                                height,
                                save_path,
                                ...){

  dat_w <-  dat %>%
    tibble::rownames_to_column("description") %>%
    dplyr::mutate(fdr_q_val = dplyr::if_else(.data$fdr_q_val == 0, 10^-10, .data$fdr_q_val)) %>%
    dplyr::mutate(n_logp_sign = -log10(.data$fdr_q_val) * sign(.data$nes)) %>%
    dplyr::filter(.data$gs == !!gs) %>%
    dplyr::filter(.data$fdr_q_val < !!p_th) %>%
    tidyr::pivot_wider(id_cols = "description", names_from = "contrast", values_from = "n_logp_sign", values_fill = 0)  %>%
    tibble::column_to_rownames(var = "description")

  file_name <- paste0(gs, "_", p_th, ".png")
  grDevices::png(
    filename = file.path(save_path, file_name),
    width = width,
    height = height,
    res = res
  )

  pheatmap::pheatmap(as.matrix(dat_w),
           col = rev(RColorBrewer::brewer.pal(11, "RdBu")),
           main = gs,
           ...)
  grDevices::dev.off()

}

#' rich_pheatmap
#'
#' Given a dataframe with pathway, gs, contrast, FDR.q.val and NES, it reshapes it, calculates
#' negative log10(fdr_q_val) * sign of NES and plots a pheatmap. Values of FDR.q.val equal to 0 are
#' substituted with `10^10`. Plot is generate with `{pheatmap}` package and so function accepts those arguments too.
#' (Yes a method of of pheatmap would have been politer but this is use internally in the lab)
#'
#' @param dat Dataframe.
#' @param res Resolution in dpi.
#' @param p_th Threshold for p value filtering.
#' @param width Width of the graph.
#' @param height Height of the graph.
#' @param save_path Where to save the data.
#' @param ... pheatmap arguments.
#' @export
rich_pheatmap <- function(dat,
                                p_th,
                                res,
                                width,
                                height,
                                save_path,
                                ...){

  dat <- janitor::clean_names(dat)

  lapply(unique(dat$gs), rich_pheatmap_,
     dat = dat,
     p_th = p_th,
     res = res,
     width = width,
     height = height,
     save_path = save_path
     )
}




