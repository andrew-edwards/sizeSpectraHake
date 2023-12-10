##' Plot estimates of b and recruitment
##'
##' Adds the prior, which is assumed to be the final year in `recruitment`
##' (which will be the prior for hake).
##' TODO Could add switch for `b_w`
##' TODO subscript x label properly
##'
##' @param b_tibble currently with `Year`, `b_l`, `b_l_confMin`, `b_l_confMax`,
##' @param recruitment_tibble tibble with same years as `b_tibble`, with columns
##'   `year`, `low`, `median`, `high`
##' @param years_exclude vector of years to exclude the `b_tibble` results for; if NULL then
##'   use all years
##' @param conf_col colour for confidence intervals
##' @param label_col colour for labels
##' @param xlim usual, default encompass all years being plotted
##' @param ylim usual default includes max of 2023 credible interval
##' @param xlab usual
##' @param ylab usual
##' @param pch usual
##' @param cex usual
##' @param text_cex for year labels
##' @param adj adjustment for labelling years
##' @param show_prior logical, whether to show prior distribution
##' @param prior_loc x-value to plot prior distribution
##' @param ... extra arguments for `plot()` TODO not implemented yet
##' @return plots recruitment estimates (with credible intervals) against `b_l`
##'   estimates (with credible intervals)
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' # See .Rmd
##' }
plot_b_recruitment <- function(b_tibble,
                               recruitment_tibble = hake_recruitment_over_2010,
                               years_exclude = NULL,
                               conf_col = "grey",
                               label_col = "red",
                               xlim = NULL,
                               ylim = c(0, 1.1),
                               xlab = "Length size spectrum exponent, b_l",
                               ylab = "Estimated age-0 hake recruitment scaled by 2010 value",
                               pch = 20,
                               cex = 1,
                               text_cex = 0.7,
                               adj = c(-0.15, -0.3),
                               show_prior = TRUE,
                               prior_loc = 0.3,
                               ...){

  # This works even if years_exclude is NULL
  b_plot <- dplyr::filter(b_tibble,
                          !year %in% years_exclude)

  recruitment_plot <- dplyr::filter(recruitment_tibble,
                                    year %in% b_plot$year)

  if(is.null(xlim)){
    xlim <- c(min(b_plot$b_l_confMin), 0)
  }

  recruitment_prior <- dplyr::filter(recruitment_tibble,
                                     year == max(recruitment_tibble$year))

  plot(b_plot$b_l,
     recruitment_plot$median,
     xlim = xlim,
     xlab = xlab,
     ylab = ylab,
     pch = pch,
     cex = cex)

  # Confidence intervals for b_l
  segments(x0 = b_plot$b_l_confMin,
           y0 = recruitment_plot$median,
           x1 = b_plot$b_l_confMax,
           y1 = recruitment_plot$median,
           col = conf_col)

  # Credible intervals for recruitment
  segments(x0 = b_plot$b_l,
           y0 = recruitment_plot$low,
           x1 = b_plot$b_l,
           y1 = recruitment_plot$high,
           col = conf_col)
  # Add labels
  text(b_plot$b_l,
       recruitment_plot$median,
       recruitment_plot$year,
       adj = adj,
       col = label_col,
       cex = text_cex)

  # Add prior distribution:
  if(show_prior){
    segments(x0 = prior_loc,
             y0 = recruitment_prior$low,
             x1 = prior_loc,
             y1 = recruitment_prior$high,
             col = "blue")

    points(prior_loc,
           recruitment_prior$median,
           col = "blue",
           pch = 20)
  }
}
