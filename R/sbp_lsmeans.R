#' @title Calculate LS Means of SBP for Spatial Correlation Model
#'
#' @description This function calculates a spatially correlated covariance matrix and then finds the LS means for
#'     SBP from an EHR dataset which has the following names: avg_sbp, sbp_calc_date, and Patient.MRN
#' @param df EHR dataset which has had variable names properly formatted to match modeling
#' @export
#' @import nlme
#' @import emmeans
#' @return A data frame with lsmeans for avg_sbp by month and year
#' @seealso \code{\link[nlme]{gls}}, \code{\link[emmeans]{emmeans}}
#' @examples \dontrun{
#' sbp_lsmeans(df)
#' }


sbp_lsmeans <- function(df){

  obj <- gls(avg_sbp ~ as.factor(sbp_calc_date), data = df, na.action = (na.omit),
             method = "REML", correlation = corAR1(form = ~ sbp_calc_date|Patient.MRN))

  res <- as.data.frame(emmeans(obj, ~ sbp_calc_date, data = df))
  invisible(res)
}
