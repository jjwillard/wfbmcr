#' @title Format EHR Date
#'
#' @description This function allows you to format dates pulled from WFBMC EHR into standard format
#' @param df Raw EHR dataset which includes covariate
#' @param covariate Date covariate which needs to be reformatted
#' @export
#' @import dplyr
#' @import lubridate
#' @return A data frame with a reformatted date
#' @examples \dontrun{
#' format_ehr_date(df, Birth.Date)
#' }


format_ehr_date <- function(df, covariate) {

  covariate <- enquo(covariate)
  cov_name <- quo_name(covariate)

  res <- df %>% mutate(!!cov_name := as.Date(dmy_hms(!!covariate)))

  invisible(res)
}
