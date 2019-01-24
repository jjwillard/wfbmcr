#' @title Find patients who have seen a nephrologist within specified time period and create flag
#'
#' @description This function allows you to create a dataframe which includes the Patient MRN
#'     and an indicator variable (returns 'Yes' for 'has_seen_neph') for all patients who have seen
#'     a nephrologist within a specified time period.  This dataframe can then be merged with a summarized
#'     blood pressure data table (see wfbmcr::summarize_bp_dt' for data table creation and
#'     'wfbmcr::merge_w_indicator' for merging)
#' @param df Raw EHR dataset which includes blood pressure data
#' @param cutoff_date End date of interest in month-day-4digityear format (ie. 11012018 is Nov 1, 2018)
#' @param num_years Number of years to search back from cutoff_date
#' @param pat_mrn The variable name which includes the Patient MRN information
#' @export
#' @import lubridate
#' @import dplyr
#' @return A data table listing Patient MRN and an indicator variable for nephrology visit
#' @examples \dontrun{
#' has_seen_nephrologist(df, enc_date_var = CONTACT_DATE, cutoff_date, num_years = 1,
#' pat_mrn = PAT_MRN_ID)
#' }



has_seen_nephrologist <- function(df, cutoff_date, num_years, enc_date_var, pat_mrn){

  enc_date_var <- enquo(enc_date_var)
  cov_name <- quo_name(enc_date_var)
  pat_mrn <- enquo(pat_mrn)

  ### Format Encounter Date ###

  neph <- df %>% mutate(!!cov_name := as.Date(dmy_hms(!!enc_date_var)))


  ### Make One Year Time Interval ###

  int <- interval(cutoff_date - years(num_years), cutoff_date)
  has_seen_neph <- neph %>% filter(!!enc_date_var %within% int) %>% select(!!pat_mrn)


  has_seen_neph$has_seen_neph <- "Yes"

  ## Take only one pid

  has_seen_neph <- has_seen_neph %>% group_by(!!pat_mrn) %>% slice(1)

  invisible(has_seen_neph)

}
