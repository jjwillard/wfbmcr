#' @title Summarize Blood Pressure Data Table
#'
#' @description This function allows you summarize a dataframe consisting of blood pressure data and
#'     put it into a data table
#' @param df Raw EHR dataset which includes blood pressure data
#' @param cutoff_date End date of interest in month-day-4digityear format (ie. 11012018 is Nov 1, 2018)
#' @param num_years Number of years to search back from cutoff_date
#' @param vec_of_pcps A vector listing names of PCP's in format found in EHR
#' @param pcp_var The variable name which includes the names of the PCP's
#' @param pat_mrn The variable name which includes the Patient MRN information
#' @param enc_date_var The variable name which includes information pertaining to visit/encounter dates
#' @param enc_csn_var The variable name which includes information pertaining to encounter CSN
#' @param sbp_var The variable name which includes information pertaining to systolic blood pressure
#' @export
#' @import rlang
#' @import lubridate
#' @import dplyr
#' @return A data table summarizing patient blood pressure information
#' @examples \dontrun{
#' summarize_bp_dt(df, cutoff_date, num_years = 1, vec_of_pcps, enc_date_var = Encounter.Date,
#'  sbp_var = Systolic.BP, enc_csn_var = Encounter.CSN, pat_mrn = Patient.MRN, pcp_var = Patient.PCP)
#' }



summarize_bp_dt <- function(df, cutoff_date, num_years, enc_date_var, pat_mrn, sbp_var, enc_csn_var, pcp_var,
                            vec_of_pcps) {

  enc_date_var <- enquo(enc_date_var)
  cov_name <- quo_name(enc_date_var)
  sbp_var <- enquo(sbp_var)
  pat_mrn <- enquo(pat_mrn)
  enc_csn_var <- enquo(enc_csn_var)
  pcp_var <- enquo(pcp_var)

  bp <- df %>% mutate(!!cov_name := as.Date(dmy_hms(!!enc_date_var)))

  ### Filter out missing SBP data, take only the first of each 'Encounter CSN' because represents ###
  ### same visit and same SBP ###

  bp2 <- bp %>%
    filter(!is.na(!!sbp_var), !!enc_date_var >= cutoff_date - years(num_years),
           !!enc_date_var <= cutoff_date) %>%
    group_by(!!pat_mrn, !!enc_csn_var, .by_group = TRUE) %>%
    slice(1) %>%
    ungroup()

  bp3 <- bp2 %>%
    group_by(!!pat_mrn, .by_group = TRUE) %>%
    summarize(n_visits = n(),
              d_last_visit = max(!!enc_date_var),
              mean_sbp = round(mean(!!sbp_var, na.rm = TRUE), 2),
              sd_sbp = round(sd(!!sbp_var, na.rm = TRUE), 2),
              min_sbp = min(!!sbp_var),
              max_sbp = max(!!sbp_var)) %>%
    mutate(MeanSBPgt140 = if_else(mean_sbp > 140, "Yes", "No")) %>%
    arrange(!!pat_mrn)

  ### Find PCP for last patient visit and merge with result (since some patients have seen multiple PCP) ###

  pcp_mrn_list <- bp %>%
    filter(!is.na(!!sbp_var), !!enc_date_var >= cutoff_date - years(num_years),
           !!enc_date_var <= cutoff_date) %>%
    filter(!!pcp_var %in% vec_of_pcps) %>%
    select(!!pat_mrn, !!pcp_var, !!enc_date_var) %>%
    group_by(!!pat_mrn, .by_group = TRUE) %>%
    arrange(desc(!!enc_date_var)) %>%
    slice(1) %>%
    ungroup() %>%
    select(!!pat_mrn, !!pcp_var)


  ### Join the results together ###

  res <- left_join(pcp_mrn_list, bp3, by = set_names(quo_name(pat_mrn))) %>%
    select(!!pcp_var, !!pat_mrn, n_visits, d_last_visit, mean_sbp, sd_sbp, min_sbp, max_sbp, MeanSBPgt140)


  invisible(res)

}
