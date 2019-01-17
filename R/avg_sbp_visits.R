#' @title Average Systolic Blood Pressures and Visit Information
#'
#' @description This function allows you to find average SBP, DBP, and number of visits per month for patients
#'     in addition to calculating total number of visits.
#' @param df EHR dataset which has had dates properly formatted by format_ehr_date function
#' @param birth_date_var The variable name which includes information pertaining to birthdate
#' @param pat_mrn The variable name which includes the Patient MRN information
#' @param pcp_var The variable name which includes the names of the PCP's
#' @param enc_date_var The variable name which includes information pertaining to visit/encounter dates
#' @param sbp_var The variable name which includes information pertaining to systolic blood pressure
#' @param dbp_var The variable name which includes information pertaining to diastolic blood pressure
#' @param age_var The variable name which includes information pertaining to age
#' @export
#' @import dplyr
#' @import lubridate
#' @return A data frame with several newly derived variables related to sbp and number of visits
#' @examples \dontrun{
#' average_sbp(df, birth_date_var = Birth.Date, pat_mrn = Patient.MRN, pcp_var = Patient.PCP,
#' enc_date_var = Encounter.Date, sbp_var = Systolic.BP, dbp_var = Diastolic.BP,
#' age_var = age_at_last_encounter)
#' }



avg_sbp_visits <- function(df, birth_date_var, pat_mrn, pcp_var, enc_date_var, sbp_var, dbp_var, age_var){

  birth_date_var <- enquo(birth_date_var)
  pat_mrn <- enquo(pat_mrn)
  pcp_var <- enquo(pcp_var)
  enc_date_var <- enquo(enc_date_var)
  sbp_var <- enquo(sbp_var)
  dbp_var <- enquo(dbp_var)
  age_var <- enquo(age_var)

  res <- df %>% group_by(!!pat_mrn, year = year(!!enc_date_var), month = month(!!enc_date_var)) %>%
    mutate(avg_sbp = mean(!!sbp_var, na.rm = TRUE), avg_dbp = mean(!!dbp_var, na.rm = TRUE),
           n_visits_month = n()) %>%
    select(!!birth_date_var, !!pat_mrn, !!pcp_var, !!age_var, year, month, avg_sbp, avg_dbp,
           n_visits_month) %>%
    group_by(!!pat_mrn, year, month) %>%
    slice(1) %>%
    mutate(sbp_calc_date = as.Date(paste0(year, "-", month, "-01"))) %>%
    group_by(!!pat_mrn) %>%
    mutate(total_visits = sum(n_visits_month))

  invisible(res)
}
