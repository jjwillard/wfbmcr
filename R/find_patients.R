#' @title Find Patients who see PCP's of interest within past year
#'
#' @description This function allows you to find the patients who see PCP's of interest and who have had visits
#'     within one year of the cutoff date.
#' @param df EHR dataset which has had dates properly formatted by format_ehr_date function
#' @param cutoff_date End date of interest in month-day-4digityear format (ie. 11012018 is Nov 1, 2018)
#' @param vec_of_pcps A vector listing names of PCP's in format found in EHR
#' @param pcp_var The variable name which includes the names of the PCP's
#' @param pat_mrn The variable name which includes the Patient MRN information
#' @param enc_date_var The variable name which includes information pertaining to visit/encounter dates
#' @export
#' @import dplyr
#' @import lubridate
#' @return A subsetted data frame which includes patients with PCP's of interest and within date range
#' @examples \dontrun{
#' find_patients(df, cutoff_date, vec_of_pcps = pcp_vec, pcp_var = Patient.PCP,
#' pat_mrn = Patient.MRN, enc_date_var = Encounter.Date)
#'}



find_patients <- function(df, cutoff_date, vec_of_pcps, pcp_var, pat_mrn, enc_date_var){

  pcp_var <- enquo(pcp_var)
  pat_mrn <- enquo(pat_mrn)
  enc_date_var <- enquo(enc_date_var)

  res <- df %>% filter(!!pcp_var %in% vec_of_pcps, !!enc_date_var >= cutoff_date - years(1),
                            !!enc_date_var < cutoff_date) %>%
    group_by(!!pat_mrn, !!enc_date_var) %>% arrange(!!pat_mrn, .by_group = TRUE) %>% slice(1)

  invisible(res)
}
