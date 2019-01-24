#' @title Merge a Blood Pressure Data Table with and Indicator Variable
#'
#' @description This function allows you to merge a blood pressure dataframe with an indicator variable
#'     dataframe on the Patient MRN variable.  The variable names for Patient MRN can be different in the
#'     two dataframes.  (See also: 'wfbmcr::summarize_bp_dt' for blood pressure data table creation and
#'     'wfbmcr::has_seen_nephrologist' for an example of an indicator variable data table creation using
#'     nephrology data)
#' @param bp_df A dataframe which includes summarized blood pressure data (see: 'wfbmcr::summarize_bp_dt')
#' @param bp_pat_mrn Patient MRN variable for the blood pressure dataframe
#' @param indic_df A dataframe which includes indicator variable data (ex: 'wfbmcr::has_seen_nephrologist')
#' @param indic_pat_mrn Patient MRN variable for the indicator variable dataframe
#' @param indic_var_name The name of the indicator variable from the indicator variable dataframe
#' @export
#' @import dplyr
#' @return A merged data table providing blood pressure and indicator variables of interest
#' @examples \dontrun{
#' merge_bpdt_w_indicator(bp_df, bp_pat_mrn = Patient.MRN, indic_df = neph_df, indic_pat_mrn = PAT_MRN_ID,
#' indic_var_name = has_seen_neph)
#' }



merge_bpdt_w_indicator <- function(bp_df, bp_pat_mrn, indic_df, indic_pat_mrn, indic_var_name){

  bp_pat_mrn <- enquo(bp_pat_mrn)
  indic_pat_mrn <- enquo(indic_pat_mrn)
  indic_var_name <- enquo(indic_var_name)
  cov_name <- quo_name(indic_var_name)

  new_df<- left_join(bp_df, indic_df, by = set_names(quo_name(indic_pat_mrn), quo_name(bp_pat_mrn)))

  new_df <- new_df %>% mutate(!!cov_name := if_else(is.na(!!indic_var_name), "No", !!indic_var_name))

  invisible(new_df)
}
