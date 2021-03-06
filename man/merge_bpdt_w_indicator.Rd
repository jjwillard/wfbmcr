% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_bpdt_w_indicator.R
\name{merge_bpdt_w_indicator}
\alias{merge_bpdt_w_indicator}
\title{Merge a Blood Pressure Data Table with and Indicator Variable}
\usage{
merge_bpdt_w_indicator(bp_df, bp_pat_mrn, indic_df, indic_pat_mrn,
  indic_var_name)
}
\arguments{
\item{bp_df}{A dataframe which includes summarized blood pressure data (see: 'wfbmcr::summarize_bp_dt')}

\item{bp_pat_mrn}{Patient MRN variable for the blood pressure dataframe}

\item{indic_df}{A dataframe which includes indicator variable data (ex: 'wfbmcr::has_seen_nephrologist')}

\item{indic_pat_mrn}{Patient MRN variable for the indicator variable dataframe}

\item{indic_var_name}{The name of the indicator variable from the indicator variable dataframe}
}
\value{
A merged data table providing blood pressure and indicator variables of interest
}
\description{
This function allows you to merge a blood pressure dataframe with an indicator variable
    dataframe on the Patient MRN variable.  The variable names for Patient MRN can be different in the
    two dataframes.  (See also: 'wfbmcr::summarize_bp_dt' for blood pressure data table creation and
    'wfbmcr::has_seen_nephrologist' for an example of an indicator variable data table creation using
    nephrology data)
}
\examples{
\dontrun{
merge_bpdt_w_indicator(bp_df, bp_pat_mrn = Patient.MRN, indic_df = neph_df, indic_pat_mrn = PAT_MRN_ID,
indic_var_name = has_seen_neph)
}
}
