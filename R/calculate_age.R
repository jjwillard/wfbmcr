#' @title Calculate Age of Patients
#'
#' @description This function calculates ages from patient birthdays
#' @param df Raw EHR dataset which includes birthdate covariate
#' @param birth_date_var Birthdate covariate
#' @param end_date_var End date for age calculation
#' @param new_var_name What the newly created age variable should be called
#' @export
#' @import dplyr
#' @import lubridate
#' @return A data frame with a derived age variable
#' @examples \dontrun{
#' calculate_age(df, Birth.Date, Encounter.Date, Derived.Age)
#' }


calculate_age <- function(df, birth_date_var, end_date_var, new_var_name) {

  birth_date_var <- enquo(birth_date_var)
  end_date_var <- enquo(end_date_var)
  new_var_name <- enquo(new_var_name)
  age_name <- quo_name(new_var_name)

  res <- df %>% mutate(!!age_name := as.period(interval(start = !!birth_date_var,
                                                             end = !!end_date_var))$year)
  invisible(res)

}

