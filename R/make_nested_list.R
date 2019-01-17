#' @title Function to make nested lists (store modeling output)
#'
#' @desciption This function will make nested lists so you can store modeling information about
#'     specific PCP's as well as covariates of interest
#' @param dim_lvl_1 Dimensions of outer level (ie. Number of PCP's)
#' @param dim_lvl_2 Dimensions of inner level (ie. Number of covariates)
#' @export
#' @return A nested list with two levels
#' @examples \dontrun{
#' make_nested_list(5, 7)
#' }


make_nested_list <- function(dim_lvl_1, dim_lvl_2) {

  rep(
    list(
      rep(list(0), dim_lvl_2)
    ),
    dim_lvl_1)
}
