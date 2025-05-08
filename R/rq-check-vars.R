#' Helper function to check variables
#'
#' @param .df data frame or tibble
#' @param .vars vector of required column names
#'
#' @return throws error, if not available
#' @export
check_variables <- function(.df, .vars){
  if (! all(.vars %in% colnames(.df)) ){
    usethis::ui_stop("Required variable(s) is(are) not in the data set.")
  }
}
