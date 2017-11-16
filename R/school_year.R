#' @title school_year
#' @description Takes a value in the form SY16-17 or 2016-17
#' and converts to a number representing the second/spring year.
#' @export
#' @examples
#'  sy_number("SY16-17")

sy_number <- function(school_year, before_2000 = FALSE) {
  if(!before_2000) {
    2000 + as.numeric(stringr::str_sub(school_year, -2))
  }
  else{
    1900 + as.numeric(stringr::str_sub(school_year, -2))
  }
}
