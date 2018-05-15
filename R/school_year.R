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



#' @title change_school_year
#' @description Takes a value in the form 2016-2017
#' and converts to 2016-17
#' @export
#' @examples
#'  change_school_year("2014-2015")
change_school_year <- function(school_year){
  first_year <- str_sub(school_year, 1, 4)
  second_year <- str_sub(school_year, -2, -1)
  paste0(first_year, "-", second_year)
}
