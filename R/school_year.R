#' @title school_year
#' @param school_year school year in the form SY16-17 or 2016-17
#' @param before_200 if the year is prior to 2000 and should be written as 1900 - 1901. Dafaults to FALSE (i.e. 2001-02)
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
#'  
change_school_year <- function(school_year){
  first_year <- substr(school_year, 1, 4)
  second_year <- stringr::str_sub(school_year, -2, -1)
  paste0(first_year, "-", second_year)
}


#' @title sy_form
#' @param school_year school year in the form of a number
#' @param spring_year If the given year represents the Spring portion then TRUE. Defaults to TRUE
#' @description Takes a value in the form of a number (i.e. 2017)
#' and converts to 2016-17. The opposite of `sy_number` with 20 instead of SY.
#' @export
#' @examples
#'  sy_form("2015")

sy_form <- function(school_year, spring_year = TRUE) {
  cent <- as.numeric(substr(school_year, 1, 2)) * 100
  school_year <- as.numeric(school_year)
  if(spring_year) {
    paste(school_year - 1, school_year - cent, sep = "-")
  } else {
    paste(school_year, school_year - cent + 1 , sep = "-")
  }
}
