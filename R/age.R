#' @title age
#' @description Calculate someone's age based on their birthday and a provided date
#' @param birthdate birthday in the form YMD
#' @param as_of_date current day in the form YMD, default is today
#' @return age
#' @export

age <- function(birthdate, as_of_date = NULL) {
  if(is.null(as_of_date)) as_of_date <- lubridate::today()

  int <- lubridate::interval(lubridate::ymd(birthdate), lubridate::ymd(as_of_date))
  trunc(lubridate::time_length(int, "year"))

}