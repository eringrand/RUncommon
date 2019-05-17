#' @title read_clean_data
#' @description Reads in clean data from excel using janitor package and readxl
#' - Read in the data, assumes na = "",
#' - Removes empty columns and rows
#' - Cleans column names
#'
#' @param file The location of the file you are using.
#' @param sheetname The name of number of the sheet (as in readxl)
#' @param ... Additional parameters to pass to readxl
#'
#' @examples
#' \dontrun{
#' library(RUncommon)
#' read_clean_data(filelocation, 1)
#' }
#' @export

read_clean_data <- function(file, sheetname, ...) {
  readxl::read_excel(file, sheet = sheetname, na = "", ...) %>%
    janitor::remove_empty("cols") %>%
    janitor::remove_empty("rows") %>%
    janitor::clean_names()
}


#' @title len
#' @description For similarity between Python and R.  Returns the length of a vector.
#' @param x vector
#' @examples
#' library(RUncommon)
#' x <- 1:5
#' len(x)
#' @export

len <- function(x) {
  length(x)
}

#' @title tochar
#' @param x a vector of factors
#' @description Turns factor vector into a character vector - without have numbered levels.
#' @export

tochar <- function(x) {
  as.character(levels(x)[x])
}


#' Cohort Tag
#' @description Defines a student's cohort given the student's grade and current school year.
#'
#' If you do not provide a school year the default is the current (spring) year.
#' e.g If today's date is Jan 10, 2017, the default year will be 2017,
#' however if the current date is Sep 10, 2017, the year will default to 2018.
#'
#' @param grade Enrolled grade of the student  (0 - 12)
#' @param school_year School year either in 20XX-YY or 20XX (spring) format.
#' @examples
#' library(RUncommon)
#' cohort(10)
#' cohort(10, 2008)
#' @export

cohort <- function(grade, school_year = "") {
  if(any(!is.numeric(grade))) stop("Grade must be a number.")
  if(any(grade > 12 | grade < 0)) stop("Grade is not a real grade.")

  years_to_grad <- 12 - as.numeric(grade)

  if (all(school_year == "")) {
    month_of_year <- format(Sys.Date(), "%m")

    # uses two functions from school_year.R
    current_year <- sy_number(school_year_from_date())
  }

  else {
    # uses function from school_year.R
    current_year <- sy_number(school_year)
  }

  current_year + years_to_grad
}

#' yes_no
#' @description Changes all 1s to YES and 0s to NO.
#' Will not effect other values - so NAs remain the same.
#' @param x vectors of 1s and 0s
#' @export
yes_no <- function(x) {
  dplyr::recode(x, "1" = "Yes", "0" = "No")
}


#' @title change_firstlast_to_lastfirst
#' @description change first_name last_name, to last_name, first_name
#' This function does not work with two part last names as it assumes
#' there is only one space between the first and last name.
#' @param name The full name of someone in the form of "First_Name Last_Name"
#' @return Last_Name, First Name
#' @export

change_firstlast_to_lastfirst <- function(name) {
  name_list <- stringr::str_split(name, pattern = " ")[[1]]
  x <- ""
  if (length(name_list) <= 2) {
    x <- paste(name_list[2], name_list[1], sep = ", ")
  }
  if (length(name_list) >= 3) {
    x <- paste(name_list[3], name_list[1], sep = ", ")
  }
  return(x)
}


#' @title round_percent
#' @description  A version of scales::percent() that allows for rounded digit
#' so that you can have 20% or 20.5% as needed.
#' @param x a numeric vector to format
#' @param dig the number of digits after the percent to round. Default to 1.
#' @examples
#'  round_percent(.24601)
#'  round_percent(.24601, dig = 0)
#' @export

round_percent <- function(x, dig = 1) {
  if(x > 1) stop("Percents cannot be larger than one.")
  paste0(round(x * 100, digits = dig), "%")
}

#' @title cols_with_nas
#' @description Count the number of NAs in each column
#'
#' @param data data frame to look through
#'
#' @examples
#' \dontrun{
#' cols_with_nas(nycflights13::flights)
#' }
#' @export

cols_with_nas <- function(data) {
  new_data <- tidyr::gather(data, key = "key", value = "value") %>%
    dplyr::filter(is.na(value)) %>%

  if (nrow(new_data) != 0) {
    new_data <- dplyr::count(new_data, key)
  }

  return(new_data)
}
