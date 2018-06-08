#' @title read_clean_data
#' @description Reads in clean data from excel using janitor package and readxl
#' Remove empty columns, rows and then clean the names of the remaining columns.
#'
#' @param file The location of the file you are using.
#' @param sheename The name of number of the sheet (as in readxl)
#' @param ... Additional parameters to pass to readxl
#'
#' @examples
#' \dontrun{
#' library(RUncommon)
#' /read_clean_data(filelocation, 1)
#' }
#' @export

read_clean_data <- function(file, sheetname, ...) {
  readxl::read_excel(file, sheet = sheetname, na = "", ...) %>%
    janitor::remove_empty("cols") %>%
    janitor::remove_empty("rows") %>%
    janitor::clean_names()
}


#' @title len
#' @description For consistincy between Python and R.  Returns the length of a vector.
#'
#' @param x vector
#' @examples
#'  library(RUncommon)
#' x <- 1:5
#' len(x)
#' @export

len <- function(x) {
  length(x)
}

#' @title tochar
#' @description Turns factor vector into a character vector - without have numbered levels.
#' @export

tochar <- function(x) {
  as.character(levels(x)[x])
}



#' @title colorgorical
#' @description uses Colorgoical to create color paletes that work well.
#' Requires httr and jsonlite packages.
#'  @export

colorgorical <- function(n = 10) {
  post_body <- jsonlite::toJSON(
    auto_unbox = TRUE,
    list(
      "paletteSize" = n,
      "weights" = list(
        "ciede2000" = 0,
        "nameDifference" = 0,
        "nameUniqueness" = 0,
        "pairPreference" = 0
      ),
      "hueFilters" = list(),
      "lightnessRange" = c("25", "85"),
      "startPalette" = list()
    )
  )
  retval <- httr::POST(
    url = "http://vrl.cs.brown.edu/color/makePalette",
    body = post_body
  )
  retval <- httr::content(retval)
  return(sapply(retval$palette, function(x) {
    sprintf("rgb(%s,%s,%s)", x[[1]], x[[2]], x[[3]])
  }))
}


#' Cohort Tag
#' @param grade Enrolled grade of the student
#' @param school_year School year either in 20XX-YY or 20XX format.
#' @examples
#' library(RUncommon)
#' cohort(10)
#' cohort(10, 2008)
#' @export

cohort <- function(grade, school_year="") {
  years_to_grade <- 12 - as.numeric(grade)
  if (all(school_year == "")) {
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
  }
  else {
    current_year <- as.numeric(paste0("20", stringr::str_sub(school_year, -2, -1)))
  }
  current_year + years_to_grade
}

#' yes_no
#' @description Changes all 1's to "Yes" and all 0's to "No." Does not change NAs or other values.
#' @export
yes_no <- function(x) {
  dplyr::recode(x, "1" = "Yes", "0" = "No")
}


#' @title change_firstlast_to_lastfirst
#' @description change first_name last_name, to last_name, first_name. Does not work with two part last names as it assumes only one space between the first and last name.
#' @param name The full name of someone in the form of First Name, Last Name. A character.
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


#' @title cols_with_nas
#' @description Count the number of NAs in each column
#' @param data Data frame to use
#' @export

cols_with_nas <- function(data) {
  
  new_data <- tidyr::gather(data) %>%
    dplyr::filter(is.na(value))
  
  if(nrow(new_data) != 0) {
    new_data <- dplyr::count(new_data, key)
  } 
  
  return(new_data)
}
