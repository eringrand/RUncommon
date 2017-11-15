#' @title read_clean_data
#' @description Reads in clean data from excel using janitor package and readxl
#'
#' @param file The location of the file you are using.
#' @param sheename The name of number of the sheet (as in readxl)
#' @param ... Additional parameters to pass to readxl
#'
#' @examples
#'
#' read_clean_data(filelocation, 1)

read_clean_data <- function(file, sheetname, ...) {
  readxl::read_excel(file, sheet = sheetname, na = "", ...) %>%
    janitor::remove_empty_cols() %>%
    janitor::remove_empty_rows() %>%
    janitor::clean_names()
}


#' @title len
#' @description For consistincy between Python and R.  Returns the length of a vector
#'
#' @param x vector
#' @examples
#' x <- 1:5
#' len(x)

len <- function(x) {
  return(length(x))
}

#' @title tochar
#' @description Turn factor vector into a character vector
tochar <- function(x) {
  as.character(levels(x)[x])
}



#' @title colorgorical
#' @description uses Colorgoical to create color paletes that work well.
#' Requires httr and jsonlite packages.
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
yes_no <- function(x) {
  dplyr::if_else(x %in% 1, "Yes", "No")
}


#' @title change_firstlast_to_lastfirst
#' @description change first_name last_name, to last_name, first_name
#' Does not work with two part last names as it assumes only one space
#' between the first and last name.
change_firstlast_to_lastfirst <- function(name) {
  name_list <- str_split(name, pattern = " ")[[1]]
  x <- ""
  if (length(name_list) <= 2) {
    x <- paste(name_list[2], name_list[1], sep = ", ")
  }
  if (length(name_list) >= 3) {
    x <- paste(name_list[3], name_list[1], sep = ", ")
  }
  return(x)
}
