#' @title strsplit_that_works
#' @description We got tired of having to work with spliting strings in weird ways.
#' @param cell the string you want to split
#' @param entity_requested a number representing the
#' @param split split used in strplit Default: '_'
#' @details Returns only the part of a string as designated by entity_requested
#' @examples
#' \dontrun{
#'    try <- ("1_2_3_4")
#'    strsplit_that_works(try, 1, split = "_")
#'
#'    library(tidyverse)
#'    mutate(tidyr::table3, map_chr(rate, first_part = strsplit_that_works, 1, split = "/")
#' }
#' @export

strsplit_that_works <- function(cell, entity_requested, split = "_") {
  x <- strsplit(cell, split = split)
  y <- x[[1]][entity_requested]
  return(y)
}


