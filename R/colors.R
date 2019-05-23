
#' @title colorgorical
#' @description uses colorgorical to create color palettes that work well.
#' Requires httr and jsonlite packages.
#' @param n number of colors in the palette. defaults to 10
#' Credit to \href{https://gist.github.com/slowkow/22daea426607416bfcd573ce9cbd89ab}{slowkrow}
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
