#' spread_many
#'
#' Spread more than one column out at once

#' @param data data
#' @param key_col key_col
#' @param ... List of column, unquoted to spread out
#' @param fill fill
#' @param convert convert
#' @param drop drop
#' @param sep sep
#'
#' @return data frame with new columns
#' @examples
#' library(dplyr)
#' library(tidyr)
#'
#' fruits <- data.frame(
#'     case = c(1, 1, 2, 2),
#'     fruit = rep(c("Apple", "Orange"), 2),
#'     height = rnorm(4, 0, 1),
#'     width = rnorm(4, 0, 2)
#' )
#'
#' spread_many(fruits, fruit, width, height)
#'
#' @export

spread_many <- function(data,
                        key_col,
                        ...,
                        fill = NA,
                        convert = TRUE,
                        drop = TRUE,
                        sep = "_"
                        ) {
  key_quo <- rlang::enquo(key_col)
  val_quos <- rlang::quos(...)
  value_cols <- unname(tidyselect::vars_select(names(data),!!!val_quos))
  key_col <- rlang::quo_name(key_quo)

  data %>%
    tidyr::gather(key = "..var..", value = "..val..", !!!val_quos) %>%
    tidyr::unite(col = ..grp.., c(key_col,"..var.."), sep = sep) %>%
    tidyr::spread(key = ..grp..,value = ..val..,
                  fill = fill, convert = convert,
                  drop = drop,
                  sep = NULL) %>%
    tibble::as_tibble()
}
