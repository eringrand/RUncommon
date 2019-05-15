#' spread_many
#'
#' Spread more than one column out at once
#'
#' @param df Data Frame
#' @param key Key column used in spread
#' @param ... List of column, unquoted to spread out
#'
#' @return data frame with new columns

#' @export

spread_many <- function(data,key_col,...,fill = NA,
                      convert = TRUE,drop = TRUE,sep = "_"){
  key_quo <- rlang::enquo(key_col)
  val_quos <- rlang::quos(...)
  value_cols <- unname(tidyselect::vars_select(names(data),!!!val_quos))
  key_col <- quo_name(!!key_quo)
  
  data %>%
    tidyr::gather(key = "..var..", value = "..val..", !!!val_quos) %>%
    tidyr::unite(col = ..grp.., c(key_col,"..var.."), sep = sep) %>%
    tidyr::spread(key = ..grp..,value = ..val..,fill = fill,
           convert = convert,drop = drop,sep = NULL) %>%
    tibble::as_tibble()
}


