#' spread_many
#'
#' Spread more than one column out at once
#'
#' @param df Data Frame
#' @param key Key column used in spread
#' @param ... List of column, unquoted to spread out
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

spread_many <- function(data,key_col,...,fill = NA,
                      convert = TRUE,drop = TRUE,sep = "_"){
  key_quo <- rlang::enquo(key_col)
  val_quos <- rlang::quos(...)
  value_cols <- unname(tidyselect::vars_select(names(data),!!!val_quos))
  key_col <- quo_name(!!key_quo)
  
  data %>%
    gather(key = "..var..", value = "..val..", !!!val_quos) %>%
    unite(col = ..grp.., c(key_col,"..var.."), sep = sep) %>%
    spread(key = ..grp..,value = ..val..,fill = fill,
           convert = convert,drop = drop,sep = NULL) %>%
    tibble::as_tibble()
}

# spread_many <- function(df, key, ..., fill = NA,
#                         convert = TRUE,drop = TRUE,sep = "_") {
#   # Define the variables that we want to spread out
#   value_vars <- rlang::quos(...)
#   # Define the key variable for the spread, will define how many seperates there are
#   key <- rlang::enquo(key)
#   
#   # Orginal data types of columns  
#   dat_type <- dplyr::select(df, !!! value_vars) %>%
#     purrr::map(class) %>%
#     tibble::as_tibble() %>%
#     tidyr::gather(colname, dat_type) %>%
#     dplyr::mutate(funs = paste0("as.", dat_type))
#   
#   # Unite the variables we want to spread together, them spread the united vars  
#   newdf <- df %>%
#     tidyr::unite(temp, !!! value_vars) %>%
#     tidyr::spread(!! key, temp, fill = fill,
#                   convert = convert, drop = drop, sep = NULL) 
#   
#   newdf
  
  # # Collect the additional names to the new DF 
  # # E.g. What groups were in the "key" column?
  # new_names <- setdiff(names(newdf), names(df))
  # 
  # # Convert the column names into strings for manipulation
  # # Create the new column names as combinations of the old and key groups
  # string_cols <- sapply(value_vars, rlang::quo_text)
  # new_col_names <- purrr::map(new_names, ~paste(string_cols, .x, sep = "_"))
  # 
  # dat_type <- new_names %>% 
  #   purrr::map_dfr(~dat_type %>%
  #             dplyr::mutate(new_name = paste(colname, .x, sep = "_"),
  #                    key_name = .x)) %>%
  #   dplyr::select(key_name, new_name, dat_type, funs) %>%
  #   split(.$key_name) 
  # 
  # 
  # final_df <- purrr::map_dfc(dat_type, function(x) {
  #   key <- unique(x$key_name)
  #   new_col_names <- x$new_name
  #   funs <- x$dat_type
  #   
  #   newdf %>%
  #     tidyr::separate(key, into = new_col_names, sep = "_")  %>%
  #     dplyr::select(new_col_names) }
  #   
  # ) %>%
  #   dplyr::bind_cols(newdf, .) %>%
  #   dplyr::select(-new_names) 
  # 
  # 
  # dat_type <- purrr::map_dfr(dat_type, bind_rows)
  # # Characters
  # chr <- dplyr::filter(dat_type, dat_type == "character") %>%
  #   dplyr::pull(new_name)
  # 
  # # Numeric
  # num <- dplyr::filter(dat_type, dat_type == "numeric")  %>%
  #   dplyr::pull(new_name)
  # 
  # # Interger
  # int <- dplyr::filter(dat_type, dat_type == "integer") %>%
  #   dplyr::pull(new_name)
  # 
  # 
  # final_df %>%
  #   tibble::as_tibble() %>%
  #   dplyr::mutate_at(vars(chr), as.character) %>%
  #   dplyr::mutate_at(vars(int), as.integer) %>%
  #   dplyr::mutate_at(vars(num), as.numeric)
  
# }
