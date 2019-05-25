#' @title create_colname_lookup
#' @description This creates a lookup table from columns that are automatically
#' named according to the scheme apple_1, apple_2, banana_3, banana_4 to
#' apple, apple_1, banana, banana_1
#' @param df a dataframe
#' @export
#
# This creates a lookup table from columns that are automatically named
# according to the scheme apple_1, apple_2, banana_3, banana_4 to
# apple, apple_1, banana, banana_1

create_colname_lookup <- function(df) {
  lookup_table <- df %>%
    tidyr::gather(colname, value) %>%
    dplyr::distinct(colname) %>%
    dplyr::mutate(
      orig_colname = colname,
      field = stringr::str_sub(stringr::str_extract(colname, "^.+_"), 1, -2),
      number_at_end = as.numeric(stringr::str_sub(stringr::str_extract(colname, "_[0-9]+$"), 2)) - 1
    ) %>%
    dplyr::group_by(field) %>%
    dplyr::mutate(
      field_rank = dplyr::dense_rank(number_at_end),
      formatted_rank = dplyr::if_else(
        field_rank == 1, "",
        stingr::str_c("_", as.character(field_rank - 1))),
      new_colname = stingr::str_c(field, formatted_rank),
      lookup_name = dplyr::coalesce(new_colname, colname)
      ) %>%
    dplyr::ungroup() %>%
    dplyr::select(colname, lookup_name)

  return(lookup_table)
}
