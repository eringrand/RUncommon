#' @title create_colname_lookup
#' @description This creates a lookup table from columns that are automatically 
#' named according to the scheme apple_1, apple_2, banana_3, banana_4 to
#' apple, apple_1, banana, banana_1
#' @params df a dataframe
#' @export
# This creates a lookup table from columns that are automatically named
# according to the scheme apple_1, apple_2, banana_3, banana_4 to 
# apple, apple_1, banana, banana_1

create_colname_lookup <- function(df) {

 lookup_table <- df %>%
   gather(colname, value) %>%
   distinct(colname) %>%
   mutate(orig_colname = colname,
          field = str_sub(str_extract(colname, "^.+_"), 1, -2),
          number_at_end = as.numeric(str_sub(str_extract(colname, "_[0-9]+$"), 2)) - 1) %>%
   group_by(field) %>%
   mutate(field_rank = dense_rank(number_at_end),
          formatted_rank = if_else(field_rank == 1, "",
              str_c("_", as.character(field_rank - 1))),
          new_colname = str_c(field, formatted_rank),
          lookup_name = coalesce(new_colname, colname)) %>%
   ungroup() %>%
   select(colname, lookup_name)

 return(lookup_table)

}