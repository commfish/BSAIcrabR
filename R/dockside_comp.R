#' Get retained size composition
#'
#' Compute retained size composition from dockside data
#' @param data NULL. Dockside sample data in the format as the output of `load_dockside()`
#' @param by NULL. Vector of column names other than crab year, fishery, and size to use as a grouping variable.
#' @param lump T/F lump shell condition into new and old (TRUE) or not (FALSE). Default = TRUE.
#' @return Size frequency by year, fishery, and size
#' @examples dockside_comp(data, by = "shell", lump = T)
#'
#' @export
#'
dockside_comp <- function(data, by, lump = T) {

  if("shell" %in% by & lump == T){
    data %>%
      filter(!is.na(shell),
             shell != -9) %>%
      mutate(shell = case_when(shell %in% c(0:2, 9) ~ 2,
                               shell %in% c(3:5) ~ 3)) -> data
  }
  if("shell" %in% by & lump == F){
    data %>%
      filter(!is.na(shell),
             shell != -9)  -> data
  }
  if("legal" %in% by){
    data %>%
      filter(!is.na(legal),
             legal != -9)  -> data
  }
  if("adfg" %in% by){
    data %>%
      filter(!is.na(adfg),
             adfg != -9)  -> data
  }
  if("spcode" %in% by){
    data %>%
      filter(!is.na(spcode),
             spcode != -9)  -> data
  }
  if("sample_date" %in% by){
    data %>%
      filter(!is.na(sample_date),
             sample_date != -9)  -> data
  }

  data[, c("crab_year", "fishery", "size", by, "numcrab")] %>%
    group_by_at(1:(ncol(.)-1)) %>%
    summarise(total = sum(numcrab)) %>% ungroup

}
