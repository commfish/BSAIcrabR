#' Get Average Weight
#'
#' Get average weight of crab by grouping variable using weight at length parameters
#' @param data NULL. Crab sample data in the format as the output of `load_crab_dump()`
#' @param stock NULL. Character string stock abbreviation: BSSC, WBT, EBT, BBRKC, EAG, WAG, PIGKC, SMBKC, PIBKC, PIRKC, WAIRKC.
#' @param by NULL. Vector of column names other than crab year, fishery, and size to use as a grouping variable.
#' @param lump T/F Lump shell condition into new and old (TRUE) or not (FALSE). Default = TRUE.
#' @param units NULL. Unit of measurement, "t" or "lb". Default = "t".
#' @return Data frame including crab year, fishery, grouping variables, and average weight.
#' @examples get_avg_wt(data, by = "group")
#'
#' @export
#'
get_avg_wt <- function(data, stock, by, lump = T, units = "t") {

  # make sure fishery and crab_year are not in by
  by = by[!(by %in% c("fishery", "crab_year"))]

  data %>%
    # remove known bad biotwine records
    filter((biotwine_ok %in% c("Y", "-") | is.na(biotwine_ok))) %>%
    # add calc weight
    add_calc_wt(., stock = stock, units = units) -> data

  if("shell" %in% by & lump == T){
    data %>%
      filter(!is.na(shell),
             shell != -9) %>%
      mutate(shell = case_when(shell %in% c(0:2, 9) ~ "new",
                               shell %in% c(3:5) ~ "old")) -> data
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
  if("sex" %in% by){
    data %>%
      filter(sex %in% 1:2)  -> data
  }
  if("group" %in% by){
    data %>%
      filter(group %in% c("female", "legal_male", "sublegal_male"))  -> data
  }

  data[, c("crab_year", "fishery", by, "calc_wt")] %>%
    group_by_at(1:(ncol(.) - 1)) %>%
    summarise(avg_wt = mean(calc_wt, na.rm = T)) %>% ungroup -> out

  return(out)

}
