#' Get Total Catch
#'
#' Estimate total catch from observer CPUE data
#' @param pot_data NULL. Observer count pot data in the same format as the output of `load_pot_dump()`
#' @param crab_data NULL. Observer measure pot data in the same format as the output of `load_crab_dump()`
#' @param ft_data NULL. Fish ticket data with fields 'crab_year', 'fishery', 'effort' and whatever variables are specified in by.
#' @param stock NULL. Character string stock abbreviation: BSSC, WBT, EBT, BBRKC, EAG, WAG, PIGKC, SMBKC, PIBKC, PIRKC, WAIRKC.
#' @param by NULL. Vector of column names other than crab year, fishery, and group to use as a grouping variable. Must by present in count pot data.
#' @param units NULL. Unit of measurement, "t" or "lb". Default = "t".
#' @return Data frame including crab year, fishery, grouping variables, and average weight.
#' @examples get_avg_wt(pot_data, crab_data, ft_data, stock = "BBRKC")
#'
#' @export
#'
get_total_catch <- function(pot_data, crab_data, ft_data, stock, by = NULL, units = "t") {

  if(!(units %in% c("t", "lb"))){stop("Do not recognize the units, use t or lb")}

  # make sure fishery and crab_year are not in by
  by = by[!(by %in% c("fishery", "crab_year", "group"))]
  if(length(by) == 0){by = NULL}

  # get average wt
  crab_data %>%
    # remove known bad biotwine records
    filter((biotwine_ok %in% c("Y", "-") | is.na(biotwine_ok))) %>%
    # add average weight
    add_calc_wt(., stock = stock, units = units) %>%
    get_avg_wt(., stock = stock, by = c("group", by), units = units) -> avg_wt

  # get average weight of all years by fishery
  avg_wt %>%
    dplyr::select(-crab_year) %>%
    mutate(fishery = substring(fishery, 1, 2)) %>%
    rename(fish_type = fishery) %>%
    group_by_at(1:(ncol(.) - 1)) %>%
    summarise(avg_wt_mean = mean(avg_wt)) %>%
    ungroup -> avg_wt_fish_type

  # get observer cpue
  pot_data %>%
    # remove known bad biotwine records
    filter((biotwine_ok %in% c("Y", "-") | is.na(biotwine_ok))) %>%
    group_by_at(c("crab_year", "fishery", by)) %>%
    summarise(female = sum(female, na.rm = T),
              sublegal_male = sum(sublegal, na.rm = T),
              legal_male = sum(tot_legal, na.rm = T),
              obs_effort = n()) %>% ungroup%>%
    # pivot to long format
    pivot_longer(c(female, sublegal_male, legal_male), names_to = "group", values_to = "count") %>%
    # cpue
    mutate(cpue = count / obs_effort) -> obs_cpue

  # summarise fish ticket data
  ft_data %>%
    group_by_at(c("crab_year", "fishery", by)) %>%
    summarise(effort = sum(effort, na.rm = T)) %>% ungroup -> ft_data

  # get total catch
  if(!is.null(by)) {
    if(!(by %in% names(ft_data))){stop(paste0(by, " not present in fish ticket data"))}
  }
  # join to effort and avg wt
  obs_cpue %>%
    left_join(ft_data, by = c("crab_year", "fishery", by)) %>%
    left_join(avg_wt, by = c("crab_year", "fishery", "group", by)) %>%
    # fill in average weight when missing if available from another year
    mutate(fish_type = substring(fishery, 1, 2)) %>%
    left_join(avg_wt_fish_type, by = c("fish_type", "group", by)) %>%
    mutate(avg_wt = ifelse(is.na(avg_wt), avg_wt_mean, avg_wt)) %>%
    dplyr::select(-fish_type, -avg_wt_mean) %>%
    # scale to total catch
    mutate(total_catch_n = cpue * effort,
           total_catch_wt = cpue * effort * avg_wt)  %>%
    dplyr::select(crab_year, fishery, by, group, count, obs_effort, effort, cpue, avg_wt, total_catch_n, total_catch_wt) -> out

  return(out)

}





