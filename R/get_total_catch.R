#' Get Total Catch
#'
#' Estimate total catch from observer CPUE data
#' @param pot_data NULL. Observer count pot data in the same format as the output of `load_pot_dump()`
#' @param crab_data NULL. Observer measure pot data in the same format as the output of `load_crab_dump()`
#' @param ft_data NULL. Fish ticket data with fields 'crab_year', 'fishery', 'effort' and whatever variables are specified in by.
#' @param stock NULL. Character string stock abbreviation: BSSC, WBT, EBT, BBRKC, EAG, WAG, PIGKC, SMBKC, PIBKC, PIRKC, WAIRKC.
#' @param by NULL. Vector of column names other than crab year, fishery, and group to use as a grouping variable.
#' @param units NULL. Unit of measurement, "t" or "lb". Default = "t".
#' @return Data frame including crab year, fishery, grouping variables, and average weight. Average weights used for expansion to weight are only based on crab year, fishery, and sex. No other 'by' variables are included here, so that the sum of total catch by variables (i.e., sex and shell condition) will equal the total catch by sex.
#' @examples get_avg_wt(pot_data, crab_data, ft_data, stock = "BBRKC")
#'
#' @export
#'
get_total_catch <- function(pot_data, crab_data, ft_data, stock, by = NULL, units = "t") {

  if(!(units %in% c("t", "lb"))){stop("Do not recognize the units, use t or lb")}

  by_in <- by
  # make sure fishery and crab_year are not in by
  by <- by[!(by %in% c("fishery", "crab_year", "group", "sex"))]
  # 'by' for pot data can only include columns %in% count data
  by_cp <- by[by %in% names(pot_data)]

  if(length(by) == 0){by = NULL}
  if(length(by_cp) == 0){by_cp = NULL}
  if(!is.null(by_cp)){
    if(!(by_cp %in% names(ft_data))){stop(paste0("One or more of ", by_cp, " not in fish ticket data !!"))}
  }
  # clean up by data in crab data and save it as a separate object
  crab_data_tc <- crab_data
  if("shell" %in% by & lump == T){
    crab_data_tc %>%
      filter(!is.na(shell),
             shell != -9) %>%
      mutate(shell = case_when(shell %in% c(0:2, 9) ~ "new",
                               shell %in% c(3:5) ~ "old")) -> crab_data_tc
  }
  if("shell" %in% by & lump == F){
    crab_data_tc %>%
      filter(!is.na(shell),
             shell != -9)  -> crab_data_tc
  }
  if("legal" %in% by){
    crab_data_tc %>%
      filter(!is.na(legal),
             legal != -9)  -> crab_data_tc
  }
  if("sex" %in% by_in){
    crab_data_tc %>%
      filter(sex %in% 1:2)  -> crab_data_tc
  }

  # get average wt
  crab_data %>%
    # remove known bad biotwine records
    filter((biotwine_ok %in% c("Y", "-") | is.na(biotwine_ok))) %>%
    # add average weight
    add_calc_wt(., stock = stock, units = units) %>%
    get_avg_wt(., stock = stock, by = c("group"), units = units) %>%
    mutate(sex = case_when(group == "female" ~ 2,
                           group == "sublegal_male"~ 1,
                           group == "legal_male"~ 1)) %>%
    dplyr::select(crab_year, fishery, group, sex, avg_wt) -> avg_wt

  # get average weight of all years by fishery
  avg_wt %>%
    dplyr::select(-crab_year) %>%
    mutate(fishery = substring(fishery, 1, 2)) %>%
    rename(fish_type = fishery) %>%
    group_by_at(1:(ncol(.) - 1)) %>%
    summarise(avg_wt_mean = mean(avg_wt), .groups = "drop") -> avg_wt_fish_type

  # get percent of crab measured in by variables
  crab_data_tc %>%
    group_by_at(c("crab_year", "fishery", "group", by)) %>%
    count() %>%
    group_by_at(c("crab_year", "fishery", "group", by)) %>%
    group_by(crab_year, fishery, group) %>%
    mutate(tot = sum(n)) %>% ungroup %>%
    mutate(prop = n / tot) %>%
    dplyr::select("crab_year", "fishery", "group", all_of(by), "prop") %>%
    # expand out to all crab year fishery and group combos
    right_join(expand_grid(distinct(., crab_year, fishery),
                          group = c("female", "sublegal_male", "legal_male")),
               by = join_by(crab_year, fishery, group)) %>%
    replace_na(list(prop = 0)) %>%
    # add sex
    mutate(sex = case_when(group == "female" ~ 2,
                           group == "sublegal_male"~ 1,
                           group == "legal_male"~ 1)) %>%
    dplyr::select(crab_year, fishery, group, sex, all_of(by), prop) -> prop_by

  # get observer cpue
  pot_data %>%
    # remove known bad biotwine records
    filter((biotwine_ok %in% c("Y", "-") | is.na(biotwine_ok))) %>%
    group_by_at(c("crab_year", "fishery", by_cp)) %>%
    summarise(female = sum(female, na.rm = T),
              sublegal_male = sum(sublegal, na.rm = T),
              legal_male = sum(tot_legal, na.rm = T),
              obs_effort = n(), .groups = "drop") %>%
    # pivot to long format
    pivot_longer(c(female, sublegal_male, legal_male), names_to = "group", values_to = "count") %>%
    replace_na(list(female = 0, sublegal_male = 0, legal_male = 0)) %>%
    # add sex
    mutate(sex = case_when(group == "female" ~ 2,
                           group == "sublegal_male"~ 1,
                           group == "legal_male"~ 1)) -> obs_count

  # summarise fish ticket data
  ft_data %>%
    group_by_at(c("crab_year", "fishery", by_cp)) %>%
    summarise(ft_effort = sum(effort, na.rm = T), .groups = "drop") -> ft_data

  # join to effort and avg wt
  obs_count %>%
    left_join(ft_data, by = c("crab_year", "fishery", by_cp)) %>%
    full_join(prop_by, by = c("crab_year", "fishery", "group", "sex", by_cp)) %>%
    left_join(avg_wt, by = c("crab_year", "fishery", "group", "sex")) %>%
    # fill in average weight when missing if available from another year
    mutate(fish_type = substring(fishery, 1, 2)) %>%
    left_join(avg_wt_fish_type, by = c("fish_type", "group", "sex")) %>%
    mutate(avg_wt = ifelse(is.na(avg_wt), avg_wt_mean, avg_wt)) %>%
    dplyr::select(-fish_type, -avg_wt_mean) %>%
    # fill in prop if by is NULL
    {if(is.null(by)) {replace_na(., list(prop = 1))} else{.}} %>%
    # scale count
    # add observer cpue
    mutate(count = count * prop,
           cpue = count / obs_effort) %>%
    # scale to total catch
    mutate(total_catch_n = ifelse(count != 0, cpue * ft_effort, 0),
           total_catch_wt = ifelse(count != 0, cpue * ft_effort * avg_wt, 0))  %>%

    dplyr::select(crab_year, fishery, group, all_of(by_in), count, obs_effort, ft_effort, cpue, avg_wt, total_catch_n, total_catch_wt) -> out

  if(("sex" %in% by_in) & !("group" %in% by_in)) {

    out %>%
      group_by_at(c("crab_year", "fishery", "sex", by)) %>%
      summarise(count_s = sum(count),
                obs_effort = mean(obs_effort),
                ft_effort = mean(ft_effort),
                cpue = count_s / obs_effort,
                avg_wt = weighted.mean(avg_wt, ifelse(count == 0, 1, count)),
                total_catch_n = cpue * ft_effort,
                total_catch_wt = cpue * ft_effort * avg_wt, .groups = "drop") %>%
      rename(count = count_s) -> out

  }

  return(out)

}





