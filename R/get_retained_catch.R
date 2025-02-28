#' Get Retained Catch
#'
#' Get retained catch from fish ticket data
#' @param ft_data NULL. Fish ticket data with fields 'crab_year', 'fishery', 'effort' and whatever variables are specified in by.
#' @param by NULL. Vector of column names other than crab year, fishery to use as a grouping variable. Must by present in ft_data.
#' @param units NULL. Unit of measurement, "t" or "lb". Default = "t".
#' @return Data frame including crab year, fishery, grouping variables, and retained catch.
#' @examples get_retained_catch(ft_data, by = NULL)
#'
#' @export
#'
get_retained_catch <- function(ft_data, by = NULL, units = "t") {

  # make sure fishery and crab_year are not in by ----
  by = by[!(by %in% c("fishery", "crab_year"))]
  if(length(by) == 0){by = NULL}
  if(!is.null(by)) {
    if(!(by %in% names(ft_data))){stop(paste0(by, " not present in fish ticket data"))}
  }

  # unit conversion parameter (fish ticket data always in lb) ----
  if(units == "lb"){unit_convert = 1}
  if(units == "t"){unit_convert = 0.000453592}
  if(!(units %in% c("t", "lb"))){stop("Do not recognize the units, use t or lb")}

  # get retained catch ----

  ft_data %>%
    replace_na(list(tot_live_n = 0, tot_live_lb = 0, tot_deadloss_n = 0, tot_deadloss_lb = 0)) %>%
    group_by_at(c("crab_year", "fishery", by)) %>%
    summarise(tot_retained_n = sum(tot_live_n + tot_deadloss_n),
              tot_retained_wt = sum(tot_live_lb + tot_deadloss_lb) * unit_convert,
              dir_retained_n = sum(dir_live_n),
              dir_retained_wt = sum(dir_live_lb) * unit_convert,
              inc_retained_n = sum(inc_live_n),
              inc_retained_wt = sum(inc_live_lb) * unit_convert) %>% ungroup %>%
    filter(!is.na(fishery)) -> out

  return(out)

}





