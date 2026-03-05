# Get Retained Catch

Get retained catch from fish ticket data

## Usage

``` r
get_retained_catch(ft_data, by = NULL, stock = NULL, units = "t")
```

## Arguments

- ft_data:

  NULL. Fish ticket data with fields 'crab_year', 'fishery', 'effort'
  and whatever variables are specified in by.

- by:

  NULL. Vector of column names other than crab year, fishery to use as a
  grouping variable. Must by present in ft_data.

- units:

  NULL. Unit of measurement, "t" or "lb". Default = "t".

## Value

Data frame including crab year, fishery, grouping variables, and
retained catch.

## Examples

``` r
get_retained_catch(ft_data, by = NULL)
#> Error in if (is.null(stock) | !(stock %in% c("AIGKC", "WAG", "EAG"))) {    out <- ft_data %>% group_by_at(c("crab_year", "fishery",         by)) %>% summarise(dir_retained_n = sum(dir_live_n, dir_deadloss_n,         na.rm = T), dir_retained_wt = sum(dir_live_lb, dir_deadloss_lb,         na.rm = T) * unit_convert, inc_retained_n = sum(inc_live_n,         inc_deadloss_n, na.rm = T), inc_retained_wt = sum(inc_live_lb,         inc_deadloss_lb, na.rm = T) * unit_convert, tot_retained_n = sum(tot_live_n,         tot_deadloss_n, na.rm = T), tot_retained_wt = sum(tot_live_lb,         tot_deadloss_lb, na.rm = T) * unit_convert) %>% ungroup %>%         mutate(dir_retained_n = ifelse(crab_year < 2005, NA,             dir_retained_n), dir_retained_wt = ifelse(crab_year <             2005, NA, dir_retained_wt), inc_retained_n = ifelse(crab_year <             2005, NA, inc_retained_n), inc_retained_wt = ifelse(crab_year <             2005, NA, inc_retained_wt), tot_retained_n = ifelse(crab_year >             2005, dir_retained_n + inc_retained_n, tot_retained_n),             tot_retained_wt = ifelse(crab_year > 2005, dir_retained_wt +                 inc_retained_wt, tot_retained_wt)) %>% filter(!is.na(fishery))}: argument is of length zero
```
