# BSAIcrabR: A simple R workflow for accessing and summarizing ADF\&G BSAI crab observer and dockside sampling data

## Installation
```
devtools::install_github("commfish/BSAIcrabR")
```

## Data Access
Data access features under construction. Eventually, data access will be through the load_* series of functions. Use of these functions to download data will require connected to an ADF\&G network and an OceanAK account (fish ticket data).

## Example
Below is an example workflow for summarizing fishery data necessary for the BBRKC stock assessment:

```
# load

library(BSAIcrabR)

# data 
## count pot data
pot_sum <- load_pot_dump("./bbrkc/data/RKC-1990-2023_potsum.csv", stock = "BBRKC", clean = T)

## measure pot data
obs_meas <- load_crab_dump("./bbrkc/data/RKC-1990-2023_crab_dump.csv", stock = "BBRKC", clean = T)

## dockside data
dock <- load_dockside("./bbrkc/data/RKC-1990-2023_retained_size_freq.csv", stock = "BBRKC", clean = T)

## timerseries of directed effort
dir_effort <- read_csv("./bbrkc/data/directed_effort_timeseries_DP.csv") %>% add_crab_year(., date_correct = F)

## fish ticket summary by stat area
readRDS("./misc/data/fish_ticket_stat_area_summary.RDS") %>%
  # combine bbrkc tf and directed fishery
  mutate(fishery = gsub("XR|CR", "TR", fishery)) %>%
  mutate(fishery = ifelse(fishery == "TR93 (2)", "TR93", fishery)) %>%
  filter(substring(fishery, 1, 2) == "TR") %>%
  # add crab year
  add_crab_year(., date_correct = F) %>%
  # rename fields
  rename(live_n = live_number,
         deadloss_n = deadloss_number,
         live_lb = live_lbs,
         deadloss_lb = deadloss_lbs) -> ft

# retained catch 

get_retained_catch(ft_data = ft) %>%
  write_csv("./bbrkc/output/2024/retained_catch.csv")

# total catch

get_total_catch(pot_data = pot_sum, crab_data = obs_meas, ft_data = dir_effort, stock = "BBRKC") %>%
  write_csv("./bbrkc/output/2024/total_catch.csv")

# retained size comp 

get_dockside_comp(data = dock, by = NULL) %>%
  write_csv("./bbrkc/output/2024/retained_catch_composition.csv")

# observer size comp 

get_observer_comp(data = obs_meas, by = "sex") -> obs_comp

## directed fishery
obs_comp %>% filter(substring(fishery, 1, 2) == "TR") %>%
  write_csv("./bbrkc/output/2024/directed_total_composition.csv")

## e166 tanner crab
obs_comp %>% filter(substring(fishery, 1, 2) == "TT") %>%
  write_csv("./bbrkc/output/2024/tanner_bycatch_composition.csv")
```
