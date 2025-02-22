# BSAIcrabR: A simple R workflow for accessing and summarizing ADF&G BSAI crab observer and dockside sampling data
## Installation

```{r}
devtools::install_github("commfish/BSAIcrabR")
```

## Data Access
Data access features under construction. Eventually, data access will be through the load_* series of functions. Use of these functions to download data will require connected to an ADF&G network and an OceanAK account (fish ticket data).

## Example
Below is an example workflow for summarizing fishery data necessary for the BBRKC stock assessment:

```{r}
# load

library(BSAIcrabR)

# data ----

## count pot data
pot_sum <- load_pot_dump("./bbrkc/data/RKC-1990-2023_potsum.csv", stock = "BBRKC", clean = T)

## measure pot data
obs_meas <- load_crab_dump("./bbrkc/data/RKC-1990-2023_crab_dump.csv", stock = "BBRKC", clean = T)

## dockside data
dock <- load_dockside("./bbrkc/data/RKC-1990-2023_retained_size_freq.csv", stock = "BBRKC", clean = T)

## timeseries of directed effort
dir_effort_prerat <- read_csv("./misc/data/fish_ticket_timeseries/directed_effort_timeseries_DP.csv") 

## fish ticket summary by stat area
readRDS("../adfg_crab_observer/misc/data/fish_ticket_timeseries/fish_ticket_stat_area_summary.RDS") %>%
  mutate(fishery = gsub("XR", "TR", fishery)) %>%
  filter(substring(fishery, 1, 2) == "TR") %>%
  mutate(fishery = substring(fishery, 1, 4)) %>%
  # add crab year
  add_crab_year(., date_correct = F) %>%
  # rename fields
  rename(live_n = live_number,
         deadloss_n = deadloss_number,
         live_lb = live_lbs,
         deadloss_lb = deadloss_lbs) -> ft_statarea

## incidental-directected fish ticket report
read_csv("../adfg_crab_observer/misc/data/fish_ticket_timeseries/Q_dir_inc_fish_ticket.csv", skip = 2) %>%
  janitor::clean_names() %>%
  filter(grepl("Total", seasons)) %>%
  filter(!grepl("Grand", seasons)) %>%
  mutate(year = substring(seasons, 3, 4)) %>%
  transmute(year, QT = wbt_directed_effort, QO = bss_directed_effort) %>%
  left_join(read_csv("../adfg_crab_observer/misc/data/fish_ticket_timeseries/T_dir_inc_fish_ticket.csv", skip = 2) %>%
              janitor::clean_names() %>%
              filter(grepl("Total", seasons)) %>% 
              filter(!grepl("Grand", seasons)) %>%
              mutate(year = substring(seasons, 3, 4)) %>%
              transmute(year, TT = ebt_directed_effort, TR = bbr_directed_effort) %>%
              full_join(readRDS("../adfg_crab_observer/misc/data/fish_ticket_timeseries/fish_ticket_stat_area_summary_2_21_25.RDS") %>%
                           filter(substring(fishery, 1, 2) == "XR") %>%
                           mutate(year = substring(fishery, 3, 4)) %>%
                           group_by(year) %>%
                           summarise(XR = sum(pots, na.rm = T))), by = "year") %>%
  pivot_longer(2:ncol(.), names_to = "fish", values_to = "effort") %>%
  mutate(fish = gsub("XR", "TR", fish)) %>%
  mutate(fishery = paste0(fish, year)) %>%
  group_by(fishery) %>%
  summarise(effort = sum(effort, na.rm = T)) %>%
  # join to prerationalized doug pengilly data
  bind_rows(dir_effort_prerat) %>%
  # add crab year
  add_crab_year(date_correct = F) %>%
  # other fisheries
  bind_rows(readRDS("../adfg_crab_observer/misc/data/fish_ticket_timeseries/fish_ticket_stat_area_summary_2_21_25.RDS") %>%
              filter(!(substring(fishery, 1, 2) %in% c("QO", "TT", "TR", "QT", "XR"))) %>%
              group_by(fishery, crab_year) %>%
              summarise(effort = sum(pots, na.rm = T)) %>% ungroup) %>%
  arrange(fishery) %>%
  # temporarily add CP98 (as QP)
  add_row(fishery = "QP98", effort = 89500, crab_year = 1998) %>%
  group_by(fishery, crab_year) %>%
  summarise(effort = sum(effort, na.rm = T)) %>%
  ungroup -> dir_effort


# retained catch ----

get_retained_catch(ft_data = ft_statarea) %>% 
  write_csv("./bbrkc/output/2025/retained_catch.csv")

# total catch ----

get_total_catch(pot_data = pot_sum, crab_data = obs_meas, ft_data = dir_effort, stock = "BBRKC") %>%
  write_csv("./bbrkc/output/2025/total_catch.csv")

# crab fishery discards ----

get_discards(retained_catch = get_retained_catch(ft_data = ft_statarea),
             total_catch = get_total_catch(pot_data = pot_sum, crab_data = obs_meas, ft_data = dir_effort, stock = "BBRKC"),
             stock = "BBRKC") %>%
  write_csv("./bbrkc/output/2025/discards.csv")

# retained size comp ----

get_dockside_comp(data = dock, by = NULL) %>% 
  write_csv("./bbrkc/output/2025/retained_catch_composition.csv")

# observer size comp ----

get_observer_comp(data = obs_meas, by = "sex") -> obs_comp

## directed fishery
obs_comp %>% filter(substring(fishery, 1, 2) == "TR") %>%
  write_csv("./bbrkc/output/2025/directed_total_composition.csv")

## e166 tanner crab
obs_comp %>% filter(substring(fishery, 1, 2) == "TT") %>%
  write_csv("./bbrkc/output/2025/tanner_bycatch_composition.csv")

  ```