#' Add Permit Holder to Observer  Pot Data
#'
#' Use fish ticket dump and CFEC permit holder information to join permit holder as proxy for captain to AIGKC data
#' @param ft_dump NULL. AIGKC fish ticket dump.
#' @param pot_data NULL. Observer count pot data in the same format as the output of `load_pot_dump()`
#' @param permits_path NULL. File path to CFEC permit data directory
#' @return Pot data with additional column 'permit_holder'.
#' @examples load_pot_dump("./data.csv", stock = "BBRKC")
#'
#' @export
#'
add_permit_holder <- function(ft_dump, obs, permit_path = "aigkc/data/permits") {

  # permit data ----

  purrr::map(list.files(permit_path, full.names = T), function(x){
    read_csv(x) %>%
      janitor::clean_names() %>%
      # get needed info and create permit number on fish ticket dump
      transmute(year, fishery, adfg = as.numeric(vessel_adfg), last_name, first_name, middle_initial, permit_number,
                cfec_permit = paste0(fishery, permit_number)) %>%
      filter(grepl("K91", fishery)) %>%
      distinct
  }) %>%
    do.call("bind_rows",. ) -> permits

  # prep fish ticket data ----

  ft_dump %>%
    transmute(season = case_when(!is.na(season) ~ season,
                                 (is.na(season) & dol_year < 2000 & month_landed < 9) ~ paste0(dol_year-1, "/", substring(dol_year, 3, 4)),
                                 (is.na(season) & dol_year < 2000 & month_landed >= 9) ~ paste0(dol_year, "/", substring(dol_year+1, 3, 4)),
                                 (is.na(season) & dol_year >= 2000 & month_landed < 7) ~ paste0(dol_year-1, "/", substring(dol_year, 3, 4)),
                                 (is.na(season) & dol_year >= 2000 & month_landed >= 7) ~ paste0(dol_year, "/", substring(dol_year+1, 3, 4))),
              start = as_date(date_fishing_began),
              end = as_date(date_of_landing),
              year = year(end),
              adfg_number,
              cfec_permit_holder_name,
              cfec_permit) %>%
    # join to permit holder information to fill in gaps by year and permit number (should be 51 that do not match)
    left_join(permits, by = c("year", "cfec_permit")) %>%
    # select columns
    transmute(season,
              start, end, adfg_number,
              last = last_name,
              first = first_name,
              mi = middle_initial) %>%
    distinct %>%
    filter(!grepl("ADFG", last)) %>%
    nest_by(season, adfg_number, .keep = T) %>% ungroup %>%
    # make trip dates continuous from begining to end of season
    mutate(out = purrr::map(data, function(data) {
      data = arrange(data, end)
      if(nrow(data) > 1) {
        for(i in 2:nrow(data)){
          data$start[1] <- ymd(paste0(substring(data$season[1], 1, 4), "/7/1"))
          if(as.numeric(substring(data$season[1], 1, 4)) < 2000) {data$end[nrow(data)] <- ymd(paste0(as.numeric(substring(data$season[1], 1, 4))+1, "/8/31"))}
          if(as.numeric(substring(data$season[1], 1, 4)) >= 2000) {data$end[nrow(data)] <- ymd(paste0(as.numeric(substring(data$season[1], 1, 4))+1, "/6/30"))}
          data$start[i] <- data$end[i-1] + 1
        }
      }

      return(data)
    })) %>%
    transmute(out) %>% unnest(out) %>%
    transmute(crab_year = as.numeric(substring(season, 1, 4)),
              trip = interval(start, end),
              adfg_number,
              permit_holder = paste(last, first, mi, sep = "_")) -> ft_trips

  # match to observer data ----
  pot_data %>%
    # align sample date with fish ticket permit holder
    mutate(permit_holder = purrr::pmap_chr(list(sample_date, crab_year, adfg), function(sample_date, yr, adfg) {

             ph_tmp <- ft_trips$permit_holder[ft_trips$crab_year == yr & ft_trips$adfg_number == adfg & sample_date %within% ft_trips$trip]
             if(length(unique(ph_tmp)) == 0) {return(NA)}
             else{return(unique(ph_tmp))}

           })) -> out

  return(out)


}
