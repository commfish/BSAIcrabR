#' Load BSAI Observer Count Pot Data
#'
#' Load BSAI observer count pot data and do data management routine
#' @param path NULL. Path to data file if not pulling directly from database.
#' @param stock NULL. Character string stock abbreviation: BSSC, WBT, EBT, BBRKC, EAG, WAG, PIGKC, SMBKC, PIBKC, PIRKC, WAIRKC.
#' @param database_pull Default = FALSE. Do updated pull from database.
#' @param clean Default = TRUE. Do stock specific data cleaning.
#' @return Measure pot data time series by fishery.
#' @examples load_pot_dump("./data.csv", stock = "BBRKC")
#'
#' @export
#'
load_pot_dump <- function(path, stock, database_pull = F, clean = T) {

  # load data
  if(database_pull == T){stop("Database pull not set up yet")}
  if(database_pull == F){
    pot <- read_csv(path)
  }

  if(!("subdistrict" %in% names(pot))){pot$subdistrict <- NA}
  if(!("eastwest" %in% names(pot))){pot$eastwest <- NA}

  pot %>%
    # fix biotwine status data
    mutate(biotwine_ok = case_when(biotwine_ok == "-" ~ NA,
                                   biotwine_ok %in% c("n", "N") ~ "N",
                                   biotwine_ok %in% c("y", "Y") ~ "Y")) %>%
    rename(sample_date = sampdate) %>%
    add_crab_year() %>%
    # reorder
    transmute(crab_year, fishery, trip, adfg, sample_date, spn, statarea, subdistrict, latitude, longitude, eastwest, depth, soaktime, gearcode, ring, mesh, biotwine_ok, female, sublegal, tot_legal, msr_pot) -> out
  if(clean == T){
    # stock specific
    if(stock == "BBRKC"){
      ## data mgmt specific to bbrkc
      out %>%
        mutate(fishery = gsub("XR|CR", "TR", fishery),
        # filter EI and QT fisheries in early 90s by stat areas e166
        fishery = ifelse(grepl("EI|QT|TT", fishery) & (statarea > 660000), paste0("QT", substring(fishery, 3, 4)), fishery),
        fishery = ifelse(grepl("EI|QT|TT", fishery) & (statarea <= 660000), paste0("TT", substring(fishery, 3, 4)), fishery)) %>%
               #fishery = paste0(substring(fishery, 1, 2), substring(crab_year, 3, 4))) %>%
        dplyr::select(-subdistrict) %>%
        # remove erroneous sample
        filter(!(crab_year == 1993 & fishery == "TR92")) -> out
    }
    if(stock == "PIRKC"){
      ## data mgmt specific to pirkc
      out %>%
        mutate(fishery = gsub("XR|CR", "TR", fishery),
               fishery = gsub("CO|EO", "QO", fishery),
               # cdq rkc and bkc fisheries to PIBKC
               fishery = gsub("CK|CP", "QP", fishery),
               # filter EI and QT fisheries in early 90s by stat areas e166
               fishery = ifelse(grepl("EI|QT|TT", fishery) & (statarea > 660000), paste0("QT", substring(fishery, 3, 4)), fishery),
               fishery = ifelse(grepl("EI|QT|TT", fishery) & (statarea <= 660000), paste0("TT", substring(fishery, 3, 4)), fishery),
              # fishery = ifelse(grepl("QO", fishery) & (statarea > 660000), paste0("QO", substring(fishery, 3, 4)), fishery),
              # fishery = ifelse(grepl("QO", fishery) & (statarea <= 660000), NA, fishery)) %>%
        ) %>%
        dplyr::select(-subdistrict)  %>%
        # remove erroneous sample
        filter(!(crab_year == 1993 & fishery == "TR92")) -> out
    }
    if(stock %in% c("BSSC", "BSTC", "WBT", "EBT")) {
      ## data mgmt specific to bssc
      out %>%
        # fix transition to rationalization yr
        mutate(#fishery = gsub("QO05r", "QO05", fishery),
               #fishery = gsub("QO05o", "QO04", fishery),
               # bbrkc test fish and cdq fisheries to TR
               fishery = gsub("CO|EO", "QO", fishery),
               # change QO pre-rationalization to QO05o
               fishery = ifelse(fishery == "QO05" & sample_date <= mdy("6/30/2006"), "QO05o", fishery),
               # cdq rkc and bkc fisheries to PIBKC
               fishery = gsub("CK|CP", "QP", fishery),
               # bbrkc test fish and cdq fisheries to TR
               fishery = gsub("XR|CR", "TR", fishery),
               # parse area for bstc fisheries
               fishery = ifelse(grepl("EI|QT|TT", fishery) & (statarea > 660000), paste0("QT", substring(fishery, 3, 4)), fishery),
               fishery = ifelse(grepl("EI|QT|TT", fishery) & (statarea <= 660000), paste0("TT", substring(fishery, 3, 4)), fishery),
               # gkc
               fishery = gsub("XE", "OB", fishery),
               fishery = ifelse(fishery == "OB08" & longitude < -174, "RB08", fishery),
               fishery = ifelse(grepl("OB", fishery), paste0("OB", substring(crab_year, 3, 4)), fishery),
               fishery = ifelse(grepl("RB", fishery), paste0("OB", substring(crab_year, 3, 4)), fishery)) %>%
        filter(!(fishery == "TR92" & year(sample_date) == 1995)) %>%
        dplyr::select(-subdistrict) -> out
    }
    if(stock %in% c("AIGKC", "EAG", "WAG")) {
      ## data mgmt specific to gkc
      out %>%
        clean_lat_lon() %>%
        mutate(subdistrict = ifelse(fishery == "OB08" & longitude < -174, "WAG", subdistrict),
               subdistrict = ifelse(substring(fishery, 1, 2) == "XE", "EAG", subdistrict),
               fishery = ifelse(fishery == "OB08" & longitude < -174, "RB08", fishery),
               fishery = gsub("XE", "OB", fishery),
               fishery = paste0(substring(fishery, 1, 2), substring(crab_year, 3, 4))) %>%
        mutate(fishery = ifelse(subdistrict == "EAG", gsub("RB", "OB", fishery), fishery),
               fishery = ifelse(subdistrict == "WAG", gsub("OB", "RB", fishery), fishery)) %>%
        # remove pots without district info
        filter(subdistrict != "-") -> out
    }
    if(stock == "EAG") {
      out %>%
        filter(subdistrict == "EAG") -> out
    }
    if(stock == "WAG") {
      out %>%
        filter(subdistrict == "WAG") -> out
    }
    if(stock == "PIGKC") {
      out %>%
        mutate(fishery = gsub("CO|EO", "QO", fishery)) %>%
        dplyr::select(-subdistrict) -> out
    }


    if(stock %in% c("SMBKC", "PIBKC", "WAIRKC")){
      stop(paste0("No method for ", stock, " yet !!"))
    }

    # add crab year
    #out <- add_crab_year(out, date_correct = T)

  }



  return(out)

}
