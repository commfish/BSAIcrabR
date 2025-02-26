#' Load BSAI Observer Measure Pot Data
#'
#' Load BSAI observer measure pot data and do data management routine
#' @param path NULL. Path to data file if not pulling directly from database.
#' @param stock NULL. Character string stock abbreviation: BSSC, WBT, EBT, BBRKC, EAG, WAG, PIGKC, SMBKC, PIBKC, PIRKC, WAIRKC.
#' @param database_pull Default = FALSE. Do updated pull from database.
#' @param clean Default = TRUE. Do stock specific data cleaning.
#' @return Measure pot data time series by fishery.
#' @examples load_crab_dump("./data.csv", stock = "BBRKC")
#'
#' @export
#'
load_crab_dump <- function(path, stock, database_pull = F, clean = T) {

  # load data
  if(database_pull == T){stop("Database pull not set up yet")}
  if(database_pull == F){
    obs <- read_csv(path)
  }

  obs %>%
    rename(sample_date = sampdate) %>%
    add_crab_year() %>%
    mutate(subdistrict = ifelse(!("subdistrict" %in% names(.)), NA, subdistrict),
           eastwest = ifelse(!("eastwest" %in% names(.)), NA, eastwest),
           maturity = ifelse(!("maturity" %in% names(.)), NA, maturity),
           gearcode = ifelse(!("gearcode" %in% names(.)), NA, gearcode)) %>%
    # reorder
    transmute(crab_year, fishery, trip, adfg, sample_date, spn, statarea, subdistrict, latitude, longitude,
              eastwest, depth, soaktime, gearcode, ring, mesh, biotwine_ok, spcode, sex, size, legal, shell, clutch, eggdev,
              clutchcon, maturity, parasite) -> out
  if(clean == T){
    # stock specific
    if(stock == "BBRKC"){
      ## fishery codes for early 90s tanner e166 fisheries
      early_90s_tt <- c("EI91", "EI92", paste0("QT", 93:96))
      ## data mgmt specific to bbrkc
      out %>%
        mutate(fishery = gsub("XR|CR", "TR", fishery)) %>%
        # filter EI and QT fisheries in early 90s by stat areas e166
        filter(!(fishery %in% early_90s_tt & (statarea > 660000 | statarea < 0))) %>%
        # combine all tanner e166 fishery codes
        mutate(fishery = ifelse(fishery %in% early_90s_tt, gsub("EI|QT", "TT", fishery), fishery)) %>%
        # fill in legal
        add_legal(., stock = stock) %>%
        # add regulatory group
        mutate(group = case_when(sex == 2 ~ "female",
                                 sex == 1 & legal == 0 ~ "sublegal_male",
                                 sex == 1 & legal == 1 ~ "legal_male"))  %>%
        dplyr::select(-subdistrict)  %>%
        # remove erroneous sample
        filter(!(crab_year == 1993 & fishery == "TR92")) -> out
    }
    if(stock %in% c("BSSC", "BSTC", "WBT", "EBT")) {
      ## fishery codes for early 90s tanner e166 fisheries
      early_90s_tt <- c("EI91", "EI92", paste0("QT", 93:96))
      ## data mgmt specific to bssc
      out %>%
        # fix transition to rationalization yr
        mutate(#fishery = gsub("QO05r", "QO05", fishery),
               #fishery = gsub("QO05o", "QO04", fishery),
               fishery = gsub("CO|EO", "QO", fishery),
               # change QO pre-rationalization to QO05o
               fishery = ifelse(fishery == "QO05" & sample_date <= mdy("6/30/2006"), "QO05o", fishery),
               # cdq rkc and bkc fisheries to PIBKC
               fishery = gsub("CK|CP", "QP", fishery),
               # bbrkc test fish and cdq fisheries to TR
               fishery = gsub("XR|CR", "TR", fishery),
               fishery = ifelse((fishery %in% early_90s_tt) & (statarea > 660000 | statarea < 0), paste0("QT", substring(fishery, 3, 4)), fishery),
               fishery = ifelse((fishery %in% early_90s_tt) & (statarea <= 660000 | statarea >= 0), paste0("TT", substring(fishery, 3, 4)), fishery),
               # gkc
               fishery = gsub("XE", "OB", fishery),
               fishery = ifelse(fishery == "OB08" & longitude < -174, "RB08", fishery),
               fishery = ifelse(grepl("OB", fishery), paste0("OB", substring(crab_year, 3, 4)), fishery),
               fishery = ifelse(grepl("RB", fishery), paste0("OB", substring(crab_year, 3, 4)), fishery)) %>%
        filter(!(fishery == "TR92" & year(sample_date) == 1995)) %>%
        # fill in legal
        add_legal(., stock = stock) %>%
        # add regulatory group
        mutate(group = case_when(sex == 2 ~ "female",
                                 sex == 1 & legal == 0 ~ "sublegal_male",
                                 sex == 1 & legal == 1 ~ "legal_male")) %>%
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
        mutate(fishery = ifelse(subdistrict == "EAG", gsub("RB", "OB", fishery),
                                ifelse(subdistrict == "WAG", gsub("OB", "RB", fishery), fishery))) %>%
        # fill in legal
        add_legal(., stock = stock) %>%
        # add regulatory group
        mutate(group = case_when(sex == 2 ~ "female",
                                 sex == 1 & legal == 0 ~ "sublegal_male",
                                 sex == 1 & legal == 1 ~ "legal_male")) -> out
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


    if(stock %in% c("SMBKC", "PIBKC", "PIRKC", "WAIRKC")){
      stop(paste0("No method for ", stock, " yet !!"))
    }

    # add crab year
    #out <- add_crab_year(out, date_correct = T)

  }



  return(out)

}
