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
    {if(!("subdistrict" %in% names(.))){mutate(subdistrict = NA) %>% .} else{.}} %>%
    {if(!("eastwest" %in% names(.))){mutate(eastwest = NA) %>% .} else{.}} %>%
    {if(!("maturity" %in% names(.))){mutate(maturity = NA) %>% .} else{.}} %>%
    {if(!("gearcode" %in% names(.))){mutate(gearcode = NA) %>% .} else{.}} %>%
    # reorder
    transmute(fishery, trip, adfg, sample_date, spn, statarea, subdistrict, latitude, longitude,
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
        mutate(fishery = ifelse(fishery %in% early_90s_tt, gsub("EI|QT", "TT", fishery), fishery),
               fishery = paste0(substring(fishery, 1, 2), substring(crab_year, 3, 4))) %>%
        # fill in legal
        add_legal(., stock = stock) %>%
        # add regulatory group
        mutate(group = case_when(sex == 2 ~ "female",
                                 sex == 1 & legal == 0 ~ "sublegal_male",
                                 sex == 1 & legal == 1 ~ "legal_male"))  %>%
        dyply::select(-subdistrict) -> out
    }
    if(stock %in% c("BSSC", "BSTC", "WBT", "EBT")) {
      ## fishery codes for early 90s tanner e166 fisheries
      early_90s_tt <- c("EI91", "EI92", paste0("QT", 93:96))
      ## data mgmt specific to bssc
      out %>%
        # fix transition to rationalization yr
        mutate(fishery = gsub("QO05r", "QO05", fishery),
               fishery = gsub("QO05o", "QO04", fishery),
               fishery = gsub("CO|EO", "QO", fishery),
               # cdq rkc and bkc fisheries to PIBKC
               fishery = gsub("CK", "QP", fishery),
               # bbrkc test fish and cdq fisheries to TR
               fishery = gsub("XR|CR", "TR", fishery),
               fishery = ifelse((fishery %in% early_90s_tt) & (statarea > 660000 | statarea < 0), paste0("QT", substring(fishery, 3, 4)), fishery),
               fishery = ifelse((fishery %in% early_90s_tt) & (statarea <= 660000 | statarea >= 0), paste0("TT", substring(fishery, 3, 4)), fishery),
               fishery = paste0(substring(fishery, 1, 2), substring(crab_year, 3, 4))) %>%
        # fill in legal
        add_legal(., stock = stock) %>%
        # add regulatory group
        mutate(group = case_when(sex == 2 ~ "female",
                                 sex == 1 & legal == 0 ~ "sublegal_male",
                                 sex == 1 & legal == 1 ~ "legal_male")) %>%
        dyply::select(-subdistrict) -> out
    }
    if(stock %in% c("AIGKC", "EAG", "WAG")) {
      ## data mgmt specific to gkc
      out %>%
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
        dyply::select(-subdistrict) -> out
    }


    if(stock %in% c("SMBKC", "PIBKC", "PIRKC", "WAIRKC")){
      stop(paste0("No method for ", stock, " yet !!"))
    }

    # add crab year
    out <- add_crab_year(out, date_correct = T)

  }



  return(out)

}
