#' Load BSAI Dockside Sampling Data
#'
#' Load BSAI dockside data and do data management routine
#' @param path NULL. Path to data file if not pulling directly from database.
#' @param stock NULL. Character string stock abbreviation: BSSC, WBT, EBT, BBRKC, EAG, WAG, PIGKC, SMBKC, PIBKC, PIRKC, WAIRKC.
#' @param database_pull Default = FALSE. Do updated pull from database.
#' @param clean Default = TRUE. Do stock specific data cleaning.
#' @return Dock data time series by fishery.
#' @examples load_dockside("./data.csv", stock = "BBRKC")
#'
#' @export
#'
load_dockside <- function(path, stock, database_pull = F, clean = T) {

  # load data
  if(database_pull == T){stop("Database pull not set up yet")}
  if(database_pull == F){
    dock <- read_csv(path)
  }

  dock %>%
    # add crab year
    add_crab_year() %>%
    mutate(subdistrict = ifelse(!("subdistrict" %in% names(.)), NA, subdistrict)) %>%
    # reorder
    transmute(crab_year, fishery, adfg, sample_date, subdistrict, spcode, size, legal, shell, numcrab) -> out

  if(clean == T){
    # stock specific
    if(stock == "BBRKC"){
      out %>%
        mutate(fishery = gsub("XR|CR", "TR", fishery)) %>%
        filter(substring(fishery, 1, 2) == "TR") %>%
        dplyr::select(-subdistrict) %>%
        # remove erroneous sample
        filter(!(crab_year == 1993 & fishery == "TR92")) -> out
    }
    if(stock %in% c("BSSC", "BSTC", "EBT", "WBT")){
      early_90s_tt <- c("EI89", "EI90", "EI91", "EI92", paste0("QT", 93:96))
      out %>%
        # fix transition to rationalization yr
        # cdq and eo fisheries to QO
        # bbrkc test fish and cdq fisheries to TR
        # early tanner crab fisheries to QT or TT based on e166 line
        # fisheries without any dates in dockside data (make hard change)
        mutate(#fishery = gsub("QO05r", "QO05", fishery),
               #fishery = gsub("QO05o", "QO04", fishery),
               fishery = gsub("CO|EO", "QO", fishery),
               fishery = gsub("XR|CR", "TR", fishery),
               fishery = ifelse(fishery %in% early_90s_tt, paste0("QT", substring(fishery, 3, 4)), fishery),
               # gkc
               fishery = gsub("XE", "OB", fishery),
               fishery = ifelse(grepl("OB", fishery), paste0("OB", substring(crab_year, 3, 4)), fishery),
               fishery = ifelse(grepl("RB", fishery), paste0("OB", substring(crab_year, 3, 4)), fishery)) %>%
        dplyr::select(-subdistrict) -> out
    }
    if(stock %in% c("AIGKC", "EAG", "WAG")){
      out %>%
        # make XE fisheries EAG
        mutate(subdistrict = ifelse(substring(fishery, 1, 2) == "XE", "EAG", subdistrict)) %>%
        # fix subdistrict for OB08
        mutate(subdistrict = case_when(crab_year != 2008 ~ subdistrict,
                                       (crab_year == 2008 & adfg %in% c(35767, 37887)) ~ "WAG",
                                       (crab_year == 2008 & adfg %in% c(103, 5992, 20556)) ~ "EAG",
                                       (crab_year == 2008 & adfg == 5992 & sample_date > as_date("2008-12-1")) ~ "WAG")) %>%
        mutate(fishery = paste0(substring(fishery, 1, 2), substring(crab_year, 3, 4))) %>%
        mutate(fishery = ifelse(subdistrict == "EAG", gsub("RB", "OB", fishery),
                                ifelse(subdistrict == "WAG", gsub("OB", "RB", fishery), fishery)))  -> out
    }
    if(stock == "EAG"){
      out %>%
        filter(subdistrict == "EAG") -> out
    }
    if(stock == "WAG"){
      out %>%
        filter(subdistrict == "WAG") -> out
    }
    if(stock %in% c("PIGKC")){
      out %>%
        dplyr::select(-subdistrict) -> out
    }


    if(stock %in% c("SMBKC", "PIBKC", "PIRKC", "WAIRKC")){
      stop(paste0("No method for ", stock, " yet !!"))
    }
  }

  return(out)

}
