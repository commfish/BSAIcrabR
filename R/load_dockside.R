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
    # reorder
    transmute(crab_year, fishery, adfg, sample_date, spcode, size, legal, shell, numcrab) -> out

  if(clean == T){
    # stock specific
    if(stock == "BBRKC"){
      out %>%
        mutate(fishery = gsub("XR|CR", "TR", fishery)) %>%
        filter(substring(fishery, 1, 2) == "TR") -> out
    }
    if(stock == "BSSC"){
      early_90s_tt <- c("EI91", "EI92", paste0("QT", 93:96))
      out %>%
        # fix transition to rationalization yr
        # cdq and eo fisheries to QO
        # bbrkc test fish and cdq fisheries to TR
        # early tanner crab fisheries to QT or TT based on e166 line
        # fisheries without any dates in dockside data (make hard change)
        mutate(fishery = gsub("QO05r", "QO05", fishery),
               fishery = gsub("QO05o", "QO04", fishery),
               fishery = gsub("CO|EO", "QO", fishery),
               fishery = gsub("XR|CR", "TR", fishery),
               fishery = ifelse(fishery %in% early_90s_tt, paste0("QT", substring(fishery, 3, 4)), fishery),
               fishery = ifelse(fishery %in% c("EO91", "EO92"), paste0(substring(fishery, 1, 2), as.numeric(substring(fishery, 3, 4))-1), fishery)) -> out
    }


    if(stock %in% c("BSTC", "WBT", "EBT", "AIGKC", "EAG", "WAG", "PIGKC", "SMBKC", "PIBKC", "PIRKC", "WAIRKC")){
      stop(paste0("No method for ", stock, " yet !!"))
    }
  }

  return(out)

}
