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
    if(stock %in% c("BSSC", "BSTC", "WBT", "EBT", "AIGKC", "EAG", "WAG", "PIGKC", "SMBKC", "PIBKC", "PIRKC", "WAIRKC")){
      stop(paste0("No method for ", stock, " yet !!"))
    }
  }

  return(out)

}
