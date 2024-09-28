#' Load BSAI dockside data
#'
#' Load BSAI dockside data and do data management routine
#' @param path NULL. Path to data file if not pulling directly from database.
#' @param stock NULL. Character sring stock abbreviation: BSSC, WBT, EBT, BBRKC, EAG, WAG, PIGKC, SMBKC, PIBKC, PIRKC, WAIRKC.
#' @param database_pull Default = FALSE. Do updated pull from database.
#' @return Dock data time series by fishery.
#' @examples load_dockside("./data.csv", stock = "BBRKC")
#'
#' @export
#'
load_dockside <- function(path, stock, database_pull = F) {

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

  # filter data by stock
  if(stock == "BBRKC"){
    out %>%
      mutate(fishery = gsub("XR|CR", "TR", fishery)) %>%
      filter(substring(fishery, 1, 2) == "TR") -> out
  }

  return(out)

}
