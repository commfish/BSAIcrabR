#' Get Directed Discards
#'
#' Estimate directed fishery discards by sex using subtraction method

#' @param retained_catch NULL. Output of `get_retaied_catch()`.
#' @param total_catch NULL. Output of `get_total_catch()`. 'by' argument could be anything as long as group or sex is included.
#' @param stock NULL. Character string stock abbreviation: BSSC, WBT, EBT, BBRKC, EAG, WAG, PIGKC, SMBKC, PIBKC, PIRKC, WAIRKC.
#' @return Data frame including crab year, fishery, sex, discards, and discard mortality.
#' @examples get_discards(retained_catch, total_catch, stock = "BBRKC")
#'
#' @export
#'
get_discards <- function(retained_catch, total_catch, stock) {

  # directed fishery codes
  tibble(stock_abbrev = c("BSSC", "WBT", "EBT", "BSTC", "BBRKC", "EAG", "WAG", "PIGKC", "SMBKC", "PIBKC", "PIRKC", "WAIRKC"),
         fish_code = c("QO", "QT", "TT", "QT|TT", "TR", "OB", "RB", "QB", "QP", "QP", "QR", "RR")) %>%
    filter(stock_abbrev == stock) %>%
    pull(fish_code) -> fish_code

  # handling mortality
  if(stock %in% c("BBRKC", "EAG", "WAG", "PIGKC", "SMBKC", "PIBKC", "PIRKC", "WAIRKC")) {handling_mortality <- 0.2}
  if(stock %in% c("BBRKC")) {non_dir_hm <- 0.25}
  if(stock %in% c("EAG", "WAG", "PIGKC", "SMBKC", "PIBKC", "PIRKC", "WAIRKC")) {non_dir_hm <- 0.2}
  if(stock %in% c("BSSC")) {handling_mortality <- 0.3; non_dir_hm <- 0.3}
  if(stock %in% c("WBT", "EBT", "BSTC")) {handling_mortality <- 0.321;  non_dir_hm <- 0.321}

  # summarise total catch
  if("group" %in% names(total_catch)){
    total_catch %>%
      # add sex
      mutate(sex = case_when(group == "female" ~ 2,
                             group == "sublegal_male"~ 1,
                             group == "legal_male"~ 1)) -> total_catch
  }

  if(sum(grepl("group|sex", names(total_catch))) == 0){
    stop("Total catch must be by 'group', 'sex', or both  !!")
  }

  # restructure retained catch
  retained_catch %>% filter(crab_year < 2005) %>%
    transmute(crab_year, fishery, retained_n = tot_retained_n, retained_wt = tot_retained_wt) %>%
    bind_rows(retained_catch %>% filter(crab_year >= 2005) %>%
                dplyr::select(-tot_retained_n, -tot_retained_wt) %>%
                pivot_longer(cols = starts_with("dir_retained") | starts_with("inc_retained"),
                             names_to = c("type", ".value"),
                             names_pattern = "(.*)_retained_(.*)") %>%
                # change fishery in the incidental catch
                mutate(fishery = ifelse(stock == "BSSC" & type == "inc", gsub("QO", "QT", fishery), fishery),
                       fishery = ifelse(stock == "WBT" & type == "inc", gsub("QT", "QO", fishery), fishery),
                       fishery = ifelse(stock == "EBT" & type == "inc", gsub("TT", "TR", fishery), fishery),
                       fishery = ifelse(stock == "BBRKC" & type == "inc", gsub("TR", "TT", fishery), fishery),
                       fishery = ifelse(stock == "BSTC" & type == "inc" & grepl("QT", fishery), gsub("QT", "QO", fishery), fishery),
                       fishery = ifelse(stock == "BSTC" & type == "inc" & grepl("TT", fishery), gsub("TT", "TR", fishery), fishery),
                       fishery = ifelse(fishery == "QO05", "QO05r", fishery),
                       fishery = gsub("QT05r", "QT05", fishery)) %>%
                dplyr::select(-type) %>%
                rename(retained_n = n, retained_wt = wt) ) %>%
    # remove closed or non incidental fisheries
    filter(retained_n > 0, retained_wt > 0) -> ret

  # compute discards
  total_catch %>%
    group_by(crab_year, fishery, sex) %>%
    summarise(total_catch_n = sum(total_catch_n, na.rm = T),
             total_catch_wt = sum(total_catch_wt, na.rm = T)) %>% ungroup %>%
    # join to retained catch
    full_join(ret %>%
                mutate(sex = 1), by = c("crab_year", "fishery", "sex")) %>%
    replace_na(list(retained_n = 0, retained_wt = 0)) %>%
    # add discards
    mutate(discard_n = total_catch_n - retained_n,
           discard_wt = total_catch_wt - retained_wt,
           discard_mortality_n = discard_n * handling_mortality + retained_n,
           discard_mortality_wt = discard_wt * handling_mortality + retained_wt) %>%
    arrange(crab_year, fishery) -> out

  return(out)

}

