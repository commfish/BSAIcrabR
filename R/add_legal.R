#' Add legal status
#'
#' Add legal status to crab data
#' @param x crab data that includes fields 'size' and 'sex', and 'legal' (if use_observer = T) and 'crab_year' (if use_historic = T).
#' @param stock NULL. Character string stock abbreviation: BSSC, WBT, EBT, BBRKC, EAG, WAG, PIGKC, SMBKC, PIBKC, PIRKC, WAIRKC.
#' @param use_observer T/F. Default to observer record when available. x must contain field 'legal'. Default = T.
#' @param use_current T/F. Use current legal size on the whole time series, for WBT and EBT only. Default = T.
#' @return x with additional column 'legal' overwritten so that legal definition is based on 1) observer records if use_observer = T,
#' 2) size and sex if observer record is missing or uncertain. 0 = sublegal, 1 = legal.
#' @examples
#' add_legal(x = crab_dump, stock = "BBRKC")
#' @export
add_legal <- function(x, stock = NULL, use_observer = T, use_current = T) {

  ## errors ----
  if(missing(stock)){stop("What stock ?!?")}
  if(!("sex" %in% names(x))){stop("Cannot find sex !!")}
  if(!("size" %in% names(x))){stop("Cannot find size !!")}
  if(use_observer == T & !("legal" %in% names(x))){stop("Cannot find observer data 'legal' !!")}
  if(use_current == F & !("crab_year" %in% names(x))){stop("Cannot find observer data 'crab_year' !!")}
  if(use_current == F & !(stock %in% c("WBT", "EBT"))) {stop(paste0("No historic legal size info for ", stock," !!"))}

  ## routine ----
  # add legal size to data
  if(use_current == T) {
    legal_sizes <- tibble(BSAIstock = c("BSSC", "WBT", "EBT", "BBRKC", "AIGKC", "EAG", "WAG", "PIGKC",
                                        "SMBKC", "PIBKC", "PIRKC", "WAIRKC"),
                          legal_size = c(78, 111, 121, 124, 135, 135, 135, 135, 124, 124, 124, 124))
    x %>% mutate(legal_size = filter(legal_sizes, BSAIstock == stock) %>% pull(legal_size)) -> x
  }
  if(use_current == F) {

    tibble(BSAIstock = c("BSSC", "WBT", "EBT", "BBRKC", "AIGKC", "EAG", "WAG", "PIGKC",
                         "SMBKC", "PIBKC", "PIRKC", "WAIRKC"),
           legal_size = c(78, 111, 121, 124, 135, 135, 135, 135, 124, 124, 124, 124)) %>%
      expand_grid(crab_year = min(x$crab_year):max(x$crab_year)) %>%
      mutate(legal_size = ifelse(stock %in% c("WBT", "EBT") & crab_year <= 2010, 140, legal_size)) -> legal_sizes

    x %>%
      left_join(legal_sizes %>%
                  filter(BSAIstock == stock) %>%
                  transmute(crab_year, legal_size)) -> x
  }
  # determine legal
  if(use_observer == T){
    x %>%
      # 1) observer code
      mutate(legal = case_when(legal == 0 ~ 0,
                               legal %in% c(1:3, 6) ~ 1,
                               is.na(legal) ~ NA,
                               legal < 0 ~ NA)) %>%
      # 2) size / sex
      mutate(legal = case_when(!is.na(legal) ~ legal,
                               (is.na(legal) & sex != 1) ~ 0,
                               (is.na(legal) & sex == 1 & size >= legal_size) ~ 1,
                               (is.na(legal) & sex == 1 & size < legal_size) ~ 0)) %>%
      dplyr::select(-legal_size) -> out

  }
  if(use_observer == F){
    x %>%
     # 2) size / sex
      mutate(legal = case_when(sex != 1 ~ 0,
                              (sex == 1 & size >= legal_size) ~ 1,
                              (sex == 1 & size < legal_size) ~ 0)) %>%
      dplyr::select(-legal_size) %>%
      count(sex, legal) -> out

  }

  return(out)

}
