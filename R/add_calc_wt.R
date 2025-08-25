t#' Add Calculated Weight
#'
#' Add calculated weight based on weight at length parameters.
#' @param x crab data that includes fields 'size', 'sex', and 'clutch' and 'maturity' for chionoecetes stocks (if use_observer = T) and 'crab_year' (if use_historic = T).
#' @param stock NULL. Character string stock abbreviation: BSSC, WBT, EBT, BBRKC, EAG, WAG, PIGKC, SMBKC, PIBKC, PIRKC, WAIRKC.
#' @param units NULL. Unit of measurement, "t" or "lb". Default = "t".
#' @return x with additional column 'calc_wt'
#' @examples add_calc_wt(data, stock = "BBRKC")
#'
#' @export
#'
add_calc_wt <- function(x, stock, units = "t") {

  ## errors ----
  if(missing(stock)){stop("What stock ?!?")}
  if(!(units %in% c("t", "lb"))){stop("Do not recognize the units, use t or lb")}
  if(!("sex" %in% names(x))){stop("Cannot find sex !!")}
  if(!("size" %in% names(x))){stop("Cannot find size !!")}
  if(stock %in% c("BSSC", "WBT", "EBT", "BSTC") & !("clutch" %in% names(x))){stop("Cannot find size !!")}
  if(stock %in% c("BSSC", "WBT", "EBT", "BSTC") & !("maturity" %in% names(x))){stop("Cannot find size !!")}

  ## add parameters ----
  if(stock %in% c("BBRKC", "PIRKC", "WAIRKC", "EAG", "WAG", "AIGKC", "PIGKC", "SMBKC", "PIBKC")) {
    x %>%
      mutate(a = case_when(stock %in% c("BBRKC", "WAIRKC") & sex == 1 ~ 0.000403,
                           stock %in% c("BBRKC", "WAIRKC") & sex == 2 & (maturity == 1 | clutch > 0) ~ 0.003593,
                           stock %in% c("BBRKC", "WAIRKC") & sex == 2 & (maturity == 0 | clutch == 0) ~ 0.000408,
                           stock %in% c("BBRKC", "WAIRKC") & sex == 2 & ((clutch < 0  | (is.na(maturity) & is.na(clutch))) & size >= 90) ~ 0.003593,
                           stock %in% c("BBRKC", "WAIRKC") & sex == 2 & ((clutch < 0  | (is.na(maturity) & is.na(clutch))) & size < 90) ~ 0.000408,
                           stock %in% c("PIRKC") & crab_year %in% 1973:2009 & sex == 1 ~ 0.000361,
                           stock %in% c("PIRKC") & crab_year > 2009 & sex == 1 ~ 0.000403,
                           stock %in% c("PIRKC") & crab_year %in% 1973:2009 & sex == 2 ~ 0.022863,
                           stock %in% c("PIRKC") & crab_year > 2009 & sex == 2 & (maturity == 1 | clutch > 0) ~ 0.003593,
                           stock %in% c("PIRKC") & crab_year > 2009 & sex == 2 & (maturity == 0 | clutch == 0) ~ 0.000408,


                           stock %in% c("PIGKC") & sex == 1 ~ 0.000271236,
                           stock %in% c("EAG", "WAG", "AIGKC") & sex == 1 ~ 0.0001445,
                           stock %in% c("EAG", "WAG", "AIGKC", "PIGKC") & sex == 2 ~ 0.001424,
                           stock %in% c("SMBKC", "PIBKC") & sex == 1 ~ 0.000502,
                           stock %in% c("SMBKC", "PIBKC") & sex == 2 ~ 0.02065)) %>%
      mutate(b = case_when(stock %in% c("BBRKC", "WAIRKC") & sex == 1 ~ 3.141334,
                           stock %in% c("BBRKC", "WAIRKC") & sex == 2 & (maturity == 1 | clutch > 0) ~ 2.666076,
                           stock %in% c("BBRKC", "WAIRKC") & sex == 2 & (maturity == 0 | clutch == 0) ~ 3.127956,
                           stock %in% c("BBRKC", "WAIRKC") & sex == 2 & ((clutch < 0 | (is.na(maturity) & is.na(clutch))) & size >= 90) ~ 2.666076,
                           stock %in% c("BBRKC", "WAIRKC") & sex == 2 & ((clutch < 0  | (is.na(maturity) & is.na(clutch))) & size < 90) ~ 3.127956,
                           stock %in% c("PIRKC") & crab_year %in% 1973:2009 & sex == 1 ~ 3.16,
                           stock %in% c("PIRKC") & crab_year > 2009 & sex == 1 ~ 3.141334,
                           stock %in% c("PIRKC") & crab_year %in% 1973:2009 & sex == 2 ~ 2.23382,
                           stock %in% c("PIRKC") & crab_year > 2009 & sex == 2 & (maturity == 1 | clutch > 0) ~ 2.666076,
                           stock %in% c("PIRKC") & crab_year > 2009 & sex == 2 & (maturity == 0 | clutch == 0) ~ 3.127956,
                           stock %in% c("PIGKC") & sex == 1 ~ 3.167577,
                           stock %in% c("EAG", "WAG", "AIGKC") & sex == 1 ~ 3.28113,
                           stock %in% c("EAG", "WAG", "AIGKC", "PIGKC") & sex == 2 ~ 2.781,
                           stock %in% c("SMBKC", "PIBKC") & sex == 1 ~ 3.107158,
                           stock %in% c("SMBKC", "PIBKC") & sex == 2 ~ 2.27)) -> x
  }
  if(stock %in% c("BSSC", "BSTC", "WBT", "EBT")) {
    x %>%
      mutate(a = case_when(stock == "BSSC" & sex == 1 ~ 0.000267,
                           stock == "BSSC" & sex == 2 & (maturity == 1 | clutch > 0) ~ 0.001158,
                           stock == "BSSC" & sex == 2 & (maturity == 0 | clutch <= 0 | is.na(clutch)) ~ 0.001047,
                           stock %in% c("WBT", "EBT", "BSTC") & sex == 1 ~ 0.00027,
                           stock %in% c("WBT", "EBT", "BSTC") & sex == 2 & (maturity == 1 | clutch > 0) ~ 0.000441,
                           stock %in% c("WBT", "EBT", "BSTC") & sex == 2 & (maturity == 0 | clutch <= 0 | is.na(clutch)) ~ 0.000562)) %>%
      mutate(b = case_when(stock == "BSSC" & sex == 1 ~ 3.097253,
                           stock == "BSSC" & sex == 2 & (maturity == 1 | clutch > 0) ~ 2.708793,
                           stock == "BSSC" & sex == 2 & (maturity == 0 | clutch <= 0 | is.na(clutch)) ~ 2.708367,
                           stock %in% c("WBT", "EBT", "BSTC") & sex == 1 ~ 3.022134,
                           stock %in% c("WBT", "EBT", "BSTC") & sex == 2 & (maturity == 1 | clutch > 0) ~ 2.898686,
                           stock %in% c("WBT", "EBT", "BSTC") & sex == 2 & (maturity == 0 | clutch <= 0 | is.na(clutch)) ~ 2.816928)) -> x
  }

  ## add calculated weight
  x %>%
    # calculated wt in grams
    mutate(calc_wt = a * size^b,
           calc_wt = case_when(units == "t" ~ calc_wt * 1e-6,
                               units == "lb" ~ calc_wt * 0.00220462)) %>%
    dplyr::select(-a, -b) -> out

  return(out)

}
