#' Add target stock to data
#'
#' Add target stock based on fishery code
#' @param x data frame
#' @param col fishery code colum, default = fishery
#' @return x with additional column 'target_stock'
#' @examples
#' add_target_stock(x)
#' @export
add_target_stock <- function(x, col = fishery) {

  x %>%
    dplyr::mutate(target_stock = case_when(substring({{col}}, 1, 2) == "QO" ~ "BSSC",
                                     substring({{col}}, 1, 2) == "QB" ~ "PIGKC",
                                     substring({{col}}, 1, 2) == "QP" ~ "SMBKC_PIBKC",
                                     substring({{col}}, 1, 2) == "SB" ~ "SMBKC",
                                     substring({{col}}, 1, 2) == "QR" ~ "PIRKC",
                                     substring({{col}}, 1, 2) == "QT" ~ "WBT",
                                     substring({{col}}, 1, 2) == "TT" ~ "EBT",
                                     substring({{col}}, 1, 2) == "TR" ~ "BBRKC",
                                     substring({{col}}, 1, 2) == "TB" ~ "BBGKC",
                                     substring({{col}}, 1, 2) == "RB" ~ "WAG",
                                     substring({{col}}, 1, 2) == "OB" ~ "EAG")) -> out

  return(out)

}

