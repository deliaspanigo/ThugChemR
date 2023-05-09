#' Resolution for Oxyde
#'
#' @param chem_symbol The chemical symbol of the element
#' @param element_valence The valence of the element
#' @param gas_status_element If the element is a gas is TRUE else FALSE
#'
#' @return The function
#' @export
#'
#' @examples
#' Resolution_Oxyde(chem_symbol = "H",
#'                  element_valence = 1,
#'                  gas_status_element = TRUE)
Nomenclature_Oxyde <- function(chem_symbol,
                             element_valence,
                             gas_status_element,
                             language){


  # chem_symbol <- "Fe"
  # element_valence <- 2
  # gas_status_element <- TRUE
  # language <- "esp"

  #####################################################
  dt_symbol <- DataTC_04_Oxyde[[language]]$Symbol == chem_symbol
  dt_selected_valence <- DataTC_04_Oxyde$eng$SelectedValence == element_valence

  pos_names <- c("OxydeFullName_Classic", "OxydeFullName_IUPAC",
                 "OxydeFullName_Stock")

  dt_row <- (dt_symbol + dt_selected_valence) == 2

  all_oxyde_names <- DataTC_04_Oxyde[[language]][dt_row,pos_names]
  all_oxyde_names <- as.vector(as.matrix(all_oxyde_names))

  #######################################################
  # Final Return
  return(all_oxyde_names)
}
