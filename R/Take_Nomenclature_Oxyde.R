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
Take_Nomenclature_Oxyde <- function(DataTC_04_Oxyde, chem_symbol,
                             element_valence,
                             gas_status_element,
                             language){

  # # # # #
  # input_obj_name <-  "DataTC_04_Oxyde"
  # input_folder <- "./data/"
  # input_file <- paste0(input_obj_name, ".rda")
  # input_path <- paste0(input_folder, input_file)
  #
  # # Importamos el objeto "DataTC_01_PeriodicTable"
  # load(input_path)

  # chem_symbol <- "Fe"
  # element_valence <- 2
  # gas_status_element <- TRUE
  # language <- "esp"

  #####################################################
  dt_symbol <- DataTC_04_Oxyde[[language]]$Symbol == chem_symbol
  dt_selected_valence <- DataTC_04_Oxyde$eng$SelectedValence == element_valence

  pos_names <- c("Name_Classic_Oxyde", "Name_IUPAC_Oxyde", "Name_Stock_Oxyde02")

  dt_row <- (dt_symbol + dt_selected_valence) == 2

  all_oxyde_names <- DataTC_04_Oxyde[[language]][dt_row,pos_names]
  all_oxyde_names <- as.vector(as.matrix(all_oxyde_names))
  names(all_oxyde_names) <- pos_names

  add_details <- c("Clásica: ", "IUPAC: ", "Stock: ", "Stock: ")
  add_details <- paste0(add_details, all_oxyde_names)

  the_output <- list(all_oxyde_names, add_details)

  #######################################################
  # Final Return
  return(the_output)
}
