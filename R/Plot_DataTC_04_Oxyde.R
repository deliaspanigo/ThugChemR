


#' Plot Oxyde Resolution from Data ThugChem
#'
#' @param chem_symbol The chemical symbol of the element
#' @param element_valence The valence of the element
#' @param gas_status_element If the element is a gas is TRUE else FALSE
#' @param verbose Logic value. Speaking about the processing
#'
#' @return A plot with all steps of resolution.
#' @export
#'
#' @examples
#' Plot_DataTC_04_Oxyde(chem_symbol = "H",
#'                      element_valence = 1,
#'                      gas_status_element = TRUE,
#'                       verbose = TRUE)
#'
Plot_DataTC_04_Oxyde <- function(chem_symbol, element_valence,
                                 gas_status_element, verbose = TRUE){
  # chem_symbol = "H"
  #element_valence = 1
  #gas_status_element = TRUE


  resolution_complite <- Resolution_Oxyde(chem_symbol = chem_symbol,
                                          element_valence = element_valence,
                                          gas_status_element = gas_status_element)

  selected_resolution <- resolution_complite$Level06_LaTeX02

  chem_formula <- resolution_complite$ChemFormule

  plot(1:10, 1:10, col = "white", main = chem_formula)

  for(k in 1:nrow(selected_resolution)){

    graphics::text(2, nrow(selected_resolution) - k, latex2exp::TeX(selected_resolution[k,1]), pos = 4)

  }
  graphics::text(8, 5, chem_formula, pos = 4)


}












