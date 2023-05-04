#' Plot Oxyde Resolution from Pack ThugChem
#'
#' @param chem_symbol The chemical symbol of the element
#' @param element_valence The valence of the element
#' @param verbose Logic value. Speaking about the processing
#'
#' @return A plot with all steps of resolution.
#' @export
#'
#' @examples
#' Plot_PackTC_04_Oxyde(chem_symbol = "Fe", element_valence = 2, verbose = T)



Plot_PackTC_04_Oxyde <- function(chem_symbol,
                                 element_valence,
                                 verbose = TRUE){

  # chem_symbol = "H"
  # element_valence = 1
  # gas_status_element = TRUE

  all_oxyde <- names(ThugChemR::PackTC_04_Oxyde)
  armado <- paste0("_", chem_symbol, element_valence, "_")
  pos_oxyde <- grep(armado, all_oxyde)

  selected_oxyde <- all_oxyde[pos_oxyde]

  complite_solution <- ThugChemR::PackTC_04_Oxyde[[selected_oxyde]]
  selected_resolution <- complite_solution$Level06_LaTeX02

  chem_formula <- complite_solution$ChemFormule

  plot(1:10, 1:10, col = "white", main = chem_formula)

  for(k in 1:nrow(selected_resolution)){

    graphics::text(2, nrow(selected_resolution) - k, latex2exp::TeX(selected_resolution[k,1]), pos = 4)

  }
  graphics::text(8, 5, chem_formula, pos = 4)


}












