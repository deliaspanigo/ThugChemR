


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
Plot_PackTC_04_Oxyde02 <- function(chem_symbol,
                                   element_valence,
                                   verbose = TRUE,
                                   language){

  # chem_symbol = "H"
  # element_valence = 1
  # gas_status_element = TRUE
  # language <- "esp"

  input_data <- ThugChemR::PackTC_04_Oxyde[[language]]

  all_oxyde <- names(input_data)
  armado <- paste0("_", chem_symbol, element_valence, "_")
  pos_oxyde <- grep(armado, all_oxyde)

  selected_oxyde <- all_oxyde[pos_oxyde]

  complite_solution <- input_data[[selected_oxyde]]
  selected_resolution <- complite_solution$Level06_LaTeX02

  chem_formula <- paste0("Fórmula: ", complite_solution$ChemFormule)
  nomenclatura <- complite_solution$Nomenclature_Oxyde_02

  plot(1:10, 1:10, col = "white", main = chem_formula,
       xlab = "", ylab = "", axes = F)


  # Plot region color
  rect(par("usr")[1], par("usr")[3],
       par("usr")[2], par("usr")[4],
       col = "#f7f7f7") # Color

  # Add a new plot
  par(new = TRUE)


  plot(1:10, 1:10, col = "white", main = chem_formula,
       xlab = "", ylab = "", axes = F)




  graphics::text(3, 9, chem_formula, pos = 4, cex = 2)

  graphics::text(3, 7, nomenclatura[1], pos = 4, cex = 2)
  graphics::text(3, 5, nomenclatura[2], pos = 4, cex = 2)
  graphics::text(3, 3, nomenclatura[3], pos = 4, cex = 2)

}












