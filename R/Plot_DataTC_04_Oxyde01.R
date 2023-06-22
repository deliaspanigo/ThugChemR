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
#' Plot_PackTC_04_Oxyde01(chem_symbol = "Fe", element_valence = 2, verbose = T,
#' language = "esp",)
#'Plot_PackTC_04_Oxyde02(chem_symbol = "Fe", element_valence = 2, verbose = T,
#'                       language = "esp",)


Plot_PackTC_04_Oxyde01 <- function(chem_symbol,
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

  chem_formula <- complite_solution$ChemFormule
  nomenclatura <- complite_solution$Nomenclature_Oxyde

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



  for(k in 1:nrow(selected_resolution)){

    graphics::text(x = 2,
                   y = nrow(selected_resolution) - (k*1),
                   labels = latex2exp::TeX(selected_resolution[k,1]),
                   pos = 4,
                   cex = 2)

  }


}













