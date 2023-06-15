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
ChemFormule_Oxyde <- function(chem_symbol,
                             element_valence,
                             gas_status_element,
                             language){

  # chem_symbol <- "Fe"
  # element_valence <- 2
  # gas_status_element <- TRUE
  # language = "eng"



  the_solver <- Solver_Oxyde(element_valence = element_valence,
                             gas_status_element = gas_status_element)



  vector_symbol <- rep(chem_symbol, nrow(the_solver))
  vector_oxygen <- rep("O", nrow(the_solver))



  output <- list()

  # 1) Original Output Solver
  output[[1]] <- the_solver
  names(output)[1] <- "Level01_Solver"

  # 2) Add Element and Oxygen in reactives and products
  output[[2]] <- cbind.data.frame(the_solver[1], vector_symbol,
                                  the_solver[c(2,3)], vector_oxygen,
                                  the_solver[c(4,5)],
                                  vector_symbol, the_solver[c(6)],
                                  vector_oxygen, the_solver[c(7)])

  colnames(output[[2]])[2] <- "Element_R"
  colnames(output[[2]])[5] <- "Oxygen_R"
  colnames(output[[2]])[8] <- "Element_P"
  colnames(output[[2]])[10] <- "Oxygen_P"

  names(output)[2] <- "Level02_SymbolsAdded"

  # 3) Fision 01: Element, Oxigen and Oxide with subindex
  #    as an pack each one.
  output[[3]] <- cbind.data.frame(output[[2]][1],
                                  paste0(output[[2]][,2], output[[2]][,3]),
                                  output[[2]][4],
                                  paste0(output[[2]][,5], output[[2]][,6]),
                                  output[[2]][7],
                                  paste0(output[[2]][,8], output[[2]][,9],
                                         output[[2]][,10], output[[2]][,11]))

  colnames(output[[3]])[c(2,4,6)] <- c("ER", "OR", "Oxyde")
  names(output)[3] <- "Level03_Fusion01"


  output[[4]] <- output[[3]]$Oxyde[nrow(output[[3]])]
  names(output)[4] <- "ChemFormule_pure"

  ####################################################################################
  output[[5]] <- output[[4]]
    dt_0 <- grepl("0",  output[[4]])
    dt_1 <- grepl("1",  output[[4]])

  dt_algo <- sum(sum(dt_0) + sum(dt_1)) > 0
  if(dt_algo){
    if (dt_0) output[[5]] <- paste0("\\phantom{", output[[5]], "}")
    if (dt_1) output[[5]] <- stringr::str_replace_all(string = output[[5]], "1", "\\\\phantom{1}")
  }

  names(output)[5] <- "ChemFormule_LaTeX"

  ####################################################################################
  # Final Return
  return(output)
}



