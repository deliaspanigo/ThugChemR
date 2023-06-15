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
Resolution_Oxyde <- function(chem_symbol,
                             element_valence,
                             gas_status_element,
                             language){

  # # # # #
  # input_obj_name <-  "DataTC_04_Oxyde"
  # input_folder <- "./data/"
  # input_file <- paste0(input_obj_name, ".rda")
  # input_path <- paste0(input_folder, input_file)

  # # Importamos el objeto "DataTC_04_Oxyde"
  # load(input_path)

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


  # 4) LaTeX 01: Element, Oxigen and Oxide with subindex
  #    as an pack each one with special format for LaTeX
  output[[4]] <- cbind.data.frame(output[[2]][1],
                                  paste0(output[[2]][,2], "_{", output[[2]][,3], "}"),
                                  output[[2]][4],
                                  paste0(output[[2]][,5], "_{", output[[2]][,6], "}"),
                                  output[[2]][7],
                                  paste0(output[[2]][,8],  "_{", output[[2]][,9] , "}",
                                         output[[2]][,10], "_{", output[[2]][,11], "}"))

  colnames(output[[4]]) <- colnames(output[[3]])
  names(output)[4] <- "Level03_LaTeX01"

  # 5) Latex + Symbols ( + and arrow)
  output[[5]] <- cbind.data.frame(output[[4]][c(1,2)],
                                  rep("+", nrow(output[[4]])),
                                  output[[4]][c(3,4)],
                                  # rep("\rightarrow", nrow(output[[4]])),
                                  rep("       ------>     ", nrow(output[[4]])),
                                  output[[4]][c(5,6)])
  colnames(output[[5]])[3] <- "Plus"
  colnames(output[[5]])[6] <- "Arrow"
  names(output)[5] <- "Level04_PlusAndArrow"

  output[[6]] <- apply( output[[5]], 1 , function(x){
    paste0("$", paste0(x, collapse =""), "$")
  })
  output[[6]] <- as.data.frame(output[[6]])
  colnames(output[[6]])[1] <- "Oxyde"
  names(output)[6] <- "Level06_LaTeX02"


  # output[[7]] <- output[[3]]$Oxyde[nrow(output[[3]])]
  output[[7]] <- ChemFormule_Oxyde(chem_symbol = chem_symbol,
                                   element_valence = element_valence,
                                   gas_status_element = gas_status_element,
                                   language = language)$ChemFormule_pure

  names(output)[7] <- "ChemFormule_pure"


  #######################################################

  # # # # # # Los siguientes pasos son efectivamente "SOLVER"...
  # # # 5) Simplificacion de los subindices en el oxido
  # # # 6) Balanceo del oxido
  # # # 7) Balanceo del elemento
  # # # 8) Simplificacion de los coeficientes

  TC_Text_Oxyde_General <- list()
  TC_Text_Oxyde_General[[1]] <- "Presentacion del elemento y del oxigeno, todo con subindices 1."
  TC_Text_Oxyde_General[[2]] <- "Los elementos en estado gaseoso se detallan con subindice 2"
  TC_Text_Oxyde_General[[3]] <- "Presentacion del oxido todo con sunindices 1."
  TC_Text_Oxyde_General[[4]] <- "Subindices en el oxido son las valencias cruzadas de los elementos."
  TC_Text_Oxyde_General[[5]] <- "Simplificacion de los subindices en el oxido"
  TC_Text_Oxyde_General[[6]] <- "Balanceo del oxigeno"
  TC_Text_Oxyde_General[[7]] <- "Balanceo del elemento"
  TC_Text_Oxyde_General[[8]] <- "Simplificacion de los coeficientes"
  text_oxyde_general <- do.call(rbind.data.frame, TC_Text_Oxyde_General)


  TC_Text_Oxyde_Particular <- list()
  TC_Text_Oxyde_Particular[[1]] <- "Los reactivos son _elemento_ y el oxigeno."
  TC_Text_Oxyde_Particular[[2]] <- "El oxigeno es un gas, por lo tanto lleva subindice 2... y faltaría agregar lo que le pasa al otro elemento."
  TC_Text_Oxyde_Particular[[3]] <- "Comenzamos con _el_oxido_ con subindices 1."
  TC_Text_Oxyde_Particular[[4]] <- "El subindice del _elemento_ sera 2, y el subindice del oxigeno sera _valencia_."
  TC_Text_Oxyde_Particular[[5]] <- "En este caso se simplificara por ... o no se simplificara"
  TC_Text_Oxyde_Particular[[6]] <- "En este caso colocamos... ... ... explicar!"
  TC_Text_Oxyde_Particular[[7]] <- "Para lograrlo, colocamos..."
  TC_Text_Oxyde_Particular[[8]] <- "En este caso no es necesario... o si es necesario"
  text_oxyde_particular <- do.call(rbind.data.frame, TC_Text_Oxyde_Particular)

  entrega_mod <- paste0("$", output[["Level06_LaTeX02"]][,1],  "$")
  # entrega_mod <- paste0(" withMathJax(", entrega_mod, ")")
  # entreda_mod[1] <- 'You do not see me initially: $$e^{i \\pi} + 1 = 0$$'
  # entrega_mod <- rep("$$e^{i \\pi} + 1 = 0$$", 8)

  output[[8]] <- cbind.data.frame(entrega_mod,
                                  text_oxyde_general,
                                  text_oxyde_particular)

  colnames( output[[8]]) <- c("Steps", "General", "Particular")
  names(output)[8] <- "format02_oxyde"


  #####################################################
  output[[9]] <- Take_Nomenclature_Oxyde(DataTC_04_Oxyde,
                                         chem_symbol,
                                         element_valence,
                                         gas_status_element,
                                         language)[[1]]

  names(output)[9] <- "Nomenclature_Oxyde_01"


  output[[10]] <-  Take_Nomenclature_Oxyde(DataTC_04_Oxyde,
                                           chem_symbol,
                                           element_valence,
                                           gas_status_element,
                                           language)[[2]]

  names(output)[10] <- "Nomenclature_Oxyde_02"
  #######################################################


  #####################################################

  materia_prima <- output[[1]]

  the_cols <- c("Element", "Reactive", "Product")
  the_rows <- c(chem_symbol, "O")

  balance_modelo <- matrix(NA, length(the_rows), length(the_cols))
  colnames(balance_modelo) <- the_cols
  rownames(balance_modelo) <- 1:length(the_rows)
  balance_modelo[,1] <- the_rows

  balance_bruto <- lapply(1:nrow(materia_prima), function(x){

    the_output <- balance_modelo

    # El elemento
    the_output[1,2] <- paste0(materia_prima$coef1[x], "*", materia_prima$subER[x])
    the_output[1,3] <- paste0(materia_prima$coef3[x], "*", materia_prima$subEP[x])

    # El oxigeno
    the_output[2,2] <- paste0(materia_prima$coef2[x], "*", materia_prima$subOR[x])
    the_output[2,3] <- paste0(materia_prima$coef3[x], "*", materia_prima$subOP[x])

    return(the_output)
  })

  balance_suma <- lapply(1:length(balance_bruto), function(x){

    the_output <- balance_bruto[[x]]

    # El elemento
    the_output[1,2] <- eval(parse(text = the_output[1,2]))
    the_output[1,3] <- eval(parse(text = the_output[1,3]))

    # El oxigeno
    the_output[2,2] <- eval(parse(text = the_output[2,2]))
    the_output[2,3] <- eval(parse(text = the_output[2,3]))

    return(the_output)
  })

  balance_rejunte <- lapply(1:length(balance_bruto), function(x){

    elegida_bruto <- balance_bruto[[x]]
    elegida_suma <- balance_suma[[x]]
    the_output <- balance_modelo

    # El elemento
    the_output[1,2] <- paste0(elegida_bruto[1,2], " = ", elegida_suma[1,2])
    the_output[1,3] <- paste0(elegida_bruto[1,3], " = ", elegida_suma[1,3])

    # El oxigeno
    the_output[2,2] <- paste0(elegida_bruto[2,2], " = ", elegida_suma[2,2])
    the_output[2,3] <- paste0(elegida_bruto[2,3], " = ", elegida_suma[2,3])

    return(the_output)
  })

  output[[11]] <- balance_rejunte

  names(output)[11] <- "Balance_Oxyde"

  #######################################################

  # Final Return
  return(output)
}



