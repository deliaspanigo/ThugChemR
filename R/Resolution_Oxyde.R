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
#
#
# chem_symbol <- "H"
# element_valence <- 1
# gas_status_element <- TRUE
# language = "esp"


  ##############################################################
  input_obj_name02 <-  "DataTC_03_Valences"
  input_folder02 <- "./data/"
  input_file02 <- paste0(input_obj_name02, ".rda")
  input_path02 <- paste0(input_folder02, input_file02)
  load(input_path02)

  dt_symbol_extra <- DataTC_03_Valences[[language]]$Symbol == chem_symbol
  dt_valencia_extra <- DataTC_03_Valences[[language]]$SelectedValence == element_valence
  dt_row <- (dt_symbol_extra + dt_valencia_extra ) == 2

  ##############################################################


  chem_name <- DataTC_03_Valences[[language]]$Name[dt_row]
  roman_number <- DataTC_03_Valences[[language]]$RomanValence[dt_row]

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

  #######################################################

  mod_phantom <- output[[5]]

  for(k1 in 1:ncol(mod_phantom)) {
    for(k2 in 1:nrow(mod_phantom)) {

      # k1 <- 8
      # k2 <- 3
      esta_seleccion <- mod_phantom[k2, k1]

      dt_0 <- grepl("0", esta_seleccion)
      dt_1 <- grepl("1", esta_seleccion)

      dt_algo <- sum(sum(dt_0) + sum(dt_1)) > 0
      if(dt_algo){
        if (dt_0) esta_seleccion <- paste0("\\phantom{", esta_seleccion, "}")
        if (dt_1) esta_seleccion <- stringr::str_replace_all(string = esta_seleccion, "1", "\\\\phantom{1}")


        # if (dt_1) esta_seleccion <- gsub(pattern = "1", replacement = "\\phantom{1}", x = esta_seleccion)


        mod_phantom[k2, k1] <- esta_seleccion
      }

    }
  }

  super_chem_formule <- mod_phantom[nrow(mod_phantom), ncol(mod_phantom )]
  #######################################################

  mod_phantom$coef1 <- paste0("\\overset{\\phantom{", mod_phantom$coef1, "}}{",mod_phantom$coef1,"}")
  mod_phantom$ER <- paste0("\\overset{{", roman_number, "}_{\\phantom{3}}}{",mod_phantom$ER,"}")
  mod_phantom$Plus <- paste0("\\overset{\\phantom{", mod_phantom$Plus, "}}{",mod_phantom$Plus,"}")
  mod_phantom$coef2 <- paste0("\\overset{\\phantom{", mod_phantom$coef2, "}}{",mod_phantom$coef2,"}")
  mod_phantom$OR <- paste0("\\overset{", "{II}_{\\phantom{3}}", "}{",mod_phantom$OR,"}")
  mod_phantom$Arrow <- paste0("\\overset{\\phantom{", mod_phantom$Arrow, "}}{",mod_phantom$Arrow,"}")
  mod_phantom$coef3 <- paste0("\\overset{\\phantom{", mod_phantom$coef3, "}}{",mod_phantom$coef3,"}")
  mod_phantom$Oxyde <- paste0("\\overset{\\phantom{", mod_phantom$Oxyde, "}}{",mod_phantom$Oxyde,"}")

  #######################################################
  output[[6]] <- mod_phantom


  names(output)[6] <- "Level05_PlusAndArrow"


  ######################################################

  output[[7]] <- apply( output[[6]], 1 , function(x){
    paste0("$", paste0(x, collapse =""), "$")
  })
  output[[7]] <- as.data.frame(output[[7]])
  colnames(output[[7]])[1] <- "Oxyde"
  names(output)[7] <- "Level06_LaTeX02"


  ###################################################################################

  output[[8]] <- ChemFormule_Oxyde(chem_symbol = chem_symbol,
                                   element_valence = element_valence,
                                   gas_status_element = gas_status_element,
                                   language = language)$ChemFormule_pure

  names(output)[8] <- "ChemFormule_pure"


  #######################################################################################

  # output[[9]] <- ChemFormule_Oxyde(chem_symbol = chem_symbol,
  #                                  element_valence = element_valence,
  #                                  gas_status_element = gas_status_element,
  #                                  language = language)$ChemFormule_LaTeX
  output[[9]] <- super_chem_formule
  output[[9]] <- paste0("$", output[[9]], "$")
  names(output)[9] <- "ChemFormule_LaTeX"


  #######################################################################################

  # # # # # # Los siguientes pasos son efectivamente "SOLVER"...
  # # # 5) Simplificacion de los subindices en el oxido
  # # # 6) Balanceo del oxido
  # # # 7) Balanceo del elemento
  # # # 8) Simplificacion de los coeficientes

  TC_Text_Oxyde_General <- list()
  TC_Text_Oxyde_General[[1]] <- "Presentación del elemento y del Oxígeno ambos con subíndices\ny sus valencias en números romanos"
  TC_Text_Oxyde_General[[2]] <- "Los elementos en estado gaseoso se detallan con subíndice 2"
  TC_Text_Oxyde_General[[3]] <- "Presentación del óxido con subindices 1"
  TC_Text_Oxyde_General[[4]] <- "Subíndices en el óxido son las valencias cruzadas de los elementos"
  TC_Text_Oxyde_General[[5]] <- "Simplificación de los subíndices en el óxido"
  TC_Text_Oxyde_General[[6]] <- "Balanceo del Oxígeno"
  TC_Text_Oxyde_General[[7]] <- "Balanceo del elemento"
  TC_Text_Oxyde_General[[8]] <- "Simplificación de los coeficientes"
  text_oxyde_general <- do.call(rbind.data.frame, TC_Text_Oxyde_General)

  # 1) "Presentacion del elemento y del oxigeno ambos con subindices 1."
  step01 <- "Los reactivos son _elemento_ y Oxígeno."
  step01 <- paste0(step01, collapse = "\n")
  step01 <- stringr::str_replace_all(string = step01,
                                     pattern = "_elemento_",
                                     replacement = chem_name)

  # 2) "Los elementos en estado gaseoso se detallan con subindice 2"
  step02 <- "Oxígeno es un gas por lo tanto indicamos subíndice 2."
  if(gas_status_element) step02 <- c(step02, "_elemento_ es también un gas e indicamos subíndice 2.")
  step02 <- paste0(step02, collapse = "\n")
  step02 <- stringr::str_replace_all(string = step02,
                                     pattern = "_elemento_",
                                     replacement = chem_name)

  # 3) "Presentacion del oxido con subindices 1."
  step03 <- c("_elemento_ y Oxígeno en una sola molécula conforman el óxido.",
              "Comenzamos con subindices 1 para el óxido.")
  step03 <- paste0(step03, collapse = "\n")
  step03 <- stringr::str_replace_all(string = step03,
                                     pattern = "_elemento_",
                                     replacement = chem_name)

  # 4) "Subindices en el oxido son las valencias cruzadas de los elementos."
  step04 <- c("El subindice del _elemento_ sera 2.",
              "El subindice del oxigeno sera _valencia_.")
  step04 <- paste0(step04, collapse = "\n")
  step04 <- stringr::str_replace_all(string = step04,
                                     pattern = "_elemento_",
                                     replacement = chem_name)
  step04 <- stringr::str_replace_all(string = step04,
                                     pattern = "_valencia_",
                                     replacement = as.character(element_valence))

  # 5) "Simplificacion de los subindices en el oxido"
  coef3_paso4 <- the_solver$coef3[4]
  coef3_paso5 <- the_solver$coef3[5]
  mdc_paso4 <- coef3_paso4

  dt_cambio <- coef3_paso4 != coef3_paso5

  step05 <- c("No se realiza simplificación en los subíncides.")
  if(dt_cambio) step05 <- c("Se realiza simplificación en los subíndices.",
                           "El máximo común dividor es _mcd_paso4_.",
                           "El coeficiente del óxido ahora es _mdc_paso4_.")
  step05 <- paste0(step05, collapse = "\n")
  step05 <- stringr::str_replace_all(string = step05,
                                     pattern = "_mdc_paso4_",
                                     replacement = as.character(mdc_paso4))

  # 6) "Balanceo del oxigeno"
  coef3_paso5 <- the_solver$coef3[5]
  coef3_paso6 <- the_solver$coef3[6]
  coef2_paso5 <- the_solver$coef2[5]
  coef2_paso6 <- the_solver$coef2[6]
  sub01 <- the_solver$subOP[5]

  dt_igual_coef03 <- coef3_paso5 == coef3_paso6
  dt_igual_coef02 <- coef2_paso5 == coef2_paso6
  dt_igual_all <- (dt_igual_coef02 + dt_igual_coef03) == 2

  step06 <- c("")
  if(dt_igual_all) step06 <- c("El Oxígeno está balanceado.") else
    step06 <- c("En el paso 5 el Oxígeno no estaba balanceado.",
           "Balanceamos colocando el subíndice del Oxígeno en reactivos (2) como
           coeficiente del óxido y el subíndice del Oxígeno en produtos (_sub01_) como
           coeficiente del Oxígeno en reactivos.")
  step06 <- paste0(step06, collapse = "\n")
  step06 <- stringr::str_replace_all(string = step06,
                                     pattern = "_sub01_",
                                     replacement = as.character(sub01))


  # 7) "Balanceo del elemento"
  step07 <- c("Se realiza solo modificando el coeficiente del elemento en reactivos.")

  coef1_paso6 <- the_solver$coef1[6]
  coef1_paso7 <- the_solver$coef1[7]
  dt_igual_coef01 <- coef1_paso6 == coef1_paso7
  nuevo_coef1 <- coef1_paso7

  if(dt_igual_coef01) step07 <- c(step07, "El _elemento_ ya se encuentra balanceado.",
                                  "No es necesario modificar el coeficiente.") else
                                    step07 <- c(step07, "El _elemento_ no está desbalanceado en el paso 6.",
                                                "Se coloca '_nuevo_coef1_' como coeficiente del _elemento_ en reactivos para balancearlo.")
  step07 <- paste0(step07, collapse = "\n")
  step07 <- stringr::str_replace_all(string = step07,
                                     pattern = "_elemento_",
                                     replacement = chem_name)
  step07 <- stringr::str_replace_all(string = step07,
                                     pattern = "_nuevo_coef1_",
                                     replacement = as.character(nuevo_coef1))

  # 8) "Simplificacion de los coeficientes"
  all_coef_paso07 <- c(the_solver$coef1[7], the_solver$coef2[7], the_solver$coef3[7])
  all_coef_paso08 <- c(the_solver$coef1[8], the_solver$coef2[8], the_solver$coef3[8])
  mcd_final <- all_coef_paso07[1] / all_coef_paso08[1]
  dt_igual <- mcd_final == 1

  step08 <- c("")
  if(dt_igual)  step08 <- c("No es posible simplificar los coeficientes.") else
    step08 <- c("Los coeficientes se simplifican por _mdf_final_.")

  step08 <- c(step08, "Se tiene la ecuación química balanceada y coeficientes mínimos!!!")
  step08 <- paste0(step08, collapse = "\n")
  step08 <- stringr::str_replace_all(string = step08,
                                     pattern = "_mdf_final_",
                                     replacement = as.character(mcd_final))

  TC_Text_Oxyde_Particular <- list()
  TC_Text_Oxyde_Particular[[1]] <- step01
  TC_Text_Oxyde_Particular[[2]] <- step02
  TC_Text_Oxyde_Particular[[3]] <- step03
  TC_Text_Oxyde_Particular[[4]] <- step04
  TC_Text_Oxyde_Particular[[5]] <- step05
  TC_Text_Oxyde_Particular[[6]] <- step06
  TC_Text_Oxyde_Particular[[7]] <- step07
  TC_Text_Oxyde_Particular[[8]] <- step08
  text_oxyde_particular <- do.call(rbind.data.frame, TC_Text_Oxyde_Particular)

  entrega_mod <- paste0("$", output[["Level06_LaTeX02"]][,1],  "$")
  # entrega_mod <- paste0(" withMathJax(", entrega_mod, ")")
  # entreda_mod[1] <- 'You do not see me initially: $$e^{i \\pi} + 1 = 0$$'
  # entrega_mod <- rep("$$e^{i \\pi} + 1 = 0$$", 8)

  output[[10]] <- cbind.data.frame(entrega_mod,
                                  text_oxyde_general,
                                  text_oxyde_particular)

  colnames( output[[10]]) <- c("Steps", "General", "Particular")
  names(output)[10] <- "format02_oxyde"


  #####################################################
  output[[11]] <- Take_Nomenclature_Oxyde(DataTC_04_Oxyde,
                                         chem_symbol,
                                         element_valence,
                                         gas_status_element,
                                         language)[[1]]

  names(output)[11] <- "Nomenclature_Oxyde_01"

  #################################################################
  output[[12]] <-  Take_Nomenclature_Oxyde(DataTC_04_Oxyde,
                                           chem_symbol,
                                           element_valence,
                                           gas_status_element,
                                           language)[[2]]

  names(output)[12] <- "Nomenclature_Oxyde_02"
  #######################################################


  #####################################################

  materia_prima <- output[[1]]

  the_cols <- c("Elemento", "Reactivo", "Producto", "Balance")
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
    dt_igual01 <- the_output[1,2] == the_output[1,3]
    if(dt_igual01) the_output[1,4] <- "Si" else the_output[1,4] <- "No"

    # El oxigeno
    the_output[2,2] <- eval(parse(text = the_output[2,2]))
    the_output[2,3] <- eval(parse(text = the_output[2,3]))
    dt_igual02 <- the_output[2,2] == the_output[2,3]
    if(dt_igual02) the_output[2,4] <- "Si" else the_output[2,4] <- "No"

    return(the_output)
  })

  balance_rejunte <- lapply(1:length(balance_bruto), function(x){

    elegida_bruto <- balance_bruto[[x]]
    elegida_suma <- balance_suma[[x]]
    the_output <- balance_modelo

    # El elemento
    the_output[1,2] <- paste0(elegida_bruto[1,2], " = ", elegida_suma[1,2])
    the_output[1,3] <- paste0(elegida_bruto[1,3], " = ", elegida_suma[1,3])
    the_output[1,4] <- elegida_suma[1,4]

    # El oxigeno
    the_output[2,2] <- paste0(elegida_bruto[2,2], " = ", elegida_suma[2,2])
    the_output[2,3] <- paste0(elegida_bruto[2,3], " = ", elegida_suma[2,3])
    the_output[2,4] <- elegida_suma[2,4]
    return(the_output)
  })


  output[[13]] <- balance_rejunte

  names(output)[13] <- "Balance_Oxyde"


  # Final Return
  return(output)
}



