

#' Create PackTC 04 - Oxyde
#'
#' @return Generates a list where each element is a list with everything
#' related to the resolution of a particular oxide. Each item in the list is
#' named with:
#'   - Order number of the oxide in DataTC_04_Oxyde, with 3 digits.
#'   - Element and Valencia
#'   - Chemical formula of oxide
#' Example of the nomenclature: 001_H1_H2O1.
#' @export
#'
#' @examples
#' if(FALSE) Create_PackTC_04_Oxyde()
Create_PackTC_04_Oxyde <- function(){

  # Input Details
  input_obj_name <-  "DataTC_04_Oxyde"
  input_folder <- "./data-raw/"
  input_file <- paste0(input_obj_name, ".csv")
  input_path <- paste0(input_folder, input_file)

  # Output Details
  output_obj_name <-  "PackTC_04_Oxyde"
  output_folder <- "./data-raw/"
  output_file <- paste0(output_obj_name, ".RData")
  output_path <- paste0(output_folder, output_file)

  data_input <- utils::read.csv(file = input_path, sep = ";", dec=".")

  pack_output <- list()

  max_digits <- floor(log10(nrow(data_input))) + 1

  for(x in 1:nrow(data_input)){

    chem_symbol <- data_input$Symbol[x]
    element_valence <- data_input$EachValence[x]
    gas_status_element <- data_input$State_Gas[x]

    resolution_complite <- Resolution_Oxyde(chem_symbol = chem_symbol,
                                            element_valence = element_valence,
                                            gas_status_element = gas_status_element)


    the_atomic_number <- data_input$Order[x]
    the_order <- stringr::str_pad(string = the_atomic_number,
                                  width = max_digits,
                                  side = "left",
                                  pad = "0")

    chem_formule <-  resolution_complite$ChemFormule

    final_name <- paste0("Oxyde_", the_order, "_",
                         chem_symbol, element_valence, "_",
                         chem_formule)

    pack_output[[final_name]] <- resolution_complite

  }


  assign(output_obj_name, pack_output)

  # save()
  # readr::write_csv(x = data_output,file = output_path, )
  # utils::write.table(x = data_output, file = output_path, dec = ".",
  #                    sep = ";", col.names = T, row.names = F)
  gen_sentence01 <- "save(_ObjName_, file = '_OutputPath_')"
  the_sentence01 <- gsub("_ObjName_", output_obj_name, gen_sentence01)
  the_sentence01 <- gsub("_OutputPath_", output_path, the_sentence01)
  eval(parse(text = the_sentence01))

  gen_sentence02 <- "usethis::use_data(_ObjName_, overwrite = TRUE)"
  the_sentence02 <- gsub("_ObjName_", output_obj_name, gen_sentence02)
  eval(parse(text = the_sentence02))



}


