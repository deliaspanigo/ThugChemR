

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


  # Output Details
  output_obj_name <-  "PackTC_04_Oxyde"
  output_folder <- "./data-raw/Output/"

  all_languages <- names(ThugChemR::DataTC_04_Oxyde)

  # Unique .RData
  # Is a big big list with all about oxyde
  output_file <- paste0(output_obj_name,"_",".RData")
  output_path <- paste0(output_folder, output_file)


  data_output <- sapply(all_languages, function(y){

    # # # y <- "ita"

    data_input <- DataTC_04_Oxyde[[y]]

    pack_output <- list()

    max_digits <- floor(log10(nrow(data_input))) + 1

    for(x in 1:nrow(data_input)){

      chem_symbol <- data_input$Symbol[x]
      element_valence <- data_input$SelectedValence[x]
      element_valence <- as.numeric(as.character(element_valence))
      gas_status_element <- data_input$State_Gas[x]

      resolution_complite <- Resolution_Oxyde(chem_symbol = chem_symbol,
                                              element_valence = element_valence,
                                              gas_status_element = gas_status_element,
                                              language = y)


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

    return(pack_output)

  }, simplify = F, USE.NAMES = T)

  assign(output_obj_name, data_output)


  gen_sentence <- "usethis::use_data(_ObjName_, overwrite = TRUE)"
  the_sentence <- gsub("_ObjName_", output_obj_name, gen_sentence)
  eval(parse(text = the_sentence))

  ###################################################





}


