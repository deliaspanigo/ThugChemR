

#' Create Data TC 04 - Oxyde
#'
#' @return It generates a data.frame where each row is an element of the
#' periodic table with a particular valence in such a way that each element
#' appears in rows as many times as the number of valences it has. We then have
#' a particular oxide in each row. Different columns have also been added,
#' such as "Chem_formula" that details the chemical formula of each oxide.
#' @export
#'
#' @examples
#' if(FALSE) Create_DataTC_04_Oxyde()
Create_DataTC_04_Oxyde <- function(){

  # Input Details
  input_obj_name <-  "DataTC_03_Valences"
  input_folder <- "./data-raw/"
  input_file <- paste0(input_obj_name, ".csv")
  input_path <- paste0(input_folder, input_file)

  # Output Details
  output_obj_name <-  "DataTC_04_Oxyde"
  output_folder <- "./data-raw/"
  output_file <- paste0(output_obj_name, ".csv")
  output_path <- paste0(output_folder, output_file)

  data_input <- utils::read.csv(file = input_path, sep = ";", dec=".")

  selected_cols_01 <- c("Order", "Symbol",  "Name", "AtomicNumber",
                        "Group", "Period",  "Type", "Subtype",
                        "State", "Valence", "AmountOfValences",
                        "OrderGeneralValences", "OrdenOnElement", "EachValence",
                        "Type_Metal", "Type_NonMetal", "Type_Metalloide",
                        "Type_NobleGas", "State_Solid", "State_Liquid",
                        "State_Gas", "State_Dude", "Subtype_Halogen",
                        "Status_oxyde")

  seccion01 <- data_input[selected_cols_01]

  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  # Any change on the data set must the put here!
  # Por example: if same valences of same element is as metal and other of the
  # same element is non-metal.

  new_columns <- list()

  # # # Oxide Status
  # We must define for each valence on each element if its avairable for to be
  # an oxide or not. The new vairable will be named 'status_oxyde'.
  # Only noble gas cant be oxyde!
  new_columns[["ChemFormule"]] <- sapply(1:nrow(seccion01), function(x){

    dt_oxyde <- seccion01$Status_oxyde[x]
    chem_formula <- "----"

    selected_symbol <- seccion01$Symbol[x]
    selected_valence <- seccion01$EachValence[x]
    selected_gas_status <- seccion01$State_Gas[x]

    if(dt_oxyde) {
      chem_formula <- Resolution_Oxyde(chem_symbol = selected_symbol,
                                       element_valence = selected_valence,
                                       gas_status_element = selected_gas_status)$ChemFormule
    }

    return(chem_formula)

  })




  # Final Armed
  columns_pack <- do.call(cbind.data.frame, new_columns)
  data_output <- cbind.data.frame(seccion01, columns_pack)


  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



  assign(output_obj_name, data_output)


  # readr::write_csv(x = data_output,file = output_path, )
  utils::write.table(x = data_output, file = output_path, dec = ".",
                     sep = ";", col.names = T, row.names = F)

  gen_sentence <- "usethis::use_data(_ObjName_, overwrite = TRUE)"
  the_sentence <- gsub("_ObjName_", output_obj_name, gen_sentence)

  eval(parse(text = the_sentence))

}



