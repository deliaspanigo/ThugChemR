

#' Create DataTC 02 - Elements
#'
#' @return It generates a data.frame where each row is an element of
#' the periodic table to which more columns have been added that
#' will be used for different utilities.
#' For example, the 'Status_oxyde' column will allow you to filter the
#' database to only have available elements that are susceptible to
#' forming oxides.
#' @export
#'
#' @examples
#' if(FALSE) Create_DataTC_02_Elements()
Create_DataTC_02_Elements <- function(){

  input_obj_name <-  "DataTC_01_PeriodicTable"
  input_folder <- "./data/"
  input_file <- paste0(input_obj_name, ".rda")
  input_path <- paste0(input_folder, input_file)

  # Importamos el objeto "DataTC_01_PeriodicTable"
 load(input_path)

  output_obj_name <-  "DataTC_02_Elements"
  output_folder <- "./data-raw/Output/"

  all_languages <- names(DataTC_01_PeriodicTable)

  output_file <- paste0(output_obj_name,"_", all_languages,".csv")
  output_path <- paste0(output_folder, output_file)


  # data_input <- utils::read.csv(file = input_path, sep = ";", dec=".")
  # data_input <- ThugCHemR::DataTC_01_PeriodicTable[["en"]]
  data_input <- DataTC_01_PeriodicTable[["eng"]]

  new_columns <- list()

  new_columns[["Type_Metal"]] <- data_input$Type == "Metal"
  new_columns[["Type_NonMetal"]] <- data_input$Type == "Non-metal"
  new_columns[["Type_Metalloide"]] <- data_input$Type == "Metalloide"
  new_columns[["Type_NobleGas"]] <- data_input$Type == "Noble gas"

  new_columns[["State_Solid"]]	<- data_input$State == "Solid"
  new_columns[["State_Liquid"]]	<- data_input$State == "Liquid"
  new_columns[["State_Gas"]]	<- data_input$State == "Gas"
  new_columns[["State_Dude"]] <- data_input$State == "???"
  new_columns[["Subtype_Halogen"]] <- data_input$Subtype == "Halogens"




  columns_pack <- do.call(cbind.data.frame, new_columns)


  data_output <- sapply(all_languages, function(x){

    ### x <- "esp"

    ElementSelector01 <- paste0(stringr::str_pad(DataTC_01_PeriodicTable[[x]]$Order, 3, "left"), " - ",
                                stringr::str_pad(DataTC_01_PeriodicTable[[x]]$Symbol, 2, "right"), " - ",
                                        DataTC_01_PeriodicTable[[x]]$Name)

    the_output <- cbind.data.frame(DataTC_01_PeriodicTable[[x]],
                     columns_pack, ElementSelector01)

    return(the_output)

  }, simplify = F, USE.NAMES = T)


  assign(output_obj_name, data_output)
  gen_sentence <- "usethis::use_data(_ObjName_, overwrite = TRUE)"
  the_sentence <- gsub("_ObjName_", output_obj_name, gen_sentence)
  eval(parse(text = the_sentence))

  # Save Each Table as .csv file
  for(x in 1:length(all_languages)){

    selected_output_path <- output_path[x]
    selected_language <- all_languages[x]

    # readr::write_csv(x = data_output,file = output_path, )
    # utils::write.table(x = data_output[[selected_language]],
    #                    file = selected_output_path,
    #                    dec = ".",
    #                    sep = ";", col.names = T, row.names = F,
    #                    fileEncoding = "UTF-8")


    readr::write_excel_csv(x = data_output[[selected_language]],
                           file = selected_output_path,
                           col_names = T,
                           delim = ";")
  }



}



