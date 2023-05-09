


#' Create DataTC 02 - PeriodicTable
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
#' if(FALSE) Create_DataTC_01_PeriodicTable()
Create_DataTC_01_PeriodicTable <- function(){

  input_obj_name <-  ""
  input_folder <- "./data-raw/OriginalData/"
  input_file <- "PeriodicTable_eng.csv"
  input_path <- paste0(input_folder, input_file)

  input_translate_folder <- "./data-raw/OriginalData/Translate/"
  all_languages <- list.files(input_translate_folder)
  all_languages <- c("eng", all_languages)

  # Info Output
  output_obj_name <-  "DataTC_01_PeriodicTable"
  output_folder <- "./data-raw/Output/"
  output_file <- paste0(output_obj_name,"_", all_languages, ".csv")
  output_path <- paste0(output_folder, output_file)

  data_input <- utils::read.csv(file = input_path,
                                sep = ";", dec=".",
                                stringsAsFactors = FALSE,
                                header = T)



  data_output <- sapply(all_languages, function(x){

    # Spacial Case: english
    if(x == "eng") return(data_input)

    # Other Languages
    data_output <- data_input

    one_translate_folder <- paste0(input_translate_folder, x, "/")
    all_translate_files <- list.files(one_translate_folder)
    all_path_translate <- paste0(one_translate_folder, all_translate_files)

    for(k1 in 1:length(all_path_translate)){

      selected_language <- x
      selected_file <- all_translate_files[k1]
      selected_path <- all_path_translate[k1]
      selected_var <- strsplit(selected_file, "_")[[1]][1]

      the_traduction <- utils::read.csv(file = selected_path,
                                        sep = ";", dec = ".",
                                        stringsAsFactors = F,
                                        header = T)



      old_categories <- the_traduction[,selected_var]
      new_categories <- the_traduction[,selected_language]

      for(k2 in 1:length(old_categories)){

        dt_old <- data_output[,selected_var] == old_categories[k2]
        data_output[,selected_var][dt_old] <- new_categories[k2]

      }

    }


    # Final Return
    return(data_output)

  },simplify = F, USE.NAMES = T)

# names(data_output)

  # Save the list as a object for ThugChemR
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
                           file = selected_output_path, col_names = T,
                           delim = ";")
  }



}



