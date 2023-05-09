

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
#' if(FALSE) Create_ExtraDataTC_04_Oxyde()
Create_ExtraDataTC_04_Oxyde <- function(){

  input_obj_name <- "ExtraDataTC_04_Oxyde"
  input_folder <- "./data-raw/ExtraData/"

  all_files <- list.files(input_folder)
  dt_special <- grepl(input_obj_name, all_files)
  special_files <- all_files[dt_special]
  special_path <- paste0(input_folder, special_files)

  all_languages <- sapply(special_files, function(x){

    recorte <- strsplit(x, "_")[[1]]
    recorte <- recorte[length(recorte)]
    recorte <- strsplit(recorte, ".csv")[[1]]


  })
  names(all_languages) <- all_languages

  names(special_path) <- all_languages

  # Output Details
  output_obj_name <-  input_obj_name



  data_output <- sapply(all_languages, function(x){


    data_input <- utils::read.csv(file = special_path[x],
                                  sep = ";", dec=".",
                                  stringsAsFactors = FALSE,
                                  header = T)

    return(data_input)


  }, simplify = F, USE.NAMES = T)



  assign(output_obj_name, data_output)
  gen_sentence <- "usethis::use_data(_ObjName_, overwrite = TRUE)"
  the_sentence <- gsub("_ObjName_", output_obj_name, gen_sentence)
  eval(parse(text = the_sentence))





}



