#' Create DataTC 03 - Valences
#'
#' @return It generates a data.frame where each row is an element of the
#' periodic table with a particular valence in such a way that each element
#' appears in rows as many times as the number of valences it has.
#' Different columns have also been added.
#' @export
#'
#' @examples
#' if(FALSE) Create_DataTC_03_Valences()
#'
Create_DataTC_03_Valences <- function(){

  input_obj_name <-  "DataTC_02_Elements"
  input_folder <- "./data/"
  input_file <- paste0(input_obj_name, ".rda")
  input_path <- paste0(input_folder, input_file)

  # Importamos el objeto "DataTC_01_PeriodicTable"
  load(input_path)


  output_obj_name <-  "DataTC_03_Valences"
  output_folder <- "./data-raw/Output/"

  all_languages <- names(DataTC_02_Elements)

  output_file <- paste0(output_obj_name,"_", all_languages,".csv")
  output_path <- paste0(output_folder, output_file)


  data_output <- sapply(all_languages, function(x){

    # # # x <- "eng"

    # Reference Data Input : ENG
    data_input <- DataTC_02_Elements[[x]]

    # Take information from each element
    selected_cols_01 <- 1:11
    selected_cols_02 <- max(selected_cols_01 + 1) : ncol(data_input)
    seccion01 <- data_input[, selected_cols_01]
    seccion02 <- data_input[, selected_cols_02]


    # Generate some vector not for element, than for valence of each element
    order_row <- seccion01$Order
    count_valences <- seccion01$AmountOfValences
    original_valences <- strsplit(seccion01$Valence, ",")
    order_valences <- lapply(original_valences, function(x){as.numeric(as.factor(x))})

    # New Position!
    new_position <- rep(order_row, count_valences)

    # Part 01
    part01 <- seccion01[new_position,]


    # Part 02
    part02 <- list()
    part02[["OrderGeneralValences"]] <-  1:sum(count_valences)
    part02[["OrderValenceOnElement"]] <-  unlist(order_valences)
    part02[["SelectedValence"]] <- unlist(original_valences)

    part02[["RomanValence"]] <- as.character(utils::as.roman(part02[["SelectedValence"]]))
    part02[["RomanValence"]][is.na(part02[["RomanValence"]])] <- ""

    part02 <- do.call(cbind.data.frame, part02)




    # Part 03
    part03 <- seccion02[new_position, ]


    # All Original Information together
    the_output <- cbind.data.frame(part01, part02, part03)
    the_output[,1] <- 1:nrow(the_output)

    rownames(the_output) <- 1:nrow(the_output)


    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    # Any change on the data set must the put here!
    # Por example: if same valences of same element is as metal and other of the
    # same element is non-metal.

    new_columns <- list()

    # # # Oxide Status
    # We must define for each valence on each element if its avairable for to be
    # an oxide or not. The new vairable will be named 'status_oxyde'.
    # Only noble gas cant be oxyde!
    new_columns[["Status_oxyde"]] <- !the_output$Type_NobleGas

    # and the oxygen dont make and oxyde with other oxygen
    dt_oxygen <- the_output$Symbol == "O"
    new_columns[["Status_oxyde"]][dt_oxygen] <- FALSE


    # Final Armed
    columns_pack <- do.call(cbind.data.frame, new_columns)
    the_output <- cbind.data.frame(the_output, columns_pack)

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

