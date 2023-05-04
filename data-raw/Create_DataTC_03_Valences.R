
# Esta funcion lo que hace es tormar la Tabla Periodica, y detallar
# varias cosas relacionadas a los elementos quimicos.
# Por ejem

Create_DataTC_03_Valences <- function(){

  input_obj_name <-  "DataTC_02_Elements"
  input_folder <- "./data-raw/"
  input_file <- paste0(input_obj_name, ".csv")
  input_path <- paste0(input_folder, input_file)

  output_obj_name <-  "DataTC_03_Valences"
  output_folder <- "./data-raw/"
  output_file <- paste0(output_obj_name, ".csv")
  output_path <- paste0(output_folder, output_file)

  data_input <- read.csv(file = input_path, sep = ";", dec=".")


  selected_cols_01 <- 1:11
  selected_cols_02 <- max(selected_cols_01 + 1) : ncol(data_input)
  seccion01 <- data_input[, selected_cols_01]
  seccion02 <- data_input[, selected_cols_02]


  order_row <- seccion01$Order
  count_valences <- seccion01$AmountOfValences
  original_valences <- strsplit(seccion01$Valence, ",")
  order_valences <- lapply(original_valences, function(x){as.numeric(as.factor(x))})

  # New Position!
  new_position <- rep(order_row, count_valences)


  vector_original_valences <- unlist(original_valences)
  vector_order_valences <- unlist(order_valences)
  vector_general_new_order <- 1:length(vector_order_valences)

  part01 <- seccion01[new_position,]
  part02 <- cbind.data.frame(vector_general_new_order,
                             vector_order_valences,
                             vector_original_valences)

  colnames(part02) <- c("OrderGeneralValences", "OrdenOnElement", "EachValence")
  part03 <- seccion02[new_position, ]


  # All Original Information together
  data_output <- cbind.data.frame(part01, part02, part03)
  rownames(data_output) <- 1:nrow(data_output)


  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  # Any change on the data set must the put here!
  # Por example: if same valences of same element is as metal and other of the
  # same element is non-metal.

  new_columns <- list()

  # # # Oxide Status
  # We must define for each valence on each element if its avairable for to be
  # an oxide or not. The new vairable will be named 'status_oxyde'.
  # Only noble gas cant be oxyde!
  new_columns[["Status_oxyde"]] <- !data_output$Type_NobleGas




  # Final Armed
  columns_pack <- do.call(cbind.data.frame, new_columns)
  data_output <- cbind.data.frame(data_output, columns_pack)


  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



  assign(output_obj_name, data_output)


  # readr::write_csv(x = data_output,file = output_path, )
  utils::write.table(x = data_output, file = output_path, dec = ".",
                     sep = ";", col.names = T, row.names = F)

  gen_sentence <- "usethis::use_data(_ObjName_, overwrite = TRUE)"
  the_sentence <- gsub("_ObjName_", output_obj_name, gen_sentence)

  eval(parse(text = the_sentence))

}


# Create_DataTC_03_Valences()
