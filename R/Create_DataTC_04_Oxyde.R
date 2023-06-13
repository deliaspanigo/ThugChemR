

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

  # Input 01 y 02
  input_obj_name01 <-  "DataTC_02_Elements"
  input_obj_name02 <-  "DataTC_03_Valences"

  input_folder <- "./data/"

  input_file01 <- paste0(input_obj_name01, ".rda")
  input_file02 <- paste0(input_obj_name02, ".rda")

  input_path01 <- paste0(input_folder, input_file01)
  input_path02 <- paste0(input_folder, input_file02)

  # Importamos el objeto "DataTC_02_Elements" y "DataTC_03_Valences"
  load(input_path01)
  load(input_path02)
  # # # # # # # # # # # # # # #

  # Input 03
  input_obj_name03 <-  "ExtraDataTC_04_Oxyde"
  input_folder03 <- "./data-raw/ExtraData/"
  input_file03 <- list.files(input_folder03)
  input_path03 <- paste0(input_folder03, input_file03)


  ExtraDataTC_04_Oxyde <- sapply(input_path03, function(x){
                            utils::read.csv(file = x,
                                            stringsAsFactors = FALSE,
                                            header = T,
                                            sep = ";",
                                            dec = ".")
    },simplify = F, USE.NAMES = T)

  names(ExtraDataTC_04_Oxyde) <- sapply(names(ExtraDataTC_04_Oxyde), function(x){
                aver <- strsplit(x, "_")[[1]]
                aver <- aver[length(aver)]

                strsplit(aver, ".csv")[[1]]

            }, simplify = T)

  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

  # Output Details
  output_obj_name <-  "DataTC_04_Oxyde"
  output_folder <- "./data-raw/Output/"

  all_languages <- names(DataTC_02_Elements)

  output_file <- paste0(output_obj_name,"_", all_languages,".csv")
  output_path <- paste0(output_folder, output_file)



  selected_cols_01 <- c("Order", "Symbol",  "Name", "AtomicNumber",
                        "Group", "Period",  "Type", "Subtype",
                        "State", "Valence", "AmountOfValences",
                        "OrderGeneralValences", "OrderValenceOnElement",
                        "SelectedValence", "RomanValence",
                        "Type_Metal", "Type_NonMetal", "Type_Metalloide",
                        "Type_NobleGas", "State_Solid", "State_Liquid",
                        "State_Gas", "State_Dude", "Subtype_Halogen",
                        "Status_oxyde")


  data_output <- sapply(all_languages, function(selected_language){

    # # # selected_language <- "eng"

    # Reference Data Input : ENG
    data_input <- DataTC_03_Valences[[selected_language]]
    seccion01 <- data_input[selected_cols_01]


    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    # Any change on the data set must the put here!
    # Por example: if same valences of same element is as metal and other of the
    # same element is non-metal.

    new_columns <- list()

    # # # Oxide Status
    # We must define for each valence on each element if its available for to be
    # an oxide or not. The new variable will be named 'status_oxyde'.
    # Only noble gas cant be oxyde!
    new_columns[[1]] <- sapply(1:nrow(seccion01), function(x){

      #### x <- 1
      dt_oxyde <- seccion01$Status_oxyde[x]
      chem_formula <- "----"

      selected_symbol <- seccion01$Symbol[x]
      selected_valence <- as.numeric(as.character(seccion01$SelectedValence[x]))
      selected_gas_status <- seccion01$State_Gas[x]

      if(dt_oxyde) {
        chem_formula <- Resolution_Oxyde(chem_symbol = selected_symbol,
                                         element_valence = selected_valence,
                                         gas_status_element = selected_gas_status,
                                         language = selected_language)$ChemFormule_pure
      }

      return(chem_formula)

    })
    new_columns[[1]][!data_input$Status_oxyde] <- "----"
    names(new_columns)[1] <- "ChemFormule_pure"

    # Classic Name
    # Este nombre es tomado directamente del archiv CSV del idioma
    # Esta info hay que agregarla al Help o documentacion en algun momento.
    new_columns[[2]] <- ExtraDataTC_04_Oxyde[[selected_language]]$Name_Classic_Oxyde
    names(new_columns)[2] <- "Name_Classic_Oxyde"


    new_columns[[3]] <- ExtraDataTC_04_Oxyde[[selected_language]]$Name_IUPAC_Oxyde
    names(new_columns)[3] <- "Name_IUPAC_Oxyde"


    new_columns[[4]] <- sapply(new_columns[["ChemFormule_pure"]], function(x){

      #  x <- new_columns[["ChemFormule_pure"]][1]
      # # # language = "esp"
      # all_languages <- c("eng", "esp", "ita")
      #if(sum(all_languages == language) == 0) language = "eng"

      # Internal Prefix
      the_prefix <- list()  #    1       2        3       4     5
      the_prefix[["eng"]] <- c("Mono",  "Di",  "Tri", "Tetra", "Penta", "Hexa", "Hepta")
      the_prefix[["esp"]] <- c("Mono",  "Di",  "Tri", "Tetra", "Penta", "Hexa", "Hepta")
      the_prefix[["ita"]] <- c("Mono",  "Di",  "Tri", "Tetra", "Penta", "Hexa", "Hepta")

      # Selection for on language
      selected_prefix <- the_prefix[["esp"]]


      metralla <- strsplit(x, "")[[1]]
      prueba01 <- suppressWarnings(as.numeric(as.character(metralla)))
      prueba01 <- na.omit(prueba01)
      if(length(prueba01) == 0) output <- "----" else
        if(length(prueba01) == 2) output <- selected_prefix[prueba01[1]]

      return(output)
    })
    new_columns[[4]][!data_input$Status_oxyde] <- "----"
    names(new_columns)[4] <- "amount01_oxyde_Stock"


    new_columns[[5]] <- sapply(new_columns[["ChemFormule_pure"]], function(x){

      #  x <- new_columns[["ChemFormule_pure"]][1]
      # Internal Prefix
      the_prefix <- list()  #    1       2        3       4     5
      the_prefix[["eng"]] <- c("Mono",  "Di",  "Tri", "Tetra", "Penta", "Hexa", "Hepta")
      the_prefix[["esp"]] <- c("Mono",  "Di",  "Tri", "Tetra", "Penta", "Hexa", "Hepta")
      the_prefix[["ita"]] <- c("Mono",  "Di",  "Tri", "Tetra", "Penta", "Hexa", "Hepta")

      # Selection for on language
      selected_prefix <- the_prefix[["esp"]]

      metralla <- strsplit(x, "")[[1]]
      prueba01 <- suppressWarnings(as.numeric(as.character(metralla)))
      prueba01 <- na.omit(prueba01)
      if(length(prueba01) == 0) output <- "----" else
        if(length(prueba01) == 2) output <- selected_prefix[prueba01[2]]

      return(output)
    })
    new_columns[[5]][!data_input$Status_oxyde] <- "----"
    names(new_columns)[5] <- "amount02_oxyde_Stock"


    # Esto de aca es una chanchada...
    # Le impone el espanion a todas las salidas.
    # Hay que ver despues como armamos esta parte para cada idioma. ######
    new_columns[[6]] <- paste0(new_columns[["amount01_oxyde_Stock"]],
                                                   "Óxido",
                                                   " de ",
                                                   new_columns[["amount02_oxyde_Stock"]],
                                                   data_input$Name)
    new_columns[[6]][!data_input$Status_oxyde] <- "----"
    names(new_columns)[6] <- "OxydeFullName_Stock"



    # Final Armed
    columns_pack <- do.call(cbind.data.frame, new_columns)
    the_output <- cbind.data.frame(seccion01, columns_pack)




    return(the_output)

  }, simplify = F, USE.NAMES = T)


  # Creamos "DataTC_04_Oxyde" como objeto
  assign(output_obj_name, data_output)


  gen_sentence <- "usethis::use_data(_ObjName_, overwrite = TRUE)"
  the_sentence <- gsub("_ObjName_", output_obj_name, gen_sentence)
  eval(parse(text = the_sentence))



  # Save Each Table as .csv file
  for(x in 1:length(all_languages)){

    selected_output_path <- output_path[x]
    selected_language <- all_languages[x]


    readr::write_excel_csv(x = data_output[[selected_language]],
                           file = selected_output_path,
                           col_names = T,
                           delim = ";")
  }

  ###############################################################3
  # Agregamos a ExtraDataTC_04_Oxyde al package
  usethis::use_data(ExtraDataTC_04_Oxyde, overwrite = TRUE)

}



