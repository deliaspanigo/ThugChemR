


#' Title
#'
#' @param language Select your language.
#'
#' @return A vector with all prefix for each valence of each elemento.
#' @export
#'
#' @examples prefix_oxyde_Classic(language = "esp")
prefix_oxyde_Classic <- function(language = "esp"){

  # # # language = "esp"
  all_languages <- c("eng", "esp", "ita")
  if(sum(all_languages == language) == 0) language = "eng"

  # Internal Prefix
  the_prefix <- list()  #    1       2        3       4     5
  the_prefix[["eng"]] <- c("Hipo",  "Per",  "Super")
  the_prefix[["esp"]] <- c("Hipo",  "Per",  "Super")
  the_prefix[["ita"]] <- c("Hipo",  "Per",  "Super")

  # Selection for on language
  selected_prefix <- the_prefix[[language]]

  # Diferents case of resolution
  case_resolution <- list()
  case_resolution[[1]] <- c("")
  case_resolution[[2]] <- c("", "")
  case_resolution[[3]] <- c(selected_prefix[1], "", "")
  case_resolution[[4]] <- c(selected_prefix[1], "", "", selected_prefix[2])
  case_resolution[[5]] <- c(selected_prefix[1], "", "",
                            selected_prefix[2], selected_prefix[5])

  all_prefix <- sapply(1:nrow(ThugChemR::DataTC_03_Valences[[language]]), function(x){

    data_input <- ThugChemR::DataTC_03_Valences[[language]]
    dt_oxyde <- data_input$Status_oxyde[x]
    my_prefix <- "----"
    if(!dt_oxyde) return(my_prefix)


    selected_AmountOfValences <- as.numeric(as.character(data_input$AmountOfValences[x]))
    selected_OrdenOnElement <- as.numeric(as.character(data_input$OrderValenceOnElement[x]))

    my_prefix <- case_resolution[[selected_AmountOfValences]][selected_OrdenOnElement]
    return(my_prefix)


  })

  return(all_prefix)
}
