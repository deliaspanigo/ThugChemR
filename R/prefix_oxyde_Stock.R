


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
  the_prefix[["eng"]] <- c("Mono",  "Di",  "Tri", "Tetra", "Penta", "Hexa", "Hepta")
  the_prefix[["esp"]] <- c("Mono",  "Di",  "Tri", "Tetra", "Penta", "Hexa", "Hepta")
  the_prefix[["ita"]] <- c("Mono",  "Di",  "Tri", "Tetra", "Penta", "Hexa", "Hepta")

  # Selection for on language
  selected_prefix <- the_prefix[[language]]




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
