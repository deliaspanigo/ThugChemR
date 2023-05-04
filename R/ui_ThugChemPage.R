
#' ThugChemPage - UI
#'
#' @return sdfsd
#' @export
#'
#' @examples
#' if(FALSE) ui_ThugChemPage()
ui_ThugChemPage <- function(){

ui <- shiny::fluidPage(
  shiny::mainPanel(
    shiny::h1("Thug Chem"), shiny::br(),
    shiny::fluidRow(
      shiny::column(6,
                    shiny::selectInput(inputId = "symbol", label = "Symbol",
                                       choices = ThugChemR::DataTC_01_PeriodicTable_eng$Symbol),
                    shiny::uiOutput("ui_valence")
      ),
      shiny::column(4,
                    shiny::actionButton("goButton", "Go!", class = "btn-success"),
                    shiny::p("Click the button to update the value displayed in the main panel.")
      )
    ) ,

    shiny::uiOutput("show_resolution")

  )
)
  return(ui)

}
