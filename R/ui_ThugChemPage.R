
#' ThugChemPage - UI
#'
#' @return sdfsd
#' @export
#'
#' @examples
#' if(FALSE) ui_ThugChemPage()
ui_ThugChemPage <- function(){

ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "Chem Solver"),
  shinydashboard::dashboardSidebar(),
  shinydashboard::dashboardBody(
    tags$head(
    tags$style(type='text/css',
             ".nav-tabs {font-size: 150%} ")),
    shiny::h1("Chem Solver - Óxidos"), shiny::br(),
    div(
    shiny::fluidRow(
      shiny::column(4,
                    shiny::selectInput(inputId = "symbol",
                                       label = "Símbolo Químico",
                                       choices = ThugChemR::DataTC_02_Elements[["esp"]]$ElementSelector01),
                    shiny::uiOutput("ui_valence")
      ),
      shiny::column(6,
                    fluidRow(
                    column(4, colourpicker:: colourInput("bg_col", "Color de Fondo", "#F2DD1F")),
                    column(4, colourpicker:: colourInput("text_col01", "Texto General", "#2414B3")),
                    column(4, colourpicker:: colourInput("text_col02", "Texto Específico", "#F520CE"))
                    ),
                    shiny::actionButton("goButton", "Resolver!", class = "btn-success",
                                        style="color: #fff; background-color: #337ab7;
                                        border-color: #2e6da4; font-size:130%"),
                    shiny::p("Click en el botón para ejecutar la resolución.")
                    # shiny::p("Click the button to update the value displayed in the main panel.")
      )
    ), style = "font-size:150%") ,
    br(), br(),
    shiny::uiOutput("show_resolution")

  )
)
  return(ui)

}
