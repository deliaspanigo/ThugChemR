#' ThugChemPage - Server
#'
#' @param input cosas
#' @param output cosas
#' @param session mas cosas
#'
#' @return asdas
#' @export
#'
#' @examples
#' if(FALSE) server_ThugChemPage
server_ThugChemPage <- function(input, output, session) {


  ###########
  symbol <- shiny::reactive({
    input$symbol
  })

  all_valence <- shiny::reactive({

    the_symbol <- symbol()

    dt_valence <- ThugChemR::DataTC_03_Valences$Symbol == the_symbol
    all_valence <- ThugChemR::DataTC_03_Valences$EachValence[dt_valence]
    all_valence

  })

  output$ui_valence <- shiny::renderUI({

    shiny::selectInput(inputId = "valence", label = "Valence",
                       choices = all_valence(),
                       selected = all_valence()[1])


  })

  selected_valence <- shiny::reactive({ input$valence})

  gas_status_element <- shiny::reactive({

    dt_symbol <- ThugChemR::DataTC_01_PeriodicTable_eng$Symbol == symbol()
    the_status <- ThugChemR::DataTC_02_Elements$State_Gas[dt_symbol]
    the_status
    # cat("the_status: ", the_status, "\n")
  })



  capsule <-  shiny::eventReactive(input$goButton, {

    if(is.null(symbol())) return(NULL)
    if(is.null(selected_valence())) return(NULL)
    if(is.null(gas_status_element())) return(NULL)

    armed <- list(symbol(),
                  as.numeric(as.character(selected_valence())),
                  gas_status_element())
    armed
  })

  output$plotPack <- shiny::renderPlot({

    if(is.null(capsule())) return(NULL)

    chem_symbol <- capsule()[[1]]
    the_valence <- capsule()[[2]]

    ThugChemR::Plot_PackTC_04_Oxyde(chem_symbol = chem_symbol,
                                    element_valence = the_valence)

    # })
  })



  Resol02_oxyde <- shiny::reactive({
    if(is.null(capsule())) return(NULL)

    chem_symbol <- capsule()[[1]][1]
    the_valence <- capsule()[[2]][1]
    gas_status_element <- capsule()[[3]][1]

    # chem_symbol <- "H"
    # the_valence <- 1
    # gas_status_element <- TRUE


    ThugChemR::Resolution_Oxyde(chem_symbol = chem_symbol,
                                element_valence = the_valence,
                                gas_status_element = gas_status_element)[["format02_oxyde"]]


  })

  # Texto Dinamico
  output$newtabs <- shiny::renderUI({

    # tabs_n <- lapply(paste("tabs ", 1:4, sep = ""), tabPanel)
    #do.call(tabsetPanel, tabs_n)

    # tabs_n <- lapply(paste("tabs ", 1:nrow(Resol02_oxyde()), sep = ""), tabPanel)
    # do.call(tabsetPanel, tabs_n)
    n <- nrow(Resol02_oxyde())
    tabs_n <- paste("tabsA ", 1:n, sep = "")

    sapply(1:n, function(i) {

      shiny::renderUI({
        shiny::div(
          shiny::fluidRow(
            # column(4, withMathJax(Resol02_oxyde()[i,1])),
            shiny::column(3, Resol02_oxyde()[i,2]),
            shiny::column(6, shiny::withMathJax(Resol02_oxyde()[i,1])),
            shiny::column(3, Resol02_oxyde()[i,3])
          ), shiny::br()
        )
      })

    })


    # tabs_n <- as.list( tabs_n )
    # do.call(h2, tabs_n)

  })



  output$format02_oxyde <- shiny::renderTable({

    if(is.null(Resol02_oxyde())) return(Resol02_oxyde())

    Resol02_oxyde()
    #
    # x <- rnorm(2)
    # y <- rnorm(2, 3)
    # tab <- data.frame(x = x, y = y)
    # rownames(tab) <- c("\\(\\alpha\\)",
    #                    "\\(\\beta\\)")
    # tab

  },
  include.rownames = TRUE,
  include.colnames = TRUE)



  output$los_textos <- shiny::renderUI({
    # num <- as.integer(input$num)
    if(is.null(Resol02_oxyde())) return(NULL)



  })

  # ui.R

  #tableOutput("table")

  output$show_resolution <- shiny::renderUI({
    # input$goButton

    tabsetPanel(
      shiny::tabPanel("En una imagen", shiny::plotOutput("plotPack")),
      shiny::tabPanel("En una imagen", shiny::uiOutput("newtabs")),
      shiny::tabPanel("En una matrix",
                      shiny::withMathJax(
                        shiny::helpText('You do not see me initially: $$1H_{1}+1O_{1} ------> 0H_{0}O_{0}$$')
                      ),
                      shiny::withMathJax(shiny::tableOutput("format02_oxyde"))),
      shiny::tabPanel("Como texto", "LA NADA 02"),
    )

  })

  ######################
}
