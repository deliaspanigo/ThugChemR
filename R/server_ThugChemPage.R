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
  language <- shiny::reactive({ "esp" })


  symbol <- shiny::reactive({

    # input$symbol
    eleccion <- input$symbol

    el_simbolo <- strsplit(eleccion, " - ")[[1]][2]
    el_simbolo <- stringr::str_replace(string = el_simbolo, pattern = " ", replacement = "")
    # print(el_simbolo)
    el_simbolo

  })

  all_valence <- shiny::reactive({

    the_symbol <- symbol()

    dt_valence <- ThugChemR::DataTC_03_Valences[["eng"]]$Symbol == the_symbol
    all_valence <- ThugChemR::DataTC_03_Valences[["eng"]]$SelectedValence[dt_valence]
    all_valence

  })

  output$ui_valence <- shiny::renderUI({

    shiny::radioButtons(inputId = "valence", label = "Valencia",
                       choices = all_valence(),
                       selected = all_valence()[1])


  })

  selected_valence <- shiny::reactive({ input$valence})

  gas_status_element <- shiny::reactive({

    dt_symbol <- ThugChemR::DataTC_01_PeriodicTable[[language()]]$Symbol == symbol()
    the_status <- ThugChemR::DataTC_02_Elements[[language()]]$State_Gas[dt_symbol]
    the_status
    # cat("the_status: ", the_status, "\n")
  })



  capsule <-  shiny::eventReactive(input$goButton, {

    if(is.null(symbol())) return(NULL)
    if(is.null(selected_valence())) return(NULL)
    if(is.null(gas_status_element())) return(NULL)
    if(is.null(language())) return(NULL)

    armed <- list(symbol(),
                  as.numeric(as.character(selected_valence())),
                  gas_status_element(),
                  language())
    armed
  })

  output$plotPack01 <- shiny::renderPlot({

    if(is.null(capsule())) return(NULL)

    chem_symbol <- capsule()[[1]]
    the_valence <- capsule()[[2]]
    language <- capsule()[[4]]

    ThugChemR::Plot_PackTC_04_Oxyde01(chem_symbol = chem_symbol,
                                    element_valence = the_valence,
                                    language = language)

    # })
  }, height = 800, width = 1000)

  output$plotPack02 <- shiny::renderPlot({

    if(is.null(capsule())) return(NULL)

    chem_symbol <- capsule()[[1]]
    element_valence <- capsule()[[2]]
    language <- capsule()[[4]]



    Plot_PackTC_04_Oxyde02(chem_symbol,
                           element_valence,
                           verbose = TRUE,
                           language)

    # })
  })


  Resol02_oxyde <- shiny::reactive({
    if(is.null(capsule())) return(NULL)

    chem_symbol <- capsule()[[1]][1]
    the_valence <- capsule()[[2]][1]
    gas_status_element <- capsule()[[3]][1]
    language <- capsule()[[4]][1]
    fusion <- paste0("_",chem_symbol, the_valence, "_")
    #
    # chem_symbol <- "H"
    # the_valence <- 1
    # gas_status_element <- TRUE
    # language <- "esp"
    # # # NOTAAAAA # # #
    # Esto hay que cambiarlo, para que no lo calcule, sino que
    # lo tome directamente de PackTC_04_Oxyde !!!!!!!!!!!
    aca <- names(PackTC_04_Oxyde[[language]])
    dt_este <- grepl(pattern = fusion, x = aca)
    el_elegido <- aca[dt_este]

    ThugChemR::PackTC_04_Oxyde[[language]][[el_elegido]][["format02_oxyde"]]

    # ThugChemR::Resolution_Oxyde(chem_symbol = chem_symbol,
    #                             element_valence = the_valence,
    #                             gas_status_element = gas_status_element,
    #                             language = language)[["format02_oxyde"]]


  })

  all_posibilities <- reactive({

    names(PackTC_04_Oxyde[["eng"]])

  })

  the_selected <- reactive({

    if(is.null(capsule())) return(NULL)

    chem_symbol <- capsule()[[1]]
    the_valence <- capsule()[[2]]
    language <- capsule()[[4]]

    armado <- paste0("_", chem_symbol, the_valence, "_")

    dt_pos <- grepl(pattern = armado, x = all_posibilities())
    all_posibilities()[dt_pos]

  })

  special_selection <- shiny::reactive({

    the_selected <- the_selected()
    language <- language()

    # selected_pack_oxyde()[["Balance_Oxyde"]]
    PackTC_04_Oxyde[[language]][[the_selected]][["Balance_Oxyde"]]
  })

  special_selection02 <- reactive({

    the_selected <- the_selected()
    language <- language()
    PackTC_04_Oxyde[[language]][[the_selected]][["Level06_LaTeX02"]]
  })
  max_table <- reactive({nrow(special_selection())})

  # aca hayq ue hacer alguna magia para no tener que poner el 8
  # y que tome max_table() directamente
  observe({
    lapply(1:8, function(x) {

      output[[paste0("table_", x)]] <- renderTable({ special_selection()[[x]] })
    })
  })

  # Texto Dinamico
  output$tablas_dinamico <- shiny::renderUI({
    # i <- 1
    # shiny::div(
    #   tableOutput(paste0("table_", i))
    # )

    n <- 8
    sapply(1:n, function(i) {

      shiny::renderUI(
        div(
        shiny::fluidRow(
          shiny::column(6, h2(shiny::withMathJax(Resol02_oxyde()[i,1]))),
          shiny::column(2),
          shiny::div(shiny::column(4, tableOutput(paste0("table_", i))), style = "font-size:150%")
        ), br())
      )

  })
  })


  # Texto Dinamico
  output$newtabs <- shiny::renderUI({

    # tabs_n <- lapply(paste("tabs ", 1:4, sep = ""), tabPanel)
    #do.call(tabsetPanel, tabs_n)



    # tabs_n <- lapply(paste("tabs ", 1:nrow(Resol02_oxyde()), sep = ""), tabPanel)
    # do.call(tabsetPanel, tabs_n)
    n <- nrow(Resol02_oxyde())
    # tabs_n <- paste("tabsA ", 1:n, sep = "")
    tabs_n <- paste("tabsA ", 1:n, sep = "")

    sapply(1:n, function(i){

      texto01 <- Resol02_oxyde()[i,2]
      texto02 <- Resol02_oxyde()[i,2]


      shiny::renderUI({
        shiny::div(
          shiny::fluidRow(
            # column(4, withMathJax(Resol02_oxyde()[i,1])),
            shiny::column(3, h3(texto01)),
            shiny::column(6, h2(shiny::withMathJax(Resol02_oxyde()[i,1]))),
            shiny::column(3, h3(texto02)) # ,
           #  shiny::column(3, tableOutput(paste0("table_", i)))
          ), shiny::br()
        )
      })
      })



    })


    # tabs_n <- as.list( tabs_n )
    # do.call(h2, tabs_n)







  output$los_textos <- shiny::renderUI({
    # num <- as.integer(input$num)
    if(is.null(Resol02_oxyde())) return(NULL)



  })


  ##################################
  # aca hayq ue hacer alguna magia para no tener que poner el 8
  # y que tome max_table() directamente
  observe({
    lapply(1:8, function(x) {
      output[[paste0("plot_", x)]] <- renderPlot({

        plot(1:10, 1:10, xlab = "", ylab = "",
             axes = F, col = input$bg_col)

        # Plot region color
        rect(par("usr")[1], par("usr")[3],
             par("usr")[2], par("usr")[4],
             col = input$bg_col) # Color

        # Add a new plot
        par(new = TRUE)


        plot(1:10, 1:10, xlab = "", ylab = "",
             axes = F, col = input$bg_col,
             main = paste0("Paso ", x, " de ", "9 - Óxidos"),
             cex.main=2)

        # Ecuacion
        graphics::text(x = 5.5, y = 4,
             labels = latex2exp::TeX(special_selection02()[x,1]),
                                     cex = 3, pos = 1 )

        # General
        text(5.5, 10, Resol02_oxyde()[x,2], cex = 2, col = input$"text_col01", pos = 1 )

        # Especifico
        text(5.5, 8, Resol02_oxyde()[x,3], cex = 2, col = input$"text_col02", pos = 1 )

         })#, height = 800, width = 1000
    })
  })
#
#   # Grafico 9 agregado extra
#   observe({
#     lapply(9, function(x) {
#       output[[paste0("plot_", x)]] <- renderPlot({
#
#         plot(1:10, 1:10, xlab = "", ylab = "",
#              axes = F, col = input$bg_col)
#
#         # Plot region color
#         rect(par("usr")[1], par("usr")[3],
#              par("usr")[2], par("usr")[4],
#              col = input$bg_col) # Color
#
#         # Add a new plot
#         par(new = TRUE)
#
#
#         plot(1:10, 1:10, xlab = "", ylab = "",
#              axes = F, col = input$bg_col,
#              main = paste0("Paso ", x, " de ", "9 - Óxidos"),
#              cex.main=2)
#
#         # Ecuacion
#         graphics::text(x = 5.5, y = 4,
#                        labels = latex2exp::TeX(special_selection02()[x-1,1]),
#                        cex = 3, pos = 1 )
#
#         # General
#         text(5.5, 10, "Ecuación Final Balanceada", cex = 4, col = input$"text_col01", pos = 1 )
#
#         # Especifico
#         # text(5.5, 8, Resol02_oxyde()[x,3], cex = 2, col = input$"text_col02", pos = 1 )
#
#       })#, height = 800, width = 1000
#     })
#   })
  ############################
  observe({
    lapply(1:8, function(x) {
      output[[paste0("table2_", x)]] <- renderTable({ special_selection()[[x]] })
    })
  })

  # # Tabla 9 agregada extra
  # observe({
  #   lapply(9, function(x) {
  #     output[[paste0("table2_", x)]] <- renderTable({ special_selection()[[x-1]] })
  #   })
  # })

  ###############################

  # Solo solucion final
  output$SolucionFinal <- renderPlot({

    x <- 8

    plot(1:10, 1:10, xlab = "", ylab = "",
         axes = F, col = input$bg_col)

    # Plot region color
    rect(par("usr")[1], par("usr")[3],
         par("usr")[2], par("usr")[4],
         col = input$bg_col) # Color

    # Add a new plot
    par(new = TRUE)


    plot(1:10, 1:10, xlab = "", ylab = "",
         axes = F, col = input$bg_col,
         main = "",
         cex.main=2)

    # Ecuacion
    graphics::text(x = 5.5, y = 4,
                   labels = latex2exp::TeX(special_selection02()[x-1,1]),
                   cex = 3, pos = 1 )

    # General
    text(5.5, 10, "Ecuación Final Balanceada", cex = 4, col = input$"text_col01", pos = 1 )

    # Especifico
    # text(5.5, 8, Resol02_oxyde()[x,3], cex = 2, col = input$"text_col02", pos = 1 )

  })

  ##########################################################################
  # Texto Dinamico
  output$ploteo_dinamico <- shiny::renderUI({

    # n <- 8
    n <- 8
    sapply(1:n, function(i) {


      shiny::renderUI(

        shiny::fluidRow(
          shiny::column(8, plotOutput(paste0("plot_", i))),
          shiny::column(4, br(), br(),br(), br(),
                        div(tableOutput(paste0("table2_", i))), style = "font-size:150%")

      )
      )


    })
  })
  ########################################################



  output$show_resolution <- shiny::renderUI({
    # input$goButton

    div(
    shiny::tabsetPanel(
      shiny::tabPanel("Paso a paso", shiny::uiOutput("newtabs")),
      shiny::tabPanel("Pleoteo Dinamico", shiny::uiOutput("ploteo_dinamico")),
      shiny::tabPanel("Solución Final", shiny::plotOutput("SolucionFinal")),
      shiny::tabPanel("Una imagen", shiny::plotOutput("plotPack01"), br(),br(),br(),
                      br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                      br(),br(),br(),br()),
      shiny::tabPanel("Nomenclatura", shiny::plotOutput("plotPack02")),
      shiny::tabPanel("Pasos sin textos", shiny::uiOutput("tablas_dinamico"))


    )
    , style = "font-size:110%")

  })

  ######################
}
