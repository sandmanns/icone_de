library(shiny)
library(shinythemes)
library(DT)
library(stringi)
#install_github("dreamRs/shinyWidgets")
library(shinyWidgets)
library(networkD3)

shinyUI(fluidPage(
    theme = shinytheme("cerulean"),
    titlePanel(div("Infektionsketten in Krankenhäusern",
                   img(height = 43, width = 20, src = "white.png",class="pull-right"),
                   img(height = 43, width = 50, src = "UKM.png",class="pull-right")),
               windowTitle="ICONE"
    ),
    sidebarPanel(
      shinyjs::useShinyjs(),
      tabsetPanel(id="config",
                  tabPanel("Input",
                           br(),
                           column(4,offset = 10,actionButton('do_clear',"RESET",class = "btn-warning")),
                           br(),
                           h4("Input"),
                           radioButtons("ownData",label="",choices=c("Eigene Daten hochladen","Demo Daten laden"),
                                        selected = "Eigene Daten hochladen",inline = T),
                           br(),
                           uiOutput("inputFileUI"),
                           uiOutput("sepInputUI"),
                           br(),
                           uiOutput("inputFileUIb"),
                           uiOutput("sepInputUIb"),
                           br(),
                           actionButton('do_in',"Daten einlesen",class = "btn-primary"),
                           hr(),
                           h4(textOutput("columnUI0")),
                           tags$head(
                             tags$style(type="text/css", 
                                        "#inline label.control-label, #inline .selectize-control.single { 
         display: table-cell; 
         text-align: left; 
         vertical-align: middle; 
      } 
      #inline label.control-label {
        padding-right: 20px;
      }
      #inline .form-group { 
        display: table-row;
      }
      #inline .selectize-control.single div.item {
        padding-right: 150px;
      }")
                           ),
                           
                           tags$div(id = "inline", uiOutput("columnUI1")),
                           tags$div(id = "inline", uiOutput("columnUI2")),
                           tags$div(id = "inline", uiOutput("columnUI3")),
                           tags$div(id = "inline", uiOutput("columnUI4")),
                           tags$div(id = "inline", uiOutput("columnUI5")),
                           h6(textOutput("columnUI6")),
                           br(),
                           h4(textOutput("columnUI0b")),
                           tags$div(id = "inline", uiOutput("columnUI1b")),
                           tags$div(id = "inline", uiOutput("columnUI2b")),
                           tags$div(id = "inline", uiOutput("columnUI3b")),
                           tags$div(id = "inline", uiOutput("columnUI4b")),
                           tags$div(id = "inline", uiOutput("columnUI5b")),
                           h6(textOutput("columnUI6b")),
                           br(),
                           uiOutput("do_in2UI")
                           
                  ),
            tabPanel("Kontaktanalyse",
                     radioGroupButtons("FA_oder_Station",label = "",
                                       choices = c("Fachabteilungen","Stationen"),selected = "Fachabteilungen",
                                       status = "primary",justified = T,
                                       checkIcon = list(yes = icon("ok",
                                                                   lib = "glyphicon"))),
                     br(),
                     h4(textOutput("FA_intro")),
                     h4(textOutput("ST_intro")),
                     h3(textOutput("FA_add1a")),
                     uiOutput("FA_UI2"),
                     uiOutput("FA_UI2a"),
                     uiOutput("FA_UI2b"),
                     uiOutput("FA_UI2c"),
                     uiOutput("FA_UI3"),
                     uiOutput("FA_UI3b"),
                     
                     uiOutput("ST_UI2"),
                     uiOutput("ST_UI2a"),
                     uiOutput("ST_UI2b"),
                     uiOutput("ST_UI2c"),
                     uiOutput("ST_UI3"),
                     uiOutput("ST_UI3b"),
                     br(),
                     
                     h3(textOutput("FA_add2a")),
                     uiOutput("FA_UI4"),
                     uiOutput("FA_UI4b"),
                     uiOutput("FA_UI5"),
                     uiOutput("FA_UI5b"),
                     
                     uiOutput("ST_UI4"),
                     uiOutput("ST_UI4b"),
                     uiOutput("ST_UI5"),
                     uiOutput("ST_UI5b"),
                     br(),
                     
                     h3(textOutput("FA_add3a")),
                     fluidRow(
                       column(7,
                              uiOutput("FA_add4a")),
                       column(5,
                              uiOutput("FA_add5a"))
                     ), 
                     uiOutput("FA_add6a"),
                     fluidRow(
                         column(7,
                                uiOutput("FA_add7a")),
                         column(5,
                                uiOutput("FA_add8a"))
                     ),
                     fluidRow(
                         column(7,
                                uiOutput("FA_add9a")),
                         column(5,
                                uiOutput("FA_add10a"))
                     ),
                     
                     uiOutput("FA_add11a"),
                     br(),
                     uiOutput("FA_button1"),
                     uiOutput("ST_button1"),
                     
                     br(),
                     br(),
                     uiOutput("FA_erweitert_UI2"),
                     br(),
                     br(),
                     uiOutput("FA_erweitert_UI3"),
                     br()
            )
        )
    ),
    mainPanel(
      shinyjs::useShinyjs(),
      tabsetPanel(id="main",
                  tabPanel("Log",
                           h3("Log"),
                           div(id = "text")
                  ),
                  tabPanel("Ergebnisse",
                    h4(htmlOutput("text_analyse1")),
                    h5(htmlOutput("text_analyse2")),
                    #plotOutput("plotBasic",width=2,height=2),
                    
                    forceNetworkOutput("force",width = 1500,height = 1500)
                  )
      )
      )
))



