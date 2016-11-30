
require(shinydashboard)
require(shiny)
require(rCharts)
require(DT)

#### PARAMS
list.league <- list("Ligue 1"="F1",
                    "Ligue 2"="F2",
                    "Premiere League"="E0",
                    "Ligua"="SP1",
                    "Bundeliga"="D1"
                    )


shinyUI(
  fluidPage(

  headerPanel("Supa Bet"),
  
  # Barre de menu
  sidebarPanel(width = 2,
               
               actionButton(inputId = 'download_datas',label = "Download datas"),
               selectInput("league", label = h2("League"), 
                           choices = list.league, 
                           selected = 1),               
               htmlOutput('selectHome'),
               htmlOutput('selectAway')
              
        ), #end sidebar
  
  # Corps
  mainPanel(
             column(width = 5,
                    showOutput('graph_but_home','highcharts')
                    ), # end colonne
             
            column(width = 5,                              
                   showOutput('graph_but_away','highcharts')
                    )
           
            ) # end main panel
    
    ) # end Page with sidebar
  ) #end shiny UI

