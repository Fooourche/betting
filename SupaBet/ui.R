
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
               htmlOutput('selectAway'),
               sliderInput("moy_mob", label = h4("moyenne sur :"), min = 1,max = 20, value = 3)
              
        ), #end sidebar
  
  # Corps
  mainPanel(
    tabsetPanel(
    tabPanel(title = "Goals",
        fluidRow(
                 h2("Goals"),
                 column(width = 6,
                        showOutput('graph_buts_match_home','highcharts'),
                        showOutput('graph_but_home_match','highcharts'),
                        showOutput('graph_but_home','highcharts'),
                        showOutput('graph_but_home_encaisse','highcharts')
                        ), # end colonne
                 
                column(width = 6,   
                       showOutput('graph_buts_match_away','highcharts'),
                       showOutput('graph_but_away_match','highcharts'),
                       showOutput('graph_but_away','highcharts'),
                       showOutput('graph_but_away_encaisse','highcharts')
                       )
          ), #end fluid row
    
        fluidRow(
                #h2("datas"),
                column(width = 6
#                        br(),
#                        br(),
#                        DT::dataTableOutput("dt_shoots_away")
                )
               )#end fluid row
              ), #end tabpanel
    
    tabPanel(title = "Shoots",
             fluidRow(height=800,
                      h2("Shoots"),
                     column(width = 6,                              
                          showOutput('graph_shoots_home','highcharts'),
                          showOutput('graph_shoots_home_target','highcharts')
                      ),
                     column(width = 6,   
                            showOutput('graph_shoots_away','highcharts'),
                            showOutput('graph_shoots_away_target','highcharts')
                      )
                     ) #end fluid row
            ), #end tabpanel
          tabPanel(title = "Corner",
                  fluidRow(height=800
                           
                           )
                  ), #end fluid row
          tabPanel(title = "Results",
                   fluidRow(height=800
                      
     
                     ) #end fluid row
            ) #end tabpanel
        ) #end tabsetpanel
      ) # end main panel
    
    ) # end Page with sidebar
  ) #end shiny UI

