
require(shiny)
require(shinydashboard)
require(rCharts)
require(reshape2)
require(DT)


shinyServer(function(input, output,session) {
  
  makeReactiveBinding("team")
  makeReactiveBinding("datas_bet")
  team <<- list("First","Two")
    
  #Recupere les donnees
  observe({
      if(input$download_datas>0)
      {
      datas_to_dl <- list(ligue1=c('http://www.football-data.co.uk/mmz4281/1516/F1.csv','data/F1.csv'),
                          all.leagues=c('http://football-data.co.uk/mmz4281/1516/data.zip','data/data.zip'))
      
      lapply(X = datas_to_dl,FUN = function(x) utils::download.file(url = x[1],destfile=x[2]))
      unzip ("data/data.zip",exdir = "data")
      }
   })
  
  # Selection league
  observe({
    if(input$download_datas>0)
    {
      datas_bet <- read.csv(paste('data/',input$league,'.csv',sep=""))
      datas_bet$Date <- as.Date(as.character(datas_bet$Date),"%d/%m/%Y")
      year(datas_bet$Date) <- year(datas_bet$Date) +2000
      datas_bet <<- data.frame(datas_bet)
      team <- levels(factor(datas_bet$HomeTeam))
      list.team <- list()
      
      for (i in 1:length(team))
      {
        list.team[i] <- list(team[i])
      }
      
      team<<-list.team
    }
    
  })
  
  
  output$selectHome <- renderUI({ 
    if(length(team)>0)
    {
      selectInput("home_team", label = h3("Home Team"), 
                      choices = team,
                      selected = 1)
        } # end du if download
  }) #en du render UI
  
  output$selectAway <- renderUI({ 
    if(length(team)>0)
    {
      selectInput("away_team", label = h3("Away Team"), 
                  choices = team,
                  selected = 3)
    } # end du if download
  }) #en du render UI
  

  
  output$graph_but_home <- renderChart2({    
    
    h_buts <- Highcharts$new()
    
    if(length(team)>0)
    {
      #selction de l equipe
      i=which(input$home_team==team)
      datas.gg <- dplyr::filter(datas_bet,HomeTeam==team[i])
      
      ##### Nombre de buts      
      #transfo dates pour hplot
      datas.gg=data.frame(Date=datas.gg$Date,
                          nbre_buts_totaux=datas.gg$FTHG,
                          nbre_buts_first_half=datas.gg$HTHG
                          )
      
      datas.gg$Date=as.numeric(datas.gg$Date)*1000
      
      datas.gg.melt=melt(datas.gg,value.name = 'value',id.vars = c('Date'))
      
      # Graphiques
      h_buts <- rCharts::hPlot(data = datas.gg.melt,
                                x="Date",y="value",group="variable",
                                type="column",
                                title =paste("Nbre de buts : ",input$home_team,sep=""))
      
      h_buts$xAxis(type = "datetime")
      h_buts$chart(height=600,zoomType = "xy",width=450)
      h_buts$exporting(enabled = T)
      h_buts$params$yAxis[1]=list(list(title = list(text = 'Nbre de buts')))
      #h_buts$params$yAxis[2]=list(list(title = list(text = 'moyenne mobile')))
      
      # Moyenne mobile
      h_buts$series(
        data = toJSONArray2(data.frame(datas.gg$Date,runmean(datas.gg$nbre_buts_totaux,3)), json = F, names = F),
        name = "Nbre de buts totaux",
        type = "spline",
        color="black",
        enableMouseTracking=F,
        marker=list(enabled=F),
        lineWidth=1.5
      )
      
      h_buts$series(
        data = toJSONArray2(data.frame(datas.gg$Date,runmean(datas.gg$nbre_buts_first_half,3)), json = F, names = F),
        name = "Nbre de buts first half",
        type = "spline",
        color="blue",
        enableMouseTracking=F,
        marker=list(enabled=F),
        lineWidth=1.5
        #yAxis=1
      )
      h_buts
      
    }
  })
    
  output$graph_but_away <- renderChart2({    
    
    h_buts_away <- Highcharts$new()
    
    if(length(team)>0)
    {
      #selction de l equipe
      i=which(input$away_team==team)
      datas.away <- dplyr::filter(datas_bet,AwayTeam==team[i])
      
      ##### Nombre de buts      
      #transfo dates pour hplot
      datas.away=data.frame(Date=datas.away$Date,
                            nbre_buts_totaux=datas.away$FTAG,
                            nbre_buts_first_half=datas.away$HTAG
      )
      
      datas.away$Date=as.numeric(datas.away$Date)*1000
      
      datas.away.melt=melt(datas.away,value.name = 'value',id.vars = c('Date'))
      
      # Graphiques
      h_buts_away <- rCharts::hPlot(data = datas.away.melt,
                               x="Date",y="value",group="variable",
                               type="column",
                               title =paste("Nbre de buts : ",input$away_team,sep=""))
      
      h_buts_away$xAxis(type = "datetime")
      h_buts_away$chart(height=600,zoomType = "xy",width=450)
      h_buts_away$exporting(enabled = T)
      h_buts_away$params$yAxis[1]=list(list(title = list(text = 'Nbre de buts')))
      #h_buts_away$params$yAxis[2]=list(list(title = list(text = 'moyenne mobile')))
      
      # Moyenne mobile
      h_buts_away$series(
        data = toJSONArray2(data.frame(datas.away$Date,runmean(datas.away$nbre_buts_totaux,3)), json = F, names = F),
        name = "Nbre de buts totaux",
        type = "spline",
        color="black",
        enableMouseTracking=F,
        marker=list(enabled=F),
        lineWidth=1.5
        #yAxis=1
      )
      
      h_buts_away$series(
        data = toJSONArray2(data.frame(datas.away$Date,runmean(datas.away$nbre_buts_first_half,3)), json = F, names = F),
        name = "Nbre de buts first half",
        type = "spline",
        color="blue",
        enableMouseTracking=F,
        marker=list(enabled=F),
        lineWidth=1.5
        #yAxis=1
      )
      
      h_buts_away
      
    }
  })

  
  
}) # end shiny server




