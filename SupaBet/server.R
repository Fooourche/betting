
require(shiny)
require(shinydashboard)
require(rCharts)
require(reshape2)
require(lubridate)
require(caTools)
require(DT)

shinyServer(function(input, output,session) {
  
  makeReactiveBinding("team")
  makeReactiveBinding("datas_bet")
  makeReactiveBinding("datas.shoot.away")
  team <<- list("First","Two")
  
  repertoire <- tempdir()
  
  
  #Recupere les donnees
  observe({
      if(input$download_datas>0)
      {
      datas_to_dl <- list(ligue1=c('http://www.football-data.co.uk/mmz4281/1516/F1.csv',paste(repertoire,'/F1.csv',sep="")),
                          all.leagues=c('http://football-data.co.uk/mmz4281/1516/data.zip',paste(repertoire,'/data.zip',sep="")))
      
      lapply(X = datas_to_dl,FUN = function(x) utils::download.file(url = x[1],destfile=x[2]))
      unzip (file.path(repertoire,"data.zip"),exdir = repertoire)
      }
   })
  
  # Selection league
  observe({
   
    if(input$download_datas>0)
    {
      datas_bet <- read.csv(file.path(repertoire,paste(input$league,'.csv',sep="")))
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
  
  #### Graphique nombres de buts a domicile
  output$graph_but_home_match <- renderChart2({  
    
    h_buts <- Highcharts$new()
    
    if(length(team)>0)
    {
      #selction de l equipe
      i=which(input$home_team==team)
      datas.home <- dplyr::filter(datas_bet,HomeTeam==team[i])
      
      ##### Nombre de buts      
      #transfo dates pour hplot
      datas.home=data.frame(Date=datas.home$Date,
                            total_goals=datas.home$FTHG+datas.home$FTAG ,
                            goals_first_half=datas.home$HTHG+datas.home$HTAG,
                            goals_second_half=(datas.home$FTHG+datas.home$FTAG)-(datas.home$HTHG+datas.home$HTAG)
      )
      
      datas.home$Date=as.numeric(as.POSIXct(datas.home$Date))*1000
      
      datas.home.melt=melt(datas.home,value.name = 'value',id.vars = c('Date'))
      
      # Graphiques
      h_buts <- rCharts::hPlot(data = datas.home.melt,
                               x="Date",y="value",group="variable",
                               type="column",
                               title =paste("Nbre de buts par matchs a domicile: ",input$home_team,sep=""))
      
      h_buts$xAxis(type = "datetime")
      h_buts$chart(height=450,zoomType = "xy",width=600)
      h_buts$exporting(enabled = T)
      h_buts$params$yAxis[1]=list(list(title = list(text = 'Nbre de buts par matchs a domicile')))
      h_buts$colors('#99CC99', '#f4b541', '#4587DE')
      #   green, yellow, blue
      
      # Moyenne mobile
      h_buts$series(
        data = toJSONArray2(data.frame(datas.home$Date,runmean(datas.home$total_goals,input$moy_mob)), json = F, names = F),
        name = paste("Mean goals total",input$moy_mob,"days"),
        type = "spline",
        color="#4587DE",
        enableMouseTracking=T,
        marker=list(enabled=F),
        lineWidth=1.5
      )
      
      h_buts$series(
        data = toJSONArray2(data.frame(datas.home$Date,runmean(datas.home$goals_first_half,input$moy_mob)), json = F, names = F),
        name = paste("Mean goals first half",input$moy_mob,"days"),
        type = "spline",
        color="#99CC99",
        enableMouseTracking=T,
        marker=list(enabled=F),
        lineWidth=1.5
        #yAxis=1
      )
      
      h_buts$series(
        data = toJSONArray2(data.frame(datas.home$Date,runmean(datas.home$goals_second_half,input$moy_mob)), json = F, names = F),
        name =  paste("Mean goals second half",input$moy_mob,"days"),
        type = "spline",
        color="#f4b541",
        enableMouseTracking=T,
        marker=list(enabled=F),
        lineWidth=1.5
        #yAxis=1
      )
      
            h_buts$series(
              data = toJSONArray2(data.frame(datas.home$Date,2.5), json = F, names = F),
              type = "spline",
              color="black",
              enableMouseTracking=F,
              marker=list(enabled=F),
              lineWidth=2
              #yAxis=1
            )
      
      h_buts
      
    }
    
  })
  
  
  #### Graphique nombres de buts away match
  output$graph_but_away_match <- renderChart2({  
    
    h_buts <- Highcharts$new()
    
    if(length(team)>0)
    {
      #selction de l equipe
      i=which(input$away_team==team)
      datas.home <- dplyr::filter(datas_bet,AwayTeam==team[i])
      
      ##### Nombre de buts      
      #transfo dates pour hplot
      datas.home=data.frame(Date=datas.home$Date,
                            total_goals=datas.home$FTHG+datas.home$FTAG ,
                            goals_first_half=datas.home$HTHG+datas.home$HTAG,
                            goals_second_half=(datas.home$FTHG+datas.home$FTAG)-(datas.home$HTHG+datas.home$HTAG)
      )
      
      datas.home$Date=as.numeric(as.POSIXct(datas.home$Date))*1000
      
      datas.home.melt=melt(datas.home,value.name = 'value',id.vars = c('Date'))
      
      # Graphiques
      h_buts <- rCharts::hPlot(data = datas.home.melt,
                               x="Date",y="value",group="variable",
                               type="column",
                               title =paste("Nbre de buts par match a l exterieur: ",input$away_team,sep=""))
      
      h_buts$xAxis(type = "datetime")
      h_buts$chart(height=450,zoomType = "xy",width=600)
      h_buts$exporting(enabled = T)
      h_buts$params$yAxis[1]=list(list(title = list(text = 'Nbre de buts par match a l exterieur')))
      h_buts$colors('#99CC99', '#f4b541', '#4587DE')
      #   green, yellow, blue
      
      # Moyenne mobile
      h_buts$series(
        data = toJSONArray2(data.frame(datas.home$Date,runmean(datas.home$total_goals,input$moy_mob)), json = F, names = F),
        name = paste("Mean goals total",input$moy_mob,"days"),
        type = "spline",
        color="#4587DE",
        enableMouseTracking=T,
        marker=list(enabled=F),
        lineWidth=1.5
      )
      
      h_buts$series(
        data = toJSONArray2(data.frame(datas.home$Date,runmean(datas.home$goals_first_half,input$moy_mob)), json = F, names = F),
        name = paste("Mean goals first half",input$moy_mob,"days"),
        type = "spline",
        color="#99CC99",
        enableMouseTracking=T,
        marker=list(enabled=F),
        lineWidth=1.5
        #yAxis=1
      )
      
      h_buts$series(
        data = toJSONArray2(data.frame(datas.home$Date,runmean(datas.home$goals_second_half,input$moy_mob)), json = F, names = F),
        name =  paste("Mean goals second half",input$moy_mob,"days"),
        type = "spline",
        color="#f4b541",
        enableMouseTracking=T,
        marker=list(enabled=F),
        lineWidth=1.5
        #yAxis=1
      )
      
      h_buts$series(
        data = toJSONArray2(data.frame(datas.home$Date,2.5), json = F, names = F),
        type = "spline",
        color="black",
        enableMouseTracking=F,
        marker=list(enabled=F),
        lineWidth=2
        #yAxis=1
      )
      
      h_buts
      
    }
    
  })
  
  #### Graphique nombres de buts a domicile
  output$graph_but_home <- renderChart2({  

    h_buts <- Highcharts$new()
    
    if(length(team)>0)
    {
      #selction de l equipe
      i=which(input$home_team==team)
      datas.home <- dplyr::filter(datas_bet,HomeTeam==team[i])
      
      ##### Nombre de buts      
      #transfo dates pour hplot
      datas.home=data.frame(Date=datas.home$Date,
                            total_goals=datas.home$FTHG,
                            goals_first_half=datas.home$HTHG,
                            goals_second_half=datas.home$FTHG-datas.home$HTHG
                            )
      
      datas.home$Date=as.numeric(as.POSIXct(datas.home$Date))*1000
      
      datas.home.melt=melt(datas.home,value.name = 'value',id.vars = c('Date'))
      
      # Graphiques
      h_buts <- rCharts::hPlot(data = datas.home.melt,
                                x="Date",y="value",group="variable",
                                type="column",
                                title =paste("Nbre de buts marques a domicile: ",input$home_team,sep=""))
      
      h_buts$xAxis(type = "datetime")
      h_buts$chart(height=450,zoomType = "xy",width=600)
      h_buts$exporting(enabled = T)
      h_buts$params$yAxis[1]=list(list(title = list(text = 'Nbre de buts marques a domicile')))
      h_buts$colors('#99CC99', '#f4b541', '#4587DE')
      #   green, yellow, blue
      
      # Moyenne mobile
      h_buts$series(
        data = toJSONArray2(data.frame(datas.home$Date,runmean(datas.home$total_goals,input$moy_mob)), json = F, names = F),
        name = paste("Mean goals total",input$moy_mob,"days"),
        type = "spline",
        color="#4587DE",
        enableMouseTracking=T,
        marker=list(enabled=F),
        lineWidth=1.5
      )
      
      h_buts$series(
        data = toJSONArray2(data.frame(datas.home$Date,runmean(datas.home$goals_first_half,input$moy_mob)), json = F, names = F),
        name = paste("Mean goals first half",input$moy_mob,"days"),
        type = "spline",
        color="#99CC99",
        enableMouseTracking=T,
        marker=list(enabled=F),
        lineWidth=1.5
        #yAxis=1
      )
      
      h_buts$series(
        data = toJSONArray2(data.frame(datas.home$Date,runmean(datas.home$goals_second_half,input$moy_mob)), json = F, names = F),
        name =  paste("Mean goals second half",input$moy_mob,"days"),
        type = "spline",
        color="#f4b541",
        enableMouseTracking=T,
        marker=list(enabled=F),
        lineWidth=1.5
        #yAxis=1
      )
      
#       h_buts$series(
#         data = toJSONArray2(data.frame(datas.home$Date,2.5), json = F, names = F),
#         type = "spline",
#         color="black",
#         enableMouseTracking=F,
#         marker=list(enabled=F),
#         lineWidth=2
#         #yAxis=1
#       )
      
      h_buts
      
    }
 
  })
  
  #### Graphique nombres de buts a l exterieur
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
                            total_goals=datas.away$FTAG,
                            goals_first_half=datas.away$HTAG,
                            goals_second_half=datas.away$FTAG-datas.away$HTAG
      )
      
      datas.away$Date=as.numeric(as.POSIXct(datas.away$Date))*1000
      
      datas.away.melt=melt(datas.away,value.name = 'value',id.vars = c('Date'))
      
      # Graphiques
      h_buts_away <- rCharts::hPlot(data = datas.away.melt,
                               x="Date",y="value",group="variable",
                               type="column",
                               title =paste("Nbre de buts marque a l exterieur : ",input$away_team,sep=""))
      
      h_buts_away$xAxis(type = "datetime")
      h_buts_away$chart(height=450,zoomType = "xy",width=600)
      h_buts_away$exporting(enabled = T)
      h_buts_away$params$yAxis[1]=list(list(title = list(text = 'Nbre de buts')))
      h_buts_away$colors('#99CC99', '#f4b541', '#4587DE')
      
      # Moyenne mobile
      h_buts_away$series(
        data = toJSONArray2(data.frame(datas.away$Date,runmean(datas.away$total_goals,input$moy_mob)), json = F, names = F),
        name = paste("Mean goals total",input$moy_mob,"days"),
        type = "spline",
        color="#4587DE",
        enableMouseTracking=T,
        marker=list(enabled=F),
        lineWidth=1.5
      )
      
      h_buts_away$series(
        data = toJSONArray2(data.frame(datas.away$Date,runmean(datas.away$goals_first_half,input$moy_mob)), json = F, names = F),
        name = paste("Mean goals first half",input$moy_mob,"days"),
        type = "spline",
        color="#99CC99",
        enableMouseTracking=T,
        marker=list(enabled=F),
        lineWidth=1.5
        #yAxis=1
      )
      
      h_buts_away$series(
        data = toJSONArray2(data.frame(datas.away$Date,runmean(datas.away$goals_second_half,input$moy_mob)), json = F, names = F),
        name =  paste("Mean goals second half",input$moy_mob,"days"),
        type = "spline",
        color="#f4b541",
        enableMouseTracking=T,
        marker=list(enabled=F),
        lineWidth=1.5
        #yAxis=1
      )
      
      h_buts_away
      
    }
  })
  
  #nombre de buts encaise a domicile
  output$graph_but_home_encaisse <- renderChart2({  
    
    h_buts <- Highcharts$new()
    
    if(length(team)>0)
    {
      #selction de l equipe
      i=which(input$home_team==team)
      datas.home <- dplyr::filter(datas_bet,HomeTeam==team[i])
      
      ##### Nombre de buts      
      #transfo dates pour hplot
      datas.home=data.frame(Date=datas.home$Date,
                            total_goals=datas.home$FTAG,
                            goals_first_half=datas.home$HTAG,
                            goals_second_half=datas.home$FTAG-datas.home$HTAG
      )
      
      datas.home$Date=as.numeric(as.POSIXct(datas.home$Date))*1000
      
      datas.home.melt=melt(datas.home,value.name = 'value',id.vars = c('Date'))
      
      # Graphiques
      h_buts <- rCharts::hPlot(data = datas.home.melt,
                               x="Date",y="value",group="variable",
                               type="column",
                               title =paste("Nbre de buts encaisse a domicile : ",input$home_team,sep=""))
      
      h_buts$xAxis(type = "datetime")
      h_buts$chart(height=450,zoomType = "xy",width=600)
      h_buts$exporting(enabled = T)
      h_buts$params$yAxis[1]=list(list(title = list(text = 'Nbre de buts')))
      h_buts$colors('#99CC99', '#f4b541', '#4587DE')
      #   green, yellow, blue
      
      # Moyenne mobile
      h_buts$series(
        data = toJSONArray2(data.frame(datas.home$Date,runmean(datas.home$total_goals,input$moy_mob)), json = F, names = F),
        name = paste("Mean goals total",input$moy_mob,"days"),
        type = "spline",
        color="#4587DE",
        enableMouseTracking=T,
        marker=list(enabled=F),
        lineWidth=1.5
      )
      
      h_buts$series(
        data = toJSONArray2(data.frame(datas.home$Date,runmean(datas.home$goals_first_half,input$moy_mob)), json = F, names = F),
        name = paste("Mean goals first half",input$moy_mob,"days"),
        type = "spline",
        color="#99CC99",
        enableMouseTracking=T,
        marker=list(enabled=F),
        lineWidth=1.5
        #yAxis=1
      )
      
      h_buts$series(
        data = toJSONArray2(data.frame(datas.home$Date,runmean(datas.home$goals_second_half,input$moy_mob)), json = F, names = F),
        name =  paste("Mean goals second half",input$moy_mob,"days"),
        type = "spline",
        color="#f4b541",
        enableMouseTracking=T,
        marker=list(enabled=F),
        lineWidth=1.5
        #yAxis=1
      )
      
      h_buts
      
    }
    
  })
  
  
  #nombre de buts encaise a l exterieur
  output$graph_but_away_encaisse <- renderChart2({  
    
    h_buts <- Highcharts$new()
    
    if(length(team)>0)
    {
      #selction de l equipe
      i=which(input$away_team==team)
      datas.home <- dplyr::filter(datas_bet,AwayTeam==team[i])
      
      ##### Nombre de buts      
      #transfo dates pour hplot
      datas.home=data.frame(Date=datas.home$Date,
                            total_goals=datas.home$FTHG,
                            goals_first_half=datas.home$HTHG,
                            goals_second_half=datas.home$FTHG-datas.home$HTHG
      )
      
      datas.home$Date=as.numeric(as.POSIXct(datas.home$Date))*1000
      
      datas.home.melt=melt(datas.home,value.name = 'value',id.vars = c('Date'))
      
      # Graphiques
      h_buts <- rCharts::hPlot(data = datas.home.melt,
                               x="Date",y="value",group="variable",
                               type="column",
                               title =paste("Nbre de buts encaisses a l'exterieur : ",input$away_team,sep=""))
      
      h_buts$xAxis(type = "datetime")
      h_buts$chart(height=450,zoomType = "xy",width=600)
      h_buts$exporting(enabled = T)
      h_buts$params$yAxis[1]=list(list(title = list(text = 'Nbre de buts')))
      h_buts$colors('#99CC99', '#f4b541', '#4587DE')
      #   green, yellow, blue
      
      # Moyenne mobile
      h_buts$series(
        data = toJSONArray2(data.frame(datas.home$Date,runmean(datas.home$total_goals,input$moy_mob)), json = F, names = F),
        name = paste("Mean goals total",input$moy_mob,"days"),
        type = "spline",
        color="#4587DE",
        enableMouseTracking=T,
        marker=list(enabled=F),
        lineWidth=1.5
      )
      
      h_buts$series(
        data = toJSONArray2(data.frame(datas.home$Date,runmean(datas.home$goals_first_half,input$moy_mob)), json = F, names = F),
        name = paste("Mean goals first half",input$moy_mob,"days"),
        type = "spline",
        color="#99CC99",
        enableMouseTracking=T,
        marker=list(enabled=F),
        lineWidth=1.5
        #yAxis=1
      )
      
      h_buts$series(
        data = toJSONArray2(data.frame(datas.home$Date,runmean(datas.home$goals_second_half,input$moy_mob)), json = F, names = F),
        name =  paste("Mean goals second half",input$moy_mob,"days"),
        type = "spline",
        color="#f4b541",
        enableMouseTracking=T,
        marker=list(enabled=F),
        lineWidth=1.5
        #yAxis=1
      )
      
      h_buts
      
    }
    
  })
  
  
  #### Graphique nombres de tirs on target a domicile
  output$graph_shoots_home_target <- renderChart2({    
    
    h_shoots <- Highcharts$new()
    
    if(length(team)>0)
    {
      #selction de l equipe
      i=which(input$home_team==team)
      datas.home <- dplyr::filter(datas_bet,HomeTeam==team[i])
      
      ##### Nombre de buts      
      #transfo dates pour hplot
      datas.home=data.frame(Date=datas.home$Date,                          
                            shoots_on_target=datas.home$HST,              
                            shoots_on_target_conceed=datas.home$AST
      )
      
      datas.home$Date=as.numeric(as.POSIXct(datas.home$Date))*1000
      
      datas.home.melt=melt(datas.home,value.name = 'value',id.vars = c('Date'))
      
      # Graphiques
      h_shoots <- rCharts::hPlot(data = datas.home.melt,
                               x="Date",y="value",group="variable",
                               type="column",
                               title =paste("Nbre shoots on target : ",input$home_team,sep=""))
      
      h_shoots$xAxis(type = "datetime")
      h_shoots$chart(height=450,zoomType = "xy",width=600)
      h_shoots$exporting(enabled = T)
      h_shoots$params$yAxis[1]=list(list(title = list(text = 'Nbre de tirs')))
      h_shoots$colors('#99CC99', '#f4b541', '#4587DE','#FB9058')
      #   green, yellow, blue, orange
      
 
      h_shoots$series(
        data = toJSONArray2(data.frame(datas.home$Date,runmean(datas.home$shoots_on_target,input$moy_mob)), json = F, names = F),
        name =  paste("Mean shoots on target",input$moy_mob,"days"),
        type = "spline",
        color="#99CC99",
        enableMouseTracking=T,
        marker=list(enabled=F),
        lineWidth=1.5
        #yAxis=1
      )
      
      
      h_shoots$series(
        data = toJSONArray2(data.frame(datas.home$Date,runmean(datas.home$shoots_on_target_conceed,input$moy_mob)), json = F, names = F),
        name = paste("Mean shoots conceed on target",input$moy_mob,"days"),
        type = "spline",
        color="#f4b541",
        enableMouseTracking=T,
        marker=list(enabled=F),
        lineWidth=1.5
        #yAxis=1
      )
      
      h_shoots
      
    }
  })
  
  output$graph_shoots_home <- renderChart2({    
    
    h_shoots <- Highcharts$new()
    
    if(length(team)>0)
    {
      #selction de l equipe
      i=which(input$home_team==team)
      datas.home <- dplyr::filter(datas_bet,HomeTeam==team[i])
      
      ##### Nombre de buts      
      #transfo dates pour hplot
      datas.home=data.frame(Date=datas.home$Date,                          
                            shoots=datas.home$HS,              
                            shoots_conceed=datas.home$AS
      )
      
      datas.home$Date=as.numeric(as.POSIXct(datas.home$Date))*1000
      
      datas.home.melt=melt(datas.home,value.name = 'value',id.vars = c('Date'))
      
      # Graphiques
      h_shoots <- rCharts::hPlot(data = datas.home.melt,
                                 x="Date",y="value",group="variable",
                                 type="column",
                                 title =paste("Nbre shoots : ",input$home_team,sep=""))
      
      h_shoots$xAxis(type = "datetime")
      h_shoots$chart(height=450,zoomType = "xy",width=600)
      h_shoots$exporting(enabled = T)
      h_shoots$params$yAxis[1]=list(list(title = list(text = 'Nbre shoots')))
      h_shoots$colors('#99CC99', '#f4b541', '#4587DE','#FB9058')
      #   green, yellow, blue, orange
      
      
      h_shoots$series(
        data = toJSONArray2(data.frame(datas.home$Date,runmean(datas.home$shoots,input$moy_mob)), json = F, names = F),
        name = paste("Mean shoots",input$moy_mob,"days"),
        type = "spline",
        color="#99CC99",
        enableMouseTracking=T,
        marker=list(enabled=F),
        lineWidth=1.5
        #yAxis=1
      )
      
      
      h_shoots$series(
        data = toJSONArray2(data.frame(datas.home$Date,runmean(datas.home$shoots_conceed,input$moy_mob)), json = F, names = F),
        name = paste("Mean shoots conceed",input$moy_mob,"days"),
        type = "spline",
        color="#f4b541",
        enableMouseTracking=T,
        marker=list(enabled=F),
        lineWidth=1.5
        #yAxis=1
      )
      
      h_shoots
      
    }
  })
  
  #### Graphique nombres tirs away 
  output$graph_shoots_away <- renderChart2({    
    
    h_shoots_away <- Highcharts$new()
    
    if(length(team)>0)
    {
      #selction de l equipe
      i=which(input$away_team==team)
      datas.home <- dplyr::filter(datas_bet,AwayTeam==team[i])
      
      ##### Nombre de buts      
      #transfo dates pour hplot
      datas.home=data.frame(Date=datas.home$Date,
                            shoots=datas.home$HS,
                            shoots_conceed=datas.home$AS
      )
      datas.shoot.away <<- datas.home
      datas.home$Date=as.numeric(as.POSIXct(datas.home$Date))*1000
      
      datas.home.melt=melt(datas.home,value.name = 'value',id.vars = c('Date'))
      
      # Graphiques
      h_shoots_away <- rCharts::hPlot(data = datas.home.melt,
                                 x="Date",y="value",group="variable",
                                 type="column",
                                 title =paste("Nbre shoots : ",input$away_team,sep=""))
      
      h_shoots_away$xAxis(type = "datetime")
      h_shoots_away$chart(height=450,zoomType = "xy",width=600)
      h_shoots_away$exporting(enabled = T)
      h_shoots_away$params$yAxis[1]=list(list(title = list(text = 'Nbre de tirs')))
      h_shoots_away$colors('#99CC99', '#f4b541', '#4587DE','#FB9058')
      #   green, yellow, blue, orange
      
      # Moyenne mobile
      h_shoots_away$series(
        data = toJSONArray2(data.frame(datas.home$Date,runmean(datas.home$shoots,input$moy_mob)), json = F, names = F),
        name = paste("Mean shoots",input$moy_mob,"days"),
        type = "spline",
        color="#99CC99",
        enableMouseTracking=T,
        marker=list(enabled=F),
        lineWidth=1.5
      )
      

      h_shoots_away$series(
        data = toJSONArray2(data.frame(datas.home$Date,runmean(datas.home$shoots_conceed,input$moy_mob)), json = F, names = F),
        name = paste("Mean shoots conceed",input$moy_mob,"days"),
        type = "spline",
        color="#f4b541",
        enableMouseTracking=T,
        marker=list(enabled=F),
        lineWidth=1.5
        #yAxis=1
      )
      
      
      h_shoots_away
      
    }
  })
  
  #### Graphique nombres de tirs on target a l exterieur
  output$graph_shoots_away_target <- renderChart2({    
    
    h_shoots_away <- Highcharts$new()
    
    if(length(team)>0)
    {
      #selction de l equipe
      i=which(input$away_team==team)
      datas.home <- dplyr::filter(datas_bet,AwayTeam==team[i])
      
      ##### Nombre de buts      
      #transfo dates pour hplot
      datas.home=data.frame(Date=datas.home$Date,
                            shoots_on_target=datas.home$HST,
                            shoots_on_target_conceed=datas.home$AST
      )
      datas.shoot.away <<- datas.home
      datas.home$Date=as.numeric(as.POSIXct(datas.home$Date))*1000
      
      datas.home.melt=melt(datas.home,value.name = 'value',id.vars = c('Date'))
      
      # Graphiques
      h_shoots_away <- rCharts::hPlot(data = datas.home.melt,
                                      x="Date",y="value",group="variable",
                                      type="column",
                                      title =paste("Nbre shoots on target : ",input$away_team,sep=""))
      
      h_shoots_away$xAxis(type = "datetime")
      h_shoots_away$chart(height=450,zoomType = "xy",width=600)
      h_shoots_away$exporting(enabled = T)
      h_shoots_away$params$yAxis[1]=list(list(title = list(text = 'Nbre de tirs')))
      h_shoots_away$colors('#99CC99', '#f4b541', '#4587DE','#FB9058')
      #   green, yellow, blue, orange
      
      # Moyenne mobile
      
      h_shoots_away$series(
        data = toJSONArray2(data.frame(datas.home$Date,runmean(datas.home$shoots_on_target,input$moy_mob)), json = F, names = F),
        name = paste("Mean shoots on target",input$moy_mob,"days"),
        type = "spline",
        color="#99CC99",
        enableMouseTracking=T,
        marker=list(enabled=F),
        lineWidth=1.5
        #yAxis=1
      )

      h_shoots_away$series(
        data = toJSONArray2(data.frame(datas.home$Date,runmean(datas.home$shoots_on_target_conceed,input$moy_mob)), json = F, names = F),
        name = paste("Mean shoots conceed on target",input$moy_mob,"days"),
        type = "spline",
        color="#f4b541",
        enableMouseTracking=T,
        marker=list(enabled=F),
        lineWidth=1.5
        #yAxis=1
      )
      
      
      h_shoots_away
      
    }
  })
  
#### Graphique nombres de buts a domicile et a l'exterieur
output$graph_buts_match_home <- renderChart2({  
  
  h_buts <- Highcharts$new()
  
  if(length(team)>0)
  {
    #selction de l equipe
    i=which(input$home_team==team)
    datas.home <- dplyr::filter(datas_bet,HomeTeam==team[i] | AwayTeam==team[i])
    
    ##### Nombre de buts      
    #transfo dates pour hplot
    datas.home=data.frame(Date=datas.home$Date,
                          total_goals=datas.home$FTHG+datas.home$FTAG ,
                          goals_first_half=datas.home$HTHG+datas.home$HTAG,
                          goals_second_half=(datas.home$FTHG+datas.home$FTAG)-(datas.home$HTHG+datas.home$HTAG)
    )
    
    datas.home$Date=as.numeric(as.POSIXct(datas.home$Date))*1000
    
    datas.home.melt=melt(datas.home,value.name = 'value',id.vars = c('Date'))
    
    # Graphiques
    h_buts <- rCharts::hPlot(data = datas.home.melt,
                             x="Date",y="value",group="variable",
                             type="column",
                             title =paste("Nbre de buts par matchs a domicile et a l'exterieur: ",input$home_team,sep=""))
    
    h_buts$xAxis(type = "datetime")
    h_buts$chart(height=450,zoomType = "xy",width=600)
    h_buts$exporting(enabled = T)
    h_buts$params$yAxis[1]=list(list(title = list(text = 'Nbre de buts par matchs a domicile et a l exterieur')))
    h_buts$colors('#99CC99', '#f4b541', '#4587DE')
    #   green, yellow, blue
    
    # Moyenne mobile
    h_buts$series(
      data = toJSONArray2(data.frame(datas.home$Date,runmean(datas.home$total_goals,input$moy_mob)), json = F, names = F),
      name = paste("Mean goals total",input$moy_mob,"days"),
      type = "spline",
      color="#4587DE",
      enableMouseTracking=T,
      marker=list(enabled=F),
      lineWidth=1.5
    )
    
    h_buts$series(
      data = toJSONArray2(data.frame(datas.home$Date,runmean(datas.home$goals_first_half,input$moy_mob)), json = F, names = F),
      name = paste("Mean goals first half",input$moy_mob,"days"),
      type = "spline",
      color="#99CC99",
      enableMouseTracking=T,
      marker=list(enabled=F),
      lineWidth=1.5
      #yAxis=1
    )
    
    h_buts$series(
      data = toJSONArray2(data.frame(datas.home$Date,runmean(datas.home$goals_second_half,input$moy_mob)), json = F, names = F),
      name =  paste("Mean goals second half",input$moy_mob,"days"),
      type = "spline",
      color="#f4b541",
      enableMouseTracking=T,
      marker=list(enabled=F),
      lineWidth=1.5
      #yAxis=1
    )
    
    h_buts$series(
      data = toJSONArray2(data.frame(datas.home$Date,2.5), json = F, names = F),
      type = "spline",
      color="black",
      enableMouseTracking=F,
      marker=list(enabled=F),
      lineWidth=2
      #yAxis=1
    )
    
    h_buts
    
  }
  
})

#### Graphique nombres de buts a domicile et a l'exterieur
output$graph_buts_match_away <- renderChart2({  
  
  h_buts <- Highcharts$new()
  
  if(length(team)>0)
  {
    #selction de l equipe
    i=which(input$away_team==team)
    datas.home <- dplyr::filter(datas_bet,HomeTeam==team[i] | AwayTeam==team[i])
    
    ##### Nombre de buts      
    #transfo dates pour hplot
    datas.home=data.frame(Date=datas.home$Date,
                          total_goals=datas.home$FTHG+datas.home$FTAG ,
                          goals_first_half=datas.home$HTHG+datas.home$HTAG,
                          goals_second_half=(datas.home$FTHG+datas.home$FTAG)-(datas.home$HTHG+datas.home$HTAG)
    )
    
    datas.home$Date=as.numeric(as.POSIXct(datas.home$Date))*1000
    
    datas.home.melt=melt(datas.home,value.name = 'value',id.vars = c('Date'))
    
    # Graphiques
    h_buts <- rCharts::hPlot(data = datas.home.melt,
                             x="Date",y="value",group="variable",
                             type="column",
                             title =paste("Nbre de buts par matchs a domicile et a l'exterieur: ",input$away_team,sep=""))
    
    h_buts$xAxis(type = "datetime")
    h_buts$chart(height=450,zoomType = "xy",width=600)
    h_buts$exporting(enabled = T)
    h_buts$params$yAxis[1]=list(list(title = list(text = 'Nbre de buts par matchs a domicile et a l exterieur')))
                                                  h_buts$colors('#99CC99', '#f4b541', '#4587DE')
                                                  #   green, yellow, blue
                                                  
                                                  # Moyenne mobile
                                                  h_buts$series(
                                                    data = toJSONArray2(data.frame(datas.home$Date,runmean(datas.home$total_goals,input$moy_mob)), json = F, names = F),
                                                    name = paste("Mean goals total",input$moy_mob,"days"),
                                                    type = "spline",
                                                    color="#4587DE",
                                                    enableMouseTracking=T,
                                                    marker=list(enabled=F),
                                                    lineWidth=1.5
                                                  )
                                                  
                                                  h_buts$series(
                                                    data = toJSONArray2(data.frame(datas.home$Date,runmean(datas.home$goals_first_half,input$moy_mob)), json = F, names = F),
                                                    name = paste("Mean goals first half",input$moy_mob,"days"),
                                                    type = "spline",
                                                    color="#99CC99",
                                                    enableMouseTracking=T,
                                                    marker=list(enabled=F),
                                                    lineWidth=1.5
                                                    #yAxis=1
                                                  )
                                                  
                                                  h_buts$series(
                                                    data = toJSONArray2(data.frame(datas.home$Date,runmean(datas.home$goals_second_half,input$moy_mob)), json = F, names = F),
                                                    name =  paste("Mean goals second half",input$moy_mob,"days"),
                                                    type = "spline",
                                                    color="#f4b541",
                                                    enableMouseTracking=T,
                                                    marker=list(enabled=F),
                                                    lineWidth=1.5
                                                    #yAxis=1
                                                  )
                                                  
                                                  h_buts$series(
                                                    data = toJSONArray2(data.frame(datas.home$Date,2.5), json = F, names = F),
                                                    type = "spline",
                                                    color="black",
                                                    enableMouseTracking=F,
                                                    marker=list(enabled=F),
                                                    lineWidth=2
                                                    #yAxis=1
                                                  )
                                                  
                                                  h_buts
                                                  
  }
  
})

  output$dt_shoots_away <- DT::renderDataTable({
   
      datas.shoot.away
    
  }, options = list(paging = FALSE,lengthChange = FALSE,bFilter=FALSE))

  
}) # end shiny server




