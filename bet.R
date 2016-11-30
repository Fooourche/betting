
#Librairies
require(installr)
require2(dplyr,ask=F)
require2(plyr,ask=F)
require2(tidyr,ask=F)
require2(magrittr,ask=F)
require2(reshape2,ask=F)
require2(foreach,ask=F)
require2(doParallel,ask=F)
require2(parallel,ask=F)
require2(RODBC,ask=F)
require2(httr,ask=F)
require2(downloader,ask=F)
require2(ggplot2,ask=F)
require2(caTools,ask=F)
require2(lubridate,ask=F)
require2(devtools,ask=F)
require2(RCurl)
#devtools::install_github('rCharts', 'ramnathv')
#devtools::install_github('rCharts_timeline_r', 'timelyportfolio')
require(rCharts)

#Recupere les donnees
datas_to_dl <- list(ligue1=c('http://www.football-data.co.uk/mmz4281/1516/F1.csv','data/F1.csv'),
                    all.leagues=c('http://football-data.co.uk/mmz4281/1516/data.zip','data/data.zip'))

lapply(X = datas_to_dl,FUN = function(x) utils::download.file(url = x[1],destfile=x[2]))
unzip ("data/data.zip",exdir = "data")



#### League 1
datas_bet <- read.csv('data/F1.csv')
datas_bet$Date <- as.Date(as.character(datas_bet$Date),"%d/%m/%Y")
year(datas_bet$Date) <- year(datas_bet$Date) +2000

team <- levels(factor(datas_bet$HomeTeam))



#par equipe

i=3

datas.gg <- dplyr::filter(datas_bet,HomeTeam==team[i] | AwayTeam==team[[i]]) %>% mutate(moymob=runmean(x = FTHG+FTAG,k = 3))


####### GRAPHIQUES ##########

##### Nombre de buts

#nombre de buts par matchs
ggplot(datas.gg) + geom_bar(aes(y=FTHG+FTAG,x=Date,fill=FTHG+FTAG,alpha=0.7),stat='identity') +     
                  geom_line(aes(x=Date,y=runmean(x = FTHG+FTAG,k = 3)),col='blue',size=0.9) +
                  ylab("Nbre de buts par matchs") +
                  scale_fill_gradient2("FTHG+FTAG", low = "blue",mid='green',high = "red")+
                  ggtitle(paste("Nbre de buts par match : ",team[i],sep="")) +
                  theme_bw()


#nombre de buts par matchs home
ggplot(datas.gg) + geom_bar(aes(y=FTHG,x=Date,fill=FTHG,alpha=0.7),stat='identity') +     
                  geom_line(aes(x=Date,y=runmean(x = FTHG,k = 3)),col='blue',size=0.9) +
                  ylab("Nbre de buts par matchs a domicile") +
                  scale_fill_gradient2("FTHG", low = "blue",mid='green',high = "red")+
                  ggtitle(paste("Nbre de buts par match a domicile: ",team[i],sep="")) +
                  theme_bw()


#nombre de buts par matchs home
ggplot(datas.gg) + geom_bar(aes(y=FTAG,x=Date,fill=FTAG,alpha=0.7),stat='identity') +     
                geom_line(aes(x=Date,y=runmean(x = FTAG,k = 3)),col='blue',size=0.9) +
                ylab("Nbre de buts par matchs a l'exterieur") +
                scale_fill_gradient2("FTAG", low = "blue",mid='green',high = "red")+
                ggtitle(paste("Nbre de buts par match a l'exterieur : ",team[i],sep="")) +
                theme_bw()



##### TIRS

#nombre de tirs par matchs
ggplot(datas.gg) + geom_bar(aes(y=HS+AS,x=Date,fill=HS+AS,alpha=0.7),stat='identity') +     
  geom_line(aes(x=Date,y=runmean(x = HS+AS,k = 3)),col='blue',size=0.9) +
  ylab("Nbre de tirs par matchs") +
  scale_fill_gradient2("HS+AS", low = "blue",mid='green',high = "red")+
  ggtitle(paste("Nbre de tirs  totaux par match : ",team[i],sep="")) +
  theme_bw()

#nombre de tirs par matchs home
ggplot(datas.gg) + geom_bar(aes(y=HS,x=Date,fill=HS,alpha=0.7),stat='identity') +     
  geom_line(aes(x=Date,y=runmean(x = HS,k = 3)),col='blue',size=0.9) +
  ylab("Nbre de tirs cadre par matchs a domicile") +
  scale_fill_gradient2("HS", low = "blue",mid='green',high = "red")+
  ggtitle(paste("Nbre de tirs par match a domcile : ",team[i],sep="")) +
  theme_bw()

#nombre de buts par matchs home
ggplot(datas.gg) + geom_bar(aes(y=AS,x=Date,fill=AS,alpha=0.7),stat='identity') +     
  geom_line(aes(x=Date,y=runmean(x = AS,k = 3)),col='blue',size=0.9) +
  ylab("Nbre de tirs par matchs a l'exterieur") +
  scale_fill_gradient2("AS", low = "blue",mid='green',high = "red")+
  ggtitle(paste("Nbre de tirs par match a l'exterieur: ",team[i],sep="")) +
  theme_bw()


##### TIRS Cadres

#nombre de tirs cadre par matchs
ggplot(datas.gg) + geom_bar(aes(y=HST+AST,x=Date,fill=HST+AST,alpha=0.7),stat='identity') +     
  geom_line(aes(x=Date,y=runmean(x = HST+AST,k = 3)),col='blue',size=0.9) +
  ylab("Nbre de tirs cadres par matchs") +
  scale_fill_gradient2("HST+AST", low = "blue",mid='green',high = "red")+
  ggtitle(paste("Nbre de tirs cadres totaux par match : ",team[i],sep="")) +
  theme_bw()

#nombre de tirs cadre par matchs home
ggplot(datas.gg) + geom_bar(aes(y=HST,x=Date,fill=HST,alpha=0.7),stat='identity') +     
  geom_line(aes(x=Date,y=runmean(x = HST,k = 3)),col='blue',size=0.9) +
  ylab("Nbre de tirs cadres par matchs a domicile") +
  scale_fill_gradient2("HST", low = "blue",mid='green',high = "red")+
  ggtitle(paste("Nbre de tirs cadres par match a domcile : ",team[i],sep="")) +
  theme_bw()


#nombre de tirs cadre par matchs away
ggplot(datas.gg) + geom_bar(aes(y=AST,x=Date,fill=AST,alpha=0.7),stat='identity') +     
  geom_line(aes(x=Date,y=runmean(x = AST,k = 3)),col='blue',size=0.9) +
  ylab("Nbre de tirs cadres par matchs a l'exterieur") +
  scale_fill_gradient2("AST", low = "blue",mid='green',high = "red")+
  ggtitle(paste("Nbre de tirs cadres par match a l'exterieur: ",team[i],sep="")) +
  theme_bw()


