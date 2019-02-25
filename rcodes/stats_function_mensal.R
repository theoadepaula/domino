
#carregar os pacotes abaixo para análise de dados
library(dplyr)
library(xlsx)
library(readxl)
library(ggplot2)
library(grid)
library(ggthemes)
library(lubridate)
library(tidyr)
library(tidyverse)
library(RColorBrewer)
library(forcats)
library(png)
library(scales)
library(formattable)
library(kableExtra)

# LTG ---------------------------------------------------------------------


options(pillar.round = FALSE)
options(pillar.sigfigs = Inf)

theme_update(plot.title = element_text(hjust = 0.5))


tab_mes=function(mes=month(Sys.Date()),ano=year(Sys.Date()),historico=TRUE){
  jogos_domino=read_xlsx("src/Domino.xlsm","Historico")
  jogos_domino=jogos_domino %>% 
    mutate(DataJogo=as.Date(DataJogo),
           Mes=as.integer(month(DataJogo)),
           Ano=as.integer(year(DataJogo)),
           MesAno=paste(Mes,Ano, sep="/"),
           Dia_semana= wday(DataJogo, label = T, abbr=F))
  
  #remodelar o banco de dados para o desempenho dos jogadores
  jogadores_wide= jogos_domino%>% gather(times, Jogador,-c(IdJogo,DataJogo,Mes,Ano,MesAno,PontosTime1,
                                                           PontosTime2,SaldoPts,Dia_semana)) %>% arrange(IdJogo,SaldoPts,Jogador)
  
  jogadores_wide=jogadores_wide %>% mutate(Pontos=ifelse(times=="Time1Jogador1"|times=="Time1Jogador2",PontosTime1,PontosTime2),
                                           SaldoPts=ifelse(times=="Time1Jogador1"|times=="Time1Jogador2",SaldoPts,-(SaldoPts)),
                                           Resultado=ifelse(SaldoPts>0,"Vitória","Derrota"),
                                           Buchuda=ifelse((PontosTime1>PontosTime2&PontosTime2==0)&SaldoPts>0,"Sim",ifelse((PontosTime1<PontosTime2&PontosTime1==0)&SaldoPts>0,"Sim","Não")))%>% 
    arrange(IdJogo,SaldoPts,Jogador)
  
  desempenho_jogadores= jogadores_wide[,-c(3,4,9)]
  
  #remover data.frame abaixo
  rm(jogadores_wide)
  
  
  tabela_mensal=desempenho_jogadores %>% 
    filter(Ano==ano & Mes==mes) %>% group_by(Jogador) %>% 
    summarize(Jogos=n(),Vitórias=sum(Resultado=="Vitória"),Saldo_Pt=as.integer(sum(SaldoPts)),
              Buchudas=sum(Buchuda=="Sim"),Aprov=mean(Resultado=="Vitória")*100,
              pontuacao=ifelse(Jogos<20, formatC(0,2,format="f") ,ifelse(Jogos<40,formatC((0.9+0.1*(Jogos-20)/20)*Aprov,2,format="f"),formatC(Aprov,2,format="f")))) %>%
    arrange(desc(pontuacao),desc(Aprov),desc(Buchudas),desc(Saldo_Pt),desc(Vitórias))
  
  tabela_mensal$Aprov=formatC(tabela_mensal$Aprov,2,format="f")
  
  tabela_mensal%>% mutate(Mes=toupper(month(mes,label = T,abbr = F)),
                          Ano=ano)
 
  }

graf_mes=function(mes=month(Sys.Date()),ano=year(Sys.Date())){
  jogos_domino=read_xlsx("src/Domino.xlsm","Historico")
  jogos_domino=jogos_domino %>% 
    mutate(DataJogo=as.Date(DataJogo),
           Mes=as.integer(month(DataJogo)),
           Ano=as.integer(year(DataJogo)),
           MesAno=paste(Mes,Ano, sep="/"),
           Dia_semana= wday(DataJogo, label = T, abbr=F))
  
  jogos_domino= jogos_domino %>%filter(Mes==mes,Ano==ano)
  
  #remodelar o banco de dados para o desempenho dos jogadores
  jogadores_wide= jogos_domino%>% gather(times, Jogador,-c(IdJogo,DataJogo,Mes,Ano,MesAno,PontosTime1,
                                                           PontosTime2,SaldoPts,Dia_semana)) %>% arrange(IdJogo,SaldoPts,Jogador)
  
  jogadores_wide=jogadores_wide %>% mutate(Pontos=ifelse(times=="Time1Jogador1"|times=="Time1Jogador2",PontosTime1,PontosTime2),
                                           SaldoPts=ifelse(times=="Time1Jogador1"|times=="Time1Jogador2",SaldoPts,-(SaldoPts)),
                                           Resultado=ifelse(SaldoPts>0,"Vitória","Derrota"),
                                           Buchuda=ifelse((PontosTime1>PontosTime2&PontosTime2==0)&SaldoPts>0,"Sim",ifelse((PontosTime1<PontosTime2&PontosTime1==0)&SaldoPts>0,"Sim","Não")))%>% 
    arrange(IdJogo,SaldoPts,Jogador)
  
  desempenho_jogadores= jogadores_wide[,-c(3,4,9)]
  #remover data.frame abaixo
  rm(jogadores_wide)
  
  ddj=desempenho_jogadores %>% group_by(DataJogo,Jogador) %>% 
    summarize(Jogos=as.integer(n()),Vitorias=as.integer(sum(Resultado=="Vitória"))) %>%
    arrange(Jogador,DataJogo,desc(Vitorias)) %>% ungroup() %>% group_by(Jogador) %>% ungroup()%>%
    mutate(Jogador=as.factor(Jogador),JogosAc=cumsum(Jogos),VitoriasAC=cumsum(Vitorias),AprovAc=VitoriasAC/JogosAc*100,
           pontuacao=ifelse(JogosAc<20,0,ifelse(JogosAc<40,(0.9+0.1*(JogosAc-20)/20)*AprovAc,AprovAc)))
  
  ddj1=ddj%>%select(-c(JogosAc,VitoriasAC,pontuacao,AprovAc))
  
  dia_inicial=dmy(paste0("1/",mes,"/",ano))
  dia_final=dmy(paste0(days_in_month(mes),"/",mes,"/",ano))
  
  djj=crossing(DataJogo=seq(dia_inicial,to=dia_final,by="day"),Jogador=distinct(ddj,Jogador))
  adj=djj%>%left_join(ddj1)
  adj[is.na(adj)]=0
  adj1=adj %>% group_by(Jogador) %>% mutate(Jogos=as.integer(Jogos),
                                            Vitorias=as.integer(Vitorias),
                                            JogosAc=as.integer(cumsum(Jogos)),
                                            VitoriasAC=as.integer(cumsum(Vitorias)),
                                            AprovAc=ifelse(JogosAc==0,0,VitoriasAC/JogosAc*100),
                                            pontuacao=ifelse(JogosAc<20,0,ifelse(JogosAc<40,(0.9+0.1*(JogosAc-20)/20)*AprovAc,AprovAc)))
  
   adj1$AprovAc=formatC(adj1$AprovAc,2,format="f")
   adj1$pontuacao=formatC(adj1$pontuacao,2,format="f")
   adj1$Jogador=as.character(adj1$Jogador)
  
  # adj1= adj1 %>% filter(weekdays(DataJogo)!="sábado" & weekdays(DataJogo)!="domingo")
  
  lista_cores= c("aquamarine3","chocolate","lawngreen","cadetblue1","coral1","cornflowerblue",
                 "blueviolet","darkgoldenrod1","brown2","burlywood3","darkmagenta",
                 "darkolivegreen3","firebrick1","darkorchid3","gold1","darkslategray2","indianred","chartreuse1")
  
   adj1%>%filter(as.double(pontuacao)>0, day(DataJogo)>1 & day(DataJogo)<=day(Sys.Date()) )%>%
     ggplot(aes(x=DataJogo,y=as.double(pontuacao), group=Jogador,color=Jogador))+
    geom_line(aes(linetype=Jogador), size=1.2)+
    scale_y_continuous(breaks=seq(0,100,10))+ labs(y="Pontuação")+
    scale_x_date(name="Dias", date_labels = "%d/%m/%Y", date_breaks = "2 day")+
    ggtitle(paste0("Histórico de Pontuação Diária - ",toupper(month(mes, label=T, abbr=F)),"/",ano))+
    scale_color_manual(values=lista_cores)+theme_dark()+
    theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "gray46") ,
          axis.text.x = element_text(angle=90))
}

knitr::kable(tab_mes()%>%select(-c(Mes,Ano)), caption = paste("Ranking do Mês -",month(Sys.Date(), label=T, abbr=F),"/",year(Sys.Date())),
             col.names = c("Jogador","Nº de Jogos","Vitórias","Saldo de Pontos","Buchudas","Aproveitamento","Pontuação"),
             align=c("l",rep("c",6))) %>% kable_styling(bootstrap_options = c("striped", "hover")) %>%
  row_spec(1, bold = T, color = "yellow", background = "gold")%>%
  row_spec(2, bold = T, color = "gray", background = "silver")%>%
  row_spec(3, bold = T, color = "#da9f65", background = "#e9c7a5") %>%
  row_spec(which(tab_mes()[,7]<1)[1]-1, bold = T, color = "white", background = "red") %>%
  row_spec(which(tab_mes()[,7]<1), bold = T, color = "purple", background = "green")

write.csv2(tab_mes()%>%select(-c(Mes,Ano)),file=paste("csv/","ranking_mensal_",month(Sys.Date()),"_",year(Sys.Date()),".csv"))

graf_mes()

rm(list = ls())

tab_mes(9,2017)
tempo=crossing(mes=1:12,ano=2017:2019) %>% arrange(ano,mes)
tempo=tempo[8:25,]



teste=map2(.x=tempo$mes,.y=tempo$ano,~tab_mes(.x,.y))





ganhadores=map_df(teste,~.[1,c(1,8,9)])%>% count(Jogador,sort = T)

knitr::kable(ganhadores, caption = paste("Ranking dos ganhadores de cada mês"),
             col.names = c("Jogador","Nº de Mês Ganhos"),align=c("l","c")) %>% 
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  row_spec(1, bold = T, color = "yellow", background = "gold")%>%
  row_spec(2, bold = T, color = "gray", background = "silver")%>%
  row_spec(3, bold = T, color = "#da9f65", background = "#e9c7a5")



knitr::kable(map_df(teste,~.[2,c(1,8,9)]), caption = paste("Vice de cada mês"),
             col.names = c("Jogador","Mês","Ano"),align=c("l","c","c")) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"))

vices=map_df(teste,~.[2,c(1,8,9)])%>%count(Jogador,sort = T)
knitr::kable(vices, caption = paste("Ranking dos vices de cada mês"),
             col.names = c("Jogador","Nº de Mês Ganhos"),align=c("l","c")) %>% 
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  row_spec(1, bold = T, color = "yellow", background = "gold")%>%
  row_spec(which(vices[,2]==2), bold = T, color = "gray", background = "silver")%>%
  row_spec(which(vices[,2]==1), bold = T, color = "#da9f65", background = "#e9c7a5")

teste[[1]]
tab_mes(11,2017) %>% formattable()

tabela_mensal=desempenho_jogadores %>% 
  group_by(Jogador) %>% 
  summarize(Jogos=n(),Vitórias=sum(Resultado=="Vitória"),Saldo_Pt=as.integer(sum(SaldoPts)),
            Buchudas=sum(Buchuda=="Sim"),Aprov=mean(Resultado=="Vitória")*100,
            pontuacao=ifelse(Jogos<20, formatC(0,2,format="f") ,ifelse(Jogos<40,formatC((0.9+0.1*(Jogos-20)/20)*Aprov,2,format="f"),formatC(Aprov,2,format="f")))) %>%
  arrange(desc(pontuacao),desc(Aprov),desc(Buchudas),desc(Saldo_Pt),desc(Vitórias))

tabela_mensal$Aprov=formatC(tabela_mensal$Aprov,2,format="f")

tabela_mensal %>% filter(pontuacao>30) %>% formattable()
