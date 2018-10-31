
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

# LTG ---------------------------------------------------------------------


options(pillar.round = FALSE)
options(pillar.sigfigs = Inf)

theme_update(plot.title = element_text(hjust = 0.5))

#Lendo o arquivo dos jogos e adicionando variáveis
jogos_domino=read_xlsx("src/Domino.xlsm","Historico")
jogos_domino=jogos_domino %>% 
  mutate(DataJogo=as.Date(DataJogo),
         Mes=as.integer(month(DataJogo)),
         Ano=as.integer(year(DataJogo)),
         MesAno=paste(Mes,Ano, sep="/"),
         Dia_semana= wday(DataJogo, label = T, abbr=F))

jogadores_aposentados=c("Isaias Araujo","Marcelo","Matheus Rodrigues","Pablo","Joelio","Raoni")
jogadores_principais=c("Luis Jesiel","Othon Fialho","Gilson Marçal","Gaetan Dubois","Rubens Wanderley","Sergio Glasherster","Théo Albuquerque")
lista_cores= c("aquamarine3","chocolate","lawngreen","cadetblue1","coral1","cornflowerblue",
               "blueviolet","darkgoldenrod1","brown2","burlywood3","darkmagenta",
               "darkolivegreen3","firebrick1","darkorchid3","gold1","darkslategray2","indianred","chartreuse1")

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

#Puxar a tabela atualizada dos jogadores

#tabela mensal dos jogadores
tabela_mensal=desempenho_jogadores %>% 
  filter(Ano==year(Sys.Date()) & Mes==month(Sys.Date())) %>% group_by(Jogador) %>% 
  summarize(Jogos=n(),Vitórias=sum(Resultado=="Vitória"),Saldo_Pt=as.integer(sum(SaldoPts)),
            Buchudas=sum(Buchuda=="Sim"),Aprov=mean(Resultado=="Vitória")*100,
            pontuacao=ifelse(Jogos<20, formatC(0,2,format="f") ,ifelse(Jogos<40,formatC((0.9+0.1*(Jogos-20)/20)*Aprov,2,format="f"),formatC(Aprov,2,format="f")))) %>%
  arrange(desc(pontuacao),desc(Aprov),desc(Buchudas),desc(Saldo_Pt),desc(Vitórias))

tabela_mensal$Aprov=formatC(tabela_mensal$Aprov,2,format="f")

tabela_mensal
formattable(tabela_mensal)

write.csv2(tabela_mensal,file=paste("csv/","ranking_mensal_",month(Sys.Date()),"_",year(Sys.Date()),".csv"))

#calculo para gerar gráfico diário de aproveitamento
ddj=desempenho_jogadores %>% 
  filter(Ano==year(Sys.Date()) & Mes==month(Sys.Date())) %>% group_by(DataJogo,Jogador) %>% 
  summarize(Jogos=as.integer(n()),Vitorias=as.integer(sum(Resultado=="Vitória"))) %>%
  arrange(Jogador,DataJogo,desc(Vitorias)) %>% ungroup() %>% group_by(Jogador) %>% ungroup()%>%
  mutate(Jogador=as.factor(Jogador),JogosAc=cumsum(Jogos),VitoriasAC=cumsum(Vitorias),AprovAc=VitoriasAC/JogosAc*100,
         pontuacao=ifelse(JogosAc<20,0,ifelse(JogosAc<40,(0.9+0.1*(JogosAc-20)/20)*AprovAc,AprovAc)))

ddj1=ddj%>%select(-c(JogosAc,VitoriasAC,pontuacao,AprovAc))

nomes_mensal= ddj %>% distinct(Jogador) %>% unlist(use.names = F)

dia_inicial=parse_date_time(paste0("1/",month(Sys.Date()),"/",year(Sys.Date())),order="dmy")
dia_final=parse_date_time(paste0(days_in_month(month(Sys.Date())),"/",month(Sys.Date()),"/",year(Sys.Date())),order="dmy")

datas_mes=seq(dia_inicial,to=dia_final,by="day")


djj=data.frame(DataJogo=as.Date(rep(datas_mes,each=length(nomes_mensal))),Jogador=rep(nomes_mensal,length(datas_mes)))
djj$Jogador=as.factor(djj$Jogador)
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

adj1= adj1 %>% filter(weekdays(DataJogo)!="sábado" & weekdays(DataJogo)!="domingo")



#gráfico da Pontuação diária

adj1%>% filter(as.double(pontuacao)>0, day(DataJogo)>1 & day(DataJogo)<=day(Sys.Date()) )%>%
  ggplot(aes(x=DataJogo,y=as.double(pontuacao), group=Jogador,color=Jogador))+geom_line(aes(linetype=Jogador), size=1.2)+
  scale_y_continuous(breaks=seq(0,100,10))+ labs(y="Pontuação")+
  scale_x_date(name="Dias", date_labels = "%d/%m/%Y", date_breaks = "2 day")+
  ggtitle(paste0("Histórico de Pontuação Diária - ",toupper(month(Sys.Date(), label=T, abbr=F)),"/",year(Sys.Date())))+ 
  scale_color_manual(values=lista_cores)+theme_dark()+
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "gray46") , 
        axis.text.x = element_text(angle=90))

#gráfico de Aproveitamento diário

# GA ----------------------------------------------------------------------


adj1%>% filter(as.double(AprovAc)>0,day(DataJogo)>1 & day(DataJogo)<=day(Sys.Date()) )%>%
  ggplot(aes(x=DataJogo,y=as.double(AprovAc)/100, group=fct_inorder(Jogador),color= fct_inorder(Jogador)))+geom_line(aes(linetype=fct_inorder(Jogador)), size=1.2)+
  scale_y_continuous(breaks=seq(0,1,0.1),labels=percent)+ labs(y="Aproveitamento")+
  scale_x_date(name="Dias", date_labels = "%d/%m/%Y", date_breaks = "1 days")+
  ggtitle(paste0("Histórico de Aproveitamento diário - ",toupper(month(Sys.Date(), label=T, abbr=F)),"/",year(Sys.Date())))+ 
  scale_color_manual(values=lista_cores)+theme_dark()+
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "gray46") , 
        axis.text.x = element_text(angle=90))
