#carregar os pacotes abaixo para an?lise de dados
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

options(pillar.round = FALSE)
options(pillar.sigfigs = Inf)

theme_update(plot.title = element_text(hjust = 0.5))

logo=readPNG("C:\\Users\\theo.paula\\OneDrive\\Dados Zero\\lp1.png")
rast <- rasterGrob(logo, interpolate = T)

## custom draw method to
## calculate expansion factor on-the-fly
drawDetails.watermark <- function(x, rot = 45, ...){
  cex <- convertUnit(unit(1,"npc"), "mm", val=TRUE) /
    convertUnit(unit(1,"grobwidth", textGrob(x$val)), "mm",val=TRUE)
  
  grid.text(x$lab,  rot=rot, gp=gpar(cex = cex, col="orange",
                                     fontface = "bold", alpha = 0.5))
  
}

#Lendo o arquivo dos jogos e adicionando variáveis
caminho="\\\\SFI-024000\\Users\\othon.oliveira\\OneDrive - Agencia Nacional de Aguas\\Jogo\\Domino.xlsm"
#teste_caminho="https://anaaguas-my.sharepoint.com/personal/othon_oliveira_ana_gov_br/Documents/Jogo/Domino.xlsm"
jogos_domino=read_xlsx(caminho,"Historico")
jogos_domino=read_xlsx("src/Domino.xlsm","Historico")
jogos_domino=jogos_domino %>% 
  mutate(DataJogo=as.Date(DataJogo),
         Mes=as.integer(month(DataJogo)),
         Ano=as.integer(year(DataJogo)),
         MesAno=paste(Mes,Ano, sep="/"),
         Dia_semana= wday(DataJogo, label = T, abbr=F))

jogadores_aposentados=c("Isaias Araújo","Marcelo","Matheus Rodrigues","Pablo","Joelio","Raoni","Gesi")
jogadores_principais=c("Luis Jesiel","Othon Fialho","Gilson Marçal","Gaetan Dubois","Rubens Wanderley","Sergio Glasherster","Th?o Albuquerque")
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

#quantidade de jogos nos dias da semana
jogos_domino %>% filter(Ano==2018) %>% group_by(Dia_semana) %>%
  summarize(Contagem=n()) %>% 
  ggplot(.,aes(x=Dia_semana,y=Contagem,fill=Dia_semana))+geom_bar(stat="identity")+
  labs(title="Quantidade de Jogos durante a semana em 2018",x="Dia da semana",y="Quantidade")+
  annotation_custom(rast, ymin=260, xmin=4)+
  theme(legend.position = "none") #,plot.title = element_text(hjust = 0.5))
?annotation_custom



jogos_domino %>% filter(Ano==2018) %>% group_by(Dia_semana) %>%
  summarize(Contagem=n(),Cont_dia_semana=n_distinct(DataJogo),media_jogos=Contagem/Cont_dia_semana) %>% 
  ggplot(aes(x=Dia_semana,y=media_jogos,fill=Dia_semana))+geom_bar(stat="identity")+
  scale_y_continuous(breaks = seq(7,10,0.25))+ coord_cartesian(ylim = c(7,10))+
  labs(title="M?dia de Jogos durante a semana em 2018",x="Dia da semana",y="Quantidade")+
  theme(legend.position = "none")#,plot.title = element_text(hjust = 0.5))

#saber algozes de cada um
vitima="Gesi"
jogos_algozes=desempenho_jogadores[desempenho_jogadores$Jogador==vitima&desempenho_jogadores$Resultado=="Derrota",]$IdJogo
algozes=desempenho_jogadores[desempenho_jogadores$IdJogo %in% jogos_algozes,]

algozes %>% filter(!Jogador %in% jogadores_aposentados & Jogador!=vitima) %>% group_by(Jogador) %>%
summarise(Jogos=n(),Vitórias=sum(Resultado=="Vitória"),Saldo_Pt=sum(SaldoPts),
          Buchudas=sum(Buchuda=="Sim"),Aprov=round(mean(Resultado=="Vitória"),4)*100) %>%
          filter(Jogos>5) %>% arrange(desc(Aprov))


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

write.csv2(tabela_mensal,file=paste("csv/","ranking_mensal_",month(Sys.Date()),"_",year(Sys.Date()),".csv"))

#tabela do ano atual dos jogadores
desempenho_jogadores %>% 
  filter(Ano==year(Sys.Date()) & !Jogador %in% jogadores_aposentados) %>% group_by(Jogador) %>% 
  summarize(Jogos=n(),Vitórias=sum(Resultado=="Vitória"),Saldo_Pt=as.integer(sum(SaldoPts)),Buchudas=sum(Buchuda=="Sim"),Aprov=round(mean(Resultado=="Vitória"),4)*100,pontuacao=ifelse(Jogos<20,0,ifelse(Jogos<40,(0.9+0.1*(Jogos-20)/20)*Aprov,Aprov))) %>%
  filter(Jogos>20)%>% arrange(desc(pontuacao),desc(Aprov),desc(Buchudas),desc(Saldo_Pt),desc(Vitórias))

#tabela dos jogadores ativos
tabela_geral=desempenho_jogadores %>% 
  filter(!Jogador %in% jogadores_aposentados) %>%
  group_by(Ano,Mes,Jogador) %>% 
  summarize(Jogos=n(),Vitórias=sum(Resultado=="Vitória"),Saldo_Pt=sum(SaldoPts),Buchudas=sum(Buchuda=="Sim"),Aprov=round(mean(Resultado=="Vitória"),4)*100,pontuacao=ifelse(Jogos<20,0,ifelse(Jogos<40,(0.9+0.1*(Jogos-20)/20)*Aprov,Aprov))) %>%
  arrange(desc(pontuacao),desc(Aprov),desc(Buchudas),desc(Saldo_Pt),desc(Vitórias))

desempenho_jogadores %>% 
  filter(Jogador %in% c("Gabriel","George")) %>%
  group_by(Jogador) %>% 
  summarize(Jogos=n(),Vitórias=sum(Resultado=="Vitória"),Saldo_Pt=sum(SaldoPts),Buchudas=sum(Buchuda=="Sim"),Aprov=round(mean(Resultado=="Vitória"),4)*100,pontuacao=ifelse(Jogos<20,0,ifelse(Jogos<40,(0.9+0.1*(Jogos-20)/20)*Aprov,Aprov))) %>%
  arrange(desc(pontuacao),desc(Aprov),desc(Buchudas),desc(Saldo_Pt),desc(Vitórias))

#desempenho individual de jogador
desempenho_jogadores%>% filter(Jogador=="Francisco Carlos") %>% group_by(Ano,Mes)%>% 
  summarize(Jogos=n(),Vitórias=sum(Resultado=="Vitória"),Saldo_Pt=sum(SaldoPts),
            Buchudas=sum(Buchuda=="Sim"),Aprov=round(mean(Resultado=="Vitória"),4)*100) %>%
  mutate(Aprov=formatC(Aprov,2,format="f")) %>% ungroup() %>%
  add_row(Ano="Total",
   Mes="-",
   Jogos=sum(.$Jogos),
   Vitórias=sum(.$Vitórias),
   Saldo_Pt=sum(.$Saldo_Pt),
   Buchudas=sum(.$Buchudas),
   Aprov=formatC(sum(.$Vitórias)/sum(.$Jogos)*100,2,format="f"))

#gr?fico de 95% de confian?a para o aproveitamento geral dos jogadores
tabela_geral %>% filter(Jogos>9) %>% group_by(Jogador) %>% summarise(Aproveitamento=mean(Aprov),CI=1.96*sd(Aprov)/sqrt(sum(Jogos))) %>% ggplot(aes(x=reorder(Jogador,-Aproveitamento),y=Aproveitamento,color=Jogador))+geom_errorbar(aes(ymin=Aproveitamento-CI,ymax=Aproveitamento+CI))+ylim(25,65)+
ggtitle("Intervalo de Confian?a 95% do Aproveitamento")+theme_bw()+xlab("Jogador")+theme(axis.text.x=element_text(angle=90), plot.title = element_text(hjust = 0.5), legend.position = "none")

#intervalo de confian?a descartando o m?s atual
tabela_geral %>% filter(Jogos>9) %>% filter(Mes!=month(Sys.Date())) %>% group_by(Jogador) %>% summarise(Jogos=sum(Jogos),Aproveitamento=mean(Aprov),CI=1.96*sd(Aprov)/sqrt(sum(Jogos)))
#intervalo de confian?a geral
tabela_geral %>% filter(Jogos>9) %>% group_by(Jogador) %>% summarise(Jogos=sum(Jogos),Aproveitamento=mean(Aprov),CI=1.96*sd(Aprov)/sqrt(sum(Jogos)))

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
#adj1 %>% filter(Jogador=="Gesi") %>% print(n=Inf)

#gr?fico do aproveitamento di?rio
adj1%>% filter(day(DataJogo)>1 & day(DataJogo)<=day(Sys.Date()),AprovAc>0)%>%
  ggplot(aes(x=DataJogo,y=as.double(AprovAc), group=Jogador,color=Jogador))+geom_line(aes(linetype=Jogador), size=1.2)+
  scale_y_continuous(breaks=seq(0,100,10))+ labs(y="Pontuação")+
  scale_x_date(name="Dias", date_labels = "%d/%m/%Y", date_breaks = "1 day")+
  ggtitle(paste0("Histórico de Aproveitamento Di?rio - ",month(Sys.Date(), label=T, abbr=F),"/",year(Sys.Date())))+  
  scale_color_brewer(palette="Set3")+theme_dark()+theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90))

#gráfico da Pontuação diária
adj1%>% filter(as.double(pontuacao)>0, day(DataJogo)>1 & day(DataJogo)<=day(Sys.Date()))%>%
  ggplot(aes(x=DataJogo,y=as.double(pontuacao), group=Jogador,color=Jogador))+geom_line(aes(linetype=Jogador), size=1.2)+
  scale_y_continuous(breaks=seq(0,100,10))+ labs(y="Pontuação")+
  scale_x_date(name="Dias", date_labels = "%d/%m/%Y", date_breaks = "1 day")+
  ggtitle(paste0("Histórico de Pontuação Di?ria - ",toupper(month(Sys.Date(), label=T, abbr=F)),"/",year(Sys.Date())))+  scale_color_brewer(palette="Set3")+theme_dark()+theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90))

adj1%>% filter(as.double(pontuacao)>0, day(DataJogo)>1 & day(DataJogo)<=day(Sys.Date()) )%>%
  ggplot(aes(x=DataJogo,y=as.double(pontuacao), group=Jogador,color=Jogador))+geom_line(aes(linetype=Jogador), size=1.2)+
  scale_y_continuous(breaks=seq(0,100,10))+ labs(y="Pontuação")+
  scale_x_date(name="Dias", date_labels = "%d/%m/%Y", date_breaks = "2 days")+
  ggtitle(paste0("Histórico de Pontuação Di?ria - ",toupper(month(Sys.Date(), label=T, abbr=F)),"/",year(Sys.Date())))+ 
  scale_color_manual(values=lista_cores)+theme_dark()+
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "gray46") , 
        axis.text.x = element_text(angle=90)) +
  annotation_custom(xmin=-Inf, ymin=-Inf, xmax=Inf, ymax=Inf, watermarkGrob())

 #tabelas e gr?ficos para aproveitamento.
 tabela_aproveitamento=desempenho_jogadores %>% group_by(Ano,Mes,MesAno,Jogador) %>% 
   summarize(Jogos=n(),Vitórias=sum(Resultado=="Vitória"),Saldo_Pt=sum(SaldoPts),Buchudas=sum(Buchuda=="Sim"),Aprov=round(mean(Resultado=="Vitória"),4)*100,pontuacao=ifelse(Jogos<20,0,ifelse(Jogos<40,(0.9+0.1*(Jogos-20)/20)*Aprov,Aprov))) %>%
   arrange(Ano, Mes,Jogador) %>% ungroup()

tabela_aproveitamento1=desempenho_jogadores %>% group_by(Ano,Mes,MesAno,Jogador) %>% 
  summarize(Jogos=n(),Vitórias=sum(Resultado=="Vitória"),Saldo_Pt=sum(SaldoPts),Buchudas=sum(Buchuda=="Sim"),Aprov=round(mean(Resultado=="Vitória"),4)*100,pontuacao=ifelse(Jogos<40,log(Jogos, base=40)*Aprov,Aprov)) %>%
  arrange(Ano, Mes,Jogador) %>% ungroup()


tabela_aproveitamento$MesAno= as.Date(parse_date_time(tabela_aproveitamento$MesAno, "%m/%Y"))
tabela_aproveitamento1$MesAno= as.Date(parse_date_time(tabela_aproveitamento1$MesAno, "%m/%Y"))


tabela_aproveitamento %>% filter((!Jogador %in% jogadores_aposentados & Jogador %in% jogadores_principais &Mes!=month(Sys.Date()))) %>% ggplot(aes(x=MesAno,y=Aprov, color=Jogador))+
  geom_line(linetype=2, size=1.2)+ylim(0,100)+scale_x_date(name="Meses", date_labels = "%m/%Y", date_breaks = "1 month")+
  ggtitle("Histórico de Aproveitamento")+theme(plot.title = element_text(hjust = 0.5))+  scale_color_brewer(palette="Set3")

#gr?fico estilizado
tabela_aproveitamento %>% filter((!Jogador %in% jogadores_aposentados & Jogador %in% jogadores_principais &Mes!=month(Sys.Date()))) %>% ggplot(aes(x=MesAno,y=Aprov, color=Jogador))+
  geom_line(linetype=2, size=1.2)+ylim(0,100)+scale_x_date(name="Meses", date_labels = "%m/%Y", date_breaks = "1 month")+
  ggtitle("Histórico de Aproveitamento")+ scale_color_brewer(palette="Set3")+theme_wsj()+theme(plot.title = element_text(hjust = 0.5))


#gerar xlsx da m?dia de aproveitamento dos jogadores
write.xlsx(tabela_aproveitamento %>% filter(Ano==2018&month(MesAno)!=month(Sys.Date())& !Jogador %in% jogadores_aposentados) %>% group_by(Jogador) %>% summarize(media_jogos=mean(Jogos), media_aprov=mean(Aprov), dp_aprov=sd(Aprov), media_pontuacao=mean(pontuacao), dp_pontuacao=sd(pontuacao)) %>%
  filter(media_jogos>6&is.na(dp_aprov)==F) %>% arrange(desc(media_pontuacao)) %>% print(n = Inf), "media_jogadores.xlsx")


#remodelar o banco de dados para o desempenho das duplas
jogos_domino_wide_ordenado= jogos_domino%>% gather(Time, Jogador,-c(IdJogo,DataJogo,Mes,Ano,MesAno,PontosTime1,
                                                               PontosTime2,SaldoPts,Dia_semana)) %>% 
  mutate(Times=ifelse(Time=="Time1Jogador1"|Time=="Time1Jogador2","Time1","Time2"))%>% 
  arrange(IdJogo,Times,Jogador) %>% select(-Times) 

jogos_domino_wide_ordenado$Time=c("Time1Jogador1","Time1Jogador2","Time2Jogador1","Time2Jogador2")

jogos_domino_ordenado=jogos_domino_wide_ordenado%>%spread(Time, Jogador)

jogos_duplas= jogos_domino_ordenado %>% unite(Dupla1,Time1Jogador1,Time1Jogador2,sep="/") %>%
                                        unite(Dupla2,Time2Jogador1,Time2Jogador2,sep="/") %>%
                gather(Time_Dupla,Dupla,Dupla1:Dupla2)%>% arrange(IdJogo) %>% 
  mutate(Pontos=ifelse(Time_Dupla=="Dupla1",PontosTime1,PontosTime2),
         SaldoPts=ifelse(Time_Dupla=="Dupla1",SaldoPts,-(SaldoPts)),
         Resultado=ifelse(SaldoPts>0,"Vitória","Derrota"),
         Buchuda=ifelse((PontosTime1>PontosTime2&PontosTime2==0)&SaldoPts>0,"Sim",ifelse((PontosTime1<PontosTime2&PontosTime1==0)&SaldoPts>0,"Sim","Não")),
         Placar=ifelse(PontosTime1>PontosTime2,paste0(PontosTime1,"x",PontosTime2),paste0(PontosTime2,"x",PontosTime1)))%>% 
        separate(Dupla, c("Parceiro1","Parceiro2"),sep="/", remove = F) %>%
        arrange(IdJogo,desc(SaldoPts),Dupla)

desempenho_duplas= jogos_duplas[,-c(3,4,9)]

#remover data.frames abaixo
rm(jogos_domino_wide_ordenado,jogos_duplas)

#tabela para ver os placares dos jogos
placares=desempenho_duplas %>% filter(Resultado=="Vitória" & Ano==2018) %>% group_by(Placar) %>% summarize(Quantidade=n()) %>% ungroup()

#histograma dos placares at? agora  
ggplot(placares,aes(x=Placar,y=Quantidade,fill=Placar))+geom_bar(stat="identity")+ geom_line(aes(x=Placar,y=Quantidade,group=1),color="red", size=1.2, linetype=3)+
scale_y_continuous(breaks=seq(0,800,25))+ theme_wsj()+ggtitle("Histograma dos Resultados de 2018")+theme(legend.position = "none") #,plot.title = element_text(hjust = 0.5))

#para verificar qual é o melhor parceiro
desempenho_duplas%>%  filter(Parceiro1=="Othon Fialho"|Parceiro2=="Othon Fialho") %>% group_by(Dupla) %>% 
  summarize(Jogos=n(),Vitórias=sum(Resultado=="Vitória"),Saldo_Pt=as.integer(sum(SaldoPts)),Buchudas=sum(Buchuda=="Sim"),Aprov=round(mean(Resultado=="Vitória"),4)*100,pontuacao=ifelse(Jogos<20,0,ifelse(Jogos<40,(0.9+0.1*(Jogos-20)/20)*Aprov,Aprov))) %>%
  filter(Jogos>10) %>% arrange(desc(Aprov),desc(Buchudas),desc(Saldo_Pt),desc(Vitórias))

#para verificar qual é o melhor dupla do ano
desempenho_duplas %>%  filter(!str_detect(Dupla,"Gesi|Joélio|Pablo|Isaias Araújo|Matheus Rodrigues|Marcelo"),
                              Ano==year(Sys.Date()) & semester(DataJogo)==2) %>% group_by(Dupla) %>% 
  summarize(Jogos=n(),Vitórias=sum(Resultado=="Vitória"),Saldo_Pt=as.integer(sum(SaldoPts)),Buchudas=sum(Buchuda=="Sim"),Aprov=round(mean(Resultado=="Vitória"),4)*100,pontuacao=ifelse(Jogos<20,0,ifelse(Jogos<40,(0.9+0.1*(Jogos-20)/20)*Aprov,Aprov))) %>%
  arrange(desc(Aprov),desc(Buchudas),desc(Saldo_Pt),desc(Vitórias)) %>% filter(Jogos>5) %>% 
  select(-pontuacao) %>% slice(1:20) %>% formattable()

jogos_domino %>% filter(((Time1Jogador1=="Gilson Marçal"|Time1Jogador2=="Gilson Marçal") & PontosTime1<PontosTime2)|((Time2Jogador1=="Gilson Marçal"|Time2Jogador2=="Gilson Marçal") & PontosTime1>PontosTime2)) %>%
  gather(times, Jogador,-c(IdJogo,DataJogo,Mes,Ano,MesAno,PontosTime1,PontosTime2,SaldoPts,Dia_semana)) %>%
  arrange(IdJogo,SaldoPts,Jogador) %>% mutate(Pontos=ifelse(times=="Time1Jogador1"|times=="Time1Jogador2",PontosTime1,PontosTime2),
                                              SaldoPts=ifelse(times=="Time1Jogador1"|times=="Time1Jogador2",SaldoPts,-(SaldoPts)),
                                              Resultado=ifelse(SaldoPts>0,"Vitória","Derrota"),
                                              Buchuda=ifelse((PontosTime1>PontosTime2&PontosTime2==0)&SaldoPts>0,"Sim",ifelse((PontosTime1<PontosTime2&PontosTime1==0)&SaldoPts>0,"Sim","Não")))%>% 
  arrange(IdJogo,SaldoPts,Jogador) %>%
  group_by(Jogador) %>% 
  summarize(Jogos=n(),Vitórias=sum(Resultado=="Vitória"),Saldo_Pt=as.integer(sum(SaldoPts)),
            Buchudas=sum(Buchuda=="Sim"),Aprov=mean(Resultado=="Vitória")*100) %>%
  arrange(desc(Aprov),desc(Buchudas),desc(Saldo_Pt),desc(Vitórias)) %>% filter(!str_detect(Jogador,"Gilson Marçal|Gesi|Joélio|Pablo|Isaias Araújo|Matheus Rodrigues|Marcelo"),Jogos>5) %>% slice(1:10)

algoz=function(vitima){
  jogos_domino %>% filter(((Time1Jogador1==vitima|Time1Jogador2==vitima) & PontosTime1<PontosTime2)|
                            ((Time2Jogador1==vitima|Time2Jogador2==vitima) & PontosTime1>PontosTime2)) %>%
    gather(times, Jogador,-c(IdJogo,DataJogo,Mes,Ano,MesAno,PontosTime1,PontosTime2,SaldoPts,Dia_semana)) %>%
    arrange(IdJogo,SaldoPts,Jogador) %>% mutate(Pontos=ifelse(times=="Time1Jogador1"|times=="Time1Jogador2",PontosTime1,PontosTime2),
                                                SaldoPts=ifelse(times=="Time1Jogador1"|times=="Time1Jogador2",SaldoPts,-(SaldoPts)),
                                                Resultado=ifelse(SaldoPts>0,"Vitória","Derrota"),
                                                Buchuda=ifelse((PontosTime1>PontosTime2&PontosTime2==0)&SaldoPts>0,"Sim",ifelse((PontosTime1<PontosTime2&PontosTime1==0)&SaldoPts>0,"Sim","Não")))%>% 
    arrange(IdJogo,SaldoPts,Jogador) %>% filter(!Jogador %in% jogadores_aposentados) %>%
    group_by(Jogador) %>% 
    summarize(Jogos=n(),Vitórias=sum(Resultado=="Vitória"),Saldo_Pt=as.integer(sum(SaldoPts)),
              Buchudas=sum(Buchuda=="Sim"),Aprov=mean(Resultado=="Vitória")*100) %>%
    arrange(desc(Aprov),desc(Buchudas),desc(Saldo_Pt),desc(Vitórias)) %>% filter(!str_detect(Jogador,vitima),Jogos>5) %>%
    slice(1) %>% select(Jogador)
}

algoz("Deusdeir") 

Jogadores= desempenho_jogadores %>% count(Jogador) %>% filter(n>30 & !Jogador %in% jogadores_aposentados) %>%
  .[,1,drop=T]
rm(Jogadores)
algoz(Jogadores)
length(Jogadores)

vt=as.data.frame(c("Vítima"=NULL,"Algoz"=NULL))

for(i in 1:length(Jogadores)){
  vt[i,1]=Jogadores[i]
  vt[i,2]=algoz(Jogadores[i])
}

colnames(vt)=c("Vítima","Algoz")

formattable(vt)

?formattable()

#pegar o algoz
tab_impar=desempenho_duplas %>% select(-Parceiro1,-Parceiro2,-Ano,-MesAno,-Mes) %>% filter(Resultado=="Vitória") 
tab_par=desempenho_duplas %>% select(-Parceiro1,-Parceiro2,-Ano,-MesAno,-Mes) %>% filter(Resultado=="Derrota")

tab_comp=tab_impar %>% left_join(tab_par,by=c("IdJogo","DataJogo"),suffix=c(".dupla1",".dupla2"))


desempenho_duplas %>% 
  filter(Ano==year(Sys.Date())) %>% group_by(Dupla) %>% 
  summarize(Jogos=n(),Vitórias=sum(Resultado=="Vitória"),Saldo_Pt=as.integer(sum(SaldoPts)),Buchudas=sum(Buchuda=="Sim"),Aprov=round(mean(Resultado=="Vitória"),4)*100,pontuacao=ifelse(Jogos<20,0,ifelse(Jogos<40,(0.9+0.1*(Jogos-20)/20)*Aprov,Aprov))) %>%
  filter(Jogos>4) %>% arrange(desc(Aprov),desc(Buchudas),desc(Saldo_Pt),desc(Vitórias))

quarter(Sys.Date())
#para verificar qual é o melhor dupla do ano de jogadores ativos com mais de 10 jogos
desempenho_duplas%>%  filter(!(Parceiro1 %in% jogadores_aposentados) & !(Parceiro2 %in% jogadores_aposentados)) %>% group_by(Dupla) %>% 
  summarize(Jogos=n(),Vitórias=sum(Resultado=="Vitória"),Saldo_Pt=sum(SaldoPts),Buchudas=sum(Buchuda=="Sim"),Aprov=round(mean(Resultado=="Vitória"),4)*100,pontuacao=ifelse(Jogos<20,0,ifelse(Jogos<40,(0.9+0.1*(Jogos-20)/20)*Aprov,Aprov))) %>%
  filter(Jogos>10) %>% arrange(desc(Aprov),desc(Buchudas),desc(Saldo_Pt),desc(Vitórias))

#para verificar qual é o melhor dupla do ano de jogadores ativos com mais de 5 jogos
desempenho_duplas%>%  filter(!(Parceiro1 %in% jogadores_aposentados) & !(Parceiro2 %in% jogadores_aposentados) & Ano==2018) %>% group_by(Dupla) %>% 
  summarize(Jogos=n(),Vitórias=sum(Resultado=="Vitória"),Saldo_Pt=sum(SaldoPts),Buchudas=sum(Buchuda=="Sim"),Aprov=round(mean(Resultado=="Vitória"),4)*100,pontuacao=ifelse(Jogos<20,0,ifelse(Jogos<40,(0.9+0.1*(Jogos-20)/20)*Aprov,Aprov))) %>%
  filter(Jogos>5) %>% arrange(desc(Buchudas),desc(Aprov),desc(Saldo_Pt),desc(Vitórias))

#remover todos os objetos
rm(list = ls())
