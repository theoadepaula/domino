library(dplyr)
library(xlsx)
library(readxl)
library(ggplot2)
library(lubridate)
library(purrr)
#library(rvest)

#url="https://anaaguas-my.sharepoint.com/:x:/r/personal/othon_oliveira_ana_gov_br/_layouts/15/WopiFrame.aspx?sourcedoc=%7B02D3AF50-502C-4B49-BD7A-AD4CF7F28BDD%7D&file=DominoApp.xlsm&action=default"
#webpage <- read_html(url)
url2="https://anaaguas-my.sharepoint.com/:x:/r/personal/othon_oliveira_ana_gov_br/_layouts/15/doc.aspx?sourcedoc=%7B02d3af50-502c-4b49-bd7a-ad4cf7f28bdd%7D&action=default&uid=%7B02D3AF50-502C-4B49-BD7A-AD4CF7F28BDD%7D&ListItemId=78382&ListId=%7B399E0921-B271-41D9-9590-055361E41198%7D&odsp=1&env=prod"
testao=read.xlsx()
endereco_planilha="https://anaaguas-my.sharepoint.com/personal/othon_oliveira_ana_gov_br/_layouts/15/onedrive.aspx?id=%2Fpersonal%2Fothon_oliveira_ana_gov_br%2FDocuments%2FJogo//Domino.xlsm"
download.file(url2,"teste.xlsm")

teste_endereco=read.xlsx(endereco_planilha,sheetIndex=1)
read.xlsx(endereco_planilha,1)
?download.file
?read.xlsx
jogos=NULL
vitorias=NULL
jogos_duplas=NULL

vitorias_duplas=NULL
jogos_domino=read.xlsx("C:\\Users\\theo.paula\\Documents\\2017\\SOE\\DominoApp.xlsm","Historico",encoding = "UTF-8")
jogos_domino=read.xlsx("C:\\Users\\theo.paula\\Documents\\2017\\SOE\\DominoApp.xlsm","Historico",encoding = "UTF-8")

jogos_domino=read_xlsx("C:\\Users\\theo.paula\\OneDrive\\Data Science\\R\\Domino.xlsm","Historico")
jogos_domino=jogos_domino[-c(797:798),]

jogos_domino$DataJogo=as.Date(jogos_domino$DataJogo)
str(jogos_domino)
jogos_domino$DataJogo=as.Date(jogos_domino$DataJogo, format="%d/%m/%Y")


nt1j1=jogos_domino %>%
  group_by(Time1Jogador1) %>%
summarise(jogos1=n())

nt1j2=jogos_domino %>%
  group_by(Time1Jogador2) %>%
  summarise(jogos2=n())

nt2j1=jogos_domino %>%
  group_by(Time2Jogador1) %>%
  summarise(jogos3=n())

nt2j2=jogos_domino %>%
  group_by(Time2Jogador2) %>%
  summarise(jogos4=n())

jogadores=c(jogos_domino$Time1Jogador1,jogos_domino$Time1Jogador2,jogos_domino$Time2Jogador1,jogos_domino$Time2Jogador2)
jogadores=sort(unique(jogadores))
jogadores=data.frame(Jogador=jogadores)
names(jogadores)="Jogador"


jogadores=merge(jogadores,nt1j1, by=1, all=TRUE)
jogadores=merge(jogadores,nt1j2, by=1, all=TRUE)
jogadores=merge(jogadores,nt2j1, by=1, all=TRUE)
jogadores=merge(jogadores,nt2j2, by=1, all=TRUE)
jogadores[is.na(jogadores)]=0

jogadores=mutate(jogadores,jogos=jogos1+jogos2+jogos3+jogos4)
jogadores=jogadores[,-c(2:5)]
jogadores$part=round(jogadores$jogos/(nrow(jogos_domino))*100,2)

vt1=jogos_domino$PontosTime1>jogos_domino$PontosTime2
vt2=jogos_domino$PontosTime1<jogos_domino$PontosTime2

for (k in 1:nrow(jogadores)){
   nv1=nrow(jogos_domino[(jogos_domino$Time1Jogador1==as.character(jogadores[k,1]) & vt1)| (jogos_domino$Time1Jogador2 ==as.character(jogadores[k,1]) & vt1),])
   nv2=nrow(jogos_domino[(jogos_domino$Time2Jogador1==as.character(jogadores[k,1]) & vt2)| (jogos_domino$Time2Jogador2 ==as.character(jogadores[k,1]) & vt2),])
   vitorias=c(vitorias,(nv1+nv2))
}

jogadores=cbind(jogadores,vitorias)
jogadores$ind.vitoria=round(jogadores$vitorias/jogadores$jogos*100,2)
jogadores=arrange(jogadores, desc(ind.vitoria), desc(jogos))
print(paste("A quantidade de jogos jogados até agora:",nrow(jogos_domino)))
jogadores

jogadores_oficiais=jogadores[jogadores[,2]>30,]
jogadores_oficiais

gplot

rm(nt1j1,nt1j2,nt2j1,nt2j2,j,jogos,vitorias)

ndt1=jogos_domino %>%
                   group_by(Time1Jogador1,Time1Jogador2) %>%
                   summarise(jogos1=n())

ndt2=jogos_domino %>%
  group_by(Time2Jogador1,Time2Jogador2) %>%
  summarise(jogos2=n())

duplas=merge(ndt1,ndt2,by=c(1,2), all=T)

for (j in 1:nrow(duplas)){
  jogos_duplas=c(jogos_duplas,sum(duplas[j,3:4], na.rm = T))
}
duplas=cbind(duplas,jogos_duplas)
duplas=duplas[,-c(3:4)]
duplas=unique(duplas[order(duplas[,3]),])
nrow(duplas)

rm(dip11,dip12,dip21,dip22)

dp1=(duplas[as.character(duplas$Time1Jogador1)<as.character(duplas$Time1Jogador2),])
dp2=(duplas[as.character(duplas$Time1Jogador1)>as.character(duplas$Time1Jogador2),])
dp2=dp2[,c(2,1,3)]

duplas=merge(dp1,dp2,by=c(1,2), all=T)
duplas$jogos=rowSums(duplas[,-c(1,2)],na.rm = T)
duplas=duplas[,-c(3:4)]
#duplas$part=round(duplas$jogos/(sum(duplas$jogos)/2)*100,2)

head(duplas)

rm(ndt1,ndt2,j,jogos_duplas,dp1,dp2)

for (k in 1:nrow(duplas)){
  
  ndv1=nrow(jogos_domino[((jogos_domino$Time1Jogador1==as.character(duplas[k,1]) & jogos_domino$Time1Jogador2==as.character(duplas[k,2])) & vt1) | ((jogos_domino$Time1Jogador2==as.character(duplas[k,1]) & jogos_domino$Time1Jogador1==as.character(duplas[k,2])) & vt1) ,])
  ndv2=nrow(jogos_domino[((jogos_domino$Time2Jogador1==as.character(duplas[k,1]) & jogos_domino$Time2Jogador2==as.character(duplas[k,2])) & vt2) | ((jogos_domino$Time2Jogador2==as.character(duplas[k,1]) & jogos_domino$Time2Jogador1==as.character(duplas[k,2])) & vt2),])
  vitorias_duplas=c(vitorias_duplas,(ndv1+ndv2))
}
duplas=cbind(duplas,vitorias_duplas)
duplas$ind.vitoria=round(duplas$vitorias_duplas/duplas$jogos*100,2)
names(duplas)=c("Jogador 1","Jogador 2","Jogos","Vitórias","% de Vitória")
#names(duplas)=c("Jogador 1","Jogador 2","Jogos","% de jogos jogados","Vitórias","% de Vitória")
head(duplas[order(duplas[,5], decreasing = T),])

duplas_oficiais=duplas[duplas[,3]>=10,]
head(duplas_oficiais[order(duplas_oficiais[,5], decreasing = T),],10)
head(duplas_oficiais[duplas_oficiais[,1]!="Sérgio Glasherster" & duplas_oficiais[,2]!="Sérgio Glasherster",],10)
top_duplas=head(duplas_oficiais[order(duplas_oficiais[,5], decreasing = T),],10)
top_duplas[top_duplas[,1]!="Sérgio Glasherster" & top_duplas[,2]!="Sérgio Glasherster",]
top_duplas
write.csv2(top_duplas,file="top_duplas.csv")

ruim_duplas=head(duplas_oficiais[order(duplas_oficiais[,5]),],10)
ruim_duplas
write.csv2(ruim_duplas,file="ruim_duplas.csv")
