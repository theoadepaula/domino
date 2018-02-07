library(dplyr)
library(xlsx)

jogos=NULL
vitorias=NULL
jogos_duplas=NULL
vitorias_duplas=NULL

jogos_domino=read.xlsx("C:\\Users\\theo.paula\\Documents\\2017\\SOE\\DominoApp.xlsm","Historico",encoding = "UTF-8")
jogos_domino$DataJogo=as.Date(jogos_domino$DataJogo)
jogos_domino=(jogos_domino[jogos_domino$DataJogo>="2017-12-01",])


nt1j1=data.frame(jogos_domino %>%
                   group_by(Time1Jogador1) %>%
                   count)

nt1j2=data.frame(jogos_domino %>%
                   group_by(Time1Jogador2) %>%
                   count)

nt2j1=data.frame(jogos_domino %>%
                   group_by(Time2Jogador1) %>%
                   count)

nt2j2=data.frame(jogos_domino %>%
                   group_by(Time2Jogador2) %>%
                   count)

jogadores=c(levels(jogos_domino$Time1Jogador1),levels(jogos_domino$Time1Jogador2),
            levels(jogos_domino$Time2Jogador1),levels(jogos_domino$Time2Jogador2))
jogadores=ordered(unique(jogadores))
jogadores=data.frame(jogadores)
data

jogadores=merge(jogadores,nt1j1, by=1, all=TRUE)
jogadores=merge(jogadores,nt1j2, by=1, all=TRUE)
jogadores=merge(jogadores,nt2j1, by=1, all=TRUE)
jogadores=merge(jogadores,nt2j2, by=1, all=TRUE)


for (j in 1:nrow(jogadores)){
  jogos=c(jogos,sum(jogadores[j,2:5], na.rm = T))
}
jogadores=cbind(jogadores,jogos)
jogadores=jogadores[,-c(2:5)]
#jogadores$part=round(jogadores$jogos/(nrow(jogos_domino)),2)
ncol(jogadores)

vt1=jogos_domino$PontosTime1>jogos_domino$PontosTime2
vt2=jogos_domino$PontosTime1<jogos_domino$PontosTime2

for (k in 1:nrow(jogadores)){
  nv1=nrow(jogos_domino[(jogos_domino$Time1Jogador1==as.character(jogadores[k,1]) & vt1)| (jogos_domino$Time1Jogador2 ==as.character(jogadores[k,1]) & vt1),])
  nv2=nrow(jogos_domino[(jogos_domino$Time2Jogador1==as.character(jogadores[k,1]) & vt2)| (jogos_domino$Time2Jogador2 ==as.character(jogadores[k,1]) & vt2),])
  vitorias=c(vitorias,(nv1+nv2))
}

jogadores=cbind(jogadores,vitorias)
ncol(jogadores)
jogadores$ind.vitoria=round(jogadores$vitorias/jogadores$jogos*100,2)
names(jogadores)=c("Jogador","Jogos","Vitórias","% de Vitória")
jogadores=jogadores[order(jogadores[,4], decreasing = T),]
print(paste("A quantidade de jogos jogados até agora:",nrow(jogos_domino)))
jogadores

jogadores_oficiais=jogadores[jogadores[,2]>2,]
row.names(jogadores_oficiais)=1:nrow(jogadores_oficiais)
jogadores_oficiais
write.csv2(jogadores_oficiais,file = "ranking_dezembro.csv")

rm(nt1j1,nt1j2,nt2j1,nt2j2,j,jogos,vitorias)

ndt1=data.frame(jogos_domino %>%
                  group_by(Time1Jogador1,Time1Jogador2) %>%
                  count)

ndt2=data.frame(jogos_domino %>%
                  group_by(Time2Jogador1,Time2Jogador2) %>%
                  count)

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

duplas_oficiais=duplas[duplas[,3]>=2,]
head(duplas_oficiais[order(duplas_oficiais[,5], decreasing = T),],10)
head(duplas_oficiais[duplas_oficiais[,1]!="Sérgio Glasherster" & duplas_oficiais[,2]!="Sérgio Glasherster",],10)
top_duplas=head(duplas_oficiais[order(duplas_oficiais[,5], decreasing = T),],5)
top_duplas[top_duplas[,1]!="Sérgio Glasherster" & top_duplas[,2]!="Sérgio Glasherster",]
top_duplas
write.csv2(top_duplas,file="top_duplas.csv")
?write.csv2

ruim_duplas=head(duplas_oficiais[order(duplas_oficiais[,5]),],5)
ruim_duplas
write.csv2(ruim_duplas,file="ruim_duplas.csv")
