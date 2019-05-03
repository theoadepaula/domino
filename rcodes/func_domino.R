pacman::p_load('xlsx','readxl','ggthemes','lubridate','tidyverse','RColorBrewer','formattable','kableExtra')


options(pillar.round = FALSE)
options(pillar.sigfigs = Inf)

theme_update(plot.title = element_text(hjust = 0.5))


tab_mes=function(mes=month(Sys.Date()),ano=year(Sys.Date())){
  jogos_domino=read_xlsx("src/Domino.xlsm","Historico")
  jogos_domino=jogos_domino %>% 
    mutate(DataJogo=as.Date(DataJogo),
           Mes=as.integer(month(DataJogo)),
           Ano=as.integer(year(DataJogo)),
           MesAno=paste(Mes,Ano, sep="/"),
           Dia_semana= wday(DataJogo, label = T, abbr=F))
  
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
  
  tabela_mensal=desempenho_jogadores %>% 
    filter(Ano==ano & Mes==mes) %>% group_by(Jogador) %>% 
    summarize(Jogos=n(),Vitórias=sum(Resultado=="Vitória"),Saldo_Pt=as.integer(sum(SaldoPts)),
              Buchudas=sum(Buchuda=="Sim"),Aprov=mean(Resultado=="Vitória")*100,
              pontuacao=ifelse(Jogos<20, formatC(0,2,format="f") ,ifelse(Jogos<40,formatC((0.9+0.1*(Jogos-20)/20)*Aprov,2,format="f"),formatC(Aprov,2,format="f"))),
              Ano=ano,Mes=mes) %>%
    arrange(desc(pontuacao),desc(Aprov),desc(Buchudas),desc(Saldo_Pt),desc(Vitórias))
  
  tabela_mensal$Aprov=formatC(tabela_mensal$Aprov,2,format="f")
  
  tabela_mensal
}

graf_mes=function(mes=month(Sys.Date()),ano=year(Sys.Date())){
  jogos_domino=read_xlsx("src/Domino.xlsm","Historico")
  jogos_domino=jogos_domino %>% 
    mutate(DataJogo=as.Date(DataJogo),
           Mes=as.integer(month(DataJogo)),
           Ano=as.integer(year(DataJogo)),
           MesAno=paste(Mes,Ano, sep="/"),
           Dia_semana= wday(DataJogo, label = T, abbr=F))
  
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
  
  ddj=desempenho_jogadores %>% 
    filter(Ano==year(Sys.Date()) & Mes==month(Sys.Date())) %>% 
    group_by(DataJogo,Jogador) %>% 
    summarize(Jogos=as.integer(n()),Vitorias=as.integer(sum(Resultado=="Vitória"))) %>%
    arrange(Jogador,DataJogo,desc(Vitorias)) %>%
    ungroup() %>% group_by(Jogador) %>% ungroup()%>%
    mutate(Jogador=as.factor(Jogador),JogosAc=cumsum(Jogos),VitoriasAC=cumsum(Vitorias),
           AprovAc=VitoriasAC/JogosAc*100,
           pontuacao=ifelse(JogosAc<20,0,ifelse(JogosAc<40,(0.9+0.1*(JogosAc-20)/20)*AprovAc,AprovAc)))
  
  ddj1=ddj%>%select(-c(JogosAc,VitoriasAC,pontuacao,AprovAc))
  
  nomes_mensal= ddj %>% distinct(Jogador) %>% unlist(use.names = F)
  
  dia_inicial=parse_date_time(paste0("1/",month(Sys.Date()),"/",year(Sys.Date())),order="dmy")
  dia_final=parse_date_time(paste0(days_in_month(month(Sys.Date())),"/",month(Sys.Date()),"/",year(Sys.Date())),order="dmy")
  
  djj=crossing(DataJogo=as.Date(seq(dia_inicial,to=dia_final,by="day")),Jogador=nomes_mensal)
  
  adj=djj%>%left_join(ddj1)

  adj[is.na(adj)]=0
  adj1=adj %>% group_by(Jogador) %>% mutate(Jogos=as.integer(Jogos),
                                            Vitorias=as.integer(Vitorias),
                                            JogosAc=as.integer(cumsum(Jogos)),
                                            VitoriasAC=as.integer(cumsum(Vitorias)),
                                            AprovAc=ifelse(JogosAc==0,0,VitoriasAC/JogosAc),
                                            pontuacao=ifelse(JogosAc<20,0,ifelse(JogosAc<40,(0.9+0.1*(JogosAc-20)/20)*AprovAc,AprovAc)))
  adj1$AprovAc=formatC(adj1$AprovAc,2,format="f")
  adj1$pontuacao=formatC(adj1$pontuacao,2,format="f")


  adj1%>% filter(as.double(pontuacao)>0, day(DataJogo)>1 & day(DataJogo)<=day(Sys.Date()) )%>%
    ggplot(aes(x=DataJogo,y=as.double(pontuacao), group=Jogador,color=Jogador))+geom_line(aes(linetype=Jogador), size=1.2)+
    scale_y_continuous(breaks=seq(0,1,.05), labels = scales::percent)+ labs(y="Pontuação")+
    scale_x_date(name="Dias", date_labels = "%d/%m/%Y", date_breaks = "2 day")+
    ggtitle(paste0("Histórico de Pontuação Diária - ",toupper(month(Sys.Date(), label=T, abbr=F)),"/",year(Sys.Date())))+
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
  #row_spec(which(tab_mes()[,7]<1)[1]-1, bold = T, color = "white", background = "red") %>%
  row_spec(which(tab_mes()[,7]<1), bold = T, color = "purple", background = "green")

write.csv2(tab_mes()%>%select(-c(Mes,Ano)),file=paste("csv/","ranking_mensal_",month(Sys.Date()),"_",year(Sys.Date()),".csv"))

knitr::kable(tab_mes()%>%select(-c(Mes,Ano)) %>% mutate(pontuacao=round(as.numeric(pontuacao))), caption = paste("Ranking do Mês -",month(Sys.Date(), label=T, abbr=F),"/",year(Sys.Date())),
             col.names = c("Jogador","Nº de Jogos","Vitórias","Saldo de Pontos","Buchudas","Aproveitamento","Pontuação"),
             align=c("l",rep("c",6))) %>% kable_styling(bootstrap_options = c("striped", "hover")) %>%
  row_spec(1, bold = T, color = "yellow", background = "gold")%>%
  row_spec(2, bold = T, color = "gray", background = "silver")%>%
  row_spec(3, bold = T, color = "#da9f65", background = "#e9c7a5") %>%
  row_spec(which(tab_mes()[,7]<1)[1]-1, bold = T, color = "white", background = "red") %>%
  row_spec(which(tab_mes()[,7]<1), bold = T, color = "purple", background = "green")

graf_mes()

remove(list=ls())

tab_mes() %>% select(pontuacao) %>% filter(pontuacao>1) %>% pull() %>% as.numeric() %>% boxplot(main="Boxplot da pontuação")

tab_mes()%>% filter(pontuacao>1) %>% summarise(ic_menor=mean(as.numeric(pontuacao)),media=mean(as.numeric(pontuacao)))

tab_mes() %>% filter(pontuacao>1) %>% ggplot(aes(x = "1",y=as.numeric(pontuacao)))+
  theme_bw()+
  theme(axis.text.y = element_blank())+ scale_y_continuous(breaks = seq(40,60,2))+
  geom_boxplot()+labs(title = "Boxplot de Pontuação", y="Pontuação",x='')+coord_flip()

jogos_domino %>% count(ano=year(DataJogo),teste=month(DataJogo)) %>% filter(ano>2017, teste<5) %>%
  ggplot(aes(factor(teste),n,group=ano,fill=factor(teste)))+geom_col(position="dodge2")+guides(fill=FALSE)

tab_mes() %>% summarise(mean(Jogos))



map()

meses_jogos=crossing(ano=c(2017:2019),mes=(1:12)) 
meses_jogos= meses_jogos[-c(1:7,29:36),]

jogos=map2(.x=meses_jogos$mes,.y=meses_jogos$ano,~tab_mes(.x,.y))

map_df(jogos,~.x%>%summarise(Ano=mean(Ano),Mes=mean(Mes),media=mean(Jogos))) %>%
  ggplot(aes(y=media,x=factor(Mes),group=factor(Ano),fill=factor(Ano)))+geom_col(position ="dodge2")+
  scale_y_continuous(breaks = seq(0,120,15))+
  labs(title="Média de Jogos por Mês",subtitle = "De agosto de 2017 a abril de 2019",
       y="Número de Jogos",x="Meses")

map_df(jogos,~.x%>%summarise(Ano=mean(Ano),Mes=mean(Mes),media=mean(Jogos))) %>% group_by(Ano) %>%
  summarise(media=mean(media))

meses_jogos %>% print(n=Inf)
?guides
jogos_domino %>% count(ano=year(DataJogo),teste=month(DataJogo)) %>% 
  ggplot(aes(factor(teste),n,group=ano,color=factor(ano)))+geom_line()

jogos_domino %>% count(ano=year(DataJogo),teste=month(DataJogo)) %>% print(n=Inf)
