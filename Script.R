IGD<-IGD_jan_2016_2017_2018_2019 #Cria objeto com base IGD do VisData de jan/19 
View(IGD) #Visualiza objeto criado

#Analisando IGD-M
summary(IGD$`14 - IGD-M ( Fator 1 x Fator 2 x Fator 3 x Fator 4 )-01/2016`) #Verificando quartis para aplicar fórmula Q1-1.5*(Q3-Q1) e encontrar valor abaixo do qual IGD-M 1 é outlier para jan/16
summary(IGD$`14 - IGD-M ( Fator 1 x Fator 2 x Fator 3 x Fator 4 )-01/2017`) #Verificando quartis para aplicar fórmula Q1-1.5*(Q3-Q1) e encontrar valor abaixo do qual IGD-M 1 é outlier para jan/17
summary(IGD$`14 - IGD-M ( Fator 1 x Fator 2 x Fator 3 x Fator 4 )-01/2018`) #Verificando quartis para aplicar fórmula Q1-1.5*(Q3-Q1) e encontrar valor abaixo do qual IGD-M 1 é outlier para jan/18
summary(IGD$`14 - IGD-M ( Fator 1 x Fator 2 x Fator 3 x Fator 4 )-01/2019`) #Verificando quartis para aplicar fórmula Q1-1.5*(Q3-Q1) e encontrar valor abaixo do qual IGD-M 1 é outlier para jan/19
MenorIGDM2016<-IGD[which(IGD$`14 - IGD-M ( Fator 1 x Fator 2 x Fator 3 x Fator 4 )-01/2016`<0.53),] #Cria objeto com os municípios com IGD-M considerado outlier para jan/16 (no caso, abaixo de 0.53)
MenorIGDM2017<-IGD[which(IGD$`14 - IGD-M ( Fator 1 x Fator 2 x Fator 3 x Fator 4 )-01/2017`<0.60),] #Cria objeto com os municípios com IGD-M considerado outlier para jan/17 (no caso, abaixo de 0.60)
View(MenorIGDM2017)
MenorIGDM2018<-IGD[which(IGD$`14 - IGD-M ( Fator 1 x Fator 2 x Fator 3 x Fator 4 )-01/2018`<0.67),] #Cria objeto com os municípios com IGD-M considerado outlier para jan/18 (no caso, abaixo de 0.67)
MenorIGDM2019<-IGD[which(IGD$`14 - IGD-M ( Fator 1 x Fator 2 x Fator 3 x Fator 4 )-01/2019`<0.725),] #Cria objeto com os municípios com IGD-M considerado outlier para jan/19 (no caso, abaixo de 0.725)
boxplot(summary(IGD$`14 - IGD-M ( Fator 1 x Fator 2 x Fator 3 x Fator 4 )-01/2016`),summary(IGD$`14 - IGD-M ( Fator 1 x Fator 2 x Fator 3 x Fator 4 )-01/2017`),summary(IGD$`14 - IGD-M ( Fator 1 x Fator 2 x Fator 3 x Fator 4 )-01/2018`),summary(IGD$`14 - IGD-M ( Fator 1 x Fator 2 x Fator 3 x Fator 4 )-01/2019`),main="Evolução da dispersão do IGD-M para o mês de janeiro", col=c("red","blue","green","yellow"),xlab="Ano") #plota gráfico de dispersão
text(locator(4),c("jan/16","jan/17","jan/18","jan/19")) #Possibilita colocar os períodos no gráfico
tapply(MenorIGDM2016$Território,MenorIGDM2016$UF, length) #Visualiza quantidade de municípios com IGD-M como outlier em jan/16, por UF
tapply(MenorIGDM2017$Território,MenorIGDM2017$UF, length) #Visualiza quantidade de municípios com IGD-M como outlier em jan/17, por UF
tapply(MenorIGDM2018$Território,MenorIGDM2018$UF, length) #Visualiza quantidade de municípios com IGD-M como outlier em jan/18, por UF
tapply(MenorIGDM2019$Território,MenorIGDM2019$UF, length) #Visualiza quantidade de municípios com IGD-M como outlier em jan/19, por UF
QtdeMenoresIGDM<-c(sum(tapply(MenorIGDM2016$Território,MenorIGDM2016$UF, length)),sum(tapply(MenorIGDM2017$Território,MenorIGDM2017$UF, length)),sum(tapply(MenorIGDM2018$Território,MenorIGDM2018$UF, length)),sum(tapply(MenorIGDM2019$Território,MenorIGDM2019$UF, length))) #Cria objeto com o somatório das quantidades de municípios com IGD-M como outlier em jan/16, jan/17, jan/18 e jan/19
QtdeMenoresIGDM #Visualiza objeto criado
ano<-c(2016,2017,2018,2019) #Cria objeto com os ultimos quatro anos
plot(ano,QtdeMenoresIGDM,main="Gráfico 1 - Qtde. Municípios com IGD-M considerado baixo para o mês de janeiro",ylab="Quantidade de municípios",xlab="Ano",axes = F)+axis(1,at=c(0,2016,2017,2018,2019))+axis(2,at=c(0,200,400,600,800))+lines(ano,QtdeMenoresIGDM) #Plota ano e total de municípios com IGD-M baixo para o mês de janeiro
text(locator(4),c("625","365","442","527")) #Possibilita colocar as quantidades de municípios com IGD-M baixo no gráfico
IGDMzero2016<-IGD[which(IGD$`14 - IGD-M ( Fator 1 x Fator 2 x Fator 3 x Fator 4 )-01/2016`<=0.00),] #Cria objeto com os municípios com IGD-M igual a zero para jan/16
IGDMzero2017<-IGD[which(IGD$`14 - IGD-M ( Fator 1 x Fator 2 x Fator 3 x Fator 4 )-01/2017`<=0.00),] #Cria objeto com os municípios com IGD-M igual a zero para jan/17
IGDMzero2018<-IGD[which(IGD$`14 - IGD-M ( Fator 1 x Fator 2 x Fator 3 x Fator 4 )-01/2018`<=0.00),] #Cria objeto com os municípios com IGD-M igual a zero para jan/18
IGDMzero2019<-IGD[which(IGD$`14 - IGD-M ( Fator 1 x Fator 2 x Fator 3 x Fator 4 )-01/2019`<=0.00),] #Cria objeto com os municípios com IGD-M igual a zero para jan/19
tapply(IGDMzero2016$Território,IGDMzero2016$UF, length) #Visualiza quantidade de municípios com IGD-M igual a zero em jan/16, por UF
tapply(IGDMzero2017$Território,IGDMzero2017$UF, length) #Visualiza quantidade de municípios com IGD-M igual a zero em jan/17, por UF
tapply(IGDMzero2018$Território,IGDMzero2018$UF, length) #Visualiza quantidade de municípios com IGD-M igual a zero em jan/18, por UF
tapply(IGDMzero2019$Território,IGDMzero2019$UF, length) #Visualiza quantidade de municípios com IGD-M igual a zero em jan/19, por UF
QtdeIGDMzero<-c(sum(tapply(IGDMzero2016$Território,IGDMzero2016$UF, length)),sum(tapply(IGDMzero2017$Território,IGDMzero2017$UF, length)),sum(tapply(IGDMzero2018$Território,IGDMzero2018$UF, length)),sum(tapply(IGDMzero2019$Território,IGDMzero2019$UF, length))) #Cria objeto com o somatório das quantidades de municípios com IGD-M igual a zero em jan/16, jan/17, jan/18 e jan/19
QtdeIGDMzero #Visualiza objeto gerado
plot(ano,QtdeIGDMzero,main="Qtde. Municípios com IGD-M zero para o mês de janeiro",ylab="Quantidade de municípios",xlab="Ano",axes = F)+axis(1,at=c(0,2016,2017,2018,2019))+axis(2,at=c(0,200,400,600,800))+lines(ano,QtdeIGDMzero) #Plota ano e total de municípios com IGD-M baixo para o mês de janeiro
text(locator(4),c("616","338","417","484")) #Possibilita colocar as quantidades de municípios com IGD-M baixo no gráfico
barplot(tapply(MenorIGDM2016$Território,MenorIGDM2016$UF, length),main = "Gráfico 2 - Municípios com IGD-M baixo - jan/2016, por UF", cex.names = 1,ylim=c(0, 150),xlab="Total:625 municípios") #Cria gráfico de barras com qtde de IGD-M baixo, por UF, em jan/16
barplot(tapply(MenorIGDM2017$Território,MenorIGDM2017$UF, length),main = "Gráfico 3 - Municípios com IGD-M baixo - jan/2017, por UF", cex.names = 1,ylim=c(0, 150),xlab="Total:365 municípios") #Cria gráfico de barras com qtde de IGD-M baixo, por UF, em jan/17
barplot(tapply(MenorIGDM2018$Território,MenorIGDM2018$UF, length),main = "Gráfico 4 - Municípios com IGD-M baixo - jan/2018, por UF", cex.names = 1,ylim=c(0, 150),xlab="Total:442 municípios") #Cria gráfico de barras com qtde de IGD-M baixo, por UF, em jan/18
barplot(tapply(MenorIGDM2019$Território,MenorIGDM2019$UF, length),main = "Gráfico 5 - Municípios com IGD-M baixo - jan/2019, por UF", cex.names = 1,ylim=c(0, 150),xlab="Total:527 municípios") #Cria gráfico de barras com qtde de IGD-M baixo, por UF, em jan/19

#Analisando Fator 1
summary(IGD$`10 - Fator 1: Operação (  (TAFE+TAAS) / 2 +  TAC / 2)-01/2016`) #Verificando quartis para aplicar fórmula Q1-1.5*(Q3-Q1) e encontrar valor abaixo do qual Fator 1 é outlier para jan/16
summary(IGD$`10 - Fator 1: Operação (  (TAFE+TAAS) / 2 +  TAC / 2)-01/2017`) #Verificando quartis para aplicar fórmula Q1-1.5*(Q3-Q1) e encontrar valor abaixo do qual Fator 1 é outlier para jan/17
summary(IGD$`10 - Fator 1: Operação (  (TAFE+TAAS) / 2 +  TAC / 2)-01/2018`) #Verificando quartis para aplicar fórmula Q1-1.5*(Q3-Q1) e encontrar valor abaixo do qual Fator 1 é outlier para jan/18
summary(IGD$`10 - Fator 1: Operação (  (TAFE+TAAS) / 2 +  TAC / 2)-01/2019`) #Verificando quartis para aplicar fórmula Q1-1.5*(Q3-Q1) e encontrar valor abaixo do qual Fator 1 é outlier para jan/19
MenorFator12016<-IGD[which(IGD$`10 - Fator 1: Operação (  (TAFE+TAAS) / 2 +  TAC / 2)-01/2016`<0.55),] #Cria objeto com os municípios com Fator 1 considerados baixos para jan/16 (Fator 1 < 0,55, conforme Manual do IGD-M)
MenorFator12017<-IGD[which(IGD$`10 - Fator 1: Operação (  (TAFE+TAAS) / 2 +  TAC / 2)-01/2017`<0.55),] #Cria objeto com os municípios com Fator 1 considerados baixos para jan/17 (Fator 1 < 0,55, conforme Manual do IGD-M)
MenorFator12018<-IGD[which(IGD$`10 - Fator 1: Operação (  (TAFE+TAAS) / 2 +  TAC / 2)-01/2018`<0.55),] #Cria objeto com os municípios com Fator 1 considerados baixos para jan/18 (Fator 1 < 0,55, conforme Manual do IGD-M)
MenorFator12019<-IGD[which(IGD$`10 - Fator 1: Operação (  (TAFE+TAAS) / 2 +  TAC / 2)-01/2019`<0.55),] #Cria objeto com os municípios com Fator 1 considerados baixos para jan/19 (Fator 1 < 0,55, conforme Manual do IGD-M)
tapply(MenorFator12016$Território,MenorFator12016$UF, length) #Visualiza quantidade de municípios com Fator 1 baixo em jan/16, por UF
tapply(MenorFator12017$Território,MenorFator12017$UF, length) #Visualiza quantidade de municípios com Fator 1 baixo em jan/17, por UF
tapply(MenorFator12018$Território,MenorFator12018$UF, length) #Visualiza quantidade de municípios com Fator 1 baixo em jan/18, por UF
tapply(MenorFator12019$Território,MenorFator12019$UF, length) #Visualiza quantidade de municípios com Fator 1 baixo em jan/19, por UF
QtdeMenoresFator1<-c(sum(tapply(MenorFator12016$Território,MenorFator12016$UF, length)),sum(tapply(MenorFator12017$Território,MenorFator12017$UF, length)),sum(tapply(MenorFator12018$Território,MenorFator12018$UF, length)),sum(tapply(MenorFator12019$Território,MenorFator12019$UF, length))) #Cria objeto com o somatório das quantidades de municípios com Fator 1 baixo em jan/16, jan/17, jan/18 e jan/19
QtdeMenoresFator1 #Visualiza objeto criado
plot(ano,QtdeMenoresFator1,main="Gráfico 6 - Qtde. Municípios com Fator 1 considerado baixo para o mês de janeiro",ylab="Quantidade de municípios",xlab="Ano",axes = F)+axis(1,at=c(0,2016,2017,2018,2019))+axis(2,at=c(0,200,250,300,350,400))+lines(ano,QtdeMenoresFator1) #Plota ano e total de municípios com Fator 1 baixo para o mês de janeiro
text(locator(4),c("19","3","0","1")) #Possibilita colocar as quantidades no gráfico
barplot(tapply(MenorFator12016$Território,MenorFator12016$UF, length),main = "Municípios com Fator 1 baixo - jan/2016, por UF", cex.names = 1,ylim=c(0, 60),xlab="Total:19 municípios") #Cria gráfico de barras com qtde de Fator 1 baixo, por UF, em jan/16
barplot(tapply(MenorFator12017$Território,MenorFator12017$UF, length),main = "Municípios com Fator 1 baixo - jan/2017, por UF", cex.names = 1,ylim=c(0, 60),xlab="Total:3 municípios") #Cria gráfico de barras com qtde de Fator 1 baixo, por UF, em jan/17
#barplot(tapply(MenorFator12018$Território,MenorFator12018$UF, length),main = "Municípios com Fator 1 baixo - jan/2018, por UF", cex.names = 1,ylim=c(0, 60),xlab="Total:0 municípios") #Cria gráfico de barras com qtde de Fator 1 baixo, por UF, em jan/18
barplot(tapply(MenorFator12019$Território,MenorFator12019$UF, length),main = "Municípios com Fator 1 baixo - jan/2019, por UF", cex.names = 1,ylim=c(0, 60),xlab="Total:1 municípios") #Cria gráfico de barras com qtde de Fator 1 baixo, por UF, em jan/19

#Gráficos Boxplot de Frequência Escolar, Agenda Saúde, Atualização Cadastral e Fator 1, anos 2016, 2017, 2018 e 2019.
boxplot(IGD$`10 - Fator 1: Operação (  (TAFE+TAAS) / 2 +  TAC / 2)-01/2016`,IGD$`10 - Fator 1: Operação (  (TAFE+TAAS) / 2 +  TAC / 2)-01/2017`,IGD$`10 - Fator 1: Operação (  (TAFE+TAAS) / 2 +  TAC / 2)-01/2018`,IGD$`10 - Fator 1: Operação (  (TAFE+TAAS) / 2 +  TAC / 2)-01/2019`,main="Gráfico 7- Fator 1 (TAFE, TAAS e TAC)",col=c("red","blue","green","yellow"),xlab="Ano") #Visualiza distribuição de Fator 1 < 0.55 para os meses de jan/16, jan/17, jan/18 e jan/19.
text(locator(4),c("2016","2017","2018","2019"))
boxplot(IGD$`9 - TAC - Taxa de Atualização Cadastral ( item 7 / item 8 )-01/2016`,IGD$`9 - TAC - Taxa de Atualização Cadastral ( item 7 / item 8 )-01/2017`, IGD$`9 - TAC - Taxa de Atualização Cadastral ( item 7 / item 8 )-01/2018`, IGD$`9 - TAC - Taxa de Atualização Cadastral ( item 7 / item 8 )-01/2019`,main="Gráfico 8 - Acompanhamento da Atualização Cadastral - TAC",col=c("red","blue","green","yellow"),xlab="Ano") #Visualiza distribuição de TAC<0.55 para os meses de jan/16, jan/17, jan/18 e jan/19.
text(locator(4),c("2016","2017","2018","2019"))
boxplot(IGD$`3 - TAFE - Taxa de Acompanhameto de Frequência Escolar ( item 1 / item 2 )-01/2016`,IGD$`3 - TAFE - Taxa de Acompanhameto de Frequência Escolar ( item 1 / item 2 )-01/2017`,IGD$`3 - TAFE - Taxa de Acompanhameto de Frequência Escolar ( item 1 / item 2 )-01/2018`,IGD$`3 - TAFE - Taxa de Acompanhameto de Frequência Escolar ( item 1 / item 2 )-01/2019`,main="Gráfico 9 - Acompanhamento da Frequência Escolar - TAFE",col=c("red","blue","green","yellow"),xlab="Ano") #Visualiza distribuição e valores discrepantes (outliers) de TAFE para os meses de jan/16, jan/17, jan/18 e jan/19.
text(locator(4),c("2016","2017","2018","2019"))
boxplot(IGD$`6 - TAAS - Taxa de Acompanhamento de Agenda de Saúde ( item 4 / item 5 )-01/2016`, IGD$`6 - TAAS - Taxa de Acompanhamento de Agenda de Saúde ( item 4 / item 5 )-01/2017`,IGD$`6 - TAAS - Taxa de Acompanhamento de Agenda de Saúde ( item 4 / item 5 )-01/2018`,IGD$`6 - TAAS - Taxa de Acompanhamento de Agenda de Saúde ( item 4 / item 5 )-01/2019`,main="Gráfico 10 - Acompanhamento da Agenda Saúde - TAAS",col=c("red","blue","green","yellow"),xlab="Ano") #Visualiza distribuição e valores discrepantes (outliers) de TAAS para os meses de jan/16, jan/17, jan/18 e jan/19.
text(locator(4),c("2016","2017","2018","2019"))

#Analisando menores TACs
MenorTAC2016<-IGD[which(IGD$`9 - TAC - Taxa de Atualização Cadastral ( item 7 / item 8 )-01/2016`<0.55),] #Cria objeto com os municípios com menores TAC em 2016: TAAS < 0,55 (conforme Manual do IGD)
MenorTAC2017<-IGD[which(IGD$`9 - TAC - Taxa de Atualização Cadastral ( item 7 / item 8 )-01/2017`<0.55),] #Cria objeto com os municípios com menores TAC em 2017: TAAS < 0,55 (conforme Manual do IGD)
MenorTAC2018<-IGD[which(IGD$`9 - TAC - Taxa de Atualização Cadastral ( item 7 / item 8 )-01/2018`<0.55),] #Cria objeto com os municípios com menores TAC em 2018: TAAS < 0,55 (conforme Manual do IGD)
MenorTAC2019<-IGD[which(IGD$`9 - TAC - Taxa de Atualização Cadastral ( item 7 / item 8 )-01/2019`<0.55),] #Cria objeto com os municípios com menores TAC em 2019: TAAS < 0,55 (conforme Manual do IGD)
tapply(MenorTAC2016$Território,MenorTAC2016$UF, length) #Visualiza quantidade de municípios com menores TAC em 2016, por UF
tapply(MenorTAC2017$Território,MenorTAC2017$UF, length) #Visualiza quantidade de municípios com menores TAC em 2017, por UF
tapply(MenorTAC2018$Território,MenorTAC2018$UF, length) #Visualiza quantidade de municípios com menores TAC em 2018, por UF
tapply(MenorTAC2019$Território,MenorTAC2019$UF, length) #Visualiza quantidade de municípios com menores TAC em 2019, por UF
QtdeMenoresTAC<-c(sum(tapply(MenorTAC2016$Território,MenorTAC2016$UF, length)),sum(tapply(MenorTAC2017$Território,MenorTAC2017$UF, length)),sum(tapply(MenorTAC2018$Território,MenorTAC2018$UF, length)),sum(tapply(MenorTAC2019$Território,MenorTAC2019$UF, length))) #Cria objeto com o somatório das quantidades de municípios com TAC baixo em jan/16, jan/17, jan/18 e jan/19
QtdeMenoresTAC #Visualiza objeto criado
plot(ano,QtdeMenoresTAC,main="Gráfico 11 - Qtde. de Municípios com TAC baixo para o mês de janeiro",ylab="Quantidade",xlab="Ano",axes = F)+axis(1,at=c(0,2016,2017,2018,2019))+axis(2,at=c(0,200,250,300,350,400))+lines(ano,QtdeMenoresTAC) #Plota ano e total de municípios com TAC baixo para o mês de janeiro
text(locator(4),c("306","110","18","1")) #Possibilita colocar os valores no gráfico
barplot(tapply(MenorTAC2016$Território,MenorTAC2016$UF, length),main = "Municípios com menores TAC - jan/2016, por UF", cex.names = 0.75,ylim=c(0, 100),xlab="Total:306 municípios") #Cria gráfico de barras com qtde de municícpios com TAC baixo, por UF, em jan/16
barplot(tapply(MenorTAC2017$Território,MenorTAC2017$UF, length),main = "Municípios com menores TAC - jan/2017, por UF", cex.names = 0.75, ylim=c(0, 100),xlab="Total: 110 municípios") #Cria gráfico de barras com qtde de municícpios com TAC baixo, por UF, em jan/17
barplot(tapply(MenorTAC2018$Território,MenorTAC2018$UF, length),main = "Municípios com menores TAC - jan/2018, por UF", cex.names = 0.75,ylim=c(0, 100),xlab="Total: 18 municípios") #Cria gráfico de barras com qtde de municícpios com TAC baixo, por UF, em jan/18
barplot(tapply(MenorTAC2019$Território,MenorTAC2019$UF, length),main = "Municípios com menores TAC - jan/2019, por UF", cex.names = 0.75,ylim=c(0, 100),xlab="Total:1 município") #Cria gráfico de barras com qtde de municícpios com TAC baixo, por UF, em jan/19

#Analisando menores TAFEs
summary(IGD$`3 - TAFE - Taxa de Acompanhameto de Frequência Escolar ( item 1 / item 2 )-01/2016`) #Verificando quartis para aplicar fórmula Q1-1.5*(Q3-Q1) e encontrar valor abaixo do qual o TAFE é outlier para jan/16
summary(IGD$`3 - TAFE - Taxa de Acompanhameto de Frequência Escolar ( item 1 / item 2 )-01/2017`) #Verificando quartis para aplicar fórmula Q1-1.5*(Q3-Q1) e encontrar valor abaixo do qual o TAFE é outlier para jan/17
summary(IGD$`3 - TAFE - Taxa de Acompanhameto de Frequência Escolar ( item 1 / item 2 )-01/2018`) #Verificando quartis para aplicar fórmula Q1-1.5*(Q3-Q1) e encontrar valor abaixo do qual o TAFE é outlier para jan/18
summary(IGD$`3 - TAFE - Taxa de Acompanhameto de Frequência Escolar ( item 1 / item 2 )-01/2019`) #Verificando quartis para aplicar fórmula Q1-1.5*(Q3-Q1) e encontrar valor abaixo do qual o TAFE é outlier para jan/19
MenorTAFE2016<-IGD[which(IGD$`3 - TAFE - Taxa de Acompanhameto de Frequência Escolar ( item 1 / item 2 )-01/2016`<0.75),] #Cria objeto com os municípios com menores TAFEs em 2016: TAFE < 0,75 (critério estatístico para outliers)
MenorTAFE2017<-IGD[which(IGD$`3 - TAFE - Taxa de Acompanhameto de Frequência Escolar ( item 1 / item 2 )-01/2017`<0.835),] #Cria objeto com os municípios com menores TAFEs em 2017: TAFE < 0,835
MenorTAFE2018<-IGD[which(IGD$`3 - TAFE - Taxa de Acompanhameto de Frequência Escolar ( item 1 / item 2 )-01/2018`<0.845),] #Cria objeto com os municípios com menores TAFEs em 2018: TAFE < 0,845
MenorTAFE2019<-IGD[which(IGD$`3 - TAFE - Taxa de Acompanhameto de Frequência Escolar ( item 1 / item 2 )-01/2019`<0.87),] #Cria objeto com os municípios com menores TAFEs em 2019: TAFE < 0,87
tapply(MenorTAFE2016$Território,MenorTAFE2016$UF, length) #Visualiza quantidade de municípios com menores TAFEs em 2016, por UF
tapply(MenorTAFE2017$Território,MenorTAFE2017$UF, length) #Visualiza quantidade de municípios com menores TAFEs em 2017, por UF
tapply(MenorTAFE2018$Território,MenorTAFE2018$UF, length) #Visualiza quantidade de municípios com menores TAFEs em 2018, por UF
tapply(MenorTAFE2019$Território,MenorTAFE2019$UF, length) #Visualiza quantidade de municípios com menores TAFEs em 2019, por UF
QtdeMenoresTAFE<-c(sum(tapply(MenorTAFE2016$Território,MenorTAFE2016$UF, length)),sum(tapply(MenorTAFE2017$Território,MenorTAFE2017$UF, length)),sum(tapply(MenorTAFE2018$Território,MenorTAFE2018$UF, length)),sum(tapply(MenorTAFE2019$Território,MenorTAFE2019$UF, length))) #Cria objeto com o somatório das quantidades de municípios com TAFE baixo em jan/16, jan/17, jan/18 e jan/19
QtdeMenoresTAFE #Visualiza objeto criado
plot(ano,QtdeMenoresTAFE,main="Gráfico 12 - Qtde. Municípios com TAFE baixo para o mês de janeiro",ylab="Quantidade",xlab="Ano",axes = F)+axis(1,at=c(0,2016,2017,2018,2019))+axis(2,at=c(0,200,250,300,350,400))+lines(ano,QtdeMenoresTAFE) #Plota ano e total de municípios com TAFE baixo para o mês de janeiro
text(locator(4),c("186","226","280","296")) #Possibilita colocar as quantidades no gráfico
barplot(tapply(MenorTAFE2016$Território,MenorTAFE2016$UF, length),main = "Gráfico 13- Municípios com menores TAFE - jan/2016, por UF", cex.names = 1,ylim=c(0, 80),xlab="Total:186 municípios") #Cria gráfico de barras com qtde de municícpios com TAFE baixo, por UF, em jan/16
barplot(tapply(MenorTAFE2017$Território,MenorTAFE2017$UF, length),main = "Gráfico 14 - Municípios com menores TAFE - jan/2017, por UF", cex.names = 1, ylim=c(0, 80),xlab="Total: 226 municípios") #Cria gráfico de barras com qtde de municícpios com TAFE baixo, por UF, em jan/17
barplot(tapply(MenorTAFE2018$Território,MenorTAFE2018$UF, length),main = "Gráfico 15 - Municípios com menores TAFE - jan/2018, por UF", cex.names = 1,ylim=c(0, 80),xlab="Total: 280 municípios")#Cria gráfico de barras com qtde de municícpios com TAFE baixo, por UF, em jan/18
barplot(tapply(MenorTAFE2019$Território,MenorTAFE2019$UF, length),main = "Gráfico 16 - Municípios com menores TAFE - jan/2019, por UF", cex.names = 1,ylim=c(0, 80),xlab="Total:296 municípios")#Cria gráfico de barras com qtde de municícpios com TAFE baixo, por UF, em jan/19

#Analisando menores TAAS
summary(IGD$`6 - TAAS - Taxa de Acompanhamento de Agenda de Saúde ( item 4 / item 5 )-01/2016`) #Verificando quartis para aplicar fórmula Q1-1.5*(Q3-Q1) e encontrar valor abaixo do qual o TAAS é outlier para jan/16
summary(IGD$`6 - TAAS - Taxa de Acompanhamento de Agenda de Saúde ( item 4 / item 5 )-01/2017`) #Verificando quartis para aplicar fórmula Q1-1.5*(Q3-Q1) e encontrar valor abaixo do qual o TAAS é outlier para jan/17
summary(IGD$`6 - TAAS - Taxa de Acompanhamento de Agenda de Saúde ( item 4 / item 5 )-01/2018`) #Verificando quartis para aplicar fórmula Q1-1.5*(Q3-Q1) e encontrar valor abaixo do qual o TAAS é outlier para jan/18
summary(IGD$`6 - TAAS - Taxa de Acompanhamento de Agenda de Saúde ( item 4 / item 5 )-01/2019`) #Verificando quartis para aplicar fórmula Q1-1.5*(Q3-Q1) e encontrar valor abaixo do qual o TAAS é outlier para jan/19
MenorTAAS2016<-IGD[which(IGD$`6 - TAAS - Taxa de Acompanhamento de Agenda de Saúde ( item 4 / item 5 )-01/2016`<0.485),] #Cria objeto com os municípios com menores TAAS em 2016: TAAS <= 0,485 (critério estatístico para outliers)
MenorTAAS2017<-IGD[which(IGD$`6 - TAAS - Taxa de Acompanhamento de Agenda de Saúde ( item 4 / item 5 )-01/2017`<0.40),] #Cria objeto com os municípios com menores TAAS em 2017: TAAS <= 0,40
MenorTAAS2018<-IGD[which(IGD$`6 - TAAS - Taxa de Acompanhamento de Agenda de Saúde ( item 4 / item 5 )-01/2018`<0.51),] #Cria objeto com os municípios com menores TAAS em 2018: TAAS <= 0,51
MenorTAAS2019<-IGD[which(IGD$`6 - TAAS - Taxa de Acompanhamento de Agenda de Saúde ( item 4 / item 5 )-01/2019`<0.51),] #Cria objeto com os municípios com menores TAAS em 2019: TAAS <= 0,51
tapply(MenorTAAS2016$Território,MenorTAAS2016$UF, length) #Visualiza quantidade de municípios com menores TAAS em 2016, por UF
tapply(MenorTAAS2017$Território,MenorTAAS2017$UF, length) #Visualiza quantidade de municípios com menores TAAS em 2017, por UF
tapply(MenorTAAS2018$Território,MenorTAAS2018$UF, length) #Visualiza quantidade de municípios com menores TAAS em 2018, por UF
tapply(MenorTAAS2019$Território,MenorTAAS2019$UF, length) #Visualiza quantidade de municípios com menores TAAS em 2019, por UF
QtdeMenoresTAAS<-c(sum(tapply(MenorTAAS2016$Território,MenorTAAS2016$UF, length)),sum(tapply(MenorTAAS2017$Território,MenorTAAS2017$UF, length)),sum(tapply(MenorTAAS2018$Território,MenorTAAS2018$UF, length)),sum(tapply(MenorTAAS2019$Território,MenorTAAS2019$UF, length))) #Cria objeto com o somatório das quantidades de municípios com TAAS baixo em jan/16, jan/17, jan/18 e jan/19
QtdeMenoresTAAS #Visualiza objeto criado
plot(ano,QtdeMenoresTAAS,main="Gráfico 17 - Qtde. de municípios com TAAS baixo para o mês de janeiro",ylab="Quantidade",xlab="Ano",axes = F)+axis(1,at=c(0,2016,2017,2018,2019))+axis(2,at=c(0,200,250,300,350,400))+lines(ano,QtdeMenoresTAAS) #Plota ano e total de municípios com TAAS baixo para o mês de janeiro
text(locator(4),c("266","193","223","233")) #Possibilita colocar as quantidades no gráfico
barplot(tapply(MenorTAAS2016$Território,MenorTAAS2016$UF, length),main = "Gráfico 18 - Municípios com menores TAAS - jan/2016, por UF", cex.names = 1,ylim=c(0, 100),xlab="Total:266 municípios") #Cria gráfico de barras com qtde de municícpios com TAAS baixo, por UF, em jan/16
barplot(tapply(MenorTAAS2017$Território,MenorTAAS2017$UF, length),main = "Gráfico 19 - Municípios com menores TAAS - jan/2017, por UF", cex.names = 1, ylim=c(0, 100),xlab="Total: 193 municípios") #Cria gráfico de barras com qtde de municícpios com TAAS baixo, por UF, em jan/17
barplot(tapply(MenorTAAS2018$Território,MenorTAAS2018$UF, length),main = "Gráfico 20 - Municípios com menores TAAS - jan/2018, por UF", cex.names = 1,ylim=c(0, 100),xlab="Total: 223 municípios") #Cria gráfico de barras com qtde de municícpios com TAAS baixo, por UF, em jan/18
barplot(tapply(MenorTAAS2019$Território,MenorTAAS2019$UF, length),main = "Gráfico 21 - Municípios com menores TAAS - jan/2019, por UF", cex.names = 1,ylim=c(0, 100),xlab="Total:233 municípios") #Cria gráfico de barras com qtde de municícpios com TAAS baixo, por UF, em jan/19

#Verificando evolução gráfica das condicionalidades (Taxa de Acompanhamento da Frequência Escolar - TAFE e da Taxa de Acompanhamento da Agenda Saúde - TAAS) nos exercícios 2016, 2017, 2018 e 2019
CondBaixa2016<-IGD[which((((IGD$`3 - TAFE - Taxa de Acompanhameto de Frequência Escolar ( item 1 / item 2 )-01/2016`)+(IGD$`6 - TAAS - Taxa de Acompanhamento de Agenda de Saúde ( item 4 / item 5 )-01/2016`))/2)<0.30),] #Cria objeto com os municípios com condicionalidades (TAFE + TAAS) <0.30 (conforme Manual do IGD)
CondBaixa2017<-IGD[which((((IGD$`3 - TAFE - Taxa de Acompanhameto de Frequência Escolar ( item 1 / item 2 )-01/2017`)+(IGD$`6 - TAAS - Taxa de Acompanhamento de Agenda de Saúde ( item 4 / item 5 )-01/2017`))/2)<0.30),] #Cria objeto com os municípios com condicionalidades (TAFE + TAAS) <0.30 (conforme Manual do IGD)
CondBaixa2018<-IGD[which((((IGD$`3 - TAFE - Taxa de Acompanhameto de Frequência Escolar ( item 1 / item 2 )-01/2018`)+(IGD$`6 - TAAS - Taxa de Acompanhamento de Agenda de Saúde ( item 4 / item 5 )-01/2018`))/2)<0.30),] #Cria objeto com os municípios com condicionalidades (TAFE + TAAS) <0.30 (conforme Manual do IGD)
CondBaixa2019<-IGD[which((((IGD$`3 - TAFE - Taxa de Acompanhameto de Frequência Escolar ( item 1 / item 2 )-01/2019`)+(IGD$`6 - TAAS - Taxa de Acompanhamento de Agenda de Saúde ( item 4 / item 5 )-01/2019`))/2)<0.30),] #Cria objeto com os municípios com condicionalidades (TAFE + TAAS) <0.30 (conforme Manual do IGD)
barplot(c(length(CondBaixa2016$Território),length(CondBaixa2017$Território),length(CondBaixa2018$Território),length(CondBaixa2019$Território)),main = "Gráfico 22 - Qtde Municípios com condicionalidade<0,30", cex.names = 0.75,ylim=c(0, 2),names.arg = c("jan/2016","jan/2017","jan/2018","jan/2019"),ylab="Quantidade") #Plota quantidade de municípios com condicionalidade<0.30
text(locator(2),c("São João Evangelista - MG","Três Arroios - RS")) #Inclui nomes dos municícpios com condicionalidade <0.30
plot(IGD$`3 - TAFE - Taxa de Acompanhameto de Frequência Escolar ( item 1 / item 2 )-01/2016`, IGD$`6 - TAAS - Taxa de Acompanhamento de Agenda de Saúde ( item 4 / item 5 )-01/2016`,pch=20, abline(v=0.75,h=0.485),col="RED",main="Gráfico 23 - TAFE x TAAS (jan/2016)", xlab="Frequência Escolar - TAFE jan/2016", ylab="Agenda Saúde - TAAS - jan/2016") #Plota TAFE2016 X TAAS2016, destacando a interseção entre MenoresTAFE e Menores TAAS (11 municícpios)
text(locator(7),c("175 municípios","11 municípios","255 municípios","1º quadrante","2º quadrante","3º quadrante","4º quadrante"))
plot(IGD$`3 - TAFE - Taxa de Acompanhameto de Frequência Escolar ( item 1 / item 2 )-01/2017`,IGD$`6 - TAAS - Taxa de Acompanhamento de Agenda de Saúde ( item 4 / item 5 )-01/2017`,pch=20, abline(v=0.835,h=0.40),col="RED",main="Gráfico 24 - TAFE x TAAS (jan/2017)", xlab="Frequência Escolar - TAFE jan/2017", ylab="Agenda Saúde - TAAS - jan/2017") #Plota TAFE2017 X TAAS2017, destacando a interseção entre MenoresTAFE e Menores TAAS (16 municícpios)
text(locator(7),c("210 municípios","16 municípios","177 municípios","1º quadrante","2º quadrante","3º quadrante","4º quadrante"))
plot(IGD$`3 - TAFE - Taxa de Acompanhameto de Frequência Escolar ( item 1 / item 2 )-01/2018`,IGD$`6 - TAAS - Taxa de Acompanhamento de Agenda de Saúde ( item 4 / item 5 )-01/2018`,pch=20, abline(v=0.845,h=0.51),col="RED",main="Gráfico 25 - TAFE x TAAS (jan/2018)", xlab="Frequência Escolar - TAFE jan/2018", ylab="Agenda Saúde - TAAS - jan/2018") #Plota TAFE2018 X TAAS2018, destacando a interseção entre MenoresTAFE e Menores TAAS (10 municícpios)
text(locator(7),c("270 municípios","10 municípios","213 municípios","1º quadrante","2º quadrante","3º quadrante","4º quadrante"))
plot(IGD$`3 - TAFE - Taxa de Acompanhameto de Frequência Escolar ( item 1 / item 2 )-01/2019`,IGD$`6 - TAAS - Taxa de Acompanhamento de Agenda de Saúde ( item 4 / item 5 )-01/2019`,pch=20,abline(v=0.87,h=0.51),col="RED",main="Gráfico 26 - TAFE x TAAS (jan/2019)", xlab="Frequência Escolar - TAFE jan/2019", ylab="Agenda Saúde - TAAS - jan/2019") #Plota TAFE2019 X TAAS2019, destacando a interseção entre MenoresTAFE e Menores TAAS (18 municícpios)
text(locator(7),c("278 municípios","18 municípios","215 municípios","1º quadrante","2º quadrante","3º quadrante","4º quadrante"))

#Análise Fator 2 (Adesão ao SuasWeb)
unique(IGD$`11 - Fator 2: Adesão ao SUAS-01/2016`) #Verifica as respostas existentes para Fator 2 em jan/2016
unique(IGD$`11 - Fator 2: Adesão ao SUAS-01/2017`) #Verifica as respostas existentes para Fator 2 em jan/2017
unique(IGD$`11 - Fator 2: Adesão ao SUAS-01/2018`) #Verifica as respostas existentes para Fator 2 em jan/2018
unique(IGD$`11 - Fator 2: Adesão ao SUAS-01/2019`) #Verifica as respostas existentes para Fator 2 em jan/2019
tapply(IGD$UF,IGD$`11 - Fator 2: Adesão ao SUAS-01/2016`,length) #Soma os municípios, por resposta em jan/2016
tapply(IGD$UF,IGD$`11 - Fator 2: Adesão ao SUAS-01/2017`,length) #Soma os municípios, por resposta em jan/2017
tapply(IGD$UF,IGD$`11 - Fator 2: Adesão ao SUAS-01/2018`,length) #Soma os municípios, por resposta em jan/2018
tapply(IGD$UF,IGD$`11 - Fator 2: Adesão ao SUAS-01/2019`,length) #Soma os municípios, por resposta em jan/2019
WebSuas2016<-IGD[which(IGD$`11 - Fator 2: Adesão ao SUAS-01/2016`<=0),] #Cria objeto com os municípios que não aderiram ao Suas em jan/2016
WebSuas2017<-IGD[which(IGD$`11 - Fator 2: Adesão ao SUAS-01/2017`<=0),] #Cria objeto com os municípios que não aderiram ao Suas em jan/2017
WebSuas2018<-IGD[which(IGD$`11 - Fator 2: Adesão ao SUAS-01/2018`<=0),] #Cria objeto com os municípios que não aderiram ao Suas em jan/2018
WebSuas2019<-IGD[which(IGD$`11 - Fator 2: Adesão ao SUAS-01/2019`<=0),] #Cria objeto com os municípios que não aderiram ao Suas em jan/2019
Ano<-c("jan/2016","jan/2017","jan/2018","jan/2019") #Cria objeto com os períodos 
barplot(c(length(WebSuas2016$Território),length(WebSuas2017$Território),length(WebSuas2018$Território),length(WebSuas2019$Território)), ylim=c(0, 10),names.arg=Ano,xlab="Período",ylab="Quantidade",main = "Gráfico 27 - Qtde. Municípios que não aderiram ao Suas-Fator 2",col=c("red","blue","green","yellow")) #Mostra evolução da quantidade, por exercício
text(locator(4),c("9","9","9","8")) #Inclui no gráfico a quantidade de municícpios não aderentes ao SUAS, por período

#Análise Fator 3 (Informação no SuasWeb da comprovação de gastos dos recursos do IGD-M ao respectivo CMAS)
unique(IGD$`12 - Fator 3: Comprovação de Gastos pelo FMAS-01/2016`) #Verifica as respostas existentes para Fator 3 em jan/2016
unique(IGD$`12 - Fator 3: Comprovação de Gastos pelo FMAS-01/2017`) #Verifica as respostas existentes para Fator 3 em jan/2017
unique(IGD$`12 - Fator 3: Comprovação de Gastos pelo FMAS-01/2018`) #Verifica as respostas existentes para Fator 3 em jan/2018
unique(IGD$`12 - Fator 3: Comprovação de Gastos pelo FMAS-01/2019`) #Verifica as respostas existentes para Fator 3 em jan/2019
tapply(IGD$UF,IGD$`12 - Fator 3: Comprovação de Gastos pelo FMAS-01/2016`,length) #Soma os municípios, por resposta em jan/2016
tapply(IGD$UF,IGD$`12 - Fator 3: Comprovação de Gastos pelo FMAS-01/2017`,length) #Soma os municípios, por resposta em jan/2017
tapply(IGD$UF,IGD$`12 - Fator 3: Comprovação de Gastos pelo FMAS-01/2018`,length) #Soma os municípios, por resposta em jan/2018
tapply(IGD$UF,IGD$`12 - Fator 3: Comprovação de Gastos pelo FMAS-01/2019`,length) #Soma os municípios, por resposta em jan/2019
ComprovGastos2016<-IGD[which(IGD$`12 - Fator 3: Comprovação de Gastos pelo FMAS-01/2016`<=0),] #Cria objeto com os municípios que não apresentaram comprovação de gastos em jan/2016
ComprovGastos2017<-IGD[which(IGD$`12 - Fator 3: Comprovação de Gastos pelo FMAS-01/2017`<=0),] #Cria objeto com os municípios que não apresentaram comprovação de gastos em jan/2017
ComprovGastos2018<-IGD[which(IGD$`12 - Fator 3: Comprovação de Gastos pelo FMAS-01/2018`<=0),] #Cria objeto com os municípios que não apresentaram comprovação de gastos em jan/2018
ComprovGastos2019<-IGD[which(IGD$`12 - Fator 3: Comprovação de Gastos pelo FMAS-01/2019`<=0),] #Cria objeto com os municípios que não apresentaram comprovação de gastos em jan/2019
barplot(c(length(ComprovGastos2016$Território),length(ComprovGastos2017$Território),length(ComprovGastos2018$Território),length(ComprovGastos2019$Território)), ylim=c(0, 300),names.arg=Ano,xlab="Período",ylab="Quantidade",main = "Gráfico 28 -Qtde. Municípios que não apresentaram comprovação de gastos - Fator 3",col=c("red","blue","green","yellow")) #Mostra evolução da quantidade, por exercício
text(locator(4),c("270","188","182","204")) #Inclui no gráfico a quantidade de municícpios não aderentes ao SUAS, por período
barplot(tapply(ComprovGastos2016$`12 - Fator 3: Comprovação de Gastos pelo FMAS-01/2016`,ComprovGastos2016$UF,length),main = "Gráfico 29 - Municípios sem Comprovação de gastos no SuasWeb - Fator 3 (jan/2016)", cex.names = 0.75,xlab="Total:270 municípios",ylim=c(0, 80)) #Plota municípios qtde de municícpios sem comprovação de gastos em jan/16, por UF.
barplot(tapply(ComprovGastos2017$`12 - Fator 3: Comprovação de Gastos pelo FMAS-01/2017`,ComprovGastos2017$UF,length),main = "Gráfico 30 - Municípios sem Comprovação de gastos no SuasWeb - Fator 3 (jan/2017)", cex.names = 0.75,xlab="Total:188 municípios",ylim=c(0, 80)) #Plota municípios qtde de municícpios sem comprovação de gastos em jan/17, por UF.
barplot(tapply(ComprovGastos2018$`12 - Fator 3: Comprovação de Gastos pelo FMAS-01/2018`,ComprovGastos2018$UF,length),main = "Gráfico 31 - Municípios sem Comprovação de gastos no SuasWeb - Fator 3 (jan/2018)", cex.names = 0.75,xlab="Total:182 municípios",ylim=c(0, 80)) #Plota municípios qtde de municícpios sem comprovação de gastos em jan/18, por UF.
barplot(tapply(ComprovGastos2019$`12 - Fator 3: Comprovação de Gastos pelo FMAS-01/2019`,ComprovGastos2019$UF,length),main = "Gráfico 32 - Municípios sem Comprovação de gastos no SuasWeb - Fator 3 (jan/2019)", cex.names = 0.75,xlab="Total:204 municípios",ylim=c(0, 80)) #Plota municípios qtde de municícpios sem comprovação de gastos em jan/19, por UF.

#Análise Fator 4 (Informação no SuasWeb da aprovação da comprovação de gastos dos recursos)
unique(IGD$`13 - Fator 4: Aprovação da Comprovação de Gastos pelo CMAS-01/2016`) #Verifica as respostas existentes para Fator 4 em jan/2016
unique(IGD$`13 - Fator 4: Aprovação da Comprovação de Gastos pelo CMAS-01/2017`) #Verifica as respostas existentes para Fator 4 em jan/2017
unique(IGD$`13 - Fator 4: Aprovação da Comprovação de Gastos pelo CMAS-01/2018`) #Verifica as respostas existentes para Fator 4 em jan/2018
unique(IGD$`13 - Fator 4: Aprovação da Comprovação de Gastos pelo CMAS-01/2019`) #Verifica as respostas existentes para Fator 4 em jan/2019
tapply(IGD$UF,IGD$`13 - Fator 4: Aprovação da Comprovação de Gastos pelo CMAS-01/2016`,length) #Soma os municípios, por resposta em jan/2016
tapply(IGD$UF,IGD$`13 - Fator 4: Aprovação da Comprovação de Gastos pelo CMAS-01/2017`,length) #Soma os municípios, por resposta em jan/2017
tapply(IGD$UF,IGD$`13 - Fator 4: Aprovação da Comprovação de Gastos pelo CMAS-01/2018`,length) #Soma os municípios, por resposta em jan/2018
tapply(IGD$UF,IGD$`13 - Fator 4: Aprovação da Comprovação de Gastos pelo CMAS-01/2019`,length) #Soma os municípios, por resposta em jan/2019
AprovGastos2016<-IGD[which(IGD$`13 - Fator 4: Aprovação da Comprovação de Gastos pelo CMAS-01/2016`<=0),] #Cria objeto com os municípios sem aprovação da comprovação de gastos em jan/2016
AprovGastos2017<-IGD[which(IGD$`13 - Fator 4: Aprovação da Comprovação de Gastos pelo CMAS-01/2017`<=0),] #Cria objeto com os municípios sem aprovação da comprovação de gastos em jan/2017
AprovGastos2018<-IGD[which(IGD$`13 - Fator 4: Aprovação da Comprovação de Gastos pelo CMAS-01/2018`<=0),] #Cria objeto com os municípios sem aprovação da comprovação de gastos em jan/2018
AprovGastos2019<-IGD[which(IGD$`13 - Fator 4: Aprovação da Comprovação de Gastos pelo CMAS-01/2019`<=0),] #Cria objeto com os municípios sem aprovação da comprovação de gastos em jan/2019
View(AprovGastos2019)
barplot(c(length(AprovGastos2016$Território),length(AprovGastos2017$Território),length(AprovGastos2018$Território),length(AprovGastos2019$Território)), ylim=c(0, 600),names.arg=Ano,xlab="Período",ylab="Quantidade",main = "Gráfico 33 - Qtde. Municípios sem aprovação da comprovação de gastos - Fator 4",col=c("red","blue","green","yellow")) #Mostra evolução gráfica da quantidade, por exercício
text(locator(4),c("607","329","409","484")) #Permite colocar as quantidades no gráfico
barplot(tapply(AprovGastos2016$`13 - Fator 4: Aprovação da Comprovação de Gastos pelo CMAS-01/2016`,AprovGastos2016$UF,length),main = "Gráfico 34 - Municípios sem Aprovação da Comprovação de gastos no SuasWeb - jan/2016", cex.names = 0.7,xlab="Total:607 municípios",ylim=c(0, 150)) #Plota municípios qtde de municícpios sem aprovação dos gastos em jan/16, por UF.
barplot(tapply(AprovGastos2017$`13 - Fator 4: Aprovação da Comprovação de Gastos pelo CMAS-01/2017`,AprovGastos2017$UF,length),main = "Gráfico 35 - Municípios sem Aprovação da Comprovação de gastos no SuasWeb - jan/2017", cex.names = 0.7,xlab="Total:329 municípios",ylim=c(0, 150)) #Plota municípios qtde de municícpios sem aprovação dos gastos em jan/17, por UF.
barplot(tapply(AprovGastos2018$`13 - Fator 4: Aprovação da Comprovação de Gastos pelo CMAS-01/2018`,AprovGastos2018$UF,length),main = "Gráfico 36 - Municípios sem Aprovação da Comprovação de gastos no SuasWeb - jan/2018", cex.names = 0.7,xlab="Total:409 municípios",ylim=c(0, 150)) #Plota municípios qtde de municícpios sem aprovação dos gastos em jan/18, por UF.
barplot(tapply(AprovGastos2019$`13 - Fator 4: Aprovação da Comprovação de Gastos pelo CMAS-01/2019`,AprovGastos2019$UF,length),main = "Gráfico 37 - Municípios sem Aprovação da Comprovação de gastos no SuasWeb - jan/2019", cex.names = 0.7,xlab="Total:484 municípios",ylim=c(0, 150)) #Plota municípios qtde de municícpios sem aprovação dos gastos em jan/19, por UF.

#Exportando tabelas (anexos)
write.csv(MenorIGDM2016, "MenorIGDM2016.csv") #Exportando tabela com menores valores de IGD-M para 2016
write.csv(MenorIGDM2017, "MenorIGDM2017.csv") #Exportando tabela com menores valores de IGD-M para 2017
write.csv(MenorIGDM2018, "MenorIGDM2018.csv") #Exportando tabela com menores valores de IGD-M para 2018
write.csv(MenorIGDM2019, "MenorIGDM2019.csv") #Exportando tabela com menores valores de IGD-M para 2019
write.csv(IGDMzero2016, "IGDMzero2016.csv") #Exportando tabela com municípios com IGD-M igual a zero em 2016
write.csv(IGDMzero2017, "IGDMzero2017.csv") #Exportando tabela com municípios com IGD-M igual a zero em 2017
write.csv(IGDMzero2018, "IGDMzero2018.csv") #Exportando tabela com municípios com IGD-M igual a zero em 2018
write.csv(IGDMzero2019, "IGDMzero2019.csv") #Exportando tabela com municípios com IGD-M igual a zero em 2019
write.csv(MenorFator12016, "MenorFator12016.csv") #Exportando tabela com menores valores para Fator 1 em 2016
write.csv(MenorFator12017, "MenorFator12017.csv") #Exportando tabela com menores valores para Fator 1 em 2017
write.csv(MenorFator12018, "MenorFator12018.csv") #Exportando tabela com menores valores para Fator 1 em 2018
write.csv(MenorFator12019, "MenorFator12019.csv") #Exportando tabela com menores valores para Fator 1 em 2019
write.csv(MenorTAC2016, "MenorTAC2016.csv") #Exportando tabela com menores TAC em 2016
write.csv(MenorTAC2017, "MenorTAC2017.csv") #Exportando tabela com menores TAC em 2017
write.csv(MenorTAC2018, "MenorTAC2018.csv") #Exportando tabela com menores TAC em 2018
write.csv(MenorTAC2019, "MenorTAC2019.csv") #Exportando tabela com menores TAC em 2019
write.csv(MenorTAFE2016, "MenorTAFE2016.csv") #Exportando tabela com menores TAFE em 2016
write.csv(MenorTAFE2017, "MenorTAFE2017.csv") #Exportando tabela com menores TAFE em 2017
write.csv(MenorTAFE2018, "MenorTAFE2018.csv") #Exportando tabela com menores TAFE em 2018
write.csv(MenorTAFE2019, "MenorTAFE2019.csv") #Exportando tabela com menores TAFE em 2019
write.csv(MenorTAAS2016, "MenorTAAS2016.csv") #Exportando tabela com menores TAAS em 2016
write.csv(MenorTAAS2017, "MenorTAAS2017.csv") #Exportando tabela com menores TAAS em 2017
write.csv(MenorTAAS2018, "MenorTAAS2018.csv") #Exportando tabela com menores TAAS em 2018
write.csv(MenorTAAS2019, "MenorTAAS2019.csv") #Exportando tabela com menores TAAS em 2019
write.csv(ComprovGastos2016, "ComprovGastos2016.csv") #Exportando tabela com municícpios que não comprovaram gastos em 2016 (Fator 3)
write.csv(ComprovGastos2017, "ComprovGastos2017.csv") #Exportando tabela com municícpios que não comprovaram gastos em 2017 (Fator 3)
write.csv(ComprovGastos2018, "ComprovGastos2018.csv") #Exportando tabela com municícpios que não comprovaram gastos em 2018 (Fator 3)
write.csv(ComprovGastos2019, "ComprovGastos2019.csv") #Exportando tabela com municícpios que não comprovaram gastos em 2019 (Fator 3)
write.csv(AprovGastos2016, "AprovGastos2016.csv") #Exportando tabela com municícpios que não tiveram aprovação de gastos em 2016 (Fator 4)
write.csv(AprovGastos2017, "AprovGastos2017.csv") #Exportando tabela com municícpios que não tiveram aprovação de gastos em 2017 (Fator 4)
write.csv(AprovGastos2018, "AprovGastos2018.csv") #Exportando tabela com municícpios que não tiveram aprovação de gastos em 2018 (Fator 4)
write.csv(AprovGastos2019, "AprovGastos2019.csv") #Exportando tabela com municícpios que não tiveram aprovação de gastos em 2019 (Fator 4)
