#Importa base de dados de dois meses
VarIGD<-IGD_jan_fev_2019 #Cria objeto a partir da base de dados importada

#Eliminando denominadores iguais a zero
VarIGD$`6 - TAAS - Taxa de Acompanhamento de Agenda de Saúde ( item 4 / item 5 )-02/2019`[VarIGD$`6 - TAAS - Taxa de Acompanhamento de Agenda de Saúde ( item 4 / item 5 )-02/2019`==0.00]<-0.01 #Substituindo valores 0.00 do numerador por 0.001
VarIGD$`6 - TAAS - Taxa de Acompanhamento de Agenda de Saúde ( item 4 / item 5 )-01/2019`[VarIGD$`6 - TAAS - Taxa de Acompanhamento de Agenda de Saúde ( item 4 / item 5 )-01/2019`==0.00]<-0.01 #Substituindo valores 0.00 do denominador por 0.001
VarIGD$`3 - TAFE - Taxa de Acompanhameto de Frequência Escolar ( item 1 / item 2 )-02/2019`[VarIGD$`3 - TAFE - Taxa de Acompanhameto de Frequência Escolar ( item 1 / item 2 )-02/2019`==0.00]<-0.01 #Substituindo valores 0.00 do numerador por 0.001
VarIGD$`3 - TAFE - Taxa de Acompanhameto de Frequência Escolar ( item 1 / item 2 )-01/2019`[VarIGD$`3 - TAFE - Taxa de Acompanhameto de Frequência Escolar ( item 1 / item 2 )-01/2019`==0.00]<-0.01 #Substituindo valores 0.00 do denominador por 0.001
VarIGD$`9 - TAC - Taxa de Atualização Cadastral ( item 7 / item 8 )-02/2019`[VarIGD$`9 - TAC - Taxa de Atualização Cadastral ( item 7 / item 8 )-02/2019`==0.00]<-0.01 #Substituindo valores 0.00 do numerador por 0.001
VarIGD$`9 - TAC - Taxa de Atualização Cadastral ( item 7 / item 8 )-01/2019`[VarIGD$`9 - TAC - Taxa de Atualização Cadastral ( item 7 / item 8 )-01/2019`==0.00]<-0.01 #Substituindo valores 0.00 do denominador por 0.001

UF<-VarIGD$UF #Cria objeto UF
Código<-VarIGD$Código #Cria objeto Código
Território<-VarIGD$Território #Cria objeto Território
VarTAFE<-VarIGD$`3 - TAFE - Taxa de Acompanhameto de Frequência Escolar ( item 1 / item 2 )-02/2019`/VarIGD$`3 - TAFE - Taxa de Acompanhameto de Frequência Escolar ( item 1 / item 2 )-01/2019`
VarTAAS<-VarIGD$`6 - TAAS - Taxa de Acompanhamento de Agenda de Saúde ( item 4 / item 5 )-02/2019`/VarIGD$`6 - TAAS - Taxa de Acompanhamento de Agenda de Saúde ( item 4 / item 5 )-01/2019`
VarTAC<-VarIGD$`9 - TAC - Taxa de Atualização Cadastral ( item 7 / item 8 )-02/2019`/VarIGD$`9 - TAC - Taxa de Atualização Cadastral ( item 7 / item 8 )-01/2019`
VarFATOR1<-VarIGD$`10 - Fator 1: Operação (  (TAFE+TAAS) / 2 +  TAC / 2)-02/2019`/VarIGD$`10 - Fator 1: Operação (  (TAFE+TAAS) / 2 +  TAC / 2)-01/2019`
Var<-data.frame(UF,Código,Território,VarTAFE,VarTAAS,VarTAC,VarFATOR1)
MenorVarTAAS<-Var[which(Var$VarTAAS<=0.9260),]
MaiorVarTAAS<-Var[which(Var$VarTAAS>1.3468),]
MenorVarTAC<-Var[which(Var$VarTAC<=0.9831),]
MaiorVarTAC<-Var[which(Var$VarTAC>1.015),]
MenorVarFator1<-Var[which(Var$VarFATOR1<=0.9522),]
MaiorVarFator1<-Var[which(Var$VarFATOR1>1.0542),]
par(mfrow=c(1,1))
boxplot(Var$VarTAFE,main="Variação TAFE jan-fev/2019")
boxplot(Var$VarTAAS,main="Variação TAAS jan-fev/2019")
boxplot(Var$VarTAC, main="Variação TAC jan-fev/2019")
boxplot(Var$VarFATOR1,main="Variação Fator 1 jan-fev/2019")
plot(Var$VarTAC,Var$VarFATOR1)
plot(Var$VarTAAS,Var$VarTAC)
plot(Var$VarTAAS,Var$VarFATOR1)
plot(Var$VarTAFE,Var$VarTAAS)
par(mfrow=c(1,1))
View(VarIGD)
demo(graphics)


