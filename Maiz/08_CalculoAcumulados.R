#################################################
# JAIME RECIO HUETOS 
# ETSIA DE LA UPM; DEPARTAMENTO DE QUÍMICA Y TECNOLOGIA DE ALIMENTOS 
#18/09/2017
#############################################################
# Sacar Acumulados 

############################################################################.
# el incremento del tiempo en cada dato de flujo es Dlt y esta en horas 
t1 <- chamb1_Gnal$Fecha; Dlt1 <- diff(chamb1_Gnal$Fecha) 
t2 <- chamb2_Gnal$Fecha; Dlt2 <- diff(chamb2_Gnal$Fecha)
t3 <- chamb3_Gnal$Fecha; Dlt3 <- diff(chamb3_Gnal$Fecha)
t4 <- chamb4_Gnal$Fecha; Dlt4 <- diff(chamb4_Gnal$Fecha)
t5 <- chamb5_Gnal$Fecha; Dlt5 <- diff(chamb5_Gnal$Fecha)
t6 <- chamb6_Gnal$Fecha; Dlt6 <- diff(chamb6_Gnal$Fecha)
t7 <- chamb7_Gnal$Fecha; Dlt7 <- diff(chamb7_Gnal$Fecha)
t8 <- chamb8_Gnal$Fecha; Dlt8 <- diff(chamb8_Gnal$Fecha)
t9 <- chamb9_Gnal$Fecha; Dlt9 <- diff(chamb9_Gnal$Fecha)
t10 <- chamb10_Gnal$Fecha; Dlt10 <- diff(chamb10_Gnal$Fecha)
t11 <- chamb11_Gnal$Fecha; Dlt11 <- diff(chamb11_Gnal$Fecha)
t12 <- chamb12_Gnal$Fecha; Dlt12 <- diff(chamb12_Gnal$Fecha)
t13 <- chamb13_Gnal$Fecha; Dlt13 <- diff(chamb13_Gnal$Fecha)
t14 <- chamb14_Gnal$Fecha; Dlt14 <- diff(chamb14_Gnal$Fecha)
t15 <- chamb15_Gnal$Fecha; Dlt15 <- diff(chamb15_Gnal$Fecha)
############################################################################
# Voy a localizar qué valores del flujo de las cámaras son negativos 
vals.neg1 <- which(chamb1_Gnal$FluxNOx<0); length(vals.neg1)/440  # son 440 (+ó-) los que hay 
vals.neg2 <- which(chamb2_Gnal$FluxNOx<0); length(vals.neg2)/440   # son 440 (+ó-) los que hay 
vals.neg3 <- which(chamb3_Gnal$FluxNOx<0); length(vals.neg3)/440   # son 440 (+ó-) los que hay 
vals.neg4 <- which(chamb4_Gnal$FluxNOx<0); length(vals.neg4)/440   # son 440 (+ó-) los que hay 
vals.neg5 <- which(chamb5_Gnal$FluxNOx<0); length(vals.neg5)/440   # son 440 (+ó-) los que hay 
vals.neg6 <- which(chamb6_Gnal$FluxNOx<0); length(vals.neg6)/440   # son 440 (+ó-) los que hay 
vals.neg7 <- which(chamb7_Gnal$FluxNOx<0); length(vals.neg7)/440   # son 440 (+ó-) los que hay 
vals.neg8 <- which(chamb8_Gnal$FluxNOx<0); length(vals.neg8)/440   # son 440 (+ó-) los que hay 
vals.neg9 <- which(chamb9_Gnal$FluxNOx<0); length(vals.neg9)/440   # son 440 (+ó-) los que hay 
vals.neg10 <- which(chamb10_Gnal$FluxNOx<0); length(vals.neg10)/440  # son 440 (+ó-) los que hay 
vals.neg11 <- which(chamb11_Gnal$FluxNOx<0); length(vals.neg11)/440  # son 440 (+ó-) los que hay 
vals.neg12 <- which(chamb12_Gnal$FluxNOx<0); length(vals.neg12)/440  # son 440 (+ó-) los que hay 
vals.neg13 <- which(chamb13_Gnal$FluxNOx<0); length(vals.neg13)/440  # son 440 (+ó-) los que hay 
vals.neg14 <- which(chamb14_Gnal$FluxNOx<0); length(vals.neg14)/440  # son 440 (+ó-) los que hay 
vals.neg15 <- which(chamb15_Gnal$FluxNOx<0); length(vals.neg15)/440  # son 440 (+ó-) los que hay 
############################################################################
#Voy a crear un vector donde los valores negativos del flujo de cada chamber se haga 0
FluxNOxAux1 <- FluxNOx1; FluxNOxAux1[vals.neg1]=0; 
FluxNOxAux2 <- FluxNOx2; FluxNOxAux2[vals.neg2]=0; 
FluxNOxAux3 <- FluxNOx3; FluxNOxAux3[vals.neg3]=0; 
FluxNOxAux4 <- FluxNOx4; FluxNOxAux4[vals.neg4]=0; 
FluxNOxAux5 <- FluxNOx5; FluxNOxAux5[vals.neg5]=0; 
FluxNOxAux6 <- FluxNOx6; FluxNOxAux6[vals.neg6]=0; 
FluxNOxAux7 <- FluxNOx7; FluxNOxAux7[vals.neg7]=0; 
FluxNOxAux8 <- FluxNOx8; FluxNOxAux8[vals.neg8]=0; 
FluxNOxAux9 <- FluxNOx9; FluxNOxAux9[vals.neg9]=0; 
FluxNOxAux10 <- FluxNOx10; FluxNOxAux10[vals.neg10]=0; 
FluxNOxAux11 <- FluxNOx11; FluxNOxAux11[vals.neg11]=0; 
FluxNOxAux12 <- FluxNOx12; FluxNOxAux12[vals.neg12]=0; 
FluxNOxAux13 <- FluxNOx13; FluxNOxAux13[vals.neg13]=0; 
FluxNOxAux14 <- FluxNOx14; FluxNOxAux14[vals.neg14]=0; 
FluxNOxAux15 <- FluxNOx15; FluxNOxAux15[vals.neg15]=0; 
############################################################################

#creo vectores vacíos para meter los acumulados 
CNOx1 <- NULL;CNOx2 <- NULL;CNOx3 <- NULL;CNOx4 <- NULL;CNOx5 <- NULL;CNOx6 <- NULL;
CNOx7 <- NULL;CNOx8 <- NULL;CNOx9 <- NULL;CNOx10 <- NULL;CNOx11 <- NULL;CNOx12 <- NULL;
CNOx13 <- NULL;CNOx14 <- NULL;CNOx15 <- NULL;
############################################################################
CNO1 <- NULL;CNO2 <- NULL;CNO3 <- NULL;CNO4 <- NULL;CNO5 <- NULL;CNO6 <- NULL;
CNO7 <- NULL;CNO8 <- NULL;CNO9 <- NULL;CNO10 <- NULL;CNO11 <- NULL;CNO12 <- NULL;
CNO13 <- NULL;CNO14 <- NULL;CNO15 <- NULL;
############################################################################
CNO21 <- NULL;CNO22 <- NULL;CNO23 <- NULL;CNO24 <- NULL;CNO25 <- NULL;CNO26 <- NULL;
CNO27 <- NULL;CNO28 <- NULL;CNO29 <- NULL;CNO210 <- NULL;CNO211 <- NULL;CNO212 <- NULL;
CNO213 <- NULL;CNO214 <- NULL;CNO215 <- NULL;
############################################################################
#voy a sumar valores en ug/m2
#1)NOx
CNOx1[1]=FluxNOx1[1];
Dlt_HH1<- c(0,Dlt1) 
for(k in 2:(length(FluxNOx1)))
{ CNOx1[k] <- ((FluxNOx1[k-1]+FluxNOx1[k])/2*as.numeric(Dlt_HH1[k]))+CNOx1[k-1]}
############################################################################
CNOx2[1]=FluxNOx2[1];Dlt_HH2<- c(0,Dlt2)
for(k in 2:(length(FluxNOx2)))
{ CNOx2[k] <- ((FluxNOx2[k-1]+FluxNOx2[k])/2*as.numeric(Dlt_HH2[k]))+CNOx2[k-1]}
############################################################################
CNOx3[1]=FluxNOx3[1];Dlt_HH3<- c(0,Dlt3)
for(k in 2:(length(FluxNOx3)))
{ CNOx3[k] <- ((FluxNOx3[k-1]+FluxNOx3[k])/2*as.numeric(Dlt_HH3[k]))+CNOx3[k-1]}
############################################################################
CNOx4[1]=FluxNOx4[1];Dlt_HH4<- c(0,Dlt4)
for(k in 2:(length(FluxNOx4)))
{ CNOx4[k] <- ((FluxNOx4[k-1]+FluxNOx4[k])/2*as.numeric(Dlt_HH4[k]))+CNOx4[k-1]}
############################################################################
CNOx5[1]=FluxNOx5[1];Dlt_HH5<- c(0,Dlt5)
for(k in 2:(length(FluxNOx5)))
{ CNOx5[k] <- ((FluxNOx5[k-1]+FluxNOx5[k])/2*as.numeric(Dlt_HH5[k]))+CNOx5[k-1]}
############################################################################
CNOx6[1]=FluxNOx6[1];Dlt_HH6<- c(0,Dlt6)
for(k in 2:(length(FluxNOx6)))
{ CNOx6[k] <- ((FluxNOx6[k-1]+FluxNOx6[k])/2*as.numeric(Dlt_HH6[k]))+CNOx6[k-1]}
############################################################################
CNOx7[1]=FluxNOx7[1];Dlt_HH7<- c(0,Dlt7)
for(k in 2:(length(FluxNOx7)))
{ CNOx7[k] <- ((FluxNOx7[k-1]+FluxNOx7[k])/2*as.numeric(Dlt_HH7[k]))+CNOx7[k-1]}
############################################################################
CNOx8[1]=FluxNOx8[1];Dlt_HH8<- c(0,Dlt8)
for(k in 2:(length(FluxNOx8)))
{ CNOx8[k] <- ((FluxNOx8[k-1]+FluxNOx8[k])/2*as.numeric(Dlt_HH8[k]))+CNOx8[k-1]}
############################################################################
CNOx9[1]=FluxNOx9[1];Dlt_HH9<- c(0,Dlt9)
for(k in 2:(length(FluxNOx9)))
{ CNOx9[k] <- ((FluxNOx9[k-1]+FluxNOx9[k])/2*as.numeric(Dlt_HH9[k]))+CNOx9[k-1]}
############################################################################
CNOx10[1]=FluxNOx10[1];Dlt_HH10<- c(0,Dlt10)
for(k in 2:(length(FluxNOx10)))
{ CNOx10[k] <- ((FluxNOx10[k-1]+FluxNOx10[k])/2*as.numeric(Dlt_HH10[k]))+CNOx10[k-1]}
############################################################################
CNOx11[1]=FluxNOx11[1];Dlt_HH11<- c(0,Dlt11)
for(k in 2:(length(FluxNOx11)))
{ CNOx11[k] <- ((FluxNOx11[k-1]+FluxNOx11[k])/2*as.numeric(Dlt_HH11[k]))+CNOx11[k-1]}
############################################################################
CNOx12[1]=FluxNOx12[1];Dlt_HH12<- c(0,Dlt12)
for(k in 2:(length(FluxNOx12)))
{ CNOx12[k] <- ((FluxNOx12[k-1]+FluxNOx12[k])/2*as.numeric(Dlt_HH12[k]))+CNOx12[k-1]}
############################################################################
CNOx13[1]=FluxNOx13[1];Dlt_HH13<- c(0,Dlt13)
for(k in 2:(length(FluxNOx13)))
{ CNOx13[k] <- ((FluxNOx13[k-1]+FluxNOx13[k])/2*as.numeric(Dlt_HH13[k]))+CNOx13[k-1]}
############################################################################
CNOx14[1]=FluxNOx14[1];Dlt_HH14<- c(0,Dlt14)
for(k in 2:(length(FluxNOx14)))
{ CNOx14[k] <- ((FluxNOx14[k-1]+FluxNOx14[k])/2*as.numeric(Dlt_HH14[k]))+CNOx14[k-1]}
############################################################################
CNOx15[1]=FluxNOx15[1];Dlt_HH15<- c(0,Dlt15)
for(k in 2:(length(FluxNOx15)))
{ CNOx15[k] <- ((FluxNOx15[k-1]+FluxNOx15[k])/2*as.numeric(Dlt_HH15[k]))+CNOx15[k-1]}
############################################################################

#2)NO
CNO1[1]=FluxNO1[1];
Dlt_HH1<- c(0,Dlt1) 
for(k in 2:(length(FluxNO1)))
{ CNO1[k] <- ((FluxNO1[k-1]+FluxNO1[k])/2*as.numeric(Dlt_HH1[k]))+CNO1[k-1]}
############################################################################
CNO2[1]=FluxNO2[1];Dlt_HH2<- c(0,Dlt2)
for(k in 2:(length(FluxNO2)))
{ CNO2[k] <- ((FluxNO2[k-1]+FluxNO2[k])/2*as.numeric(Dlt_HH2[k]))+CNO2[k-1]}
############################################################################
CNO3[1]=FluxNO3[1];Dlt_HH3<- c(0,Dlt3)
for(k in 2:(length(FluxNO3)))
{ CNO3[k] <- ((FluxNO3[k-1]+FluxNO3[k])/2*as.numeric(Dlt_HH3[k]))+CNO3[k-1]}
############################################################################
CNO4[1]=FluxNO4[1];Dlt_HH4<- c(0,Dlt4)
for(k in 2:(length(FluxNO4)))
{ CNO4[k] <- ((FluxNO4[k-1]+FluxNO4[k])/2*as.numeric(Dlt_HH4[k]))+CNO4[k-1]}
############################################################################
CNO5[1]=FluxNO5[1];Dlt_HH5<- c(0,Dlt5)
for(k in 2:(length(FluxNO5)))
{ CNO5[k] <- ((FluxNO5[k-1]+FluxNO5[k])/2*as.numeric(Dlt_HH5[k]))+CNO5[k-1]}
############################################################################
CNO6[1]=FluxNO6[1];Dlt_HH6<- c(0,Dlt6)
for(k in 2:(length(FluxNO6)))
{ CNO6[k] <- ((FluxNO6[k-1]+FluxNO6[k])/2*as.numeric(Dlt_HH6[k]))+CNO6[k-1]}
############################################################################
CNO7[1]=FluxNO7[1];Dlt_HH7<- c(0,Dlt7)
for(k in 2:(length(FluxNO7)))
{ CNO7[k] <- ((FluxNO7[k-1]+FluxNO7[k])/2*as.numeric(Dlt_HH7[k]))+CNO7[k-1]}
############################################################################
CNO8[1]=FluxNO8[1];Dlt_HH8<- c(0,Dlt8)
for(k in 2:(length(FluxNO8)))
{ CNO8[k] <- ((FluxNO8[k-1]+FluxNO8[k])/2*as.numeric(Dlt_HH8[k]))+CNO8[k-1]}
############################################################################
CNO9[1]=FluxNO9[1];Dlt_HH9<- c(0,Dlt9)
for(k in 2:(length(FluxNO9)))
{ CNO9[k] <- ((FluxNO9[k-1]+FluxNO9[k])/2*as.numeric(Dlt_HH9[k]))+CNO9[k-1]}
############################################################################
CNO10[1]=FluxNO10[1];Dlt_HH10<- c(0,Dlt10)
for(k in 2:(length(FluxNO10)))
{ CNO10[k] <- ((FluxNO10[k-1]+FluxNO10[k])/2*as.numeric(Dlt_HH10[k]))+CNO10[k-1]}
############################################################################
CNO11[1]=FluxNO11[1];Dlt_HH11<- c(0,Dlt11)
for(k in 2:(length(FluxNO11)))
{ CNO11[k] <- ((FluxNO11[k-1]+FluxNO11[k])/2*as.numeric(Dlt_HH11[k]))+CNO11[k-1]}
############################################################################
CNO12[1]=FluxNO12[1];Dlt_HH12<- c(0,Dlt12)
for(k in 2:(length(FluxNO12)))
{ CNO12[k] <- ((FluxNO12[k-1]+FluxNO12[k])/2*as.numeric(Dlt_HH12[k]))+CNO12[k-1]}
############################################################################
CNO13[1]=FluxNO13[1];Dlt_HH13<- c(0,Dlt13)
for(k in 2:(length(FluxNO13)))
{ CNO13[k] <- ((FluxNO13[k-1]+FluxNO13[k])/2*as.numeric(Dlt_HH13[k]))+CNO13[k-1]}
############################################################################
CNO14[1]=FluxNO14[1];Dlt_HH14<- c(0,Dlt14)
for(k in 2:(length(FluxNO14)))
{ CNO14[k] <- ((FluxNO14[k-1]+FluxNO14[k])/2*as.numeric(Dlt_HH14[k]))+CNO14[k-1]}
############################################################################
CNO15[1]=FluxNO15[1];Dlt_HH15<- c(0,Dlt15)
for(k in 2:(length(FluxNO15)))
{ CNO15[k] <- ((FluxNO15[k-1]+FluxNO15[k])/2*as.numeric(Dlt_HH15[k]))+CNO15[k-1]}
############################################################################

#3)NO2

CNO21[1]=FluxNO21[1];
Dlt_HH1<- c(0,Dlt1) 
for(k in 2:(length(FluxNO21)))
{ CNO21[k] <- ((FluxNO21[k-1]+FluxNO21[k])/2*as.numeric(Dlt_HH1[k]))+CNO21[k-1]}
############################################################################
CNO22[1]=FluxNO22[1];Dlt_HH2<- c(0,Dlt2)
for(k in 2:(length(FluxNO22)))
{ CNO22[k] <- ((FluxNO22[k-1]+FluxNO22[k])/2*as.numeric(Dlt_HH2[k]))+CNO22[k-1]}
############################################################################
CNO23[1]=FluxNO23[1];Dlt_HH3<- c(0,Dlt3)
for(k in 2:(length(FluxNO23)))
{ CNO23[k] <- ((FluxNO23[k-1]+FluxNO23[k])/2*as.numeric(Dlt_HH3[k]))+CNO23[k-1]}
############################################################################
CNO24[1]=FluxNO24[1];Dlt_HH4<- c(0,Dlt4)
for(k in 2:(length(FluxNO24)))
{ CNO24[k] <- ((FluxNO24[k-1]+FluxNO24[k])/2*as.numeric(Dlt_HH4[k]))+CNO24[k-1]}
############################################################################
CNO25[1]=FluxNO25[1];Dlt_HH5<- c(0,Dlt5)
for(k in 2:(length(FluxNO25)))
{ CNO25[k] <- ((FluxNO25[k-1]+FluxNO25[k])/2*as.numeric(Dlt_HH5[k]))+CNO25[k-1]}
############################################################################
CNO26[1]=FluxNO26[1];Dlt_HH6<- c(0,Dlt6)
for(k in 2:(length(FluxNO26)))
{ CNO26[k] <- ((FluxNO26[k-1]+FluxNO26[k])/2*as.numeric(Dlt_HH6[k]))+CNO26[k-1]}
############################################################################
CNO27[1]=FluxNO27[1];Dlt_HH7<- c(0,Dlt7)
for(k in 2:(length(FluxNO27)))
{ CNO27[k] <- ((FluxNO27[k-1]+FluxNO27[k])/2*as.numeric(Dlt_HH7[k]))+CNO27[k-1]}
############################################################################
CNO28[1]=FluxNO28[1];Dlt_HH8<- c(0,Dlt8)
for(k in 2:(length(FluxNO28)))
{ CNO28[k] <- ((FluxNO28[k-1]+FluxNO28[k])/2*as.numeric(Dlt_HH8[k]))+CNO28[k-1]}
############################################################################
CNO29[1]=FluxNO29[1];Dlt_HH9<- c(0,Dlt9)
for(k in 2:(length(FluxNO29)))
{ CNO29[k] <- ((FluxNO29[k-1]+FluxNO29[k])/2*as.numeric(Dlt_HH9[k]))+CNO29[k-1]}
############################################################################
CNO210[1]=FluxNO210[1];Dlt_HH10<- c(0,Dlt10)
for(k in 2:(length(FluxNO210)))
{ CNO210[k] <- ((FluxNO210[k-1]+FluxNO210[k])/2*as.numeric(Dlt_HH10[k]))+CNO210[k-1]}
############################################################################
CNO211[1]=FluxNO211[1];Dlt_HH11<- c(0,Dlt11)
for(k in 2:(length(FluxNO211)))
{ CNO211[k] <- ((FluxNO211[k-1]+FluxNO211[k])/2*as.numeric(Dlt_HH11[k]))+CNO211[k-1]}
############################################################################
CNO212[1]=FluxNO212[1];Dlt_HH12<- c(0,Dlt12)
for(k in 2:(length(FluxNO212)))
{ CNO212[k] <- ((FluxNO212[k-1]+FluxNO212[k])/2*as.numeric(Dlt_HH12[k]))+CNO212[k-1]}
############################################################################
CNO213[1]=FluxNO213[1];Dlt_HH13<- c(0,Dlt13)
for(k in 2:(length(FluxNO213)))
{ CNO213[k] <- ((FluxNO213[k-1]+FluxNO213[k])/2*as.numeric(Dlt_HH13[k]))+CNO213[k-1]}
############################################################################
CNO214[1]=FluxNO214[1];Dlt_HH14<- c(0,Dlt14)
for(k in 2:(length(FluxNO214)))
{ CNO214[k] <- ((FluxNO214[k-1]+FluxNO214[k])/2*as.numeric(Dlt_HH14[k]))+CNO214[k-1]}
############################################################################
CNO215[1]=FluxNO215[1];Dlt_HH15<- c(0,Dlt15)
for(k in 2:(length(FluxNO215)))
{ CNO215[k] <- ((FluxNO215[k-1]+FluxNO215[k])/2*as.numeric(Dlt_HH15[k]))+CNO215[k-1]}
############################################################################
###################
###################
chamb1_Gnal$NOxCum <- CNOx1;
chamb2_Gnal$NOxCum <- CNOx2;
chamb3_Gnal$NOxCum <- CNOx3;
chamb4_Gnal$NOxCum <- CNOx4;
chamb5_Gnal$NOxCum <- CNOx5;
chamb6_Gnal$NOxCum <- CNOx6;
chamb7_Gnal$NOxCum <- CNOx7;
chamb8_Gnal$NOxCum <- CNOx8;
chamb9_Gnal$NOxCum <- CNOx9;
chamb10_Gnal$NOxCum <- CNOx10;
chamb11_Gnal$NOxCum <- CNOx11;
chamb12_Gnal$NOxCum <- CNOx12;
chamb13_Gnal$NOxCum <- CNOx13;
chamb14_Gnal$NOxCum <- CNOx14;
chamb15_Gnal$NOxCum<- CNOx15;
#--------------------------------------
###################
chamb1_Gnal$NOCum <- CNO1;
chamb2_Gnal$NOCum <- CNO2;
chamb3_Gnal$NOCum <- CNO3;
chamb4_Gnal$NOCum <- CNO4;
chamb5_Gnal$NOCum <- CNO5;
chamb6_Gnal$NOCum <- CNO6;
chamb7_Gnal$NOCum <- CNO7;
chamb8_Gnal$NOCum <- CNO8;
chamb9_Gnal$NOCum <- CNO9;
chamb10_Gnal$NOCum <- CNO10;
chamb11_Gnal$NOCum <- CNO11;
chamb12_Gnal$NOCum <- CNO12;
chamb13_Gnal$NOCum <- CNO13;
chamb14_Gnal$NOCum <- CNO14;
chamb15_Gnal$NOCum<- CNO15;
#-----------------------------------------
###################
chamb1_Gnal$NO2Cum <- CNO21;
chamb2_Gnal$NO2Cum <- CNO22;
chamb3_Gnal$NO2Cum <- CNO23;
chamb4_Gnal$NO2Cum <- CNO24;
chamb5_Gnal$NO2Cum <- CNO25;
chamb6_Gnal$NO2Cum <- CNO26;
chamb7_Gnal$NO2Cum <- CNO27;
chamb8_Gnal$NO2Cum <- CNO28;
chamb9_Gnal$NO2Cum <- CNO29;
chamb10_Gnal$NO2Cum <- CNO210;
chamb11_Gnal$NO2Cum <- CNO211;
chamb12_Gnal$NO2Cum <- CNO212;
chamb13_Gnal$NO2Cum <- CNO213;
chamb14_Gnal$NO2Cum <- CNO214;
chamb15_Gnal$NO2Cum<- CNO215;
#Todos los acumulados estan en ug/m2 
#OJO que es lo unico en GAPDmean que tiene distinta unidad 
