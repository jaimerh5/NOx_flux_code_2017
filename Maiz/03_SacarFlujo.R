#################################################

# JAIME RECIO HUETOS 
# ETSIA DE LA UPM; DEPARTAMENTO DE QUÍMICA Y TECNOLOGIA DE ALIMENTOS 
#13/09/2017
#############################################################
# vamos a sacar el flujo sin los datos de ambiente
################################################
#[FLUJO]=ug/m2/h
setwd('C:/R/Expmto2017/MAIZ')
library(lattice); library(latticeExtra); library(ggplot2); library(data.table)
#Script para sacar flujos 
#Flujo= m[ug]/m^2*h= cte*Qaudal*Conc
# k1 <- Mw*60*1e6/(vol*1e9*SS)
#1ppb_v== 14e-3/22.4 [ug/L]
# másico= [kg/s]= caudal*concentracion {L/h*ug/L}
#Flujo= [ug/m^2*h] = másico/sup 
################################################################
# Calculo Flujos y los meto en la matriz de cada caja 
#datos en archivo 00_ExperiemntData

FluxNOx1 <- k1*Qaudal[1]*chamb1_Gnal$NOx_ABS; chamb1_Gnal$FluxNOx <- FluxNOx1
FluxNOx2 <- k1*Qaudal[2]*chamb2_Gnal$NOx_ABS; chamb2_Gnal$FluxNOx <- FluxNOx2
FluxNOx3 <- k1*Qaudal[3]*chamb3_Gnal$NOx_ABS; chamb3_Gnal$FluxNOx <- FluxNOx3
FluxNOx4 <- k1*Qaudal[4]*chamb4_Gnal$NOx_ABS; chamb4_Gnal$FluxNOx <- FluxNOx4
FluxNOx5 <- k1*Qaudal[5]*chamb5_Gnal$NOx_ABS; chamb5_Gnal$FluxNOx <- FluxNOx5
FluxNOx6 <- k1*Qaudal[6]*chamb6_Gnal$NOx_ABS; chamb6_Gnal$FluxNOx<- FluxNOx6
FluxNOx7 <- k1*Qaudal[7]*chamb7_Gnal$NOx_ABS; chamb7_Gnal$FluxNOx <- FluxNOx7
FluxNOx8 <- k1*Qaudal[8]*chamb8_Gnal$NOx_ABS; chamb8_Gnal$FluxNOx <- FluxNOx8
FluxNOx9 <- k1*Qaudal[9]*chamb9_Gnal$NOx_ABS; chamb9_Gnal$FluxNOx <- FluxNOx9
FluxNOx10 <- k1*Qaudal[10]*chamb10_Gnal$NOx_ABS; chamb10_Gnal$FluxNOx <- FluxNOx10
FluxNOx11 <- k1*Qaudal[11]*chamb11_Gnal$NOx_ABS; chamb11_Gnal$FluxNOx <- FluxNOx11
FluxNOx12 <- k1*Qaudal[12]*chamb12_Gnal$NOx_ABS; chamb12_Gnal$FluxNOx <- FluxNOx12
FluxNOx13 <- k1*Qaudal[13]*chamb13_Gnal$NOx_ABS; chamb13_Gnal$FluxNOx<- FluxNOx13
FluxNOx14 <- k1*Qaudal[14]*chamb14_Gnal$NOx_ABS; chamb14_Gnal$FluxNOx <- FluxNOx14
FluxNOx15 <- k1*Qaudal[15]*chamb15_Gnal$NOx_ABS; chamb15_Gnal$FluxNOx <- FluxNOx15
################################################################
FluxNO1 <- k1*Qaudal[1]*chamb1_Gnal$NO_ABS; chamb1_Gnal$Flux.NO <- FluxNO1
FluxNO2 <- k1*Qaudal[2]*chamb2_Gnal$NO_ABS; chamb2_Gnal$Flux.NO <- FluxNO2
FluxNO3 <- k1*Qaudal[3]*chamb3_Gnal$NO_ABS; chamb3_Gnal$Flux.NO <- FluxNO3
FluxNO4 <- k1*Qaudal[4]*chamb4_Gnal$NO_ABS; chamb4_Gnal$Flux.NO <- FluxNO4
FluxNO5 <- k1*Qaudal[5]*chamb5_Gnal$NO_ABS; chamb5_Gnal$Flux.NO <- FluxNO5
FluxNO6 <- k1*Qaudal[6]*chamb6_Gnal$NO_ABS; chamb6_Gnal$Flux.NO <- FluxNO6
FluxNO7 <- k1*Qaudal[7]*chamb7_Gnal$NO_ABS; chamb7_Gnal$Flux.NO <- FluxNO7
FluxNO8 <- k1*Qaudal[8]*chamb8_Gnal$NO_ABS; chamb8_Gnal$Flux.NO <- FluxNO8
FluxNO9 <- k1*Qaudal[9]*chamb9_Gnal$NO_ABS; chamb9_Gnal$Flux.NO <- FluxNO9
FluxNO10 <- k1*Qaudal[10]*chamb10_Gnal$NO_ABS; chamb10_Gnal$Flux.NO <- FluxNO10
FluxNO11 <- k1*Qaudal[11]*chamb11_Gnal$NO_ABS; chamb11_Gnal$Flux.NO <- FluxNO11
FluxNO12 <- k1*Qaudal[12]*chamb12_Gnal$NO_ABS; chamb12_Gnal$Flux.NO <- FluxNO12
FluxNO13 <- k1*Qaudal[13]*chamb13_Gnal$NO_ABS; chamb13_Gnal$Flux.NO <- FluxNO13
FluxNO14 <- k1*Qaudal[14]*chamb14_Gnal$NO_ABS; chamb14_Gnal$Flux.NO <- FluxNO14
FluxNO15 <- k1*Qaudal[15]*chamb15_Gnal$NO_ABS; chamb15_Gnal$Flux.NO <- FluxNO15
################################################################
Flux.NO21 <- k1*Qaudal[1]*chamb1_Gnal$NO2_ABS; chamb1_Gnal$Flux.NO2 <- Flux.NO21
Flux.NO22 <- k1*Qaudal[2]*chamb2_Gnal$NO2_ABS; chamb2_Gnal$Flux.NO2 <- Flux.NO22
Flux.NO23 <- k1*Qaudal[3]*chamb3_Gnal$NO2_ABS; chamb3_Gnal$Flux.NO2 <- Flux.NO23
Flux.NO24 <- k1*Qaudal[4]*chamb4_Gnal$NO2_ABS; chamb4_Gnal$Flux.NO2 <- Flux.NO24
Flux.NO25 <- k1*Qaudal[5]*chamb5_Gnal$NO2_ABS; chamb5_Gnal$Flux.NO2 <- Flux.NO25
Flux.NO26 <- k1*Qaudal[6]*chamb6_Gnal$NO2_ABS; chamb6_Gnal$Flux.NO2 <- Flux.NO26
Flux.NO27 <- k1*Qaudal[7]*chamb7_Gnal$NO2_ABS; chamb7_Gnal$Flux.NO2 <- Flux.NO27
Flux.NO28 <- k1*Qaudal[8]*chamb8_Gnal$NO2_ABS; chamb8_Gnal$Flux.NO2 <- Flux.NO28
Flux.NO29 <- k1*Qaudal[9]*chamb9_Gnal$NO2_ABS; chamb9_Gnal$Flux.NO2 <- Flux.NO29
Flux.NO210 <- k1*Qaudal[10]*chamb10_Gnal$NO2_ABS; chamb10_Gnal$Flux.NO2 <- Flux.NO210
Flux.NO211 <- k1*Qaudal[11]*chamb11_Gnal$NO2_ABS; chamb11_Gnal$Flux.NO2 <- Flux.NO211
Flux.NO212 <- k1*Qaudal[12]*chamb12_Gnal$NO2_ABS; chamb12_Gnal$Flux.NO2 <- Flux.NO212
Flux.NO213 <- k1*Qaudal[13]*chamb13_Gnal$NO2_ABS; chamb13_Gnal$Flux.NO2 <- Flux.NO213
Flux.NO214 <- k1*Qaudal[14]*chamb14_Gnal$NO2_ABS; chamb14_Gnal$Flux.NO2 <- Flux.NO214
Flux.NO215 <- k1*Qaudal[15]*chamb15_Gnal$NO2_ABS; chamb15_Gnal$Flux.NO2 <- Flux.NO215
################################################################
# Valor máx y min de los Flujos 
LimF1 <- range(FluxNOx1); LimF2 <- range(FluxNOx2);LimF3 <- range(FluxNOx3);
LimF4 <- range(FluxNOx4);LimF5 <- range(FluxNOx5);LimF6 <- range(FluxNOx6);
LimF7 <- range(FluxNOx7);LimF8 <- range(FluxNOx8);LimF9 <- range(FluxNOx9);
LimF10 <- range(FluxNOx10);LimF11 <- range(FluxNOx11);LimF12 <- range(FluxNOx12);
LimF13 <- range(FluxNOx13);LimF14 <- range(FluxNOx14);LimF15 <- range(FluxNOx15);

############
MxU <- max(LimF5[2],LimF9[2],LimF12[2]); MxUNI <- max(LimF3[2],LimF10[2],LimF14[2]); 
MxUUI <- max(LimF2[2],LimF6[2],LimF15[2]); MxU2I <- max(LimF4[2],LimF7[2],LimF11[2]);
MxCon <- max(LimF1[2],LimF8[2],LimF13[2]);
MaxValue <- max(MxU,MxUNI,MxUUI,MxU2I,MxCon);MaxValue
############
MnU <- min(LimF5[1],LimF9[1],LimF12[1]);MnUNI <- min(LimF3[1],LimF10[1],LimF14[1]);
MnUUI <- min(LimF2[1],LimF6[1],LimF15[1]); MnU2I <- min(LimF4[1],LimF7[1],LimF11[1]);
MnCon <- min(LimF1[1],LimF8[1],LimF13[1]);
MinValue <- min(MnU,MnUNI,MnUUI,MnU2I,MnCon);MinValue
