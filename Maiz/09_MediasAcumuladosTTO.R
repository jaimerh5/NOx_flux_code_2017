#################################################
# JAIME RECIO HUETOS 
# ETSIA DE LA UPM; DEPARTAMENTO DE QUÍMICA Y TECNOLOGIA DE ALIMENTOS 
#18/09/2017
#############################################################
# SacarValores Acumulados Medios 
#§1) Vuelvete y saca los valores acumulados medios por caja y por día en 
#     04_MediasDiarias y 04a_MediasHorarias
#§2) copia código de 05_FlujosMediosTTO

#############################################################
##Juntar valores medios por  día y TTO
ttoU_Cum <- data.frame(DayMean_ch5$CUM5,DayMean_ch9$CUM9,DayMean_ch12$CUM12)
ttoU_CumMean <- apply(ttoU_Cum,1,mean)
ttoU_CumSD <- apply(ttoU_Cum,1,sd)
ttoU_Cum <- data.frame(DayMean_ch5$Fecha,DayMean_ch5$CUM5,DayMean_ch9$CUM9,DayMean_ch12$CUM12,ttoU_CumMean,ttoU_CumSD)
names(ttoU_Cum) <- c('Fecha', Tto_U,'Mean','SD'); head(ttoU_Cum)
#############################################################
ttoUNI_Cum <- data.frame(DayMean_ch3$CUM3,DayMean_ch10$CUM10,DayMean_ch14$CUM14)
ttoUNI_CumMean <- apply(ttoUNI_Cum,1,mean)
ttoUNI_CumSD <- apply(ttoUNI_Cum,1,sd)
ttoUNI_Cum <- data.frame(DayMean_ch3$Fecha,DayMean_ch3$CUM3,DayMean_ch10$CUM10,DayMean_ch14$CUM14,ttoUNI_CumMean,ttoUNI_CumSD)
names(ttoUNI_Cum) <- c('Fecha', Tto_UNI,'Mean','SD'); head(ttoUNI_Cum)
#############################################################
ttoUUI_Cum <- data.frame(DayMean_ch2$CUM2,DayMean_ch6$CUM6,DayMean_ch15$CUM15)
ttoUUI_CumMean <- apply(ttoUUI_Cum,1,mean)
ttoUUI_CumSD <- apply(ttoUUI_Cum,1,sd)
ttoUUI_Cum <- data.frame(DayMean_ch2$Fecha,DayMean_ch2$CUM2,DayMean_ch6$CUM6,DayMean_ch15$CUM15,ttoUUI_CumMean,ttoUUI_CumSD)
names(ttoUUI_Cum) <- c('Fecha', Tto_UUI,'Mean','SD'); head(ttoUUI_Cum)
#############################################################
ttoU2I_Cum <- data.frame(DayMean_ch4$CUM4,DayMean_ch7$CUM7,DayMean_ch11$CUM11)
ttoU2I_CumMean <- apply(ttoU2I_Cum,1,mean)
ttoU2I_CumSD <- apply(ttoU2I_Cum,1,sd)
ttoU2I_Cum <- data.frame(DayMean_ch4$Fecha,DayMean_ch4$CUM4,DayMean_ch7$CUM7,DayMean_ch11$CUM11,ttoU2I_CumMean,ttoU2I_CumSD)
names(ttoU2I_Cum) <- c('Fecha', Tto_U2I,'Mean','SD'); head(ttoU2I_Cum)
#############################################################
ttoCon_Cum <- data.frame(DayMean_ch1$CUM1,DayMean_ch8$CUM8,DayMean_ch13$CUM13)
ttoCon_CumMean <- apply(ttoCon_Cum,1,mean)
ttoCon_CumSD <- apply(ttoCon_Cum,1,sd)
ttoCon_Cum <- data.frame(DayMean_ch1$Fecha,DayMean_ch1$CUM1,DayMean_ch8$CUM8,DayMean_ch13$CUM13,ttoCon_CumMean,ttoCon_CumSD)
names(ttoCon_Cum) <- c('Fecha', Tto_C,'Mean','SD'); head(ttoCon_Cum)