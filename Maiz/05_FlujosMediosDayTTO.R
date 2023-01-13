#################################################
# JAIME RECIO HUETOS 
# ETSIA DE LA UPM; DEPARTAMENTO DE QUÍMICA Y TECNOLOGIA DE ALIMENTOS 
#13/09/2017
#############################################################
##Juntar valores medios por  día y TTO
ttoU <- data.frame(DayMean_ch5$FluxNOx5,DayMean_ch9$FluxNOx9,DayMean_ch12$FluxNOx12)
ttoU_FluxMean <- apply(ttoU,1,mean)
ttoU_FluxSD <- apply(ttoU,1,sd)
ttoU <- data.frame(DayMean_ch5$Fecha,DayMean_ch5$FluxNOx5,DayMean_ch9$FluxNOx9,DayMean_ch12$FluxNOx12,ttoU_FluxMean,ttoU_FluxSD)
names(ttoU) <- c('Fecha', Tto_U,'Mean','SD'); head(ttoU)
#############################################################
ttoUNI <- data.frame(DayMean_ch3$FluxNOx3,DayMean_ch10$FluxNOx10,DayMean_ch14$FluxNOx14)
ttoUNI_FluxMean <- apply(ttoUNI,1,mean)
ttoUNI_FluxSD <- apply(ttoUNI,1,sd)
ttoUNI <- data.frame(DayMean_ch3$Fecha,DayMean_ch3$FluxNOx3,DayMean_ch10$FluxNOx10,DayMean_ch14$FluxNOx14,ttoUNI_FluxMean,ttoUNI_FluxSD)
names(ttoUNI) <- c('Fecha', Tto_UNI,'Mean','SD'); head(ttoUNI)
####################################################head#########
ttoUUI <- data.frame(DayMean_ch2$FluxNOx2,DayMean_ch6$FluxNOx6,DayMean_ch15$FluxNOx15)
ttoUUI_FluxMean <- apply(ttoUUI,1,mean)
ttoUUI_FluxSD <- apply(ttoUUI,1,sd)
ttoUUI <- data.frame(DayMean_ch2$Fecha,DayMean_ch2$FluxNOx2,DayMean_ch6$FluxNOx6,DayMean_ch15$FluxNOx15,ttoUUI_FluxMean,ttoUUI_FluxSD)
names(ttoUUI) <- c('Fecha', Tto_UUI,'Mean','SD'); head(ttoUUI)
#############################################################
ttoU2I <- data.frame(DayMean_ch4$FluxNOx4,DayMean_ch7$FluxNOx7,DayMean_ch11$FluxNOx11)
ttoU2I_FluxMean <- apply(ttoU2I,1,mean)
ttoU2I_FluxSD <- apply(ttoU2I,1,sd)
ttoU2I <- data.frame(DayMean_ch4$Fecha,DayMean_ch4$FluxNOx4,DayMean_ch7$FluxNOx7,DayMean_ch11$FluxNOx11,ttoU2I_FluxMean,ttoU2I_FluxSD)
names(ttoU2I) <- c('Fecha', Tto_U2I,'Mean','SD'); head(ttoU2I)
#############################################################
ttoCon <- data.frame(DayMean_ch1$FluxNOx1,DayMean_ch8$FluxNOx8,DayMean_ch13$FluxNOx13)
ttoCon_FluxMean <- apply(ttoCon,1,mean)
ttoCon_FluxSD <- apply(ttoCon,1,sd)
ttoCon <- data.frame(DayMean_ch1$Fecha,DayMean_ch1$FluxNOx1,DayMean_ch8$FluxNOx8,DayMean_ch13$FluxNOx13,ttoCon_FluxMean,ttoCon_FluxSD)
names(ttoCon) <- c('Fecha', Tto_C,'Mean','SD'); head(ttoCon)




