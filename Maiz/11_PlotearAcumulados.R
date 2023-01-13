#################################################
# JAIME RECIO HUETOS 
# ETSIA DE LA UPM; DEPARTAMENTO DE QUÍMICA Y TECNOLOGIA DE ALIMENTOS 
#21/12/2017

#############################################################

#Plotear flujos
# un grafico de los datos Horarios con la media definitiva
setwd("C:/R/Expmto2017/MAIZ/EurochemScripts/PLOTS/Acumulados_ABS")
pdf('Acumulados_ABS_MediasHorarias&Diarias.pdf')
###############UREA######################################
#Si divides entre 100 son g/ha

attach(chamb5_Gnal)
plot  ( Fecha,
        NOxCum/100,
        xlab="Year 2017",ylab="NOx_cum [g*ha-1]", main = 'UREA',
        cex.main=1, cex.lab=1,
        ty='p',col="plum",cex=1,lwd=2, axes=F,
        ylim =c(0,1500)  )
attach(chamb9_Gnal)
points(Fecha,
       NOxCum/100,
       xlab='',ylab='', main='',
       ty='p',col="maroon1",cex=1,lwd=2, 
       ylim =c(0,1500)  )
attach(chamb12_Gnal)
points(Fecha,
       NOxCum/100,
       xlab='',ylab='', main='',
       ty='p',col="wheat3",cex=1,lwd=2, 
       ylim =c(0,1500)  )
attach(Cum.ttoU_DayMean)
points(Fecha,
       Mean.Urea.NOx/100,
       xlab='',ylab='', main='',
       ty='o',col="black",pch=15,cex=1,lwd=3, 
       ylim =c(0,1500)  )
# attach(MatrizUrea)
# points(Fecha,
#        Mean.Urea.Flux,
#        xlab='',ylab='', main='',
#        ty='p',col="orange",pch=15,cex=1,lwd=3, 
#        ylim =c(MnU,1000))

# WeekMeanUrea <-aggregate(MatrizUrea,
#                          by = list('WOY'= format(MatrizUrea$Fecha,'%Y-%W' )),
#                          FUN = mean, na.rm=T )

# attach(MatrizUrea)   
# points(Fecha,
#        Mean.Urea.Cum/100,
#        xlab='',ylab='', main='',
#        ty='l',col="orange",pch=15,cex=1,lwd=3, 
#        ylim =c(MnU,1000))
######
#TUNEAR PLOT
axis(2);axis(4); box()
grid(NA,NULL) 
#Esto es para poner las separaciones en ls ejes
axis.POSIXct(1,at=seq(DayMean_ch1[['Fecha']][1],DayMean_ch1[['Fecha']][59],by='week'),format='%d/%m' )

#Esto es para poner las líneas de grid en el eje

ejeTime=seq(DayMean_ch1[['Fecha']][1],DayMean_ch1[['Fecha']][59],by='week')
abline(v= as.numeric(ejeTime),col = "lightgray", lty = "dotted")
######
legend('topright',
       legend=c(Tto_U,'CumMean_UREA'), 
       pch=c(1),bty='n',
       lty=c(NA,NA,NA,1),lwd=3,
       col = c("plum","maroon1","wheat3",'black'),cex=1)
for (k in 1:length(Cum.ttoU_DayMean$SD.Urea.NOx)) {
  segments(Cum.ttoU_DayMean[['Fecha']][k],Cum.ttoU_DayMean[["Mean.Urea.NOx"]][k]/100,Cum.ttoU_DayMean[['Fecha']][k],Cum.ttoU_DayMean[["Mean.Urea.NOx"]][k]/100+(Cum.ttoU_DayMean[["SD.Urea.NOx"]][k]/100/sqrt(3)), col='black', lwd=2)
  segments(Cum.ttoU_DayMean[['Fecha']][k],Cum.ttoU_DayMean[["Mean.Urea.NOx"]][k]/100,Cum.ttoU_DayMean[['Fecha']][k],Cum.ttoU_DayMean[["Mean.Urea.NOx"]][k]/100-(Cum.ttoU_DayMean[["SD.Urea.NOx"]][k]/100/sqrt(3)), col='black', lwd=2)}
###########################################################################
##############################UREA+DMPSA######################################

attach(chamb3_Gnal)
plot  ( Fecha,
       NOxCum/100,
        xlab="Year 2017",ylab="NOx_CUM[g*ha-1]", main = 'UREA+DMPSA',
        cex.main=1, cex.lab=1,
        ty='p',col="chartreuse1",pch=20,cex=1,lwd=2, axes=F,
        ylim =c(-30,200))
attach(chamb10_Gnal)
points(Fecha,
       NOxCum/100,
       xlab='',ylab='', main='',
       ty='p',col="aquamarine2",pch=20,cex=1,lwd=2,  
       ylim =c(0,200) )
attach(chamb14_Gnal)
points(Fecha,
       NOxCum/100,
       xlab='',ylab='', main='',
       ty='p',col="chartreuse4",pch=20,cex=1, lwd=2,
       ylim =c(0,200) )
attach(CumMatrizU.NI)
points(Fecha,
       Mean.U.NI.NOx/100,
       xlab='',ylab='', main='',
       ty='o',col="black",pch=24,cex=1,lty=1,lwd=3,
       ylim =c(0,200) )
######
#TUNEAR PLOT
axis(2);axis(4); box()
grid(NA,NULL) 
#Esto es para poner las separaciones en ls ejes
axis.POSIXct(1,at=seq(DayMean_ch1[['Fecha']][1],DayMean_ch1[['Fecha']][59],by='week'),format='%d/%m' )

#Esto es para poner las líneas de grid en el eje

ejeTime=seq(DayMean_ch1[['Fecha']][1],DayMean_ch1[['Fecha']][59],by='week')
abline(v= as.numeric(ejeTime),col = "lightgray", lty = "dotted")
######
legend('topright',
       legend=c(Tto_UNI,'CumMean_U+NI'), 
       pch=c(20,20,20,24),bty='n',
       lty=c(NA,NA,NA,1),lwd=3,
       col = c("chartreuse1","aquamarine2","chartreuse4",'black'),cex=1)
for (k in 1:length(Cum.ttoU.NI_DayMean$SD.U.NI.NOx)) {
  segments(Cum.ttoU.NI_DayMean[['Fecha']][k],Cum.ttoU.NI_DayMean[["Mean.U.NI.NOx"]][k]/100,Cum.ttoU.NI_DayMean[['Fecha']][k],Cum.ttoU.NI_DayMean[["Mean.U.NI.NOx"]][k]/100+Cum.ttoU.NI_DayMean[["SD.U.NI.NOx"]][k]/sqrt(3)/100, col='black', lwd=2)
  segments(Cum.ttoU.NI_DayMean[['Fecha']][k],Cum.ttoU.NI_DayMean[["Mean.U.NI.NOx"]][k]/100,Cum.ttoU.NI_DayMean[['Fecha']][k],Cum.ttoU.NI_DayMean[["Mean.U.NI.NOx"]][k]/100-Cum.ttoU.NI_DayMean[["SD.U.NI.NOx"]][k]/sqrt(3)/100, col='black', lwd=2)}
###########################################################################

##############################UREA+NBPT######################################


attach(chamb2_Gnal)
plot  ( Fecha,
        NOxCum/100,
        xlab="Year 2017",ylab="NOx_CUM[g*ha-1]", main = 'UREA+NBPT',
        cex.main=1, cex.lab=1,
        ty='p',col="blue1",pch=4,cex=1,lwd=2, axes=F,
        ylim =c(0,1000)  )
attach(chamb6_Gnal);
points(Fecha,
       NOxCum/100,
       xlab='',ylab='', main='',
       ty='p',col="blue4",pch=4,cex=1,  lwd=2,
       ylim =c(0,1000) )
attach(chamb15_Gnal)
points(Fecha,
       NOxCum/100,
       xlab='',ylab='', main='',
       ty='p',col="cyan2",pch=4,cex=1, lwd=2, 
       ylim =c(0,1000) )
attach(CumMatrizU.UI)
points(Fecha,
       Mean.U.UI.NOx/100,
       xlab='',ylab='', main='',
       ty='o',col="black",pch=13,cex=1,lty=1,lwd=3,
       ylim =c(0,1000))
######
#TUNEAR PLOT
axis(2);axis(4); box()
grid(NA,NULL) 
#Esto es para poner las separaciones en ls ejes
axis.POSIXct(1,at=seq(DayMean_ch1[['Fecha']][1],DayMean_ch1[['Fecha']][59],by='week'),format='%d/%m' )

#Esto es para poner las líneas de grid en el eje

ejeTime=seq(DayMean_ch1[['Fecha']][1],DayMean_ch1[['Fecha']][59],by='week')
abline(v= as.numeric(ejeTime),col = "lightgray", lty = "dotted")
######
legend('topright',
       legend=c(Tto_UUI,'CumMean_U+UI'), 
       pch=c(4,4,4,13),bty='n',
       lty=c(NA,NA,NA,1),lwd=3,
       col = c("blue1","blue4","cyan2",'black'),cex=1)
for (k in 1:length(Cum.ttoU.UI_DayMean$SD.U.UI.NOx)) {
  segments(Cum.ttoU.UI_DayMean[['Fecha']][k],Cum.ttoU.UI_DayMean[["Mean.U.UI.NOx"]][k]/100,Cum.ttoU.UI_DayMean[['Fecha']][k],Cum.ttoU.UI_DayMean[["Mean.U.UI.NOx"]][k]/100+Cum.ttoU.UI_DayMean[["SD.U.UI.NOx"]][k]/sqrt(3)/100, col='black', lwd=2)
  segments(Cum.ttoU.UI_DayMean[['Fecha']][k],Cum.ttoU.UI_DayMean[["Mean.U.UI.NOx"]][k]/100,Cum.ttoU.UI_DayMean[['Fecha']][k],Cum.ttoU.UI_DayMean[["Mean.U.UI.NOx"]][k]/100-Cum.ttoU.UI_DayMean[["SD.U.UI.NOx"]][k]/sqrt(3)/100, col='black', lwd=2)}
###########################################################################

##############################UREA+2I######################################

attach(chamb4_Gnal)
plot  ( Fecha,
        NOxCum/100,
        xlab="Year 2017",ylab="NOx_CUM[g*ha-1]", main = 'UREA+2I',
        cex.main=1, cex.lab=1,
        ty='p',col="burlywood4",pch=5,cex=1,lwd=2, axes=F,
        ylim =c(0,400)  )
attach(chamb7_Gnal)
# FluxNOx7[FluxNOx7>300]=0
points(Fecha,
       NOxCum/100,
       xlab='',ylab='', main='',
       ty='p',col="firebrick1",pch=5,cex=1, lwd=2,
       ylim =c(0,400) )
attach(chamb11_Gnal)
points(Fecha,
       NOxCum/100,
       xlab='',ylab='', main='',
       ty='p',col="firebrick4",pch=5,cex=1, lwd=2,
       ylim =c(0,400) )
attach(CumMatrizU.2I)
points(Fecha,
       Mean.U.2I.NOx/100,
       xlab='',ylab='', main='',
       ty='o',col="black",pch=18,cex=1,lty=1,lwd=3,
       ylim =c(0,400))
######
#TUNEAR PLOT
axis(2);axis(4); box()
grid(NA,NULL) 
#Esto es para poner las separaciones en ls ejes
axis.POSIXct(1,at=seq(DayMean_ch1[['Fecha']][1],DayMean_ch1[['Fecha']][59],by='week'),format='%d/%m' )

#Esto es para poner las líneas de grid en el eje

ejeTime=seq(DayMean_ch1[['Fecha']][1],DayMean_ch1[['Fecha']][59],by='week')
abline(v= as.numeric(ejeTime),col = "lightgray", lty = "dotted")
######
legend('topright',
       legend=c(Tto_U2I,'FluxMean_U+2I'), 
       pch=c(5,5,5,18),bty='n',
       lty=c(NA,NA,NA,1),lwd=3,
       col = c("burlywood4","firebrick1","firebrick4",'black'),cex=1)
for (k in 1:length(Cum.ttoU.2I_DayMean$SD.U.2I.NOx)) {
  segments(Cum.ttoU.2I_DayMean[['Fecha']][k],Cum.ttoU.2I_DayMean[["Mean.U.2I.NOx"]][k]/100,Cum.ttoU.2I_DayMean[['Fecha']][k],Cum.ttoU.2I_DayMean[["Mean.U.2I.NOx"]][k]/100+Cum.ttoU.2I_DayMean[["SD.U.2I.NOx"]][k]/sqrt(3)/100, col='black', lwd=2)
  segments(Cum.ttoU.2I_DayMean[['Fecha']][k],Cum.ttoU.2I_DayMean[["Mean.U.2I.NOx"]][k]/100,Cum.ttoU.2I_DayMean[['Fecha']][k],Cum.ttoU.2I_DayMean[["Mean.U.2I.NOx"]][k]/100-Cum.ttoU.2I_DayMean[["SD.U.2I.NOx"]][k]/sqrt(3)/100, col='black', lwd=2)}
###########################################################################
##############################CONTROL######################################

attach(chamb1_Gnal)
plot  ( Fecha,
        NOxCum/100,
        xlab="Year 2017",ylab="NOx_CUM [g*ha-1]", main = 'CONTROL',
        cex.main=1, cex.lab=1,
        ty='p',col="darkorchid1",pch='*',cex=1,lwd=2, axes=F,
        ylim =c(0,200)  )
attach(chamb8_Gnal)
points(Fecha,
       NOxCum/100,
       xlab='',ylab='', main='',
       ty='p',col="lightpink3",pch='*',cex=1, lwd=2,
       ylim =c(0,200) )
attach(chamb13_Gnal)
points(Fecha,
       NOxCum/100,
       xlab='',ylab='', main='',
       ty='p',col="lightpink4",pch='*',cex=1, lwd=2,
       ylim =c(MnCon,200) )
attach(CumMatrizControl)
points(Fecha,
       Mean.Control.NOx/100,
       xlab='',ylab='', main='',
       ty='o',col="black",pch='*',cex=2,lty=1,lwd=3,
       ylim =c(0,200))
######
#TUNEAR PLOT   

axis(2);axis(4); box()
grid(NA,NULL) 
#Esto es para poner las separaciones en ls ejes
axis.POSIXct(1,at=seq(DayMean_ch1[['Fecha']][1],DayMean_ch1[['Fecha']][59],by='week'),format='%d/%m' )

#Esto es para poner las líneas de grid en el eje

ejeTime=seq(DayMean_ch1[['Fecha']][1],DayMean_ch1[['Fecha']][59],by='week')
abline(v= as.numeric(ejeTime),col = "lightgray", lty = "dotted")
######
legend('topright',
       legend=c(Tto_C,'FluxMean_C'), 
       pch='*',bty='n',
       lty=c(NA,NA,NA,1),lwd=3,
       col = c("darkorchid1","lightpink1","lightpink4",'black'),cex=1)
for (k in 1:length(Cum.ttoControl_DayMean$SD.Control.NOx)) {
  segments(Cum.ttoControl_DayMean[['Fecha']][k],Cum.ttoControl_DayMean[["Mean.Control.NOx"]][k]/100,Cum.ttoControl_DayMean[['Fecha']][k],Cum.ttoControl_DayMean[["Mean.Control.NOx"]][k]/100+Cum.ttoControl_DayMean[["SD.Control.NOx"]][k]/sqrt(3)/100, col='black', lwd=2)
  segments(Cum.ttoControl_DayMean[['Fecha']][k],Cum.ttoControl_DayMean[["Mean.Control.NOx"]][k]/100,Cum.ttoControl_DayMean[['Fecha']][k],Cum.ttoControl_DayMean[["Mean.Control.NOx"]][k]/100-Cum.ttoControl_DayMean[["SD.Control.NOx"]][k]/sqrt(3)/100, col='black', lwd=2)}
###########################################################################

dev.off()


