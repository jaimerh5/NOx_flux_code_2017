#################################################
# JAIME RECIO HUETOS 
# ETSIA DE LA UPM; DEPARTAMENTO DE QUÍMICA Y TECNOLOGIA DE ALIMENTOS 
#13/09/2017
#############################################################
#Comparar Medias Diarias de Flujo
setwd("C:/R/Expmto2017/MAIZ/EurochemScripts/PLOTS/IndividualChambers_ABS")
pdf('CompararCum1.pdf')
attach(ttoU_Cum)   #ttoU->medias diarias

plot  (Fecha,
       Mean/100,
       xlab="Year 2017",ylab="NO cumulative [g ha-1]", main = '',
       cex.main=1, cex.lab=1,
       ty='o',col="black",pch=15,cex=1,lty=1,lwd=3, axes=F,
       ylim =c(0,1500))
attach(ttoUNI_Cum)
points(Fecha,
       Mean/100,
       xlab='',ylab='', main='',
       ty='o',col="forestgreen",pch=24,cex=1,lty=2,lwd=3,
       ylim =c(0,500) )
attach(ttoUUI_Cum)
points(Fecha,
       Mean/100,
       xlab='',ylab='', main='',
       ty='o',col="darkblue",pch=13,cex=1,lty=3,lwd=3,
       ylim =c(0,500) )
attach(ttoU2I_Cum)
points(Fecha,
       Mean/100,
       xlab='',ylab='', main='',
       ty='o',col="darkorange4",pch=4,cex=1,lty=1,lwd=3,
       ylim =c(0,500) )
attach(ttoCon_Cum)
points(Fecha,
       Mean/100,
       xlab='',ylab='', main='',
       ty='o',col="hotpink4",pch=20,cex=2,lty=1,lwd=3,
       ylim =c(0,500) )

######
#TUNEAR PLOT
axis(2, seq(0,1500,100));axis(4); box()
grid(NA,NULL) 
#Esto es para poner las separaciones en ls ejes
axis.POSIXct(1,at=seq(DayMean_ch1[['Fecha']][1],DayMean_ch1[['Fecha']][59],by='week'),format='%d-%b' )

#Esto es para poner las líneas de grid en el eje

ejeTime=seq(DayMean_ch1[['Fecha']][1],DayMean_ch1[['Fecha']][59],by='week')
abline(v= as.numeric(ejeTime),col = "lightgray", lty = "dotted")
# ############################################################
for (k in 1:length(ttoU_Cum$SD)) {
  segments(ttoU_Cum[['Fecha']][k],ttoU_Cum[["Mean"]][k]/100,ttoU_Cum[['Fecha']][k],ttoU_Cum[["Mean"]][k]/100+ttoU_Cum[["SD"]][k]/100/sqrt(7), col='black', lwd=2)
  segments(ttoU_Cum[['Fecha']][k],ttoU_Cum[["Mean"]][k]/100,ttoU_Cum[['Fecha']][k],ttoU_Cum[["Mean"]][k]/100-ttoU_Cum[["SD"]][k]/100/sqrt(7), col='black', lwd=2)}
for (k in 1:length(ttoUNI_Cum$SD)) {
  segments(ttoUNI_Cum[['Fecha']][k],ttoUNI_Cum[["Mean"]][k]/100,ttoUNI_Cum[['Fecha']][k],ttoUNI_Cum[["Mean"]][k]/100+ttoUNI_Cum[["SD"]][k]/100/sqrt(1), col='forestgreen', lwd=2)
  segments(ttoUNI_Cum[['Fecha']][k],ttoUNI_Cum[["Mean"]][k]/100,ttoUNI_Cum[['Fecha']][k],ttoUNI_Cum[["Mean"]][k]/100-ttoUNI_Cum[["SD"]][k]/100/sqrt(1), col='forestgreen', lwd=2)}
for (k in 1:length(ttoUUI_Cum$SD)) {
  segments(ttoUUI_Cum[['Fecha']][k],ttoUUI_Cum[["Mean"]][k]/100,ttoUUI_Cum[['Fecha']][k],ttoUUI_Cum[["Mean"]][k]/100+ttoUUI_Cum[["SD"]][k]/100/sqrt(3), col='darkblue', lwd=2)
  segments(ttoUUI_Cum[['Fecha']][k],ttoUUI_Cum[["Mean"]][k]/100,ttoUUI_Cum[['Fecha']][k],ttoUUI_Cum[["Mean"]][k]/100-ttoUUI_Cum[["SD"]][k]/100/sqrt(3), col='darkblue', lwd=2)}
for (k in 1:length(ttoU2I_Cum$SD)) {
  segments(ttoU2I_Cum[['Fecha']][k],ttoU2I_Cum[["Mean"]][k]/100,ttoU2I_Cum[['Fecha']][k],ttoU2I_Cum[["Mean"]][k]/100+ttoU2I_Cum[["SD"]][k]/100/sqrt(3), col='darkorange4', lwd=2)
  segments(ttoU2I_Cum[['Fecha']][k],ttoU2I_Cum[["Mean"]][k]/100,ttoU2I_Cum[['Fecha']][k],ttoU2I_Cum[["Mean"]][k]/100-ttoU2I_Cum[["SD"]][k]/100/sqrt(3), col='darkorange4', lwd=2)}
for (k in 1:length(ttoCon_Cum$SD)) {
  segments(ttoCon_Cum[['Fecha']][k],ttoCon_Cum[["Mean"]][k]/100,ttoCon_Cum[['Fecha']][k],ttoCon_Cum[["Mean"]][k]/100+ttoCon_Cum[["SD"]][k]/100/sqrt(1), col='hotpink4', lwd=2)
  segments(ttoCon_Cum[['Fecha']][k],ttoCon_Cum[["Mean"]][k]/100,ttoCon_Cum[['Fecha']][k],ttoCon_Cum[["Mean"]][k]/100-ttoCon_Cum[["SD"]][k]/100/sqrt(1), col='hotpink4', lwd=2)}
# ############################
legend('topleft',
       legend=c('CumMean_Urea','CumMean_U+NI','CumMean_U+UI','CumMean_U+2I','CumMean_Control'), 
       lty=c(1,2,3,1,1),pch=c(15,24,13,4,20),bty='n',
       lwd=3,
       col = c("black","forestgreen","darkblue",'darkorange4','hotpink4'))


dev.off()

# library(lattice);library(ggplot2)
# xyplot(ttoU$Mean~SoilT, ty=c('p','r'))
# xyplot(ttoU$Mean~WFPS, ty=c('p','r'))
# levelplot(ttoU$Mean~SoilT*WFPS)
# c2 <- lm(ttoU$Mean~SoilT);c3 <- lm(ttoU$Mean~WFPS)
# summary(c2);summary(c2)
# c4 <- lm(ttoU$Mean~SoilT*WFPS)