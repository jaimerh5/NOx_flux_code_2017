#################################################
# JAIME RECIO HUETOS 
# ETSIA DE LA UPM; DEPARTAMENTO DE QUÍMICA Y TECNOLOGIA DE ALIMENTOS 
#18/12/2017
#############################################################
#Comparar Medias Diarias con Horarias
setwd("C:/R/Expmto2017/MAIZ/EurochemScripts/PLOTS")
# pdf('ComparisonTtosHourlyVS_DayMean3.pdf')
attach(MatrizUrea)
plot  (Fecha[1:187],
       Mean.Urea.NO[1:187],
       xlab="Year 2017",ylab="FLUX NO [ug m-2 h-1]", main = '',
       cex.main=2, cex.lab=1,
       ty='p',col="red1",pch=15,cex=0.8,lty=1,lwd=3, axes=F,
       ylim =c(0,500))


attach(ttoU_DayMean)   #ttoU->medias diarias

points (Fecha[1:29],
       Mean.Urea.NO[1:29],
       xlab=" ",ylab=" ", main = '',
       cex.main=2, cex.lab=1,
       ty='o',col="red4",pch=15,cex=1,lty=1,lwd=3, 
       ylim =c(0,500))

attach(MatrizU.NI)
points  (Fecha[1:187],
       Mean.U.NI.NO[1:187],
       xlab='',ylab='', main='',
       cex.main=2, cex.lab=1,
       ty='p',col="tan3",pch=6,cex=0.8,lty=1,lwd=3, 
       ylim =c(0,500))

attach(ttoU.NI_DayMean)
points(Fecha[1:29],
       Mean.U.NI.NO[1:29],
       xlab='',ylab='', main='',
       ty='o',col="tan4",pch=6,cex=1.2,lty=1,lwd=3,
       ylim =c(0,500) )

attach(MatrizU.UI)
points  (Fecha[1:187],
         Mean.U.UI.NO[1:187],
         xlab='',ylab='', main='',
         cex.main=2, cex.lab=1,
         ty='p',col="turquoise3",pch=5,cex=0.8,lty=1,lwd=3, 
         ylim =c(0,500))
attach(ttoU.UI_DayMean)
points(Fecha[1:29],
       Mean.U.UI.NO[1:29],
       xlab='',ylab='', main='',
       ty='o',col="turquoise4",pch=18,cex=1,lty=1,lwd=3,
       ylim =c(0,500) )

attach(MatrizU.2I)
points  (Fecha[1:187],
         Mean.U.2I.NO[1:187],
         xlab='',ylab='', main='',
         cex.main=2, cex.lab=1,
         ty='p',col="lightpink3",pch=20,cex=0.8,lty=3,lwd=3, 
         ylim =c(0,500))

attach(ttoU.2I_DayMean)
points(Fecha[1:29],
       Mean.U.2I.NO[1:29],
       xlab='',ylab='', main='',
       ty='o',col="lightpink4",pch=20,cex=1,lty=1,lwd=3,
       ylim =c(0,500) )

attach(MatrizControl)
points  (Fecha[1:187],
         Mean.Control.NO[1:187],
         xlab='',ylab='', main='',
         cex.main=2, cex.lab=1,
         ty='p',col="yellow4",pch=4,cex=0.8,lty=2,lwd=1, 
         ylim =c(0,500))

attach(ttoControl_DayMean)
points(Fecha[1:29],
       Mean.Control.NO[1:29],
       xlab='',ylab='', main='',
       ty='o',col="yellow",pch=4,cex=0.8,lty=2,lwd=2,
       ylim =c(0,500) )

######
#TUNEAR PLOT
axis(2);axis(4); box()
grid(NA,NULL) 
#Esto es para poner las separaciones en ls ejes
axis.POSIXct(1,at=seq(DayMean_ch1[['Fecha']][1],DayMean_ch1[['Fecha']][59],by='week'),format='%d/%m' )
# ############################################################
for (k in 1:length(ttoU_DayMean$SD.Urea.NO)) {
  segments(ttoU_DayMean[['Fecha']][k],ttoU_DayMean[['Mean.Urea.NO']][k],ttoU_DayMean[['Fecha']][k],ttoU_DayMean[['Mean.Urea.NO']][k]+ttoU_DayMean[["SD.Urea.NO"]][k]/sqrt(24), col='red4', lwd=2)
  segments(ttoU_DayMean[['Fecha']][k],ttoU_DayMean[['Mean.Urea.NO']][k],ttoU_DayMean[['Fecha']][k],ttoU_DayMean[['Mean.Urea.NO']][k]-ttoU_DayMean[["SD.Urea.NO"]][k]/sqrt(24), col='red4', lwd=2)}
for (k in 1:length(ttoU.NI_DayMean$SD.U.NI.NO)) {
  segments(ttoU.NI_DayMean[['Fecha']][k],ttoU.NI_DayMean[['Mean.U.NI.NO']][k],ttoU.NI_DayMean[['Fecha']][k],ttoU.NI_DayMean[['Mean.U.NI.NO']][k]+ttoU.NI_DayMean[["SD.U.NI.NO"]][k]/sqrt(8), col='tan4', lwd=2)
  segments(ttoU.NI_DayMean[['Fecha']][k],ttoU.NI_DayMean[['Mean.U.NI.NO']][k],ttoU.NI_DayMean[['Fecha']][k],ttoU.NI_DayMean[['Mean.U.NI.NO']][k]-ttoU.NI_DayMean[["SD.U.NI.NO"]][k]/sqrt(8), col='tan4', lwd=2)}
for (k in 1:length(ttoU.UI_DayMean$SD.U.UI.NO)) {
  segments(ttoU.UI_DayMean[['Fecha']][k],ttoU.UI_DayMean[['Mean.U.UI.NO']][k],ttoU.UI_DayMean[['Fecha']][k],ttoU.UI_DayMean[['Mean.U.UI.NO']][k]+ttoU.UI_DayMean[["SD.U.UI.NO"]][k]/sqrt(8), col='turquoise4', lwd=2)
  segments(ttoU.UI_DayMean[['Fecha']][k],ttoU.UI_DayMean[['Mean.U.UI.NO']][k],ttoU.UI_DayMean[['Fecha']][k],ttoU.UI_DayMean[['Mean.U.UI.NO']][k]-ttoU.UI_DayMean[["SD.U.UI.NO"]][k]/sqrt(8), col='turquoise4', lwd=2)}
for (k in 1:length(ttoU.2I_DayMean$SD.U.2I.NO)) {
  segments(ttoU.2I_DayMean[['Fecha']][k],ttoU.2I_DayMean[["Mean.U.2I.NO"]][k],ttoU.2I_DayMean[['Fecha']][k],ttoU.2I_DayMean[["Mean.U.2I.NO"]][k]+ttoU.2I_DayMean[["SD.U.2I.NO"]][k]/sqrt(8), col='lightpink4', lwd=2)
  segments(ttoU.2I_DayMean[['Fecha']][k],ttoU.2I_DayMean[["Mean.U.2I.NO"]][k],ttoU.2I_DayMean[['Fecha']][k],ttoU.2I_DayMean[["Mean.U.2I.NO"]][k]-ttoU.2I_DayMean[["SD.U.2I.NO"]][k]/sqrt(8), col='lightpink4', lwd=2)}
for (k in 1:length(ttoControl_DayMean$SD.Control.NO)) {
  segments(ttoControl_DayMean[['Fecha']][k],ttoControl_DayMean[['Mean.Control.NO']][k],ttoControl_DayMean[['Fecha']][k],ttoControl_DayMean[['Mean.Control.NO']][k]+ttoControl_DayMean[["SD.Control.NO"]][k]/sqrt(8), col='yellow', lwd=2)
  segments(ttoControl_DayMean[['Fecha']][k],ttoControl_DayMean[['Mean.Control.NO']][k],ttoControl_DayMean[['Fecha']][k],ttoControl_DayMean[['Mean.Control.NO']][k]-ttoControl_DayMean[["SD.Control.NO"]][k]/sqrt(8), col='yellow', lwd=2)}
# ############################
ejeTime=seq(DayMean_ch1[['Fecha']][1],DayMean_ch1[['Fecha']][59],by='week')
#Esto es para poner las líneas de grid en el eje
arrows(x0 = ejeTime[1],y0 = 450,x1 = ejeTime[1],y1 = 400,col = 'black',lwd = 3)
text(x =ejeTime[1],y = 475,labels = 'Fertilizaton',cex=0.5)

abline(v= as.numeric(ejeTime),col = "lightgray", lty = "dotted")
# ############################################################
leyend <- c('HourlyMeanUrea','DailyMeanUrea','HourlyMeanU+NI','DailyMeanU+NI',
            'HourlyMeanU+UI','DailyMeanU+UI','HourlyMeanU+2I','DailyMeanU+2I','HourlyMeanControl','DailyMeanControl')
PtoTyp <- c(15,15,6,6,5,18,20,20,4,4)
LineTyp <- c(NA,1,NA,1,NA,1,NA,1,NA,2) 
colores <- c('red1','red4','tan3','tan4','turquoise3','turquoise4',
             'lightpink3','lightpink4','yellow4','yellow')
legend('topright',
       legend=leyend , 
       pch=PtoTyp,bty='y',
       lty=LineTyp,lwd=3,
       col = colores,cex=0.5)

# dev.off()
