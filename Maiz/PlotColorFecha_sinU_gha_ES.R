#################################################
# JAIME RECIO HUETOS 
# ETSIA DE LA UPM; DEPARTAMENTO DE QUÍMICA Y TECNOLOGIA DE ALIMENTOS 
#18/12/2017
#############################################################

library(RColorBrewer)
#Comparar Medias Diarias con Horarias
setwd("C:/R/Expmto2017/MAIZ/EurochemScripts/PLOTS/DefinitivosPlots")
# pdf('PlotsDef_Lupa_gha_ES.pdf')
attach(MatrizU.NI)
plot  (Fecha[1:187],
       Mean.U.NI.NO[1:187]/100,
       xlab="2017",ylab="Flujo NO [g ha-1 h-1]", main = '',
       cex.main=2, cex.lab=1,
       ty='p',col="#d9ef8b",pch=15,cex=1,lty=1,lwd=2, axes=F,
       ylim =c(-0.50,2.50))


attach(ttoU.NI_DayMean)
points(Fecha[1:29],
       Mean.U.NI.NO[1:29]/100,
       ty='o',col="#66bd63",
       pch=0,cex=1.5,lty=1,lwd=2)

attach(MatrizU.UI)
points  (Fecha[1:187],
         Mean.U.UI.NO[1:187]/100,
         ty='p',col="#fdae61",
         pch=18,cex=1.5,lty=1,lwd=2)

attach(ttoU.UI_DayMean)
points(Fecha[1:29],
       Mean.U.UI.NO[1:29]/100,
       ty='o',col="#d73027",
       pch=5,cex=1,lty=1,lwd=2)

attach(MatrizU.2I)
points  (Fecha[1:187],
         Mean.U.2I.NO[1:187]/100,
         ty='p',col="#1d91c0",
         pch=17,cex=1,lty=5,lwd=2)

attach(ttoU.2I_DayMean)
points(Fecha[1:29],
       Mean.U.2I.NO[1:29]/100,
       ty='o',col="#3288bd",
       pch=2,cex=1,lty=5,lwd=2)

attach(MatrizControl)
points  (Fecha[1:187],
         Mean.Control.NO[1:187]/100,
         ty='p',col="#b2abd2",
         pch=4,cex=1,lty=4,lwd=2)

attach(ttoControl_DayMean)
points(Fecha[1:29],
       Mean.Control.NO[1:29]/100,
       xlab='',ylab='', main='',
       ty='o',col="#8073ac",
       pch=8,cex=1,lty=4,lwd=2)

######
#TUNEAR PLOT
axis(2); box()
# axis(4);
grid(NA,NULL) 
#Esto es para poner las separaciones en ls ejes
axis.POSIXct(1,at=seq(DayMean_ch1[['Fecha']][1],DayMean_ch1[['Fecha']][59],by='week'),format='%d/%m' )
# ############################################################

for (k in 1:length(ttoU.NI_DayMean$SD.U.NI.NO)) {
  segments(ttoU.NI_DayMean[['Fecha']][k],ttoU.NI_DayMean[['Mean.U.NI.NO']][k]/100,ttoU.NI_DayMean[['Fecha']][k],ttoU.NI_DayMean[['Mean.U.NI.NO']][k]/100+ttoU.NI_DayMean[["SD.U.NI.NO"]][k]/100/sqrt(8), col='#66bd63',lty=1, lwd=2)
  segments(ttoU.NI_DayMean[['Fecha']][k],ttoU.NI_DayMean[['Mean.U.NI.NO']][k]/100,ttoU.NI_DayMean[['Fecha']][k],ttoU.NI_DayMean[['Mean.U.NI.NO']][k]/100-ttoU.NI_DayMean[["SD.U.NI.NO"]][k]/100/sqrt(8), col='#66bd63',lty=1, lwd=2)}
for (k in 1:length(ttoU.UI_DayMean$SD.U.UI.NO)) {
  segments(ttoU.UI_DayMean[['Fecha']][k],ttoU.UI_DayMean[['Mean.U.UI.NO']][k]/100,ttoU.UI_DayMean[['Fecha']][k],ttoU.UI_DayMean[['Mean.U.UI.NO']][k]/100+ttoU.UI_DayMean[["SD.U.UI.NO"]][k]/100/sqrt(8), col='#d73027', lwd=2)
  segments(ttoU.UI_DayMean[['Fecha']][k],ttoU.UI_DayMean[['Mean.U.UI.NO']][k]/100,ttoU.UI_DayMean[['Fecha']][k],ttoU.UI_DayMean[['Mean.U.UI.NO']][k]/100-ttoU.UI_DayMean[["SD.U.UI.NO"]][k]/100/sqrt(8), col='#d73027', lwd=2)}
for (k in 1:length(ttoU.2I_DayMean$SD.U.2I.NO)) {
  segments(ttoU.2I_DayMean[['Fecha']][k],ttoU.2I_DayMean[["Mean.U.2I.NO"]][k]/100,ttoU.2I_DayMean[['Fecha']][k],ttoU.2I_DayMean[["Mean.U.2I.NO"]][k]/100+ttoU.2I_DayMean[["SD.U.2I.NO"]][k]/100/sqrt(8), col='#3288bd',lty= 5, lwd=2)
  segments(ttoU.2I_DayMean[['Fecha']][k],ttoU.2I_DayMean[["Mean.U.2I.NO"]][k]/100,ttoU.2I_DayMean[['Fecha']][k],ttoU.2I_DayMean[["Mean.U.2I.NO"]][k]/100-ttoU.2I_DayMean[["SD.U.2I.NO"]][k]/100/sqrt(8), col='#3288bd',lty= 5, lwd=2)}
for (k in 1:length(ttoControl_DayMean$SD.Control.NO)) {
  segments(ttoControl_DayMean[['Fecha']][k],ttoControl_DayMean[['Mean.Control.NO']][k]/100,ttoControl_DayMean[['Fecha']][k],ttoControl_DayMean[['Mean.Control.NO']][k]/100+ttoControl_DayMean[["SD.Control.NO"]][k]/100/sqrt(8), col='#8073ac',lty= 4, lwd=2)
  segments(ttoControl_DayMean[['Fecha']][k],ttoControl_DayMean[['Mean.Control.NO']][k]/100,ttoControl_DayMean[['Fecha']][k],ttoControl_DayMean[['Mean.Control.NO']][k]/100-ttoControl_DayMean[["SD.Control.NO"]][k]/100/sqrt(8), col='#8073ac',lty= 4, lwd=2)}
# ############################
ejeTime=seq(DayMean_ch1[['Fecha']][1],DayMean_ch1[['Fecha']][59],by='week')
#Esto es para poner las líneas de grid en el eje
arrows(x0 = ejeTime[1],y0 = 1.8,x1 = ejeTime[1],y1 = 1.3,col = 'black',lwd = 2)
# text(x =ejeTime[1],y = 1.85,labels = 'Fertilizaton',cex=0.5)

abline(v= as.numeric(ejeTime),col = "lightgray", lty = "dotted")
# ############################################################
leyend <- c('MediasHorarias_U+NI','MediasDiarias_U+NI',
            'MediasHorarias_U+UI','MediasDiarias_U+UI','MediasHorarias_U+2I','MediasDiarias_U+2I','MediasHorarias_C','MediasDiarias_C')
PtoTyp <- c(15,0,18,5,17,2,4,8) 
LineTyp <- c(NA,1,NA,1,NA,5,NA,4 )
colores <- c('#d9ef8b','#66bd63','#fdae61','#d73027','#1d91c0','#3288bd','#b2abd2','#8073ac')
cex.pch <-  c(1,1.5,1,1.5,1,1.,1,1)
legend('topright',
       legend=leyend , 
       pch=PtoTyp,bty='y',
       lty=LineTyp,lwd=2,
       col = colores,pt.cex=cex.pch)

######################################################################################
##-----------------------------------------------------------------------------
########################################################################################

attach(MatrizU.NI)
plot  (Fecha,
       Mean.U.NI.NO/100,
       xlab="2017",ylab="Flujo NO [g ha-1 h-1]", main = '',
       cex.main=2, cex.lab=1, axes=F,
       ty='p',col="#d9ef8b",
       pch=15,cex=1,lty=1,lwd=2,
       ylim =c(-0.50,2.50))

attach(ttoU.NI_DayMean)
points(Fecha,
       Mean.U.NI.NO/100,
       ty='o',col="#66bd63",
       pch=0,cex=1.5,lty=1,lwd=2)

attach(MatrizU.UI)
points  (Fecha,
         Mean.U.UI.NO/100,
         ty='p',col="#fdae61",
         pch=18,cex=1.5,lty=1,lwd=2)

attach(ttoU.UI_DayMean)
points(Fecha,
       Mean.U.UI.NO/100,
       ty='o',col="#d73027",
       pch=5,cex=1,lty=1,lwd=2)

attach(MatrizU.2I)
points  (Fecha,
         Mean.U.2I.NO/100,
         ty='p',col="#1d91c0",
         pch=17,cex=1,lty=5,lwd=2)

attach(ttoU.2I_DayMean)
points(Fecha,
       Mean.U.2I.NO/100,
       ty='o',col="#3288bd",
       pch=2,cex=1,lty=5,lwd=2)

attach(MatrizControl)
points  (Fecha,
         Mean.Control.NO/100,
         ty='p',col="#b2abd2",
         pch=4,cex=1,lty=4,lwd=2)

attach(ttoControl_DayMean)
points(Fecha,
       Mean.Control.NO/100,
       xlab='',ylab='', main='',
       ty='o',col="#8073ac",
       pch=8,cex=1,lty=4,lwd=2)

######
#TUNEAR PLOT
axis(2); box()
# axis(4);
grid(NA,NULL) 
#Esto es para poner las separaciones en ls ejes
axis.POSIXct(1,at=seq(DayMean_ch1[['Fecha']][1],DayMean_ch1[['Fecha']][59],by='week'),format='%d/%m' )
# ############################################################
for (k in 1:length(ttoU.NI_DayMean$SD.U.NI.NO)) {
  segments(ttoU.NI_DayMean[['Fecha']][k],ttoU.NI_DayMean[['Mean.U.NI.NO']][k]/100,ttoU.NI_DayMean[['Fecha']][k],ttoU.NI_DayMean[['Mean.U.NI.NO']][k]/100+ttoU.NI_DayMean[["SD.U.NI.NO"]][k]/100/sqrt(8), col='#66bd63',lty=1, lwd=2)
  segments(ttoU.NI_DayMean[['Fecha']][k],ttoU.NI_DayMean[['Mean.U.NI.NO']][k]/100,ttoU.NI_DayMean[['Fecha']][k],ttoU.NI_DayMean[['Mean.U.NI.NO']][k]/100-ttoU.NI_DayMean[["SD.U.NI.NO"]][k]/100/sqrt(8), col='#66bd63',lty=1, lwd=2)}
for (k in 1:length(ttoU.UI_DayMean$SD.U.UI.NO)) {
  segments(ttoU.UI_DayMean[['Fecha']][k],ttoU.UI_DayMean[['Mean.U.UI.NO']][k]/100,ttoU.UI_DayMean[['Fecha']][k],ttoU.UI_DayMean[['Mean.U.UI.NO']][k]/100+ttoU.UI_DayMean[["SD.U.UI.NO"]][k]/100/sqrt(8), col='#d73027', lwd=2)
  segments(ttoU.UI_DayMean[['Fecha']][k],ttoU.UI_DayMean[['Mean.U.UI.NO']][k]/100,ttoU.UI_DayMean[['Fecha']][k],ttoU.UI_DayMean[['Mean.U.UI.NO']][k]/100-ttoU.UI_DayMean[["SD.U.UI.NO"]][k]/100/sqrt(8), col='#d73027', lwd=2)}
for (k in 1:length(ttoU.2I_DayMean$SD.U.2I.NO)) {
  segments(ttoU.2I_DayMean[['Fecha']][k],ttoU.2I_DayMean[["Mean.U.2I.NO"]][k]/100,ttoU.2I_DayMean[['Fecha']][k],ttoU.2I_DayMean[["Mean.U.2I.NO"]][k]/100+ttoU.2I_DayMean[["SD.U.2I.NO"]][k]/100/sqrt(8), col='#3288bd',lty= 5, lwd=2)
  segments(ttoU.2I_DayMean[['Fecha']][k],ttoU.2I_DayMean[["Mean.U.2I.NO"]][k]/100,ttoU.2I_DayMean[['Fecha']][k],ttoU.2I_DayMean[["Mean.U.2I.NO"]][k]/100-ttoU.2I_DayMean[["SD.U.2I.NO"]][k]/100/sqrt(8), col='#3288bd',lty= 5, lwd=2)}
for (k in 1:length(ttoControl_DayMean$SD.Control.NO)) {
  segments(ttoControl_DayMean[['Fecha']][k],ttoControl_DayMean[['Mean.Control.NO']][k]/100,ttoControl_DayMean[['Fecha']][k],ttoControl_DayMean[['Mean.Control.NO']][k]/100+ttoControl_DayMean[["SD.Control.NO"]][k]/100/sqrt(8), col='#8073ac',lty= 4, lwd=2)
  segments(ttoControl_DayMean[['Fecha']][k],ttoControl_DayMean[['Mean.Control.NO']][k]/100,ttoControl_DayMean[['Fecha']][k],ttoControl_DayMean[['Mean.Control.NO']][k]/100-ttoControl_DayMean[["SD.Control.NO"]][k]/100/sqrt(8), col='#8073ac',lty= 4, lwd=2)}
# ############################
ejeTime=seq(DayMean_ch1[['Fecha']][1],DayMean_ch1[['Fecha']][59],by='week')
#Esto es para poner las líneas de grid en el eje
arrows(x0 = ejeTime[1],y0 = 1.8,x1 = ejeTime[1],y1 = 1.3,col = 'black',lwd = 2)
# text(x =ejeTime[1],y = 1.85,labels = 'Fertilizaton',cex=0.5)

abline(v= as.numeric(ejeTime),col = "lightgray", lty = "dotted")
# ############################################################
leyend <- c('MediasHorarias_U+NI','MediasDiarias_U+NI',
            'MediasHorarias_U+UI','MediasDiarias_U+UI','MediasHorarias_U+2I','MediasDiarias_U+2I','MediasHorarias_C','MediasDiarias_C')
PtoTyp <- c(15,0,18,5,17,2,4,8) 
LineTyp <- c(NA,1,NA,1,NA,5,NA,4 )
colores <- c('#d9ef8b','#66bd63','#fdae61','#d73027','#1d91c0','#3288bd','#b2abd2','#8073ac')
cex.pch <-  c(1,1.5,1,1.5,1,1.,1,1)
legend('topright',
       legend=leyend , 
       pch=PtoTyp,bty='y',
       lty=LineTyp,lwd=2,
       col = colores,pt.cex=cex.pch)

# dev.off()
