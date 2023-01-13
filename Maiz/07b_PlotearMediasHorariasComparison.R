#################################################
# JAIME RECIO HUETOS 
# ETSIA DE LA UPM; DEPARTAMENTO DE QUÍMICA Y TECNOLOGIA DE ALIMENTOS 
#13/09/2017
#############################################################
#Comparar Medias Diarias de Flujo
setwd("C:/R/Expmto2017/MAIZ/EurochemScripts/PLOTS/IndividualChambers_ABS")
# pdf('CompararHourlyMeanPerDay.pdf')
attach(MatrizUrea)   #ttoU->medias diarias

plot  (Fecha,
       Mean.Urea.NO,
       xlab="Year 2017",ylab="FLUX  [ug m-2 h-1]", main = 'Comparison of Treatments',
       cex.main=1, cex.lab=1,
       ty='o',col="black",pch=15,cex=1,lty=1,lwd=3, axes=F,
       ylim =c(0,1000))
attach(MatrizU.NI)
points(Fecha,
       Mean.U.NI.NO,
       xlab='',ylab='', main='',
       ty='o',col="forestgreen",pch=24,cex=1,lty=2,lwd=3,
       ylim =c(0,500) )
attach(MatrizU.UI)
points(Fecha,
       Mean.U.UI.NO,
       xlab='',ylab='', main='',
       ty='o',col="darkblue",pch=13,cex=1,lty=3,lwd=3,
       ylim =c(0,500) )
attach(MatrizU.2I)
points(Fecha,
       Mean.U.2I.NO,
       xlab='',ylab='', main='',
       ty='o',col="darkorange4",pch=4,cex=1,lty=1,lwd=3,
       ylim =c(0,500) )
attach(MatrizControl)
points(Fecha,
       Mean.Control.NO,
       xlab='',ylab='', main='',
       ty='o',col="hotpink4",pch=20,cex=2,lty=1,lwd=3,
       ylim =c(0,500) )

######
#TUNEAR PLOT
axis(2);axis(4); box()
grid(NA,NULL) 
#Esto es para poner las separaciones en ls ejes
axis.POSIXct(1,at=seq(DayMean_ch1[['Fecha']][1],DayMean_ch1[['Fecha']][59],by='week'),format='%d/%m' )

#Esto es para poner las líneas de grid en el eje

ejeTime=seq(DayMean_ch1[['Fecha']][1],DayMean_ch1[['Fecha']][59],by='week')
abline(v= as.numeric(ejeTime),col = "lightgray", lty = "dotted")
# # ############################################################
# for (k in 1:length(ttoU$SD)) {
#   segments(ttoU[['Fecha']][k],ttoU[["Mean"]][k],ttoU[['Fecha']][k],ttoU[["Mean"]][k]+ttoU[["SD"]][k]/sqrt(3), col='black', lwd=2)
#   segments(ttoU[['Fecha']][k],ttoU[["Mean"]][k],ttoU[['Fecha']][k],ttoU[["Mean"]][k]-ttoU[["SD"]][k]/sqrt(3), col='black', lwd=2)}
# for (k in 1:length(ttoUNI$SD)) {
#   segments(ttoUNI[['Fecha']][k],ttoUNI[["Mean"]][k],ttoUNI[['Fecha']][k],ttoUNI[["Mean"]][k]+ttoUNI[["SD"]][k]/sqrt(3), col='forestgreen', lwd=2)
#   segments(ttoUNI[['Fecha']][k],ttoUNI[["Mean"]][k],ttoUNI[['Fecha']][k],ttoUNI[["Mean"]][k]-ttoUNI[["SD"]][k]/sqrt(3), col='forestgreen', lwd=2)}
# for (k in 1:length(ttoUUI$SD)) {
#   segments(ttoUUI[['Fecha']][k],ttoUUI[["Mean"]][k],ttoUUI[['Fecha']][k],ttoUUI[["Mean"]][k]+ttoUUI[["SD"]][k]/sqrt(3), col='darkblue', lwd=2)
#   segments(ttoUUI[['Fecha']][k],ttoUUI[["Mean"]][k],ttoUUI[['Fecha']][k],ttoUUI[["Mean"]][k]-ttoUUI[["SD"]][k]/sqrt(3), col='darkblue', lwd=2)}
# for (k in 1:length(ttoU2I$SD)) {
#   segments(ttoU2I[['Fecha']][k],ttoU2I[["Mean"]][k],ttoU2I[['Fecha']][k],ttoU2I[["Mean"]][k]+ttoU2I[["SD"]][k]/sqrt(3), col='darkorange4', lwd=2)
#   segments(ttoU2I[['Fecha']][k],ttoU2I[["Mean"]][k],ttoU2I[['Fecha']][k],ttoU2I[["Mean"]][k]-ttoU2I[["SD"]][k]/sqrt(3), col='darkorange4', lwd=2)}
# for (k in 1:length(ttoCon$SD)) {
#   segments(ttoCon[['Fecha']][k],ttoCon[["Mean"]][k],ttoCon[['Fecha']][k],ttoCon[["Mean"]][k]+ttoCon[["SD"]][k]/sqrt(3), col='hotpink4', lwd=2)
#   segments(ttoCon[['Fecha']][k],ttoCon[["Mean"]][k],ttoCon[['Fecha']][k],ttoCon[["Mean"]][k]-ttoCon[["SD"]][k]/sqrt(3), col='hotpink4', lwd=2)}
# # ############################
legend('topright',
       legend=c('FluxMean_Urea','FluxMean_U+NI','FluxMean_U+UI','FluxMean_U+2I','FluxMean_C'), 
       lty=c(1,2,3,1,1),pch=c(15,24,13,4,20),bty='n',
       lwd=3,
       col = c("black","forestgreen","darkblue",'darkorange4','hotpink4'))


# dev.off()
