#################################################
# JAIME RECIO HUETOS 
# ETSIA DE LA UPM; DEPARTAMENTO DE QUÍMICA Y TECNOLOGIA DE ALIMENTOS 
#13/09/2017
#############################################################

#Plotear flujos
# un grafico de los datos Horarios con la media definitiva
 setwd("C:/R/Expmto2017/MAIZ/EurochemScripts/PLOTS/IndividualChambers_ABS")
 # pdf('ReplicasTratamiento_medias2_ABS.pdf')
###############UREA######################################
attach(chamb5_Gnal)
plot  ( Fecha,
        FluxNOx5,
        xlab="Year 2017",ylab="Flux NOx [ug*m-2*h-1]", main = 'UREA',
        cex.main=1, cex.lab=1,
        ty='p',col="plum",cex=1,lwd=2, axes=F,
        ylim =c(MnU,1000)  )
attach(chamb9_Gnal)
points(Fecha,
       FluxNOx9,
       xlab='',ylab='', main='',
       ty='p',col="maroon1",cex=1,lwd=2, 
       ylim =c(MnU,1000))
attach(chamb12_Gnal)
points(Fecha,
       FluxNOx12,
       xlab='',ylab='', main='',
       ty='p',col="wheat3",cex=1,lwd=2, 
       ylim =c(MnU,1000))
attach(ttoU)
points(Fecha,
       Mean,
       xlab='',ylab='', main='',
       ty='o',col="black",pch=15,cex=1,lwd=3, 
       ylim =c(MnU,1000))
# attach(MatrizUrea)
# points(Fecha,
#        Mean.Urea.Flux,
#        xlab='',ylab='', main='',
#        ty='l',col="orange",pch=15,cex=1,lwd=3, 
#        ylim =c(MnU,1000))

WeekMeanUrea <-aggregate(MatrizUrea,
                     by = list('WOY'= format(MatrizUrea$Fecha,'%Y-%W' )),
                     FUN = mean, na.rm=T )

# attach(WeekMeanUrea)   
# points(Fecha,
#        Mean.Urea.Flux,
#        xlab='',ylab='', main='',
#        ty='l',col="orange",pch=15,cex=1,lwd=3, 
#        ylim =c(MnU,1000))
######
#TUNEAR PLOT
axis(2);axis(4); box()
grid(NA,NULL) 
#Esto es para poner las separaciones en ls ejes
axis.POSIXct(1,at=seq(DayMean_ch1[['Fecha']][1],DayMean_ch1[['Fecha']][59],by='week'),format='%d-%m' )

#Esto es para poner las líneas de grid en el eje

ejeTime=seq(DayMean_ch1[['Fecha']][1],DayMean_ch1[['Fecha']][59],by='week')
abline(v= as.numeric(ejeTime),col = "lightgray", lty = "dotted")
######
legend('topright',
       legend=c(Tto_U,'FluxMean_UREA'), 
       pch=c(1),bty='n',
       lty=c(NA,NA,NA,1),lwd=3,
       col = c("plum","maroon1","wheat3",'black'),cex=1)
for (k in 1:length(ttoU$SD)) {
  segments(ttoU[['Fecha']][k],ttoU[["Mean"]][k],ttoU[['Fecha']][k],ttoU[["Mean"]][k]+ttoU[["SD"]][k]/sqrt(3), col='black', lwd=2)
  segments(ttoU[['Fecha']][k],ttoU[["Mean"]][k],ttoU[['Fecha']][k],ttoU[["Mean"]][k]-ttoU[["SD"]][k]/sqrt(3), col='black', lwd=2)}
###########################################################################
##############################UREA+DMPSA######################################

attach(chamb3_Gnal)
plot  ( Fecha,
        FluxNOx3,
        xlab="Year 2017",ylab="Flux NOx[ug*m-2*h-1]", main = 'UREA+DMPSA',
        cex.main=1, cex.lab=1,
        ty='p',col="chartreuse1",pch=20,cex=1,lwd=2, axes=F,
        ylim =c(MnUNI,150)  )
attach(chamb10_Gnal)
points(Fecha,
       FluxNOx10,
       xlab='',ylab='', main='',
       ty='p',col="aquamarine2",pch=20,cex=1,lwd=2,  
       ylim =c(MnUNI,200) )
attach(chamb14_Gnal)
points(Fecha,
       FluxNOx14,
       xlab='',ylab='', main='',
       ty='p',col="chartreuse4",pch=20,cex=1, lwd=2,
       ylim =c(MnUNI,200) )
attach(ttoUNI)
points(Fecha,
       Mean,
       xlab='',ylab='', main='',
       ty='o',col="black",pch=24,cex=1,lty=1,lwd=3,
       ylim =c(MnUNI,150) )
######
#TUNEAR PLOT
axis(2);axis(4); box()
grid(NA,NULL) 
#Esto es para poner las separaciones en ls ejes
axis.POSIXct(1,at=seq(DayMean_ch1[['Fecha']][1],DayMean_ch1[['Fecha']][59],by='week'),format='%d-%m' )

#Esto es para poner las líneas de grid en el eje

ejeTime=seq(DayMean_ch1[['Fecha']][1],DayMean_ch1[['Fecha']][59],by='week')
abline(v= as.numeric(ejeTime),col = "lightgray", lty = "dotted")
######
legend('topright',
       legend=c(Tto_UNI,'FluxMean_U+NI'), 
       pch=c(20,20,20,24),bty='n',
       lty=c(NA,NA,NA,1),lwd=3,
       col = c("chartreuse1","aquamarine2","chartreuse4",'black'),cex=1)
for (k in 1:length(ttoUNI$SD)) {
  segments(ttoUNI[['Fecha']][k],ttoUNI[["Mean"]][k],ttoUNI[['Fecha']][k],ttoUNI[["Mean"]][k]+ttoUNI[["SD"]][k]/sqrt(3), col='black', lwd=2)
  segments(ttoUNI[['Fecha']][k],ttoUNI[["Mean"]][k],ttoUNI[['Fecha']][k],ttoUNI[["Mean"]][k]-ttoUNI[["SD"]][k]/sqrt(3), col='black', lwd=2)}
###########################################################################

##############################UREA+NBPT######################################

attach(chamb2_Gnal)
plot  ( Fecha,
        FluxNOx2,
        xlab="Year 2017",ylab="Flux NOx[ug*m-2*h-1]", main = 'UREA+NBPT',
        cex.main=1, cex.lab=1,
        ty='p',col="blue1",pch=4,cex=1,lwd=2, axes=F,
        ylim =c(-50,400)  )
attach(chamb6_Gnal)
points(Fecha,
       FluxNOx6,
       xlab='',ylab='', main='',
       ty='p',col="blue4",pch=4,cex=1,  lwd=2,
       ylim =c(MnUUI,600) )
attach(chamb15_Gnal)
points(Fecha,
       FluxNOx15,
       xlab='',ylab='', main='',
       ty='p',col="cyan2",pch=4,cex=1, lwd=2, 
       ylim =c(MnUUI,600) )
attach(ttoUUI)
points(Fecha,
       Mean,
       xlab='',ylab='', main='',
       ty='o',col="black",pch=13,cex=1,lty=1,lwd=3,
       ylim =c(-50,400))
######
#TUNEAR PLOT
axis(2,seq(-200,600,50));axis(4); box()
grid(NA,NULL) 
#Esto es para poner las separaciones en ls ejes
axis.POSIXct(1,at=seq(DayMean_ch1[['Fecha']][1],DayMean_ch1[['Fecha']][59],by='week'),format='%d-%m' )

#Esto es para poner las líneas de grid en el eje

ejeTime=seq(DayMean_ch1[['Fecha']][1],DayMean_ch1[['Fecha']][59],by='week')
abline(v= as.numeric(ejeTime),col = "lightgray", lty = "dotted")
######
legend('topright',
       legend=c(Tto_UUI,'FluxMean_U+UI'), 
       pch=c(4,4,4,13),bty='n',
       lty=c(NA,NA,NA,1),lwd=3,
       col = c("blue1","blue4","cyan2",'black'),cex=1)
for (k in 1:length(ttoUUI$SD)) {
  segments(ttoUUI[['Fecha']][k],ttoUUI[["Mean"]][k],ttoUUI[['Fecha']][k],ttoUUI[["Mean"]][k]+ttoUUI[["SD"]][k]/sqrt(3), col='black', lwd=2)
  segments(ttoUUI[['Fecha']][k],ttoUUI[["Mean"]][k],ttoUUI[['Fecha']][k],ttoUUI[["Mean"]][k]-ttoUUI[["SD"]][k]/sqrt(3), col='black', lwd=2)}
###########################################################################

##############################UREA+2I######################################

attach(chamb4_Gnal)
plot  ( Fecha,
        FluxNOx4,
        xlab="Year 2017",ylab="Flux NOx[ug*m-2*h-1]", main = 'UREA+2I',
        cex.main=1, cex.lab=1,
        ty='p',col="burlywood4",pch=5,cex=1,lwd=2, axes=F,
        ylim =c(MnU2I,300)  )
attach(chamb7_Gnal)
FluxNOx7[FluxNOx7>300]=0
points(Fecha,
       FluxNOx7,
       xlab='',ylab='', main='',
       ty='p',col="firebrick1",pch=5,cex=1, lwd=2,
       ylim =c(MnU2I,400) )
attach(chamb11_Gnal)
points(Fecha,
       FluxNOx11,
       xlab='',ylab='', main='',
       ty='p',col="firebrick4",pch=5,cex=1, lwd=2,
       ylim =c(MnU2I,400) )
attach(ttoU2I)
points(Fecha,
       Mean,
       xlab='',ylab='', main='',
       ty='o',col="black",pch=18,cex=1,lty=1,lwd=3,
       ylim =c(MnU2I,400))
######
#TUNEAR PLOT
axis(2);axis(4); box()
grid(NA,NULL) 
#Esto es para poner las separaciones en ls ejes
axis.POSIXct(1,at=seq(DayMean_ch1[['Fecha']][1],DayMean_ch1[['Fecha']][59],by='week'),format='%d-%m' )

#Esto es para poner las líneas de grid en el eje

ejeTime=seq(DayMean_ch1[['Fecha']][1],DayMean_ch1[['Fecha']][59],by='week')
abline(v= as.numeric(ejeTime),col = "lightgray", lty = "dotted")
abline(h= seq(-100,400,10),col = "lightgray", lty = "dotted")
######
legend('topright',
       legend=c(Tto_U2I,'FluxMean_U+2I'), 
       pch=c(5,5,5,18),bty='n',
       lty=c(NA,NA,NA,1),lwd=3,
       col = c("burlywood4","firebrick1","firebrick4",'black'),cex=1)
for (k in 1:length(ttoU2I$SD)) {
  segments(ttoU2I[['Fecha']][k],ttoU2I[["Mean"]][k],ttoU2I[['Fecha']][k],ttoU2I[["Mean"]][k]+ttoU2I[["SD"]][k]/sqrt(3), col='black', lwd=2)
  segments(ttoU2I[['Fecha']][k],ttoU2I[["Mean"]][k],ttoU2I[['Fecha']][k],ttoU2I[["Mean"]][k]-ttoU2I[["SD"]][k]/sqrt(3), col='black', lwd=2)}
###########################################################################
##############################CONTROL######################################

attach(chamb1_Gnal)
plot  ( Fecha,
        FluxNOx1,
        xlab="Year 2017",ylab="Flux NOx[ug*m-2*h-1]", main = 'CONTROL',
        cex.main=1, cex.lab=1,
        ty='p',col="darkorchid1",pch='*',cex=1,lwd=2, axes=F,
        ylim =c(-75,75)  )
attach(chamb8_Gnal)
points(Fecha,
       FluxNOx8,
       xlab='',ylab='', main='',
       ty='p',col="lightpink3",pch='*',cex=1, lwd=2,
       ylim =c(MnCon,200) )
attach(chamb13_Gnal)
points(Fecha,
       FluxNOx13,
       xlab='',ylab='', main='',
       ty='p',col="lightpink4",pch='*',cex=1, lwd=2,
       ylim =c(MnCon,200) )
attach(ttoCon)
points(Fecha,
       Mean,
       xlab='',ylab='', main='',
       ty='o',col="black",pch='*',cex=2,lty=1,lwd=3,
       ylim =c(MnCon,200))
######
#TUNEAR PLOT   

axis(2);axis(4); box()
grid(NA,NULL) 
#Esto es para poner las separaciones en ls ejes
axis.POSIXct(1,at=seq(DayMean_ch1[['Fecha']][1],DayMean_ch1[['Fecha']][59],by='week'),format='%d-%m' )

#Esto es para poner las líneas de grid en el eje

ejeTime=seq(DayMean_ch1[['Fecha']][1],DayMean_ch1[['Fecha']][59],by='week')
abline(v= as.numeric(ejeTime),col = "lightgray", lty = "dotted")
######
legend('topright',
       legend=c(Tto_C,'FluxMean_C'), 
       pch='*',bty='n',
       lty=c(NA,NA,NA,1),lwd=3,
       col = c("darkorchid1","lightpink1","lightpink4",'black'),cex=1)
for (k in 1:length(ttoCon$SD)) {
  segments(ttoCon[['Fecha']][k],ttoCon[["Mean"]][k],ttoCon[['Fecha']][k],ttoCon[["Mean"]][k]+ttoCon[["SD"]][k]/sqrt(8), col='black', lwd=2)
  segments(ttoCon[['Fecha']][k],ttoCon[["Mean"]][k],ttoCon[['Fecha']][k],ttoCon[["Mean"]][k]-ttoCon[["SD"]][k]/sqrt(8), col='black', lwd=2)}
###########################################################################

# dev.off()

