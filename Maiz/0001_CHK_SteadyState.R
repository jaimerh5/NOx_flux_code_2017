#########################################################################
# JAIME RECIO HUETOS 
# ETSIA DE LA UPM DEPARTAMENTO DE QUÍMICA Y TECNOLOGIA DE ALIMENTOS 
# 11/09/2017
######################################################################

#######################################################################
#Voy a coger los 3 ultimos valores de cada hora de las cajas y asi 
# voy a sacar los NOx correspondientes a cada hora 
library(RColorBrewer); library (lattice); library(data.table);
levelPal <- colorRampPalette(
  brewer.pal(n = 9, 'Purples'))
rm()
setwd('C:/R/Expmto2017/MAIZ')
fl <- dir(); fl <- fl[1:16];fls <- c(fl[2:length(fl)],fl[1]); fls
# fls= fls[1]=Chambj,......fls[7]=caja AmB


open <- paste(fls[1],'/csv',sep = '');open
setwd(open)
dir()
n <- dir(); summary(n)
# n <- n[5:(length(n))] ;n #le quito dias y archivos que no me interesan
                         #En cada caja cambia este valor... tienes que ir chequeando
summary(n)
# creo vbles de apoyo  
Ver <- rep(0,length(n)) 
NAME <- rep(0,length(n)); NAME2  <- rep(0,length(n)) 
Title <- rep(0,length(n));Title2 <- rep(0,length(n))
  f <- NULL; file <- NULL
  
for (j in 1:(length(n))) {


File2Open <- n[j]; File2Open
file <- c(file,File2Open)  #chequear bucle 'for '
Chambj<- read.table(File2Open, sep=';', dec=',', header=TRUE)
# Aqui cometo error de llamarla Chambj aunque depende del archivo que coja será caja2,3,4,5,6

Ver[j] <-n[j]; Ver 
Chambj$X <- NULL
names(Chambj)[1] <- 'Fecha';
Chambj$Fecha <-as.POSIXct (Chambj$Fecha,
                            format= '%Y/%m/%d %H:%M:%S', tz = 'GMT')
# Chambj$Fecha[1:5]
GAPtime <- Chambj$Fecha  # Es mas facil crear un vector de tiempo y luego jugar con el 
#con este If quito valores erróneos 
      
    if (any(Chambj[['NOx']]>(Chambj[['NO']]+Chambj[['NO2']]+6))) {
        idx <- which(Chambj[['NOx']]>(Chambj[['NO']]+Chambj[['NO2']]+6))
        Chambj[['NOx']][idx] <- Chambj[['NO']][idx]+Chambj[['NO2']][idx]}
############################################################################
YMONDOY  <- as.numeric(format(GAPtime,format="%Y%m%d")) ;  Chambj$YMONDOY <- YMONDOY
DOY  <- as.numeric(format(GAPtime,format="%j")) ;  Chambj$DOY <- DOY
HOD <- as.numeric(format(GAPtime,format="%H")); Chambj$HOD <- HOD
MeasureMinute <- as.numeric(format(GAPtime,format="%M")); Chambj$Minute <- MeasureMinute
#En este he creado columnas numericas para ver el tiempo 

#######voy a meter todos los graficos en otro archivo #######
setwd("C:/R/Expmto2017/MAIZ/EurochemScripts/PLOTS")
NAME[j] <- paste('caja1_',substr(File2Open,1,8),'.pdf' ,sep='')

Title[j] <- paste('caja1',substr(File2Open,1,8))
#AQUI CAMBIA EL NOMBRE-->Chambj<>caja6
#substr para coger cachos de cadena de caracteres
# Kind <- unique(Chambj[['HOD']]) #Kind me da un vector de (0,23) las 24 horas del dia 
##################################################################
##Con este bucle voy a crear el vector de la media de los 3 ultimos valores NOX de cada hora
Kind1 <- unique(Chambj[['YMONDOY']]) 
#vector de todos los dias del experimento 
Kind2 <- unique(Chambj[['HOD']]); Kind2 <-sort(Kind2)
#vector ordenado de las horas donde mide la caja corrspondiente
posi <- NULL;NOxMean3 <- NULL;posi2 <- NULL; pick2 <- NULL;O3Mean3 <- NULL

for (i in 1:length(Kind1)) {
  for (z in 1:length(Kind2)) {
    uno <- which(Chambj[['YMONDOY']]==Kind1[i] & Chambj[['HOD']]==Kind2[z])
    pick1 <- uno[length(uno)]
    if (length(uno)>3) {
      pick2 <- uno[(length(uno)-2):length(uno)]
      NOxMean3 <- c(NOxMean3,mean(Chambj$NOx[pick2]))}
    if (length(uno)<3) {
      NOxMean3 <- c(NOxMean3,mean(Chambj$NOx[pick1]))}
    posi <- c(posi,pick1)
    posi2 <- c(posi2,pick2)  } }

#puedes hacer tambien todo esto para calcular el NOx que más veces se ha repetido
#juega con la funcion table( )
# table(Chambj$NOx[uno])
# which(table(Chambj$NOx[uno])==max(table(Chambj$NOx[uno])))


NOXlim <- range(Chambj$NOx)  #vector de valor max y min 
setwd("C:/R/Expmto2017/MAIZ/EurochemScripts/PLOTS/CHKsteady")
# setwd("C:/Users/JaimeRecio/Desktop/peroque")
# pdf(NAME2[i])


pdf(NAME[j])
plot(as.POSIXct(Chambj[['Fecha']], format='%H') ,
     Chambj[['NOx']],
     type='p',col="springgreen1",pch=19,cex=.5,
     xlab='time',ylab='NOx', main=Title[j],
     xlim=range(Chambj[['Fecha']]),ylim=NOXlim)
# par(new=T)
points(as.POSIXct(Chambj[['Fecha']][posi], format='%H') ,
     NOxMean3,
     type='l', 
     col='red',lwd=3,
     xlab='',ylab='', main='',
     xlim=range(Chambj[['Fecha']]), ylim=NOXlim)
dev.off()

setwd('C:/R/Expmto2017/MAIZ') ; setwd(open); getwd()   }
  
