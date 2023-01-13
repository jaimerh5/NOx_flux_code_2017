#############################################################
# JAIME RECIO HUETOS 
# ETSIA DE LA UPM; DEPARTAMENTO DE QUÍMICA Y TECNOLOGIA DE ALIMENTOS 
#11/09/2017

#############################################################
#############################################################
# vamos a sacar el valor minimo de ambiente 

library(data.table);library(latticeExtra);library(lattice)
setwd('C:/R/Expmto2017/MAIZ')
#fl son todos los archivos que hay en setwd() 
#Fijate antes del script ((fl <- fl[8:14])) que pueden variar el numero 
fl <- dir(); fl <- fl[1:16];fls <- c(fl[2:length(fl)],fl[1]); fls
# fls= fls[1]=CENTER_Caja_01,......fls[16]=CENTER_Ambiente

#abrimos archivos diarios 
#tienes que ir variando fls[xxx] para que abra la caja que quiers

open<- paste(fls[16],'/csv',sep = '');open 
#recuerda que en la primera caja hubo fuga y usaste la 4 

setwd(open)
dir()
n <- dir(); summary(n)
#voy a seleccionar los dias de toma de datos que me interesan
# nn <-n
#Creo un bucle que me permita abrir todos los dias de experimento y ponerlos en una sola vble
# CajaDaily <- read.table('20160401.csv',sep=';',dec=',', header=TRUE)

CajaDaily <- NULL
for (j in 1:length(n))  {
  file <- n[j]
  dia <- read.table(file,sep=';',dec=',', header=TRUE)
  CajaDaily <- rbind(CajaDaily,dia)}

# attach(CajaDaily) # con esto ya no tienes que poner CajaDaily$X
# Puedes poner solo X

CajaDaily$X <- NULL
names(CajaDaily)[1] <- 'Fecha';
date_format<-"%Y/%m/%d %H:%M:%S"
CajaDaily$Fecha <-as.POSIXct (CajaDaily$Fecha,
                              format= date_format, tz = 'GMT')
######################################################################################
GAPtime <- CajaDaily[['Fecha']]

YMONDOY <-  as.numeric(format(CajaDaily$Fecha,format='%Y%m%d' )); CajaDaily$YMONDOY <- YMONDOY
MONDOY <- as.numeric(format(CajaDaily$Fecha,format='%m%d' )); CajaDaily$MONDOY <- MONDOY
DOY  <- as.numeric(format(CajaDaily$Fecha,format="%j")) ;  CajaDaily$DOY <- DOY
#DOM<- as.numeric(format(CajaDaily$Fecha,format="%d")) ;  CajaDaily$DOM <- DOM
YMONDOY_HM <- as.numeric(format(CajaDaily$Fecha,format='%H%M')); CajaDaily$HourMinu <- YMONDOY_HM
HOD <- as.numeric(format(CajaDaily$Fecha,format='%H')); CajaDaily$HOD <- HOD
#MONDOY_HOD <- as.numeric(format(CajaDaily$Fecha,format='%m%d%H')); CajaDaily$Day_Hour <- MONDOY_HOD
MeasureMinute <- as.numeric(format(GAPtime,format="%M")); CajaDaily$Minute <- MeasureMinute
######################################################################################

#Bucle para quitarme datos erróneos por fallos de software 
if (any(CajaDaily[['NOx']]>(CajaDaily[['NO']]+CajaDaily[['NO2']]+6))) {
  idx <- which(CajaDaily[['NOx']]>(CajaDaily[['NO']]+CajaDaily[['NO2']]+6))
  CajaDaily[['NOx']][idx] <- CajaDaily[['NO']][idx]+CajaDaily[['NO2']][idx]}
 head(CajaDaily)
###########################################################################

##Con este bucle voy a crear el vector del mínimo de los ultimos valores NOX de cada hora
#En el caso de que me falte algun valor le he repetido el ultimo valor del 20160622
 Kind1 <- unique(CajaDaily[['YMONDOY']]);  
 #vector de los dias de medidas de NOx en la chamber que toque 
 Kind2 <- unique(CajaDaily[['HOD']]); Kind2 <-sort(Kind2) 
 Kind3 <- unique(CajaDaily[['Minute']]); Kind3 <-sort(Kind3) 
 Kind4 <-seq(0,60,10) 
 # kind4  es la P.. CLAVE para coger rangos de minutos de 0a 10, de 10 a 20 etc, a 
 NOMin <- NULL;NO2Min <- NULL;NOxMin <- NULL;
 pickk <- NULL;pickk2 <- NULL; 
 
 for (i in 1:length(Kind1)) { #buscar por cada día
   for (j in 1:length(Kind2)) { #buscar por cada hora del dia
     for (k in 1:(length(Kind4)-1)) {#buscar por cada minuto de la hora
       uno <- which(CajaDaily[['YMONDOY']]==Kind1[i] & CajaDaily[['HOD']]==Kind2[j] &    #18 valores por cada i & j
                   (CajaDaily[['HourMinu']] %% 100) %in% (Kind4[k]:Kind4[k+1]))   
       if (length(uno) != 0) { #si no pongo esto me salen mucho infinito
         pickk <- uno[length(uno)]
         
         
         NO2Min <- c(NO2Min,min(CajaDaily$NO2[uno]))
         NOMin <- c(NOMin,min(CajaDaily$NO[uno]))
         NOxMin <- c(NOxMin,min(CajaDaily$NO[uno])+min(CajaDaily$NO2[uno]))
         pickk2 <- c(pickk2,pickk)}}}
 }


# chambAMB_Gnal <- CajaDaily[pickk2,]; chambAMB_Gnal$NOx <- NOxMin; chambAMB_Gnal$NO2 <- NO2Min;chambAMB_Gnal$NO <- NOMin;
chambAMB_Gnal4 <- CajaDaily[pickk2,]; chambAMB_Gnal4$NOx <- NOxMin; chambAMB_Gnal4$NO2 <- NO2Min;chambAMB_Gnal4$NO <- NOMin;
head(chambAMB_Gnal)
