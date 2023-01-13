#############################################################
# JAIME RECIO HUETOS 
# ETSIA DE LA UPM; DEPARTAMENTO DE QUÍMICA Y TECNOLOGIA DE ALIMENTOS 
#11/09/2017
#############################################################
#############################################################
library(data.table);library(latticeExtra);library(lattice)
setwd('C:/R/Expmto2017/MAIZ') #Aquí se introduce la ruta de  la carpeta que contenga tu base de datos
#fl son todos los archivos que hay en setwd() 
#Fijate antes del script ((fl <- fl[8:14])) que pueden variar el numero 
fl <- dir(); fl <- fl[1:16];fls <- c(fl[2:length(fl)],fl[1]); fls
# fls= fls[1]=CENTER_Caja_01,......fls[16]=CENTER_Ambiente

#abrimos archivos diarios 
#tienes que ir variando fls[xxx] para que abra la caja que quieres

open<- paste(fls[10],'/csv',sep = '');open 


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

##################################################################
##Con este bucle voy a crear el vector de la media de los ultimos valores NOX de cada hora
#En el caso de que me falte algun valor le he repetido el ultimo valor del 20160622
Kind1 <- unique(CajaDaily[['YMONDOY']]);  
#vector de los dias de medidas de NOx en la chamber que toque 
Kind2 <- unique(CajaDaily[['HOD']]); Kind2 <-sort(Kind2) 
#Vector de las horas del dia donde se mide  caja1,2,3,4y5= 0-3-6-9-12-15-18-21-0
if (any(CajaDaily$Foco=='Caja_05')) {Kind2  <- c(0,3,6,9,12,15,18,21)}
if (any(CajaDaily$Foco=='Caja_10')) {Kind2  <- c(1,4,7,10,13,16,19,22)}
if (any(CajaDaily$Foco=='Caja_15')) {Kind2  <- c(2,5,8,11,14,17,20,23)}

NOMean3 <- NULL;NO2Mean3 <- NULL;
posi <- NULL;NOxMean3 <- NULL;posi2 <- NULL; pick2 <- NULL;O3Mean3 <- NULL

for (i in 1:length(Kind1)) { #buscar por cada día
  for (j in 1:length(Kind2)) { #buscar por cada hora del dia
    uno <- which(CajaDaily[['YMONDOY']]==Kind1[i] & CajaDaily[['HOD']]==Kind2[j])   #18 valores por cada i & j
    pick1 <- uno[length(uno)]
    if (length(uno)>3) {
      pick2 <- uno[(length(uno)-3):length(uno)]
      NOxMean3 <- c(NOxMean3,mean(CajaDaily$NOx[pick2]))
      NO2Mean3 <- c(NO2Mean3,mean(CajaDaily$NO2[pick2]))
      NOMean3 <- c(NOMean3,mean(CajaDaily$NO[pick2]))}
    if (length(uno)<3) { #no sabes poner el else ó en R es dificil
 
      NOxMean3 <- c(NOxMean3,mean(CajaDaily$NOx[pick1]))
      NO2Mean3 <- c(NO2Mean3,mean(CajaDaily$NO2[pick1]))
      NOMean3 <- c(NOMean3,mean(CajaDaily$NO[pick1]))}
    posi <- c(posi,pick1)
    posi2 <- c(posi2,pick2) 
  }}
# summary(NOxMean3)
NOxMean3 <- NOxMean3[-(which(is.na(NOxMean3)))] # quito las filas donde no exista datos
NOxMean3 <- round(NOxMean3)   #le quito el decimal porque son ppb
# O3Mean3 <- O3Mean3[-(which(is.na(O3Mean3)))]
# O3Mean3 <- round(O3Mean3, digits = 3)
NO2Mean3 <- NO2Mean3[-(which(is.na(NO2Mean3)))]
NO2Mean3 <- round(NO2Mean3)
NOMean3 <- NOMean3[-(which(is.na(NOMean3)))]
NOMean3 <- round(NOMean3)

#NUNCA LE DES A CORRER TODO ESTE CODIGO
#TIENES QUE IR VARIANDO fls que esta al ppio 

chamb1_Gnal <- CajaDaily[posi,]; chamb1_Gnal$NOx <- NOxMean3; chamb1_Gnal$NO2 <- NO2Mean3;chamb1_Gnal$NO <- NOMean3;
chamb2_Gnal <- CajaDaily[posi,]; chamb2_Gnal$NOx <- NOxMean3;chamb2_Gnal$NO2 <- NO2Mean3;chamb2_Gnal$NO <- NOMean3
chamb3_Gnal <- CajaDaily[posi,]; chamb3_Gnal$NOx <- NOxMean3; chamb3_Gnal$NO2 <- NO2Mean3; chamb3_Gnal$NO <- NOMean3
chamb4_Gnal <- CajaDaily[posi,]; chamb4_Gnal$NOx <- NOxMean3; chamb4_Gnal$NO2 <- NO2Mean3; chamb4_Gnal$NO <- NOMean3
chamb5_Gnal <- CajaDaily[posi,]; chamb5_Gnal$NOx <- NOxMean3; chamb5_Gnal$NO2 <- NO2Mean3; chamb5_Gnal$NO <- NOMean3
chamb6_Gnal <- CajaDaily[posi,]; chamb6_Gnal$NOx <- NOxMean3; chamb6_Gnal$NO2 <- NO2Mean3; chamb6_Gnal$NO <- NOMean3
chamb7_Gnal <- CajaDaily[posi,]; chamb7_Gnal$NOx <- NOxMean3; chamb7_Gnal$NO2 <- NO2Mean3; chamb7_Gnal$NO <- NOMean3
chamb8_Gnal <- CajaDaily[posi,]; chamb8_Gnal$NOx <- NOxMean3; chamb8_Gnal$NO2 <- NO2Mean3; chamb8_Gnal$NO <- NOMean3
chamb9_Gnal <- CajaDaily[posi,]; chamb9_Gnal$NOx <- NOxMean3; chamb9_Gnal$NO2 <- NO2Mean3; chamb9_Gnal$NO <- NOMean3
chamb10_Gnal <- CajaDaily[posi,]; chamb10_Gnal$NOx <- NOxMean3; chamb10_Gnal$NO2 <- NO2Mean3; chamb10_Gnal$NO <- NOMean3
chamb11_Gnal <- CajaDaily[posi,]; chamb11_Gnal$NOx <- NOxMean3; chamb11_Gnal$NO2 <- NO2Mean3; chamb11_Gnal$NO <- NOMean3
chamb12_Gnal <- CajaDaily[posi,]; chamb12_Gnal$NOx <- NOxMean3; chamb12_Gnal$NO2 <- NO2Mean3; chamb12_Gnal$NO <- NOMean3
chamb13_Gnal <- CajaDaily[posi,]; chamb13_Gnal$NOx <- NOxMean3; chamb13_Gnal$NO2 <- NO2Mean3; chamb13_Gnal$NO <- NOMean3
chamb14_Gnal <- CajaDaily[posi,]; chamb14_Gnal$NOx <- NOxMean3; chamb14_Gnal$NO2 <- NO2Mean3; chamb14_Gnal$NO <- NOMean3
chamb15_Gnal <- CajaDaily[posi,]; chamb15_Gnal$NOx <- NOxMean3; chamb15_Gnal$NO2 <- NO2Mean3; chamb15_Gnal$NO <- NOMean3


#has creado las mismas variables que en chambers pero sacando el ultimo dato de cada hora 
# menos en la caja 5,10 y 15 que has sacado el penultimo valor 
 
caja_GNAL <- rbind(chamb1_Gnal,chamb2_Gnal,chamb3_Gnal,chamb4_Gnal,chamb5_Gnal,
                   chamb6_Gnal,chamb7_Gnal,chamb8_Gnal,chamb9_Gnal,chamb10_Gnal,
                   chamb11_Gnal,chamb12_Gnal,chamb13_Gnal,chamb14_Gnal,chamb15_Gnal)
# caja GNAL es unir todas las cajas pero faltan los valores del ambiente 
# que los sacas del archivo 2016




