#Datos del experimento 
#Datos Clave 
####################################################################################
SownDay <- as.Date("18/04/2017", "%d/%m/%Y") 
FertDay <- as.Date("29/06/2017", "%d/%m/%Y")
Long<- c(75,75,50,50,50,25,25,25,25,25,50,50,50,75,75) #metros de tubo y S de diametro de tubo
Qaudal <- c(15,15,18,18,19,19,20,20,20,20,19,19,18,15,15)#L/min=V*S
Qaudal2 <- c(20,20,21,23,25,28,30,30,30,28,25,23,21,20,20)#L/min=V*S
Mw <- 14; #N=g/mol
D <- 6e-3; #Diam Tubos--> Los tubos son de 8/6 mm  ( 6 es el dimaetro interior)
S <- pi*D^2/4; #S=Supfcie transversal tubos
SS <- 50*50e-4 #superficie de Suelo (Soil Surface)
V <- Qaudal/(60*1000)/S;#velocidad por el que pasa el caudal [m/s]
tt <- Long/V; #Tiempo de permanencia en tubo 
vol <- 22.4 #L por mol a 273K y 1 atm
k1 <- Mw*60*1e6/(vol*1e9*SS)  #Coeficiente de cambio de unidades
#1ppb_v== 14e-3/22.4 [ug/L]
# másico= [kg/s]= caudal*concentracion {L/h*ug/L}
#Flujo= [ug/m^2*h] = másico/sup 
############################################
#Flujo= k1*Qaudal*ppb
tpurg <- 20 #seg
tAMB <- 3 #min
tChamb <- 9 #min
CycleTime <- (tAMB+tChamb)*15/60 #horas
############################################
EndIHF <- as.Date("26/07/2017", "%d/%m/%Y") 
############################################
Tto_U <-  c('H5','H9','H12')
Tto_UNI <-  c('H3','H10','H14')
Tto_UUI <-  c('H2','H6','H15')
Tto_U2I <-  c('H4','H7','H11')
Tto_C <-  c('H1','H8','H13')
