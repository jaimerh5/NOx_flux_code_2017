#################################################
# JAIME RECIO HUETOS 
# ETSIA DE LA UPM; DEPARTAMENTO DE QUÍMICA Y TECNOLOGIA DE ALIMENTOS 
#10/12/2018
#############################################################

##CONTROL

Tto_C
CumMatrizControl <- cbind(chamb1_Gnal[1:438,c(1,7,11,19:21)],
                       chamb8_Gnal[1:438,c(11,19:21)],
                       chamb13_Gnal[1:438,c(11,19:21)])

names(CumMatrizControl) <- c('Fecha','YMONDOY',"HOD_ch1",'CumNOx_ch1', "Cum.NO_ch1", "Cum.NO2_ch1",
                          "HOD_ch8",'CumNOx_ch8', "Cum.NO_ch8", "Cum.NO2_ch8",
                          "HOD_ch13",'CumNOx_ch13', "Cum.NO_ch13", "Cum.NO2_ch13")
#---------------------------------------------#---------------------------------------------
CumMatrizControl$Mean.Control.NOx <- apply(CumMatrizControl[,c('CumNOx_ch1','CumNOx_ch8','CumNOx_ch13')],1,FUN = mean) #Mdia horaria de Control
CumMatrizControl$SD.Control.NOx <- apply(CumMatrizControl[,c('CumNOx_ch1','CumNOx_ch8','CumNOx_ch13')],1,FUN = sd)      #SD Horaria de Control
CumMatrizControl$Mean.Control.NO <- apply(CumMatrizControl[,c('Cum.NO_ch1','Cum.NO_ch8','Cum.NO_ch13')],1,FUN = mean)
CumMatrizControl$SD.Control.NO <- apply(CumMatrizControl[,c('Cum.NO_ch1','Cum.NO_ch8','Cum.NO_ch13')],1,FUN = sd )     
CumMatrizControl$Mean.Control.NO2 <- apply(CumMatrizControl[,c('Cum.NO2_ch1','Cum.NO2_ch8','Cum.NO2_ch13')],1,FUN = mean )
CumMatrizControl$SD.Control.NO2 <- apply(CumMatrizControl[,c('Cum.NO2_ch1','Cum.NO2_ch8','Cum.NO2_ch13')],1,FUN = sd)                           
#-----
CumMatrizControl$PeriodDay <- NA
CumMatrizControl$PeriodDay[which(CumMatrizControl$HOD_ch1>6 & CumMatrizControl$HOD_ch1<=12)] <- 'morning'
CumMatrizControl$PeriodDay[which(CumMatrizControl$HOD_ch1>12 & CumMatrizControl$HOD_ch1<=18)] <- 'afternoon'
CumMatrizControl$PeriodDay[which(CumMatrizControl$HOD_ch1>18 & CumMatrizControl$HOD_ch1<=24)] <- 'evening'
CumMatrizControl$PeriodDay[which(CumMatrizControl$HOD_ch1>=0 & CumMatrizControl$HOD_ch1<=6)] <- 'night'
CumMatrizControl$SunHours <- NA
CumMatrizControl$SunHours[which(CumMatrizControl$HOD_ch1>=7 & CumMatrizControl$HOD_ch1<=19)] <- 'light'
CumMatrizControl$SunHours[which((CumMatrizControl$HOD_ch1>=20 & CumMatrizControl$HOD_ch1<24) | (CumMatrizControl$HOD_ch1>=0 & CumMatrizControl$HOD_ch1<7))] <- 'dark'
#----------------------------------                   

########################################################
##UREA
Tto_U

CumMatrizUrea <- cbind(chamb5_Gnal[1:438,c(1,7,11,19:21)],
                    chamb9_Gnal[1:438,c(11,19:21)],
                    chamb12_Gnal[1:438,c(11,19:21)])

names(CumMatrizUrea) <- c('Fecha','YMONDOY',"HOD_ch5",'CumNOx_ch5', "Cum.NO_ch5", "Cum.NO2_ch5",
                       "HOD_ch9",'CumNOx_ch9', "Cum.NO_ch9", "Cum.NO2_ch9",
                       "HOD_ch12",'CumNOx_ch12', "Cum.NO_ch12", "Cum.NO2_ch12")
#---------------------------------------------#---------------------------------------------
CumMatrizUrea$Mean.Urea.NOx <- apply(CumMatrizUrea[,c('CumNOx_ch5','CumNOx_ch9','CumNOx_ch12')],1,FUN = mean) #Mdia horaria de Urea
CumMatrizUrea$SD.Urea.NOx <- apply(CumMatrizUrea[,c('CumNOx_ch5','CumNOx_ch9','CumNOx_ch12')],1,FUN = sd)      #SD Horaria de Urea
CumMatrizUrea$Mean.Urea.NO <- apply(CumMatrizUrea[,c('Cum.NO_ch5','Cum.NO_ch9','Cum.NO_ch12')],1,FUN = mean)
CumMatrizUrea$SD.Urea.NO <- apply(CumMatrizUrea[,c('Cum.NO_ch5','Cum.NO_ch9','Cum.NO_ch12')],1,FUN = sd )     
CumMatrizUrea$Mean.Urea.NO2 <- apply(CumMatrizUrea[,c('Cum.NO2_ch5','Cum.NO2_ch9','Cum.NO2_ch12')],1,FUN = mean )
CumMatrizUrea$SD.Urea.NO2 <- apply(CumMatrizUrea[,c('Cum.NO2_ch5','Cum.NO2_ch9','Cum.NO2_ch12')],1,FUN = sd)                           

#-----
CumMatrizUrea$PeriodDay <- NA
CumMatrizUrea$PeriodDay[which(CumMatrizUrea$HOD_ch5>6 & CumMatrizUrea$HOD_ch5<=12)] <- 'morning'
CumMatrizUrea$PeriodDay[which(CumMatrizUrea$HOD_ch5>12 & CumMatrizUrea$HOD_ch5<=18)] <- 'afternoon'
CumMatrizUrea$PeriodDay[which(CumMatrizUrea$HOD_ch5>18 & CumMatrizUrea$HOD_ch5<=24)] <- 'evening'
CumMatrizUrea$PeriodDay[which(CumMatrizUrea$HOD_ch5>=0 & CumMatrizUrea$HOD_ch5<=6)] <- 'night'
CumMatrizUrea$SunHours <- NA
CumMatrizUrea$SunHours[which(CumMatrizUrea$HOD_ch5>=7 & CumMatrizUrea$HOD_ch5<=19)] <- 'light'
CumMatrizUrea$SunHours[which((CumMatrizUrea$HOD_ch5>=20 & CumMatrizUrea$HOD_ch5<24) | (CumMatrizUrea$HOD_ch5>=0 & CumMatrizUrea$HOD_ch5<7))] <- 'dark'
#----------------------------------  
########################################################
##UREA+ DMPSA
Tto_UNI
CumMatrizU.NI <- cbind(chamb3_Gnal[1:439,c(1,7,11,19:21)],
                    chamb10_Gnal[1:439,c(11,19:21)],
                    chamb14_Gnal[1:439,c(11,19:21)])

names(CumMatrizU.NI) <- c('Fecha','YMONDOY',"HOD_ch3",'CumNOx_ch3', "Cum.NO_ch3", "Cum.NO2_ch3",
                       "HOD_ch10",'CumNOx_ch10', "Cum.NO_ch10", "Cum.NO2_ch10",
                       "HOD_ch14",'CumNOx_ch14', "Cum.NO_ch14", "Cum.NO2_ch14")
#---------------------------------------------#---------------------------------------------
CumMatrizU.NI$Mean.U.NI.NOx <- apply(CumMatrizU.NI[,c('CumNOx_ch3','CumNOx_ch10','CumNOx_ch14')],1,FUN = mean) #Mdia horaria de U.NI
CumMatrizU.NI$SD.U.NI.NOx <- apply(CumMatrizU.NI[,c('CumNOx_ch3','CumNOx_ch10','CumNOx_ch14')],1,FUN = sd)      #SD Horaria de U.NI
CumMatrizU.NI$Mean.U.NI.NO <- apply(CumMatrizU.NI[,c('Cum.NO_ch3','Cum.NO_ch10','Cum.NO_ch14')],1,FUN = mean)
CumMatrizU.NI$SD.U.NI.NO <- apply(CumMatrizU.NI[,c('Cum.NO_ch3','Cum.NO_ch10','Cum.NO_ch14')],1,FUN = sd )     
CumMatrizU.NI$Mean.U.NI.NO2 <- apply(CumMatrizU.NI[,c('Cum.NO2_ch3','Cum.NO2_ch10','Cum.NO2_ch14')],1,FUN = mean )
CumMatrizU.NI$SD.U.NI.NO2 <- apply(CumMatrizU.NI[,c('Cum.NO2_ch3','Cum.NO2_ch10','Cum.NO2_ch14')],1,FUN = sd)                           

#-----
CumMatrizU.NI$PeriodDay <- NA
CumMatrizU.NI$PeriodDay[which(CumMatrizU.NI$HOD_ch3>6 & CumMatrizU.NI$HOD_ch3<=12)] <- 'morning'
CumMatrizU.NI$PeriodDay[which(CumMatrizU.NI$HOD_ch3>12 & CumMatrizU.NI$HOD_ch3<=18)] <- 'afternoon'
CumMatrizU.NI$PeriodDay[which(CumMatrizU.NI$HOD_ch3>18 & CumMatrizU.NI$HOD_ch3<=24)] <- 'evening'
CumMatrizU.NI$PeriodDay[which(CumMatrizU.NI$HOD_ch3>=0 & CumMatrizU.NI$HOD_ch3<=6)] <- 'night'
CumMatrizU.NI$SunHours <- NA
CumMatrizU.NI$SunHours[which(CumMatrizU.NI$HOD_ch3>=7 & CumMatrizU.NI$HOD_ch3<=19)] <- 'light'
CumMatrizU.NI$SunHours[which((CumMatrizU.NI$HOD_ch3>=20 & CumMatrizU.NI$HOD_ch3<24) | (CumMatrizU.NI$HOD_ch3>=0 & CumMatrizU.NI$HOD_ch3<7))] <- 'dark'
#---------------------------------- 
########################################################
##Urea+NBPT
Tto_UUI

CumMatrizU.UI <- cbind(chamb2_Gnal[1:441,c(1,7,11,19:21)],
                    chamb6_Gnal[1:441,c(11,19:21)],
                    chamb15_Gnal[1:441,c(11,19:21)])

names(CumMatrizU.UI) <- c('Fecha','YMONDOY',"HOD_ch2",'CumNOx_ch2', "Cum.NO_ch2", "Cum.NO2_ch2",
                       "HOD_ch6",'CumNOx_ch6', "Cum.NO_ch6", "Cum.NO2_ch6",
                       "HOD_ch15",'CumNOx_ch15', "Cum.NO_ch15", "Cum.NO2_ch15")
#---------------------------------------------#---------------------------------------------
CumMatrizU.UI$Mean.U.UI.NOx <- apply(CumMatrizU.UI[,c('CumNOx_ch2','CumNOx_ch6','CumNOx_ch15')],1,FUN = mean) #Mdia horaria de U.UI
CumMatrizU.UI$SD.U.UI.NOx <- apply(CumMatrizU.UI[,c('CumNOx_ch2','CumNOx_ch6','CumNOx_ch15')],1,FUN = sd)      #SD Horaria de U.UI
CumMatrizU.UI$Mean.U.UI.NO <- apply(CumMatrizU.UI[,c('Cum.NO_ch2','Cum.NO_ch6','Cum.NO_ch15')],1,FUN = mean)
CumMatrizU.UI$SD.U.UI.NO <- apply(CumMatrizU.UI[,c('Cum.NO_ch2','Cum.NO_ch6','Cum.NO_ch15')],1,FUN = sd )     
CumMatrizU.UI$Mean.U.UI.NO2 <- apply(CumMatrizU.UI[,c('Cum.NO2_ch2','Cum.NO2_ch6','Cum.NO2_ch15')],1,FUN = mean )
CumMatrizU.UI$SD.U.UI.NO2 <- apply(CumMatrizU.UI[,c('Cum.NO2_ch2','Cum.NO2_ch6','Cum.NO2_ch15')],1,FUN = sd)                           

#-----
CumMatrizU.UI$PeriodDay <- NA
CumMatrizU.UI$PeriodDay[which(CumMatrizU.UI$HOD_ch2>6 & CumMatrizU.UI$HOD_ch2<=12)] <- 'morning'
CumMatrizU.UI$PeriodDay[which(CumMatrizU.UI$HOD_ch2>12 & CumMatrizU.UI$HOD_ch2<=18)] <- 'afternoon'
CumMatrizU.UI$PeriodDay[which(CumMatrizU.UI$HOD_ch2>18 & CumMatrizU.UI$HOD_ch2<=24)] <- 'evening'
CumMatrizU.UI$PeriodDay[which(CumMatrizU.UI$HOD_ch2>=0 & CumMatrizU.UI$HOD_ch2<=6)] <- 'night'
CumMatrizU.UI$SunHours <- NA
CumMatrizU.UI$SunHours[which(CumMatrizU.UI$HOD_ch2>=7 & CumMatrizU.UI$HOD_ch2<=19)] <- 'light'
CumMatrizU.UI$SunHours[which((CumMatrizU.UI$HOD_ch2>=20 & CumMatrizU.UI$HOD_ch2<24) | (CumMatrizU.UI$HOD_ch2>=0 & CumMatrizU.UI$HOD_ch2<7))] <- 'dark'
#---------------------------------- 
########################################################
##Urea + NBPT+DMPSA
Tto_U2I

CumMatrizU.2I <- cbind(chamb4_Gnal[1:440,c(1,7,11,19:21)],
                    chamb7_Gnal[1:440,c(11,19:21)],
                    chamb11_Gnal[1:440,c(11,19:21)])

names(CumMatrizU.2I) <- c('Fecha','YMONDOY',"HOD_ch4",'CumNOx_ch4', "Cum.NO_ch4", "Cum.NO2_ch4",
                       "HOD_ch7",'CumNOx_ch7', "Cum.NO_ch7", "Cum.NO2_ch7",
                       "HOD_ch11",'CumNOx_ch11', "Cum.NO_ch11", "Cum.NO2_ch11")
#---------------------------------------------#---------------------------------------------
CumMatrizU.2I$Mean.U.2I.NOx <- apply(CumMatrizU.2I[,c('CumNOx_ch4','CumNOx_ch7','CumNOx_ch11')],1,FUN = mean) #Mdia horaria de U.2I
CumMatrizU.2I$SD.U.2I.NOx <- apply(CumMatrizU.2I[,c('CumNOx_ch4','CumNOx_ch7','CumNOx_ch11')],1,FUN = sd)      #SD Horaria de U.2I
CumMatrizU.2I$Mean.U.2I.NO <- apply(CumMatrizU.2I[,c('Cum.NO_ch4','Cum.NO_ch7','Cum.NO_ch11')],1,FUN = mean)
CumMatrizU.2I$SD.U.2I.NO <- apply(CumMatrizU.2I[,c('Cum.NO_ch4','Cum.NO_ch7','Cum.NO_ch11')],1,FUN = sd )     
CumMatrizU.2I$Mean.U.2I.NO2 <- apply(CumMatrizU.2I[,c('Cum.NO2_ch4','Cum.NO2_ch7','Cum.NO2_ch11')],1,FUN = mean )
CumMatrizU.2I$SD.U.2I.NO2 <- apply(CumMatrizU.2I[,c('Cum.NO2_ch4','Cum.NO2_ch7','Cum.NO2_ch11')],1,FUN = sd)                           

#-----
CumMatrizU.2I$PeriodDay <- NA
CumMatrizU.2I$PeriodDay[which(CumMatrizU.2I$HOD_ch4>6 & CumMatrizU.2I$HOD_ch4<=12)] <- 'morning'
CumMatrizU.2I$PeriodDay[which(CumMatrizU.2I$HOD_ch4>12 & CumMatrizU.2I$HOD_ch4<=18)] <- 'afternoon'
CumMatrizU.2I$PeriodDay[which(CumMatrizU.2I$HOD_ch4>18 & CumMatrizU.2I$HOD_ch4<=24)] <- 'evening'
CumMatrizU.2I$PeriodDay[which(CumMatrizU.2I$HOD_ch4>=0 & CumMatrizU.2I$HOD_ch4<=6)] <- 'night'
CumMatrizU.2I$SunHours <- NA
CumMatrizU.2I$SunHours[which(CumMatrizU.2I$HOD_ch4>=7 & CumMatrizU.2I$HOD_ch4<=19)] <- 'light'
CumMatrizU.2I$SunHours[which((CumMatrizU.2I$HOD_ch4>=20 & CumMatrizU.2I$HOD_ch4<24) | (CumMatrizU.2I$HOD_ch4>=0 & CumMatrizU.2I$HOD_ch4<7))] <- 'dark'
#---------------------------------- 