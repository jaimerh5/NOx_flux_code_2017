########################################################
##CONTROL

Tto_C
MatrizControl <- cbind(chamb1_Gnal[1:438,c(1,7,11,16:18)],
                       chamb8_Gnal[1:438,c(11,16:18)],
                       chamb13_Gnal[1:438,c(11,16:18)])

names(MatrizControl) <- c('Fecha','YMONDOY',"HOD_ch1",'FluxNOx_ch1', "Flux.NO_ch1", "Flux.NO2_ch1",
                          "HOD_ch8",'FluxNOx_ch8', "Flux.NO_ch8", "Flux.NO2_ch8",
                          "HOD_ch13",'FluxNOx_ch13', "Flux.NO_ch13", "Flux.NO2_ch13")
#---------------------------------------------#---------------------------------------------
MatrizControl$Mean.Control.NOx <- apply(MatrizControl[,c('FluxNOx_ch1','FluxNOx_ch8','FluxNOx_ch13')],1,FUN = mean) #Mdia horaria de Control
MatrizControl$SD.Control.NOx <- apply(MatrizControl[,c('FluxNOx_ch1','FluxNOx_ch8','FluxNOx_ch13')],1,FUN = sd)      #SD Horaria de Control
MatrizControl$Mean.Control.NO <- apply(MatrizControl[,c('Flux.NO_ch1','Flux.NO_ch8','Flux.NO_ch13')],1,FUN = mean)
MatrizControl$SD.Control.NO <- apply(MatrizControl[,c('Flux.NO_ch1','Flux.NO_ch8','Flux.NO_ch13')],1,FUN = sd )     
MatrizControl$Mean.Control.NO2 <- apply(MatrizControl[,c('Flux.NO2_ch1','Flux.NO2_ch8','Flux.NO2_ch13')],1,FUN = mean )
MatrizControl$SD.Control.NO2 <- apply(MatrizControl[,c('Flux.NO2_ch1','Flux.NO2_ch8','Flux.NO2_ch13')],1,FUN = sd)                           
#-----
MatrizControl$PeriodDay <- NA
MatrizControl$PeriodDay[which(MatrizControl$HOD_ch1>6 & MatrizControl$HOD_ch1<=12)] <- 'morning'
MatrizControl$PeriodDay[which(MatrizControl$HOD_ch1>12 & MatrizControl$HOD_ch1<=18)] <- 'afternoon'
MatrizControl$PeriodDay[which(MatrizControl$HOD_ch1>18 & MatrizControl$HOD_ch1<=24)] <- 'evening'
MatrizControl$PeriodDay[which(MatrizControl$HOD_ch1>=0 & MatrizControl$HOD_ch1<=6)] <- 'night'
MatrizControl$SunHours <- NA
MatrizControl$SunHours[which(MatrizControl$HOD_ch1>=7 & MatrizControl$HOD_ch1<=19)] <- 'light'
MatrizControl$SunHours[which((MatrizControl$HOD_ch1>=20 & MatrizControl$HOD_ch1<24) | (MatrizControl$HOD_ch1>=0 & MatrizControl$HOD_ch1<7))] <- 'dark'
#----------------------------------                   
   
########################################################
##UREA
Tto_U

MatrizUrea <- cbind(chamb5_Gnal[1:438,c(1,7,11,16:18)],
                       chamb9_Gnal[1:438,c(11,16:18)],
                       chamb12_Gnal[1:438,c(11,16:18)])

names(MatrizUrea) <- c('Fecha','YMONDOY',"HOD_ch5",'FluxNOx_ch5', "Flux.NO_ch5", "Flux.NO2_ch5",
                          "HOD_ch9",'FluxNOx_ch9', "Flux.NO_ch9", "Flux.NO2_ch9",
                          "HOD_ch12",'FluxNOx_ch12', "Flux.NO_ch12", "Flux.NO2_ch12")
#---------------------------------------------#---------------------------------------------
MatrizUrea$Mean.Urea.NOx <- apply(MatrizUrea[,c('FluxNOx_ch5','FluxNOx_ch9','FluxNOx_ch12')],1,FUN = mean) #Mdia horaria de Urea
MatrizUrea$SD.Urea.NOx <- apply(MatrizUrea[,c('FluxNOx_ch5','FluxNOx_ch9','FluxNOx_ch12')],1,FUN = sd)      #SD Horaria de Urea
MatrizUrea$Mean.Urea.NO <- apply(MatrizUrea[,c('Flux.NO_ch5','Flux.NO_ch9','Flux.NO_ch12')],1,FUN = mean)
MatrizUrea$SD.Urea.NO <- apply(MatrizUrea[,c('Flux.NO_ch5','Flux.NO_ch9','Flux.NO_ch12')],1,FUN = sd )     
MatrizUrea$Mean.Urea.NO2 <- apply(MatrizUrea[,c('Flux.NO2_ch5','Flux.NO2_ch9','Flux.NO2_ch12')],1,FUN = mean )
MatrizUrea$SD.Urea.NO2 <- apply(MatrizUrea[,c('Flux.NO2_ch5','Flux.NO2_ch9','Flux.NO2_ch12')],1,FUN = sd)                           

#-----
MatrizUrea$PeriodDay <- NA
MatrizUrea$PeriodDay[which(MatrizUrea$HOD_ch5>6 & MatrizUrea$HOD_ch5<=12)] <- 'morning'
MatrizUrea$PeriodDay[which(MatrizUrea$HOD_ch5>12 & MatrizUrea$HOD_ch5<=18)] <- 'afternoon'
MatrizUrea$PeriodDay[which(MatrizUrea$HOD_ch5>18 & MatrizUrea$HOD_ch5<=24)] <- 'evening'
MatrizUrea$PeriodDay[which(MatrizUrea$HOD_ch5>=0 & MatrizUrea$HOD_ch5<=6)] <- 'night'
MatrizUrea$SunHours <- NA
MatrizUrea$SunHours[which(MatrizUrea$HOD_ch5>=7 & MatrizUrea$HOD_ch5<=19)] <- 'light'
MatrizUrea$SunHours[which((MatrizUrea$HOD_ch5>=20 & MatrizUrea$HOD_ch5<24) | (MatrizUrea$HOD_ch5>=0 & MatrizUrea$HOD_ch5<7))] <- 'dark'
#----------------------------------  
########################################################
##UREA+ DMPSA
Tto_UNI
MatrizU.NI <- cbind(chamb3_Gnal[1:439,c(1,7,11,16:18)],
                    chamb10_Gnal[1:439,c(11,16:18)],
                    chamb14_Gnal[1:439,c(11,16:18)])

names(MatrizU.NI) <- c('Fecha','YMONDOY',"HOD_ch3",'FluxNOx_ch3', "Flux.NO_ch3", "Flux.NO2_ch3",
                       "HOD_ch10",'FluxNOx_ch10', "Flux.NO_ch10", "Flux.NO2_ch10",
                       "HOD_ch14",'FluxNOx_ch14', "Flux.NO_ch14", "Flux.NO2_ch14")
#---------------------------------------------#---------------------------------------------
MatrizU.NI$Mean.U.NI.NOx <- apply(MatrizU.NI[,c('FluxNOx_ch3','FluxNOx_ch10','FluxNOx_ch14')],1,FUN = mean) #Mdia horaria de U.NI
MatrizU.NI$SD.U.NI.NOx <- apply(MatrizU.NI[,c('FluxNOx_ch3','FluxNOx_ch10','FluxNOx_ch14')],1,FUN = sd)      #SD Horaria de U.NI
MatrizU.NI$Mean.U.NI.NO <- apply(MatrizU.NI[,c('Flux.NO_ch3','Flux.NO_ch10','Flux.NO_ch14')],1,FUN = mean)
MatrizU.NI$SD.U.NI.NO <- apply(MatrizU.NI[,c('Flux.NO_ch3','Flux.NO_ch10','Flux.NO_ch14')],1,FUN = sd )     
MatrizU.NI$Mean.U.NI.NO2 <- apply(MatrizU.NI[,c('Flux.NO2_ch3','Flux.NO2_ch10','Flux.NO2_ch14')],1,FUN = mean )
MatrizU.NI$SD.U.NI.NO2 <- apply(MatrizU.NI[,c('Flux.NO2_ch3','Flux.NO2_ch10','Flux.NO2_ch14')],1,FUN = sd)                           

#-----
MatrizU.NI$PeriodDay <- NA
MatrizU.NI$PeriodDay[which(MatrizU.NI$HOD_ch3>6 & MatrizU.NI$HOD_ch3<=12)] <- 'morning'
MatrizU.NI$PeriodDay[which(MatrizU.NI$HOD_ch3>12 & MatrizU.NI$HOD_ch3<=18)] <- 'afternoon'
MatrizU.NI$PeriodDay[which(MatrizU.NI$HOD_ch3>18 & MatrizU.NI$HOD_ch3<=24)] <- 'evening'
MatrizU.NI$PeriodDay[which(MatrizU.NI$HOD_ch3>=0 & MatrizU.NI$HOD_ch3<=6)] <- 'night'
MatrizU.NI$SunHours <- NA
MatrizU.NI$SunHours[which(MatrizU.NI$HOD_ch3>=7 & MatrizU.NI$HOD_ch3<=19)] <- 'light'
MatrizU.NI$SunHours[which((MatrizU.NI$HOD_ch3>=20 & MatrizU.NI$HOD_ch3<24) | (MatrizU.NI$HOD_ch3>=0 & MatrizU.NI$HOD_ch3<7))] <- 'dark'
#---------------------------------- 
########################################################
##Urea+NBPT
Tto_UUI

MatrizU.UI <- cbind(chamb2_Gnal[1:441,c(1,7,11,16:18)],
                    chamb6_Gnal[1:441,c(11,16:18)],
                    chamb15_Gnal[1:441,c(11,16:18)])

names(MatrizU.UI) <- c('Fecha','YMONDOY',"HOD_ch2",'FluxNOx_ch2', "Flux.NO_ch2", "Flux.NO2_ch2",
                       "HOD_ch6",'FluxNOx_ch6', "Flux.NO_ch6", "Flux.NO2_ch6",
                       "HOD_ch15",'FluxNOx_ch15', "Flux.NO_ch15", "Flux.NO2_ch15")
#---------------------------------------------#---------------------------------------------
MatrizU.UI$Mean.U.UI.NOx <- apply(MatrizU.UI[,c('FluxNOx_ch2','FluxNOx_ch6','FluxNOx_ch15')],1,FUN = mean) #Mdia horaria de U.UI
MatrizU.UI$SD.U.UI.NOx <- apply(MatrizU.UI[,c('FluxNOx_ch2','FluxNOx_ch6','FluxNOx_ch15')],1,FUN = sd)      #SD Horaria de U.UI
MatrizU.UI$Mean.U.UI.NO <- apply(MatrizU.UI[,c('Flux.NO_ch2','Flux.NO_ch6','Flux.NO_ch15')],1,FUN = mean)
MatrizU.UI$SD.U.UI.NO <- apply(MatrizU.UI[,c('Flux.NO_ch2','Flux.NO_ch6','Flux.NO_ch15')],1,FUN = sd )     
MatrizU.UI$Mean.U.UI.NO2 <- apply(MatrizU.UI[,c('Flux.NO2_ch2','Flux.NO2_ch6','Flux.NO2_ch15')],1,FUN = mean )
MatrizU.UI$SD.U.UI.NO2 <- apply(MatrizU.UI[,c('Flux.NO2_ch2','Flux.NO2_ch6','Flux.NO2_ch15')],1,FUN = sd)                           

#-----
MatrizU.UI$PeriodDay <- NA
MatrizU.UI$PeriodDay[which(MatrizU.UI$HOD_ch2>6 & MatrizU.UI$HOD_ch2<=12)] <- 'morning'
MatrizU.UI$PeriodDay[which(MatrizU.UI$HOD_ch2>12 & MatrizU.UI$HOD_ch2<=18)] <- 'afternoon'
MatrizU.UI$PeriodDay[which(MatrizU.UI$HOD_ch2>18 & MatrizU.UI$HOD_ch2<=24)] <- 'evening'
MatrizU.UI$PeriodDay[which(MatrizU.UI$HOD_ch2>=0 & MatrizU.UI$HOD_ch2<=6)] <- 'night'
MatrizU.UI$SunHours <- NA
MatrizU.UI$SunHours[which(MatrizU.UI$HOD_ch2>=7 & MatrizU.UI$HOD_ch2<=19)] <- 'light'
MatrizU.UI$SunHours[which((MatrizU.UI$HOD_ch2>=20 & MatrizU.UI$HOD_ch2<24) | (MatrizU.UI$HOD_ch2>=0 & MatrizU.UI$HOD_ch2<7))] <- 'dark'
#---------------------------------- 
########################################################
##Urea + NBPT+DMPSA
Tto_U2I

MatrizU.2I <- cbind(chamb4_Gnal[1:440,c(1,7,11,16:18)],
                    chamb7_Gnal[1:440,c(11,16:18)],
                    chamb11_Gnal[1:440,c(11,16:18)])

names(MatrizU.2I) <- c('Fecha','YMONDOY',"HOD_ch4",'FluxNOx_ch4', "Flux.NO_ch4", "Flux.NO2_ch4",
                       "HOD_ch7",'FluxNOx_ch7', "Flux.NO_ch7", "Flux.NO2_ch7",
                       "HOD_ch11",'FluxNOx_ch11', "Flux.NO_ch11", "Flux.NO2_ch11")
#---------------------------------------------#---------------------------------------------
MatrizU.2I$Mean.U.2I.NOx <- apply(MatrizU.2I[,c('FluxNOx_ch4','FluxNOx_ch7','FluxNOx_ch11')],1,FUN = mean) #Mdia horaria de U.2I
MatrizU.2I$SD.U.2I.NOx <- apply(MatrizU.2I[,c('FluxNOx_ch4','FluxNOx_ch7','FluxNOx_ch11')],1,FUN = sd)      #SD Horaria de U.2I
MatrizU.2I$Mean.U.2I.NO <- apply(MatrizU.2I[,c('Flux.NO_ch4','Flux.NO_ch7','Flux.NO_ch11')],1,FUN = mean)
MatrizU.2I$SD.U.2I.NO <- apply(MatrizU.2I[,c('Flux.NO_ch4','Flux.NO_ch7','Flux.NO_ch11')],1,FUN = sd )     
MatrizU.2I$Mean.U.2I.NO2 <- apply(MatrizU.2I[,c('Flux.NO2_ch4','Flux.NO2_ch7','Flux.NO2_ch11')],1,FUN = mean )
MatrizU.2I$SD.U.2I.NO2 <- apply(MatrizU.2I[,c('Flux.NO2_ch4','Flux.NO2_ch7','Flux.NO2_ch11')],1,FUN = sd)                           

#-----
MatrizU.2I$PeriodDay <- NA
MatrizU.2I$PeriodDay[which(MatrizU.2I$HOD_ch4>6 & MatrizU.2I$HOD_ch4<=12)] <- 'morning'
MatrizU.2I$PeriodDay[which(MatrizU.2I$HOD_ch4>12 & MatrizU.2I$HOD_ch4<=18)] <- 'afternoon'
MatrizU.2I$PeriodDay[which(MatrizU.2I$HOD_ch4>18 & MatrizU.2I$HOD_ch4<=24)] <- 'evening'
MatrizU.2I$PeriodDay[which(MatrizU.2I$HOD_ch4>=0 & MatrizU.2I$HOD_ch4<=6)] <- 'night'
MatrizU.2I$SunHours <- NA
MatrizU.2I$SunHours[which(MatrizU.2I$HOD_ch4>=7 & MatrizU.2I$HOD_ch4<=19)] <- 'light'
MatrizU.2I$SunHours[which((MatrizU.2I$HOD_ch4>=20 & MatrizU.2I$HOD_ch4<24) | (MatrizU.2I$HOD_ch4>=0 & MatrizU.2I$HOD_ch4<7))] <- 'dark'
#---------------------------------- 