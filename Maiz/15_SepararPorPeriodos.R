# voy a crear las columnas period of he day y si horas de luz
#########################chamber 1#####################
light <-which(chamb1_Gnal$HOD>=7 & chamb1_Gnal$HOD<=19) 
dark <- which((chamb1_Gnal$HOD>=20 & chamb1_Gnal$HOD<24) | (chamb1_Gnal$HOD>=0 & chamb1_Gnal$HOD<7)) 
night <- which(chamb1_Gnal$HOD>=0 & chamb1_Gnal$HOD<6) 
morning <- which(chamb1_Gnal$HOD>=6 & chamb1_Gnal$HOD<12)
afternoon <- which(chamb1_Gnal$HOD>=12 & chamb1_Gnal$HOD<18)
evening <- which(chamb1_Gnal$HOD>=18 & chamb1_Gnal$HOD<24)
chamb1_Gnal$PeriodDay <- rep(0,nrow(chamb1_Gnal))
chamb1_Gnal$PeriodDay[night] <- 'night'
chamb1_Gnal$PeriodDay[morning] <- 'morning'
chamb1_Gnal$PeriodDay[afternoon] <- 'afternoon'
chamb1_Gnal$PeriodDay[evening] <- 'evening'
chamb1_Gnal$SunHours <- rep(0,nrow(chamb1_Gnal))
chamb1_Gnal$SunHours[light] <- 'light'
chamb1_Gnal$SunHours[dark] <- 'dark'
############################chamber2#######################################
chamb2_Gnal$PeriodDay <- NA
chamb2_Gnal$PeriodDay[which(chamb2_Gnal$HOD>6 & chamb2_Gnal$HOD<=12)] <- 'morning'
chamb2_Gnal$PeriodDay[which(chamb2_Gnal$HOD>12 & chamb2_Gnal$HOD<=18)] <- 'afternoon'
chamb2_Gnal$PeriodDay[which(chamb2_Gnal$HOD>18 & chamb2_Gnal$HOD<=24)] <- 'evening'
chamb2_Gnal$PeriodDay[which(chamb2_Gnal$HOD>=0 & chamb2_Gnal$HOD<=6)] <- 'night'
chamb2_Gnal$SunHours <- NA
chamb2_Gnal$SunHours[which(chamb2_Gnal$HOD>=7 & chamb2_Gnal$HOD<=19)] <- 'light'
chamb2_Gnal$SunHours[which((chamb2_Gnal$HOD>=20 & chamb2_Gnal$HOD<24) | (chamb2_Gnal$HOD>=0 & chamb2_Gnal$HOD<7))] <- 'dark'
############################chamber3#######################################
chamb3_Gnal$PeriodDay <- NA
chamb3_Gnal$PeriodDay[which(chamb3_Gnal$HOD>6 & chamb3_Gnal$HOD<=12)] <- 'morning'
chamb3_Gnal$PeriodDay[which(chamb3_Gnal$HOD>12 & chamb3_Gnal$HOD<=18)] <- 'afternoon'
chamb3_Gnal$PeriodDay[which(chamb3_Gnal$HOD>18 & chamb3_Gnal$HOD<=24)] <- 'evening'
chamb3_Gnal$PeriodDay[which(chamb3_Gnal$HOD>=0 & chamb3_Gnal$HOD<=6)] <- 'night'
chamb3_Gnal$SunHours <- NA
chamb3_Gnal$SunHours[which(chamb3_Gnal$HOD>=7 & chamb3_Gnal$HOD<=19)] <- 'light'
chamb3_Gnal$SunHours[which((chamb3_Gnal$HOD>=20 & chamb3_Gnal$HOD<24) | (chamb3_Gnal$HOD>=0 & chamb3_Gnal$HOD<7))] <- 'dark'
############################chamber4#######################################
chamb4_Gnal$PeriodDay <- NA
chamb4_Gnal$PeriodDay[which(chamb4_Gnal$HOD>6 & chamb4_Gnal$HOD<=12)] <- 'morning'
chamb4_Gnal$PeriodDay[which(chamb4_Gnal$HOD>12 & chamb4_Gnal$HOD<=18)] <- 'afternoon'
chamb4_Gnal$PeriodDay[which(chamb4_Gnal$HOD>18 & chamb4_Gnal$HOD<=24)] <- 'evening'
chamb4_Gnal$PeriodDay[which(chamb4_Gnal$HOD>=0 & chamb4_Gnal$HOD<=6)] <- 'night'
chamb4_Gnal$SunHours <- NA
chamb4_Gnal$SunHours[which(chamb4_Gnal$HOD>=7 & chamb4_Gnal$HOD<=19)] <- 'light'
chamb4_Gnal$SunHours[which((chamb4_Gnal$HOD>=20 & chamb4_Gnal$HOD<24) | (chamb4_Gnal$HOD>=0 & chamb4_Gnal$HOD<7))] <- 'dark'
############################chamber5#######################################
chamb5_Gnal$PeriodDay <- NA
chamb5_Gnal$PeriodDay[which(chamb5_Gnal$HOD>6 & chamb5_Gnal$HOD<=12)] <- 'morning'
chamb5_Gnal$PeriodDay[which(chamb5_Gnal$HOD>12 & chamb5_Gnal$HOD<=18)] <- 'afternoon'
chamb5_Gnal$PeriodDay[which(chamb5_Gnal$HOD>18 & chamb5_Gnal$HOD<=24)] <- 'evening'
chamb5_Gnal$PeriodDay[which(chamb5_Gnal$HOD>=0 & chamb5_Gnal$HOD<=6)] <- 'night'
chamb5_Gnal$SunHours <- NA
chamb5_Gnal$SunHours[which(chamb5_Gnal$HOD>=7 & chamb5_Gnal$HOD<=19)] <- 'light'
chamb5_Gnal$SunHours[which((chamb5_Gnal$HOD>=20 & chamb5_Gnal$HOD<24) | (chamb5_Gnal$HOD>=0 & chamb5_Gnal$HOD<7))] <- 'dark'
############################chamber6#######################################
chamb6_Gnal$PeriodDay <- NA
chamb6_Gnal$PeriodDay[which(chamb6_Gnal$HOD>6 & chamb6_Gnal$HOD<=12)] <- 'morning'
chamb6_Gnal$PeriodDay[which(chamb6_Gnal$HOD>12 & chamb6_Gnal$HOD<=18)] <- 'afternoon'
chamb6_Gnal$PeriodDay[which(chamb6_Gnal$HOD>18 & chamb6_Gnal$HOD<=24)] <- 'evening'
chamb6_Gnal$PeriodDay[which(chamb6_Gnal$HOD>=0 & chamb6_Gnal$HOD<=6)] <- 'night'
chamb6_Gnal$SunHours <- NA
chamb6_Gnal$SunHours[which(chamb6_Gnal$HOD>=7 & chamb6_Gnal$HOD<=19)] <- 'light'
chamb6_Gnal$SunHours[which((chamb6_Gnal$HOD>=20 & chamb6_Gnal$HOD<24) | (chamb6_Gnal$HOD>=0 & chamb6_Gnal$HOD<7))] <- 'dark'
############################chamber7#######################################
chamb7_Gnal$PeriodDay <- NA
chamb7_Gnal$PeriodDay[which(chamb7_Gnal$HOD>6 & chamb7_Gnal$HOD<=12)] <- 'morning'
chamb7_Gnal$PeriodDay[which(chamb7_Gnal$HOD>12 & chamb7_Gnal$HOD<=18)] <- 'afternoon'
chamb7_Gnal$PeriodDay[which(chamb7_Gnal$HOD>18 & chamb7_Gnal$HOD<=24)] <- 'evening'
chamb7_Gnal$PeriodDay[which(chamb7_Gnal$HOD>=0 & chamb7_Gnal$HOD<=6)] <- 'night'
chamb7_Gnal$SunHours <- NA
chamb7_Gnal$SunHours[which(chamb7_Gnal$HOD>=7 & chamb7_Gnal$HOD<=19)] <- 'light'
chamb7_Gnal$SunHours[which((chamb7_Gnal$HOD>=20 & chamb7_Gnal$HOD<24) | (chamb7_Gnal$HOD>=0 & chamb7_Gnal$HOD<7))] <- 'dark'
############################chamber8#######################################
chamb8_Gnal$PeriodDay <- NA
chamb8_Gnal$PeriodDay[which(chamb8_Gnal$HOD>6 & chamb8_Gnal$HOD<=12)] <- 'morning'
chamb8_Gnal$PeriodDay[which(chamb8_Gnal$HOD>12 & chamb8_Gnal$HOD<=18)] <- 'afternoon'
chamb8_Gnal$PeriodDay[which(chamb8_Gnal$HOD>18 & chamb8_Gnal$HOD<=24)] <- 'evening'
chamb8_Gnal$PeriodDay[which(chamb8_Gnal$HOD>=0 & chamb8_Gnal$HOD<=6)] <- 'night'
chamb8_Gnal$SunHours <- NA
chamb8_Gnal$SunHours[which(chamb8_Gnal$HOD>=7 & chamb8_Gnal$HOD<=19)] <- 'light'
chamb8_Gnal$SunHours[which((chamb8_Gnal$HOD>=20 & chamb8_Gnal$HOD<24) | (chamb8_Gnal$HOD>=0 & chamb8_Gnal$HOD<7))] <- 'dark'
############################chamber9#######################################
chamb9_Gnal$PeriodDay <- NA
chamb9_Gnal$PeriodDay[which(chamb9_Gnal$HOD>6 & chamb9_Gnal$HOD<=12)] <- 'morning'
chamb9_Gnal$PeriodDay[which(chamb9_Gnal$HOD>12 & chamb9_Gnal$HOD<=18)] <- 'afternoon'
chamb9_Gnal$PeriodDay[which(chamb9_Gnal$HOD>18 & chamb9_Gnal$HOD<=24)] <- 'evening'
chamb9_Gnal$PeriodDay[which(chamb9_Gnal$HOD>=0 & chamb9_Gnal$HOD<=6)] <- 'night'
chamb9_Gnal$SunHours <- NA
chamb9_Gnal$SunHours[which(chamb9_Gnal$HOD>=7 & chamb9_Gnal$HOD<=19)] <- 'light'
chamb9_Gnal$SunHours[which((chamb9_Gnal$HOD>=20 & chamb9_Gnal$HOD<24) | (chamb9_Gnal$HOD>=0 & chamb9_Gnal$HOD<7))] <- 'dark'
############################chamber10#######################################
chamb10_Gnal$PeriodDay <- NA
chamb10_Gnal$PeriodDay[which(chamb10_Gnal$HOD>6 & chamb10_Gnal$HOD<=12)] <- 'morning'
chamb10_Gnal$PeriodDay[which(chamb10_Gnal$HOD>12 & chamb10_Gnal$HOD<=18)] <- 'afternoon'
chamb10_Gnal$PeriodDay[which(chamb10_Gnal$HOD>18 & chamb10_Gnal$HOD<=24)] <- 'evening'
chamb10_Gnal$PeriodDay[which(chamb10_Gnal$HOD>=0 & chamb10_Gnal$HOD<=6)] <- 'night'
chamb10_Gnal$SunHours <- NA
chamb10_Gnal$SunHours[which(chamb10_Gnal$HOD>=7 & chamb10_Gnal$HOD<=19)] <- 'light'
chamb10_Gnal$SunHours[which((chamb10_Gnal$HOD>=20 & chamb10_Gnal$HOD<24) | (chamb10_Gnal$HOD>=0 & chamb10_Gnal$HOD<7))] <- 'dark'
############################chamber11#######################################
chamb11_Gnal$PeriodDay <- NA
chamb11_Gnal$PeriodDay[which(chamb11_Gnal$HOD>6 & chamb11_Gnal$HOD<=12)] <- 'morning'
chamb11_Gnal$PeriodDay[which(chamb11_Gnal$HOD>12 & chamb11_Gnal$HOD<=18)] <- 'afternoon'
chamb11_Gnal$PeriodDay[which(chamb11_Gnal$HOD>18 & chamb11_Gnal$HOD<=24)] <- 'evening'
chamb11_Gnal$PeriodDay[which(chamb11_Gnal$HOD>=0 & chamb11_Gnal$HOD<=6)] <- 'night'
chamb11_Gnal$SunHours <- NA
chamb11_Gnal$SunHours[which(chamb11_Gnal$HOD>=7 & chamb11_Gnal$HOD<=19)] <- 'light'
chamb11_Gnal$SunHours[which((chamb11_Gnal$HOD>=20 & chamb11_Gnal$HOD<24) | (chamb11_Gnal$HOD>=0 & chamb11_Gnal$HOD<7))] <- 'dark'
############################chamber12#######################################
chamb12_Gnal$PeriodDay <- NA
chamb12_Gnal$PeriodDay[which(chamb12_Gnal$HOD>6 & chamb12_Gnal$HOD<=12)] <- 'morning'
chamb12_Gnal$PeriodDay[which(chamb12_Gnal$HOD>12 & chamb12_Gnal$HOD<=18)] <- 'afternoon'
chamb12_Gnal$PeriodDay[which(chamb12_Gnal$HOD>18 & chamb12_Gnal$HOD<=24)] <- 'evening'
chamb12_Gnal$PeriodDay[which(chamb12_Gnal$HOD>=0 & chamb12_Gnal$HOD<=6)] <- 'night'
chamb12_Gnal$SunHours <- NA
chamb12_Gnal$SunHours[which(chamb12_Gnal$HOD>=7 & chamb12_Gnal$HOD<=19)] <- 'light'
chamb12_Gnal$SunHours[which((chamb12_Gnal$HOD>=20 & chamb12_Gnal$HOD<24) | (chamb12_Gnal$HOD>=0 & chamb12_Gnal$HOD<7))] <- 'dark'
############################chamber13#######################################
chamb13_Gnal$PeriodDay <- NA
chamb13_Gnal$PeriodDay[which(chamb13_Gnal$HOD>6 & chamb13_Gnal$HOD<=12)] <- 'morning'
chamb13_Gnal$PeriodDay[which(chamb13_Gnal$HOD>12 & chamb13_Gnal$HOD<=18)] <- 'afternoon'
chamb13_Gnal$PeriodDay[which(chamb13_Gnal$HOD>18 & chamb13_Gnal$HOD<=24)] <- 'evening'
chamb13_Gnal$PeriodDay[which(chamb13_Gnal$HOD>=0 & chamb13_Gnal$HOD<=6)] <- 'night'
chamb13_Gnal$SunHours <- NA
chamb13_Gnal$SunHours[which(chamb13_Gnal$HOD>=7 & chamb13_Gnal$HOD<=19)] <- 'light'
chamb13_Gnal$SunHours[which((chamb13_Gnal$HOD>=20 & chamb13_Gnal$HOD<24) | (chamb13_Gnal$HOD>=0 & chamb13_Gnal$HOD<7))] <- 'dark'
############################chamber14#######################################
chamb14_Gnal$PeriodDay <- NA
chamb14_Gnal$PeriodDay[which(chamb14_Gnal$HOD>6 & chamb14_Gnal$HOD<=12)] <- 'morning'
chamb14_Gnal$PeriodDay[which(chamb14_Gnal$HOD>12 & chamb14_Gnal$HOD<=18)] <- 'afternoon'
chamb14_Gnal$PeriodDay[which(chamb14_Gnal$HOD>18 & chamb14_Gnal$HOD<=24)] <- 'evening'
chamb14_Gnal$PeriodDay[which(chamb14_Gnal$HOD>=0 & chamb14_Gnal$HOD<=6)] <- 'night'
chamb14_Gnal$SunHours <- NA
chamb14_Gnal$SunHours[which(chamb14_Gnal$HOD>=7 & chamb14_Gnal$HOD<=19)] <- 'light'
chamb14_Gnal$SunHours[which((chamb14_Gnal$HOD>=20 & chamb14_Gnal$HOD<24) | (chamb14_Gnal$HOD>=0 & chamb14_Gnal$HOD<7))] <- 'dark'
############################chamber15#######################################
chamb15_Gnal$PeriodDay <- NA
chamb15_Gnal$PeriodDay[which(chamb15_Gnal$HOD>6 & chamb15_Gnal$HOD<=12)] <- 'morning'
chamb15_Gnal$PeriodDay[which(chamb15_Gnal$HOD>12 & chamb15_Gnal$HOD<=18)] <- 'afternoon'
chamb15_Gnal$PeriodDay[which(chamb15_Gnal$HOD>18 & chamb15_Gnal$HOD<=24)] <- 'evening'
chamb15_Gnal$PeriodDay[which(chamb15_Gnal$HOD>=0 & chamb15_Gnal$HOD<=6)] <- 'night'
chamb15_Gnal$SunHours <- NA
chamb15_Gnal$SunHours[which(chamb15_Gnal$HOD>=7 & chamb15_Gnal$HOD<=19)] <- 'light'
chamb15_Gnal$SunHours[which((chamb15_Gnal$HOD>=20 & chamb15_Gnal$HOD<24) | (chamb15_Gnal$HOD>=0 & chamb15_Gnal$HOD<7))] <- 'dark'
#-------------------------------------------------------------------------------------------


ggplot(chamb10_Gnal,aes(x=Fecha,y=Flux.NO,col=PeriodDay))+ geom_point() 
