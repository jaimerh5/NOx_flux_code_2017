#################################################
# JAIME RECIO HUETOS 
# ETSIA DE LA UPM; DEPARTAMENTO DE QUÍMICA Y TECNOLOGIA DE ALIMENTOS 
#13/09/2017
#############################################################
##Valores medios por día
GAPtime1 <- chamb1_Gnal[['Fecha']]

DayMean_ch1 <-aggregate(chamb1_Gnal,
                              by = list('YDAY'= format(GAPtime1,'%Y%m%d' )),
                              FUN = mean, na.rm=T )

#############################################################
GAPtime2 <- chamb2_Gnal[['Fecha']]

DayMean_ch2 <-aggregate(chamb2_Gnal,
                              by = list('YDAY'= format(GAPtime2,'%Y%m%d' )),
                              FUN = mean, na.rm=T )
#############################################################
GAPtime3 <- chamb3_Gnal[['Fecha']]

DayMean_ch3 <-      aggregate(chamb3_Gnal,
                              by = list('YDAY'= format(GAPtime3,'%Y%m%d' )),
                              FUN = mean, na.rm=T )
#############################################################
GAPtime4 <- chamb4_Gnal[['Fecha']]

DayMean_ch4 <-      aggregate(chamb4_Gnal,
                              by = list('YDAY'= format(GAPtime4,'%Y%m%d' )),
                              FUN = mean, na.rm=T )
#############################################################
GAPtime5 <- chamb5_Gnal[['Fecha']]

DayMean_ch5 <-      aggregate(chamb5_Gnal,
                              by = list('YDAY'= format(GAPtime5,'%Y%m%d' )),
                              FUN = mean, na.rm=T )
#############################################################
GAPtime6 <- chamb6_Gnal[['Fecha']]

DayMean_ch6 <-      aggregate(chamb6_Gnal,
                              by = list('YDAY'= format(GAPtime6,'%Y%m%d' )),
                              FUN = mean, na.rm=T )
#############################################################
GAPtime7 <- chamb7_Gnal[['Fecha']]

DayMean_ch7 <-      aggregate(chamb7_Gnal,
                              by = list('YDAY'= format(GAPtime7,'%Y%m%d' )),
                              FUN = mean, na.rm=T )
#############################################################
GAPtime8 <- chamb8_Gnal[['Fecha']]

DayMean_ch8 <-      aggregate(chamb8_Gnal,
                              by = list('YDAY'= format(GAPtime8,'%Y%m%d' )),
                              FUN = mean, na.rm=T )
#############################################################
GAPtime9 <- chamb9_Gnal[['Fecha']]

DayMean_ch9 <-      aggregate(chamb9_Gnal,
                              by = list('YDAY'= format(GAPtime9,'%Y%m%d' )),
                              FUN = mean, na.rm=T )
#############################################################
GAPtime10 <- chamb10_Gnal[['Fecha']]

DayMean_ch10 <-      aggregate(chamb10_Gnal,
                              by = list('YDAY'= format(GAPtime10,'%Y%m%d' )),
                              FUN = mean, na.rm=T )
#############################################################
GAPtime11 <- chamb11_Gnal[['Fecha']]

DayMean_ch11 <-      aggregate(chamb11_Gnal,
                              by = list('YDAY'= format(GAPtime11,'%Y%m%d' )),
                              FUN = mean, na.rm=T )
#############################################################
GAPtime12 <- chamb12_Gnal[['Fecha']]

DayMean_ch12 <-      aggregate(chamb12_Gnal,
                              by = list('YDAY'= format(GAPtime12,'%Y%m%d' )),
                              FUN = mean, na.rm=T )
#############################################################
GAPtime13 <- chamb13_Gnal[['Fecha']]

DayMean_ch13 <-      aggregate(chamb13_Gnal,
                              by = list('YDAY'= format(GAPtime13,'%Y%m%d' )),
                              FUN = mean, na.rm=T )
#############################################################
GAPtime14 <- chamb14_Gnal[['Fecha']]

DayMean_ch14 <-      aggregate(chamb14_Gnal,
                              by = list('YDAY'= format(GAPtime14,'%Y%m%d' )),
                              FUN = mean, na.rm=T )
#############################################################
GAPtime15 <- chamb15_Gnal[['Fecha']]

DayMean_ch15 <-      aggregate(chamb15_Gnal,
                              by = list('YDAY'= format(GAPtime15,'%Y%m%d' )),
                              FUN = mean, na.rm=T )