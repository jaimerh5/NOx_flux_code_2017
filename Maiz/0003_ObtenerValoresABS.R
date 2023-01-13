#############################################################
# JAIME RECIO HUETOS 
# ETSIA DE LA UPM; DEPARTAMENTO DE QUÍMICA Y TECNOLOGIA DE ALIMENTOS 
#18/12/2017
#############################################################
#############################################################
AMB_chamber1.net <- subset(chambAMB_Gnal, 
                       subset= (chambAMB_Gnal$HOD %in% seq(0,21,3) & chambAMB_Gnal$HourMinu %% 100==3 ),
                       select=names(chambAMB_Gnal)) 
nrow(AMB_chamber1.net)
nrow(chamb1_Gnal)
FindMissValues(chamb1_Gnal,AMB_chamber1.net)
# Lo que sale de esta función es una matriz donde te dice el número de datos que hay POR DIA en la caja (2da columna) 
# y el número de datos que hay POR DIA en el ambiente (4a columna), la 5a columna es 0 cuando son distintos
AMB_chamber1.net[AMB_chamber1.net$YMONDOY==20170629,]
chamb1_Gnal[chamb1_Gnal$YMONDOY==20170629,]

ayx2 <-  which(AMB_chamber1.net[['YMONDOY']]==20170629 & AMB_chamber1.net[['HOD']]!=21)              
AMB_chamber1.net <- AMB_chamber1.net[-ayx2,]
# AMB_chamber1 <- AMB_chamber1.net
Find.min <- which(AMB_chamber1.net$NOx==min(AMB_chamber1.net$NOx)); Find.min 
rdiff <- nrow(chamb1_Gnal)-nrow(AMB_chamber1.net)             ; rdiff
r.extra <- AMB_chamber1.net[Find.min,]                            ; r.extra

AMB_chamber1.net[diff(AMB_chamber1.net$Fecha)==6,]
#si te fijas en la línea anterior y la matriz que sale de FindMissValues ves que coincide
VectLost <- which(diff(AMB_chamber1.net$Fecha)==6) #este vector me da la posicion de los miss values
# AMB_chamber1 <- AMB_chamber1.net
w=0 
#este contador (w) me dice el número de filas que estoy insertando
#cada vez que se haga el bucle FOR insertaré una fila +, porlo tanto tendré que sumar 1 a w
for (v in 1:length(VectLost)) {
AMB_chamber1.net <- AMB_chamber1.net[c(1:(VectLost[v]+w),(VectLost[v]+w),(VectLost[v]+w+1):nrow(AMB_chamber1.net)) ,]
w=w+1
}
Find.min <- which(AMB_chamber1.net$NOx==min(AMB_chamber1.net$NOx)); row.min <- Find.min[length(Find.min)]
rdiff <- nrow(chamb1_Gnal)-nrow(AMB_chamber1.net)             ; rdiff
r.extra <- AMB_chamber1.net[Find.min,]                            ; r.extra
AMB_chamber1.net <- AMB_chamber1.net[c(1:nrow(AMB_chamber1.net),rep(row.min,rdiff)), ]
AMB_chamber1 <- AMB_chamber1.net
rm(AMB_chamber1.net)
chamb1_Gnal$NO_ABS <- (chamb1_Gnal$NO-AMB_chamber1$NO)
chamb1_Gnal$NO2_ABS <- (chamb1_Gnal$NO2-AMB_chamber1$NO2)
chamb1_Gnal$NOx_ABS <- (chamb1_Gnal$NOx-AMB_chamber1$NOx)
head(chamb1_Gnal)
##########################################################################################################################
#############################################################
AMB_chamber2.net <- subset(chambAMB_Gnal, 
                           subset= (chambAMB_Gnal$HOD %in% seq(0,21,3) & chambAMB_Gnal$HourMinu %% 100==15 ),
                           select=names(chambAMB_Gnal)) 
nrow(AMB_chamber2.net)
nrow(chamb2_Gnal)
FindMissValues(chamb2_Gnal,AMB_chamber2.net)
# Lo que sale de esta función es una matriz donde te dice el número de datos que hay POR DIA en la caja (2da columna) 
# y el número de datos que hay POR DIA en el ambiente (4a columna), la 5a columna es 0 cuando son distintos



AMB_chamber2.net[diff(AMB_chamber2.net$Fecha)>3,]
#si te fijas en la línea anterior y la matriz que sale de FindMissValues ves que coincide
VectLost <- which(diff(AMB_chamber2.net$Fecha)>3) #este vector me da la posicion de los miss values
# AMB_chamber2 <- AMB_chamber2.net
w=0 
#este contador (w) me dice el número de filas que estoy insertando
#cada vez que se haga el bucle FOR insertaré una fila +, porlo tanto tendré que sumar 1 a w
for (v in 1:length(VectLost)) {
  AMB_chamber2.net <- AMB_chamber2.net[c(1:(VectLost[v]+w),(VectLost[v]+w),(VectLost[v]+w+1):nrow(AMB_chamber2.net)) ,]
  w=w+1
}
Find.min <- which(AMB_chamber2.net$NOx==min(AMB_chamber2.net$NOx)); row.min <- Find.min[length(Find.min)]
rdiff <- nrow(chamb2_Gnal)-nrow(AMB_chamber2.net)             ; rdiff
r.extra <- AMB_chamber2.net[Find.min,]                            ; r.extra
AMB_chamber2.net <- AMB_chamber2.net[c(1:nrow(AMB_chamber2.net),rep(row.min,rdiff)), ]
AMB_chamber2 <- AMB_chamber2.net
rm(AMB_chamber2.net)
chamb2_Gnal$NO_ABS <- (chamb2_Gnal$NO-AMB_chamber2$NO)
chamb2_Gnal$NO2_ABS <- (chamb2_Gnal$NO2-AMB_chamber2$NO2)
chamb2_Gnal$NOx_ABS <- (chamb2_Gnal$NOx-AMB_chamber2$NOx)
head(chamb2_Gnal)
##########################################################################################################################
##########################################################################################################################
AMB_chamber3.net <- subset(chambAMB_Gnal, 
                           subset= (chambAMB_Gnal$HOD %in% seq(0,21,3) & chambAMB_Gnal$HourMinu %% 100==27 ),
                           select=names(chambAMB_Gnal)) 
nrow(AMB_chamber3.net)
nrow(chamb3_Gnal)
FindMissValues(chamb3_Gnal,AMB_chamber3.net)
# Lo que sale de esta función es una matriz donde te dice el número de datos que hay POR DIA en la caja (2da columna) 
# y el número de datos que hay POR DIA en el ambiente (4a columna), la 5a columna es 0 cuando son distintos



AMB_chamber3.net[diff(AMB_chamber3.net$Fecha)>3,]
#si te fijas en la línea anterior y la matriz que sale de FindMissValues ves que coincide

Find.min <- which(AMB_chamber3.net$NOx==min(AMB_chamber3.net$NOx)); row.min <- Find.min[length(Find.min)]
rdiff <- nrow(chamb3_Gnal)-nrow(AMB_chamber3.net)             ; rdiff
r.extra <- AMB_chamber3.net[row.min,]                            ; r.extra
VectLost <- which(diff(AMB_chamber3.net$Fecha)>3) #este vector me da la posicion de los miss values
# AMB_chamber2 <- AMB_chamber3.net
w=0 
#este contador (w) me dice el número de filas que estoy insertando
#cada vez que se haga el bucle FOR insertaré una fila +, porlo tanto tendré que sumar 1 a w
for (v in 1:(length(VectLost)-1)) {
  AMB_chamber3.net <- AMB_chamber3.net[c(1:(VectLost[v]+w),(VectLost[v]+w),(VectLost[v]+w+1):nrow(AMB_chamber3.net)) ,]
  w=w+1
}

AMB_chamber3<-AMB_chamber3.net
rm(AMB_chamber3.net)
chamb3_Gnal$NO_ABS <- (chamb3_Gnal$NO-AMB_chamber3$NO)
chamb3_Gnal$NO2_ABS <- (chamb3_Gnal$NO2-AMB_chamber3$NO2)
chamb3_Gnal$NOx_ABS <- (chamb3_Gnal$NOx-AMB_chamber3$NOx)
head(chamb3_Gnal)
##########################################################################################################################
AMB_chamber4.net <- subset(chambAMB_Gnal, 
                           subset= (chambAMB_Gnal$HOD %in% seq(0,21,3) & chambAMB_Gnal$HourMinu %% 100==39 ),
                           select=names(chambAMB_Gnal)) 
nrow(AMB_chamber4.net)
nrow(chamb4_Gnal)
FindMissValues(chamb4_Gnal,AMB_chamber4.net)
# Lo que sale de esta función es una matriz donde te dice el número de datos que hay POR DIA en la caja (2da columna) 
# y el número de datos que hay POR DIA en el ambiente (4a columna), la 5a columna es 0 cuando son distintos
Find.min <- which(AMB_chamber4.net$NOx==min(AMB_chamber4.net$NOx)); row.min <- Find.min[length(Find.min)]; row.min
rdiff <- nrow(chamb4_Gnal)-nrow(AMB_chamber4.net)             ; rdiff
r.extra <- AMB_chamber4.net[row.min,]                            ; r.extra

AMB_chamber4.net[diff(AMB_chamber4.net$Fecha)==6,]
#si te fijas en la línea anterior y la matriz que sale de FindMissValues ves que coincide
VectLost <- which(diff(AMB_chamber4.net$Fecha)==6) #este vector me da la posicion de los miss values
# AMB_chamber2 <- AMB_chamber4.net

w=0 
#este contador (w) me dice el número de filas que estoy insertando
#cada vez que se haga el bucle FOR insertaré una fila +, porlo tanto tendré que sumar 1 a w
for (v in 1:length(VectLost)) {
  AMB_chamber4.net <- AMB_chamber4.net[c(1:(VectLost[v]+w),(VectLost[v]+w),(VectLost[v]+w+1):nrow(AMB_chamber4.net)) ,]
  w=w+1
}
rdiff <- nrow(chamb4_Gnal)-nrow(AMB_chamber4.net)             ; rdiff
AMB_chamber4.net <- AMB_chamber4.net[c(1:nrow(AMB_chamber4.net),rep(row.min,rdiff)), ]
AMB_chamber4 <- AMB_chamber4.net
rm(AMB_chamber4.net)
chamb4_Gnal$NO_ABS <- (chamb4_Gnal$NO-AMB_chamber4$NO)
chamb4_Gnal$NO2_ABS <- (chamb4_Gnal$NO2-AMB_chamber4$NO2)
chamb4_Gnal$NOx_ABS <- (chamb4_Gnal$NOx-AMB_chamber4$NOx)
head(chamb4_Gnal)
##########################################################################################################################
##########################################################################################################################
AMB_chamber5.net <- subset(chambAMB_Gnal, 
                           subset= (chambAMB_Gnal$HOD %in% seq(0,21,3) & chambAMB_Gnal$HourMinu %% 100==51 ),
                           select=names(chambAMB_Gnal)) 
nrow(AMB_chamber5.net)
nrow(chamb5_Gnal)
FindMissValues(chamb5_Gnal,AMB_chamber5.net)
# Lo que sale de esta función es una matriz donde te dice el número de datos que hay POR DIA en la caja (2da columna) 
# y el número de datos que hay POR DIA en el ambiente (4a columna), la 5a columna es 0 cuando son distintos
Find.min <- which(AMB_chamber5.net$NOx==min(AMB_chamber5.net$NOx)); row.min <- Find.min[length(Find.min)]; row.min
rdiff <- nrow(chamb5_Gnal)-nrow(AMB_chamber5.net)             ; rdiff
r.extra <- AMB_chamber5.net[row.min,]                            ; r.extra

AMB_chamber5.net[diff(AMB_chamber5.net$Fecha)==6,]
#si te fijas en la línea anterior y la matriz que sale de FindMissValues ves que coincide
VectLost <- which(diff(AMB_chamber5.net$Fecha)==6) #este vector me da la posicion de los miss values
# AMB_chamber2 <- AMB_chamber5.net

w=0 
#este contador (w) me dice el número de filas que estoy insertando
#cada vez que se haga el bucle FOR insertaré una fila +, porlo tanto tendré que sumar 1 a w
for (v in 1:length(VectLost)) {
  AMB_chamber5.net <- AMB_chamber5.net[c(1:(VectLost[v]+w),(VectLost[v]+w),(VectLost[v]+w+1):nrow(AMB_chamber5.net)) ,]
  w=w+1
}
rdiff <- nrow(chamb5_Gnal)-nrow(AMB_chamber5.net)             ; rdiff
AMB_chamber5.net <- AMB_chamber5.net[c(1:nrow(AMB_chamber5.net),rep(row.min,rdiff)), ]
AMB_chamber5 <- AMB_chamber5.net
rm(AMB_chamber5.net)
chamb5_Gnal$NO_ABS <- (chamb5_Gnal$NO-AMB_chamber5$NO)
chamb5_Gnal$NO2_ABS <- (chamb5_Gnal$NO2-AMB_chamber5$NO2)
chamb5_Gnal$NOx_ABS <- (chamb5_Gnal$NOx-AMB_chamber5$NOx)
head(chamb5_Gnal)
##########################################################################################################################
##########################################################################################################################
AMB_chamber6.net <- subset(chambAMB_Gnal, 
                           subset= (chambAMB_Gnal$HOD %in% seq(1,22,3) & chambAMB_Gnal$HourMinu %% 100==3 ),
                           select=names(chambAMB_Gnal)) 
nrow(AMB_chamber6.net)
nrow(chamb6_Gnal)

FindMissValues(chamb6_Gnal,AMB_chamber6.net)
# Lo que sale de esta función es una matriz donde te dice el número de datos que hay POR DIA en la caja (2da columna) 
# y el número de datos que hay POR DIA en el ambiente (4a columna), la 5a columna es 0 cuando son distintos
Find.min <- which(AMB_chamber6.net$NOx==min(AMB_chamber6.net$NOx)); row.min <- Find.min[length(Find.min)]; row.min
rdiff <- nrow(chamb6_Gnal)-nrow(AMB_chamber6.net)             ; rdiff
r.extra <- AMB_chamber6.net[row.min,]                            ; r.extra

AMB_chamber6.net[diff(AMB_chamber6.net$Fecha)>3,]
#si te fijas en la línea anterior y la matriz que sale de FindMissValues ves que coincide
VectLost <- which(diff(AMB_chamber6.net$Fecha)>3) #este vector me da la posicion de los miss values
# AMB_chamber2 <- AMB_chamber6.net

w=0 
#este contador (w) me dice el número de filas que estoy insertando
#cada vez que se haga el bucle FOR insertaré una fila +, porlo tanto tendré que sumar 1 a w
for (v in 1:length(VectLost)) {
  AMB_chamber6.net <- AMB_chamber6.net[c(1:(VectLost[v]+w),(VectLost[v]+w),(VectLost[v]+w+1):nrow(AMB_chamber6.net)) ,]
  w=w+1
}
rdiff <- nrow(chamb6_Gnal)-nrow(AMB_chamber6.net)             ; rdiff
AMB_chamber6.net <- AMB_chamber6.net[c(1:nrow(AMB_chamber6.net),rep(row.min,rdiff)), ]
AMB_chamber6 <- AMB_chamber6.net
rm(AMB_chamber6.net)
chamb6_Gnal$NO_ABS <- (chamb6_Gnal$NO-AMB_chamber6$NO)
chamb6_Gnal$NO2_ABS <- (chamb6_Gnal$NO2-AMB_chamber6$NO2)
chamb6_Gnal$NOx_ABS <- (chamb6_Gnal$NOx-AMB_chamber6$NOx)
tail(chamb6_Gnal)
##########################################################################################################################
##########################################################################################################################
AMB_chamber7.net <- subset(chambAMB_Gnal, 
                           subset= (chambAMB_Gnal$HOD %in% seq(1,22,3) & chambAMB_Gnal$HourMinu %% 100==15 ),
                           select=names(chambAMB_Gnal)) 
nrow(AMB_chamber7.net)
nrow(chamb7_Gnal)

FindMissValues(chamb7_Gnal,AMB_chamber7.net)
# Lo que sale de esta función es una matriz donde te dice el número de datos que hay POR DIA en la caja (2da columna) 
# y el número de datos que hay POR DIA en el ambiente (4a columna), la 5a columna es 0 cuando son distintos


AMB_chamber7.net[diff(AMB_chamber7.net$Fecha)==6,]
#si te fijas en la línea anterior y la matriz que sale de FindMissValues ves que coincide
VectLost <- which(diff(AMB_chamber7.net$Fecha)==6) #este vector me da la posicion de los miss values
# AMB_chamber2 <- AMB_chamber7.net

w=0 
#este contador (w) me dice el número de filas que estoy insertando
#cada vez que se haga el bucle FOR insertaré una fila +, porlo tanto tendré que sumar 1 a w
for (v in 1:length(VectLost)) {
  AMB_chamber7.net <- AMB_chamber7.net[c(1:(VectLost[v]+w),(VectLost[v]+w),(VectLost[v]+w+1):nrow(AMB_chamber7.net)) ,]
  w=w+1
}
Find.min <- which(AMB_chamber7.net$NOx==min(AMB_chamber7.net$NOx)); row.min <- Find.min[length(Find.min)]; row.min
rdiff <- nrow(chamb7_Gnal)-nrow(AMB_chamber7.net)             ; rdiff
r.extra <- AMB_chamber7.net[row.min,]                            ; r.extra

AMB_chamber7.net <- AMB_chamber7.net[c(1:nrow(AMB_chamber7.net),rep(row.min,rdiff)), ]
AMB_chamber7 <- AMB_chamber7.net
rm(AMB_chamber7.net)
chamb7_Gnal$NO_ABS <- (chamb7_Gnal$NO-AMB_chamber7$NO)
chamb7_Gnal$NO2_ABS <- (chamb7_Gnal$NO2-AMB_chamber7$NO2)
chamb7_Gnal$NOx_ABS <- (chamb7_Gnal$NOx-AMB_chamber7$NOx)
head(chamb7_Gnal)
##########################################################################################################################
##########################################################################################################################
AMB_chamber8.net <- subset(chambAMB_Gnal, 
                           subset= (chambAMB_Gnal$HOD %in% seq(1,22,3) & chambAMB_Gnal$HourMinu %% 100==27),
                           select=names(chambAMB_Gnal)) 
nrow(AMB_chamber8.net)
nrow(chamb8_Gnal)

FindMissValues(chamb8_Gnal,AMB_chamber8.net)
# Lo que sale de esta función es una matriz donde te dice el número de datos que hay POR DIA en la caja (2da columna) 
# y el número de datos que hay POR DIA en el ambiente (4a columna), la 5a columna es 0 cuando son distintos


AMB_chamber8.net[diff(AMB_chamber8.net$Fecha)>=6,]
#si te fijas en la línea anterior y la matriz que sale de FindMissValues ves que coincide
VectLost <- which(diff(AMB_chamber8.net$Fecha)>=6) #este vector me da la posicion de los miss values


w=0 
#este contador (w) me dice el número de filas que estoy insertando
#cada vez que se haga el bucle FOR insertaré una fila +, porlo tanto tendré que sumar 1 a w
for (v in (1:(length(VectLost)-2))) {
  AMB_chamber8.net <- AMB_chamber8.net[c(1:(VectLost[v]+w),(VectLost[v]+w),(VectLost[v]+w+1):nrow(AMB_chamber8.net)) ,]
  w=w+1
}
Find.min <- which(AMB_chamber8.net$NOx==min(AMB_chamber8.net$NOx)); row.min <- Find.min[length(Find.min)]; row.min
rdiff <- nrow(chamb8_Gnal)-nrow(AMB_chamber8.net)             ; rdiff
r.extra <- AMB_chamber8.net[row.min,]                            ; r.extra

AMB_chamber8 <- AMB_chamber8.net
rm(AMB_chamber8.net)
chamb8_Gnal$NO_ABS <- (chamb8_Gnal$NO-AMB_chamber8$NO)
chamb8_Gnal$NO2_ABS <- (chamb8_Gnal$NO2-AMB_chamber8$NO2)
chamb8_Gnal$NOx_ABS <- (chamb8_Gnal$NOx-AMB_chamber8$NOx)
head(chamb8_Gnal)
##########################################################################################################################
AMB_chamber9.net <- subset(chambAMB_Gnal, 
                           subset= (chambAMB_Gnal$HOD %in% seq(1,22,3) & chambAMB_Gnal$HourMinu %% 100==39),
                           select=names(chambAMB_Gnal)) 
nrow(AMB_chamber9.net)
nrow(chamb9_Gnal)

FindMissValues(chamb9_Gnal,AMB_chamber9.net)
# Lo que sale de esta función es una matriz donde te dice el número de datos que hay POR DIA en la caja (2da columna) 
# y el número de datos que hay POR DIA en el ambiente (4a columna), la 5a columna es 0 cuando son distintos


AMB_chamber9.net[diff(AMB_chamber9.net$Fecha)>=6,]
#si te fijas en la línea anterior y la matriz que sale de FindMissValues ves que coincide
VectLost <- which(diff(AMB_chamber9.net$Fecha)>=6) #este vector me da la posicion de los miss values
# AMB_chamber2 <- AMB_chamber9.net

w=0 
#este contador (w) me dice el número de filas que estoy insertando
#cada vez que se haga el bucle FOR insertaré una fila +, porlo tanto tendré que sumar 1 a w
for (v in 1:(length(VectLost)-3)) {
  AMB_chamber9.net <- AMB_chamber9.net[c(1:(VectLost[v]+w),(VectLost[v]+w),(VectLost[v]+w+1):nrow(AMB_chamber9.net)) ,]
  w=w+1
}
Find.min <- which(AMB_chamber9.net$NOx==min(AMB_chamber9.net$NOx)); row.min <- Find.min[length(Find.min)]; row.min
rdiff <- nrow(chamb9_Gnal)-nrow(AMB_chamber9.net)             ; rdiff
r.extra <- AMB_chamber9.net[row.min,]                            ; r.extra

AMB_chamber9 <- AMB_chamber9.net
rm(AMB_chamber9.net)
chamb9_Gnal$NO_ABS <- (chamb9_Gnal$NO-AMB_chamber9$NO)
chamb9_Gnal$NO2_ABS <- (chamb9_Gnal$NO2-AMB_chamber9$NO2)
chamb9_Gnal$NOx_ABS <- (chamb9_Gnal$NOx-AMB_chamber9$NOx)
head(chamb9_Gnal)
##########################################################################################################################
AMB_chamber10.net <- subset(chambAMB_Gnal, 
                           subset= (chambAMB_Gnal$HOD %in% seq(1,22,3) & chambAMB_Gnal$HourMinu %% 100==51),
                           select=names(chambAMB_Gnal)) 
nrow(AMB_chamber10.net)
nrow(chamb10_Gnal)

FindMissValues(chamb10_Gnal,AMB_chamber10.net)
# Lo que sale de esta función es una matriz donde te dice el número de datos que hay POR DIA en la caja (2da columna) 
# y el número de datos que hay POR DIA en el ambiente (4a columna), la 5a columna es 0 cuando son distintos


AMB_chamber10.net[diff(AMB_chamber10.net$Fecha)>=6,]
#si te fijas en la línea anterior y la matriz que sale de FindMissValues ves que coincide
VectLost <- which(diff(AMB_chamber10.net$Fecha)>=6) #este vector me da la posicion de los miss values
# AMB_chamber2 <- AMB_chamber10.net
Find.min <- which(AMB_chamber10.net$NOx==min(AMB_chamber10.net$NOx)); row.min <- Find.min[length(Find.min)]; row.min
rdiff <- nrow(chamb10_Gnal)-nrow(AMB_chamber10.net)             ; rdiff
r.extra <- AMB_chamber10.net[row.min,]                           ; r.extra 
w=0 
#este contador (w) me dice el número de filas que estoy insertando
#cada vez que se haga el bucle FOR insertaré una fila +, porlo tanto tendré que sumar 1 a w
for (v in 1:length(VectLost)) {
  AMB_chamber10.net <- AMB_chamber10.net[c(1:(VectLost[v]+w),(VectLost[v]+w),(VectLost[v]+w+1):nrow(AMB_chamber10.net)) ,]
  w=w+1
}
rdiff <- nrow(chamb10_Gnal)-nrow(AMB_chamber10.net)             ; rdiff


AMB_chamber10.net <- AMB_chamber10.net[c(1:nrow(AMB_chamber10.net),rep(row.min,rdiff)), ]
AMB_chamber10 <- AMB_chamber10.net
rm(AMB_chamber10.net)
chamb10_Gnal$NO_ABS <- (chamb10_Gnal$NO-AMB_chamber10$NO)
chamb10_Gnal$NO2_ABS <- (chamb10_Gnal$NO2-AMB_chamber10$NO2)
chamb10_Gnal$NOx_ABS <- (chamb10_Gnal$NOx-AMB_chamber10$NOx)
head(chamb10_Gnal)
##########################################################################################################################
##########################################################################################################################
AMB_chamber11.net <- subset(chambAMB_Gnal, 
                            subset= (chambAMB_Gnal$HOD %in% seq(2,23,3) & chambAMB_Gnal$HourMinu %% 100==3),
                            select=names(chambAMB_Gnal)) 
nrow(AMB_chamber11.net)
nrow(chamb11_Gnal)
nrow(chamb11_Gnal)-nrow(AMB_chamber11.net)

FindMissValues(chamb11_Gnal,AMB_chamber11.net)
# Lo que sale de esta función es una matriz donde te dice el número de datos que hay POR DIA en la caja (2da columna) 
# y el número de datos que hay POR DIA en el ambiente (4a columna), la 5a columna es 0 cuando son distintos


AMB_chamber11.net[diff(AMB_chamber11.net$Fecha)>=6,]
#si te fijas en la línea anterior y la matriz que sale de FindMissValues ves que coincide
VectLost <- which(diff(AMB_chamber11.net$Fecha)>=6) #este vector me da la posicion de los miss values
# AMB_chamber2 <- AMB_chamber11.net
Find.min <- which(AMB_chamber11.net$NOx==min(AMB_chamber11.net$NOx)); row.min <- Find.min[length(Find.min)]; row.min
rdiff <- nrow(chamb11_Gnal)-nrow(AMB_chamber11.net)             ; rdiff
r.extra <- AMB_chamber11.net[row.min,]                           ; r.extra 
w=0 
#este contador (w) me dice el número de filas que estoy insertando
#cada vez que se haga el bucle FOR insertaré una fila +, porlo tanto tendré que sumar 1 a w
for (v in 1:(length(VectLost)-2)) {
  AMB_chamber11.net <- AMB_chamber11.net[c(1:(VectLost[v]+w),(VectLost[v]+w),(VectLost[v]+w+1):nrow(AMB_chamber11.net)) ,]
  w=w+1
}
rdiff <- nrow(chamb11_Gnal)-nrow(AMB_chamber11.net)             ; rdiff


# AMB_chamber11.net <- AMB_chamber11.net[c(1:nrow(AMB_chamber11.net),rep(row.min,rdiff)), ]
AMB_chamber11 <- AMB_chamber11.net
rm(AMB_chamber11.net)
chamb11_Gnal$NO_ABS <- (chamb11_Gnal$NO-AMB_chamber11$NO)
chamb11_Gnal$NO2_ABS <- (chamb11_Gnal$NO2-AMB_chamber11$NO2)
chamb11_Gnal$NOx_ABS <- (chamb11_Gnal$NOx-AMB_chamber11$NOx)
head(chamb11_Gnal)
##########################################################################################################################
AMB_chamber12.net <- subset(chambAMB_Gnal, 
                            subset= (chambAMB_Gnal$HOD %in% seq(2,23,3) & chambAMB_Gnal$HourMinu %% 100==15),
                            select=names(chambAMB_Gnal)) 
nrow(AMB_chamber12.net)
nrow(chamb12_Gnal)
nrow(chamb12_Gnal)-nrow(AMB_chamber12.net)
FindMissValues(chamb12_Gnal,AMB_chamber12.net)
# Lo que sale de esta función es una matriz donde te dice el número de datos que hay POR DIA en la caja (2da columna) 
# y el número de datos que hay POR DIA en el ambiente (4a columna), la 5a columna es 0 cuando son distintos


AMB_chamber12.net[diff(AMB_chamber12.net$Fecha)>=6,]
nrow(AMB_chamber12.net[diff(AMB_chamber12.net$Fecha)>=6,])
#si te fijas en la línea anterior y la matriz que sale de FindMissValues ves que coincide
VectLost <- which(diff(AMB_chamber12.net$Fecha)>=6) #este vector me da la posicion de los miss values
# AMB_chamber2 <- AMB_chamber12.net
Find.min <- which(AMB_chamber12.net$NOx==min(AMB_chamber12.net$NOx)); row.min <- Find.min[length(Find.min)]; row.min
rdiff <- nrow(chamb12_Gnal)-nrow(AMB_chamber12.net)             ; rdiff
r.extra <- AMB_chamber12.net[row.min,]                           ; r.extra 
w=0 
#este contador (w) me dice el número de filas que estoy insertando
#cada vez que se haga el bucle FOR insertaré una fila +, porlo tanto tendré que sumar 1 a w
for (v in 1:(length(VectLost)-2)) {
  AMB_chamber12.net <- AMB_chamber12.net[c(1:(VectLost[v]+w),(VectLost[v]+w),(VectLost[v]+w+1):nrow(AMB_chamber12.net)) ,]
  w=w+1
}
rdiff <- nrow(chamb12_Gnal)-nrow(AMB_chamber12.net)             ; rdiff


#AMB_chamber12.net <- AMB_chamber12.net[c(1:nrow(AMB_chamber12.net),rep(row.min,rdiff)), ]
AMB_chamber12 <- AMB_chamber12.net
rm(AMB_chamber12.net)
chamb12_Gnal$NO_ABS <- (chamb12_Gnal$NO-AMB_chamber12$NO)
chamb12_Gnal$NO2_ABS <- (chamb12_Gnal$NO2-AMB_chamber12$NO2)
chamb12_Gnal$NOx_ABS <- (chamb12_Gnal$NOx-AMB_chamber12$NOx)
head(chamb12_Gnal)
##########################################################################################################################
##########################################################################################################################
AMB_chamber13.net <- subset(chambAMB_Gnal, 
                            subset= (chambAMB_Gnal$HOD %in% seq(2,23,3) & chambAMB_Gnal$HourMinu %% 100==27),
                            select=names(chambAMB_Gnal)) 
nrow(AMB_chamber13.net)
nrow(chamb13_Gnal)
nrow(chamb13_Gnal)-nrow(AMB_chamber13.net)
FindMissValues(chamb13_Gnal,AMB_chamber13.net)
# Lo que sale de esta función es una matriz donde te dice el número de datos que hay POR DIA en la caja (2da columna) 
# y el número de datos que hay POR DIA en el ambiente (4a columna), la 5a columna es 0 cuando son distintos


AMB_chamber13.net[diff(AMB_chamber13.net$Fecha)>=6,]
nrow(AMB_chamber13.net[diff(AMB_chamber13.net$Fecha)>=6,])
#si te fijas en la línea anterior y la matriz que sale de FindMissValues ves que coincide
VectLost <- which(diff(AMB_chamber13.net$Fecha)>=6) #este vector me da la posicion de los miss values
# AMB_chamber2 <- AMB_chamber13.net
Find.min <- which(AMB_chamber13.net$NOx==min(AMB_chamber13.net$NOx)); row.min <- Find.min[length(Find.min)]; row.min
rdiff <- nrow(chamb13_Gnal)-nrow(AMB_chamber13.net)             ; rdiff
r.extra <- AMB_chamber13.net[row.min,]                           ; r.extra 
w=0 
#este contador (w) me dice el número de filas que estoy insertando
#cada vez que se haga el bucle FOR insertaré una fila +, porlo tanto tendré que sumar 1 a w
for (v in 1:length(VectLost)) {
  AMB_chamber13.net <- AMB_chamber13.net[c(1:(VectLost[v]+w),(VectLost[v]+w),(VectLost[v]+w+1):nrow(AMB_chamber13.net)) ,]
  w=w+1
}
rdiff <- nrow(chamb13_Gnal)-nrow(AMB_chamber13.net)             ; rdiff


#AMB_chamber13.net <- AMB_chamber13.net[c(1:nrow(AMB_chamber13.net),rep(row.min,rdiff)), ]
AMB_chamber13 <- AMB_chamber13.net
rm(AMB_chamber13.net)
chamb13_Gnal$NO_ABS <- (chamb13_Gnal$NO-AMB_chamber13$NO)
chamb13_Gnal$NO2_ABS <- (chamb13_Gnal$NO2-AMB_chamber13$NO2)
chamb13_Gnal$NOx_ABS <- (chamb13_Gnal$NOx-AMB_chamber13$NOx)
head(chamb13_Gnal)
##########################################################################################################################
##########################################################################################################################
AMB_chamber14.net <- subset(chambAMB_Gnal, 
                            subset= (chambAMB_Gnal$HOD %in% seq(2,23,3) & chambAMB_Gnal$HourMinu %% 100==39),
                            select=names(chambAMB_Gnal)) 
nrow(AMB_chamber14.net)
nrow(chamb14_Gnal)
nrow(chamb14_Gnal)-nrow(AMB_chamber14.net)
FindMissValues(chamb14_Gnal,AMB_chamber14.net)
# Lo que sale de esta función es una matriz donde te dice el número de datos que hay POR DIA en la caja (2da columna) 
# y el número de datos que hay POR DIA en el ambiente (4a columna), la 5a columna es 0 cuando son distintos

AMB_chamber14.net[diff(AMB_chamber14.net$Fecha)>=6,]
nrow(AMB_chamber14.net[diff(AMB_chamber14.net$Fecha)==6,])
#si te fijas en la línea anterior y la matriz que sale de FindMissValues ves que coincide
VectLost <- which(diff(AMB_chamber14.net$Fecha)==6) #este vector me da la posicion de los miss values
# AMB_chamber2 <- AMB_chamber14.net
Find.min <- which(AMB_chamber14.net$NOx==min(AMB_chamber14.net$NOx)); row.min <- Find.min[length(Find.min)]; row.min
rdiff <- nrow(chamb14_Gnal)-nrow(AMB_chamber14.net)             ; rdiff
r.extra <- AMB_chamber14.net[row.min,]                           ; r.extra 
w=0 
#este contador (w) me dice el número de filas que estoy insertando
#cada vez que se haga el bucle FOR insertaré una fila +, porlo tanto tendré que sumar 1 a w
for (v in 1:length(VectLost)) {
  AMB_chamber14.net <- AMB_chamber14.net[c(1:(VectLost[v]+w),(VectLost[v]+w),(VectLost[v]+w+1):nrow(AMB_chamber14.net)) ,]
  w=w+1
}
rdiff <- nrow(chamb14_Gnal)-nrow(AMB_chamber14.net)             ; rdiff


#AMB_chamber14.net <- AMB_chamber14.net[c(1:nrow(AMB_chamber14.net),rep(row.min,rdiff)), ]
AMB_chamber14 <- AMB_chamber14.net
rm(AMB_chamber14.net)
chamb14_Gnal$NO_ABS <- (chamb14_Gnal$NO-AMB_chamber14$NO)
chamb14_Gnal$NO2_ABS <- (chamb14_Gnal$NO2-AMB_chamber14$NO2)
chamb14_Gnal$NOx_ABS <- (chamb14_Gnal$NOx-AMB_chamber14$NOx)
head(chamb14_Gnal)
##########################################################################################################################
##########################################################################################################################
AMB_chamber15.net <- subset(chambAMB_Gnal, 
                            subset= (chambAMB_Gnal$HOD %in% seq(2,23,3) & chambAMB_Gnal$HourMinu %% 100==51),
                            select=names(chambAMB_Gnal)) 
nrow(AMB_chamber15.net)
nrow(chamb15_Gnal)
nrow(chamb15_Gnal)-nrow(AMB_chamber15.net)
FindMissValues(chamb15_Gnal,AMB_chamber15.net)
# Lo que sale de esta función es una matriz donde te dice el número de datos que hay POR DIA en la caja (2da columna) 
# y el número de datos que hay POR DIA en el ambiente (4a columna), la 5a columna es 0 cuando son distintos

AMB_chamber15.net[diff(AMB_chamber15.net$Fecha)>=6,]
nrow(AMB_chamber15.net[diff(AMB_chamber15.net$Fecha)>=6,])
#si te fijas en la línea anterior y la matriz que sale de FindMissValues ves que coincide
VectLost <- which(diff(AMB_chamber15.net$Fecha)>=6) #este vector me da la posicion de los miss values
# AMB_chamber2 <- AMB_chamber15.net
Find.min <- which(AMB_chamber15.net$NOx==min(AMB_chamber15.net$NOx)); row.min <- Find.min[length(Find.min)]; row.min
rdiff <- nrow(chamb15_Gnal)-nrow(AMB_chamber15.net)             ; rdiff
r.extra <- AMB_chamber15.net[row.min,]                           ; r.extra 
w=0 
#este contador (w) me dice el número de filas que estoy insertando
#cada vez que se haga el bucle FOR insertaré una fila +, porlo tanto tendré que sumar 1 a w
for (v in 1:length(VectLost)) {
  AMB_chamber15.net <- AMB_chamber15.net[c(1:(VectLost[v]+w),(VectLost[v]+w),(VectLost[v]+w+1):nrow(AMB_chamber15.net)) ,]
  w=w+1
}
rdiff <- nrow(chamb15_Gnal)-nrow(AMB_chamber15.net)             ; rdiff


#AMB_chamber15.net <- AMB_chamber15.net[c(1:nrow(AMB_chamber15.net),rep(row.min,rdiff)), ]
AMB_chamber15 <- AMB_chamber15.net[-c(82,413,428),]
rm(AMB_chamber15.net)
chamb15_Gnal$NO_ABS <- (chamb15_Gnal$NO-AMB_chamber15$NO)
chamb15_Gnal$NO2_ABS <- (chamb15_Gnal$NO2-AMB_chamber15$NO2)
chamb15_Gnal$NOx_ABS <- (chamb15_Gnal$NOx-AMB_chamber15$NOx)
head(chamb15_Gnal)
##########################################################################################################################







