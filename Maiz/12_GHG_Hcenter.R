## mg m-2 d-1


setwd('C:/R/Expmto2017/MAIZ/EurochemScripts/N2O')
dir()
GHG_H.out <- read.table("GHG_H.csv",header = T,sep = ';', dec=',')
dim(GHG_H.out)
names(GHG_H.out)[1] <- 'Fecha';
date_format2<-"%d/%m/%Y"
GHG_H.out$Fecha <-as.POSIXct (GHG_H.out$Fecha,
                              format= date_format2, tz = 'GMT')
#########################################################################
GHG.Urea <-GHG_H.out[,c('Fecha','H5','H9','H12')] 
GHG.Urea$Avrg.U <- apply(GHG.Urea[,2:4],1,FUN = mean)
GHG.Urea$SD.U <- apply(GHG.Urea[,2:4],1,FUN = sd)
head(GHG.Urea)
#########################################################################
GHG.UNI <-GHG_H.out[,c('Fecha','H3','H10','H14')] 
GHG.UNI$Avrg.UNI <- apply(GHG.UNI[,2:4],1,FUN = mean)
GHG.UNI$SD.UNI <- apply(GHG.UNI[,2:4],1,FUN = sd)
head(GHG.UNI)
#########################################################################
GHG.UUI <-GHG_H.out[,c('Fecha','H2','H6','H15')] 
GHG.UUI$Avrg.UUI <- apply(GHG.UUI[,2:4],1,FUN = mean,na.rm=T )
GHG.UUI$SD.UUI <- apply(GHG.UUI[,2:4],1,FUN = sd,na.rm=T )
head(GHG.UUI)
#########################################################################
GHG.U2I <-GHG_H.out[,c('Fecha','H4','H7','H11')] 
GHG.U2I$Avrg.U2I <- apply(GHG.U2I[,2:4],1,FUN = mean)
GHG.U2I$SD.U2I <- apply(GHG.U2I[,2:4],1,FUN = sd)
head(GHG.U2I)
#########################################################################
GHG.Con <-GHG_H.out[,c('Fecha','H1','H8','H13')] 
GHG.Con$Avrg.Con <- apply(GHG.Con[,2:4],1,FUN = mean)
GHG.Con$SD.Con <- apply(GHG.Con[,2:4],1,FUN = sd)
head(GHG.Con)
#########################################################################.
#########################################################################
ALL.GHG <- cbind(GHG.Urea[,c(1,5,6)],GHG.UNI[,5:6],GHG.UUI[,5:6],GHG.U2I[,5:6],GHG.Con[,5:6])
ALL.GHG$YMONDOY <- as.numeric(format(GHG_H.out$Fecha,format='%Y%m%d' )); 
library(lattice);library(RColorBrewer); library(latticeExtra);library(ggplot2)
# c1 <- xyplot(Avrg.U~Fecha,data=ALL.GHG, col='red',ylim = )
# c2 <-  xyplot(Mean[1:46]~Fecha[1:46],data=ttoU,col='black')
# c1+as.layer(c2)
c <- NULL
ttoU$YMONDOY <-  as.numeric(format(ttoU$Fecha,format='%Y%m%d' ))
for (g in 1:59) {
  for (j in 1:14) {
   if (ttoU$YMONDOY[g]==ALL.GHG$YMONDOY[j]) {  
     c=c+1}}}
# ALL.GHG$YMONDOY[2]==ttoU$YMONDOY[3]
