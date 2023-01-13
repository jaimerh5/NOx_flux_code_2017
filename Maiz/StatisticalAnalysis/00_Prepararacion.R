
###################################################################################
setwd('C:/R/Expmto2017/MAIZ/EurochemScripts/StatisticalAnalysis')
dir()
MatrizClima <- read.table("ClimateData_ExpH.csv",dec='.', sep=';',header = T)
MatrizClima$Fecha=as.POSIXct(MatrizClima$Fecha,format='%d/%m/%Y')
MatrizClima$YMONDOY=as.numeric(format(MatrizClima$Fecha,format="%Y%m%d"))
summary(MatrizClima)
todos=NULL;ay2=NULL;Missed=NULL
Days <- ttoControl_DayMean$YMONDOY
gg <- MatrizClima$YMONDOY
for (k in 1:length(Days)){
Missed <- which(MatrizClima$YMONDOY==Days[k])
todos <- c(todos,Missed)}
MatrizClimaDef <- MatrizClima[todos,]

ttoControl_DayMean$Tsoil10 <- MatrizClimaDef$Tsoil10;ttoControl_DayMean$Precip <- MatrizClimaDef$Precip;ttoControl_DayMean$Irri <- MatrizClimaDef$Irri
ttoU_DayMean$Tsoil10 <- MatrizClimaDef$Tsoil10;ttoU_DayMean$Precip <- MatrizClimaDef$Precip;ttoU_DayMean$Irri <- MatrizClimaDef$Irri
ttoU.NI_DayMean$Tsoil10 <- MatrizClimaDef$Tsoil10;ttoU.NI_DayMean$Precip <- MatrizClimaDef$Precip;ttoU.NI_DayMean$Irri <- MatrizClimaDef$Irri
ttoU.UI_DayMean$Tsoil10 <- MatrizClimaDef$Tsoil10;ttoU.UI_DayMean$Precip <- MatrizClimaDef$Precip;ttoU.UI_DayMean$Irri <- MatrizClimaDef$Irri
ttoU.2I_DayMean$Tsoil10 <- MatrizClimaDef$Tsoil10;ttoU.2I_DayMean$Precip <- MatrizClimaDef$Precip;ttoU.2I_DayMean$Irri <- MatrizClimaDef$Irri

#########################
# RManova
WeekUrea <- aggregate(MatrizUrea,
                      by = list('Week'= format(MatrizUrea$Fecha,'%Y%W' )),
                      FUN = mean, na.rm=T )
WeekUrea$WOY <-  as.numeric(format(WeekUrea$Fecha,format="%W"))
# WeekUrea$Tsoil10 <-aggregate(ttoU_DayMean$Tsoil10,
#                              by = list('Week'= format(ttoU_DayMean$Fecha,'%Y%W' )),
#                              FUN = mean, na.rm=T )[,2]
View(WeekUrea)
WeekUNI <- aggregate(MatrizU.NI,
                     by = list('Week'= format(MatrizU.NI$Fecha,'%Y%W' )),
                     FUN = mean, na.rm=T )
WeekUNI$WOY <-  as.numeric(format(WeekUNI$Fecha,format="%W"))
View(WeekUNI)
WeekUUI <- aggregate(MatrizU.UI,
                     by = list('Week'= format(MatrizU.UI$Fecha,'%Y%W' )),
                     FUN = mean, na.rm=T )
WeekUUI$WOY <-  as.numeric(format(WeekUUI$Fecha,format="%W"))
View(WeekUUI)
WeekU2I <- aggregate(MatrizU.2I,
                     by = list('Week'= format(MatrizU.2I$Fecha,'%Y%W' )),
                     FUN = mean, na.rm=T )
WeekU2I$WOY <-  as.numeric(format(WeekU2I$Fecha,format="%W"))
View(WeekU2I)
WeekControl <- aggregate(MatrizControl,
                         by = list('Week'= format(MatrizControl$Fecha,'%Y%W' )),
                         FUN = mean, na.rm=T )
WeekControl$WOY <-  as.numeric(format(WeekControl$Fecha,format="%W"))
View(WeekControl)

plot(WeekUrea$WOY,
     WeekUrea$Mean.Urea.NO, ty='l')
