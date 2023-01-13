#################################################
# JAIME RECIO HUETOS 
# ETSIA DE LA UPM; DEPARTAMENTO DE QUÍMICA Y TECNOLOGIA DE ALIMENTOS 
#10/12/2018
#############################################################

##CONTROL
Cum.ttoControl_DayMean <- aggregate(CumMatrizControl[,1:(ncol(CumMatrizControl)-2)],
                                by = list('YDAY'= format(CumMatrizControl$Fecha,'%Y%m%d' )),
                                FUN = mean, na.rm=T )
# con este aggregate la cagas con la standard deviation 
# tienes q crear otro aggregate con la funcion sd y pegarle un solo vector (mean.Control) a sd
auxControl <- aggregate(CumMatrizControl[,1:(ncol(CumMatrizControl)-2)],
                        by = list('YDAY'= format(CumMatrizControl$Fecha,'%Y%m%d' )),
                        FUN = sd, na.rm=T )
sd.new=auxControl$Mean.Control
sd.new.NO=auxControl$Mean.Control.NO
sd.new.NO2=auxControl$Mean.Control.NO2

Cum.ttoControl_DayMean$SD.Control <- sd.new
Cum.ttoControl_DayMean$SD.Control.NO <- sd.new.NO
Cum.ttoControl_DayMean$SD.Control.NO2 <- sd.new.NO2
########################################################
##Urea

Cum.ttoU_DayMean <- aggregate(CumMatrizUrea[,1:(ncol(CumMatrizUrea)-2)],
                          by = list('YDAY'= format(CumMatrizUrea$Fecha,'%Y%m%d' )),
                          FUN = mean, na.rm=T )
# con este aggregate la cagas con la standard deviation 
# tienes q crear otro aggregate con la funcion sd y pegarle un solo vector (mean.urea) a sd
auxUrea <- aggregate(CumMatrizUrea[,1:(ncol(CumMatrizUrea)-2)],
                     by = list('YDAY'= format(CumMatrizUrea$Fecha,'%Y%m%d' )),
                     FUN = sd, na.rm=T )
sd.new=auxUrea$Mean.Urea
sd.new.NO=auxUrea$Mean.Urea.NO
sd.new.NO2=auxUrea$Mean.Urea.NO2

Cum.ttoU_DayMean$SD.Urea <- sd.new
Cum.ttoU_DayMean$SD.Urea.NO <- sd.new.NO
Cum.ttoU_DayMean$SD.Urea.NO2 <- sd.new.NO2

########################################################
##Urea+DMPSA

Cum.ttoU.NI_DayMean <- aggregate(CumMatrizU.NI[,1:(ncol(CumMatrizU.NI)-2)],
                             by = list('YDAY'= format(CumMatrizU.NI$Fecha,'%Y%m%d' )),
                             FUN = mean, na.rm=T )
# con este aggregate la cagas con la standard deviation 
# tienes q crear otro aggregate con la funcion sd y pegarle un solo vector (mean.U.NI) a sd
auxU.NI <- aggregate(CumMatrizU.NI[,1:(ncol(CumMatrizU.NI)-2)],
                     by = list('YDAY'= format(CumMatrizU.NI$Fecha,'%Y%m%d' )),
                     FUN = sd, na.rm=T )
sd.new=auxU.NI$Mean.U.NI
sd.new.NO=auxU.NI$Mean.U.NI.NO
sd.new.NO2=auxU.NI$Mean.U.NI.NO2

Cum.ttoU.NI_DayMean$SD.U.NI <- sd.new
Cum.ttoU.NI_DayMean$SD.U.NI.NO <- sd.new.NO
Cum.ttoU.NI_DayMean$SD.U.NI.NO2 <- sd.new.NO2
########################################################
##Urea + NBPT

Cum.ttoU.UI_DayMean <- aggregate(CumMatrizU.UI[,1:(ncol(CumMatrizU.UI)-2)],
                             by = list('YDAY'= format(CumMatrizU.UI$Fecha,'%Y%m%d' )),
                             FUN = mean, na.rm=T )
# con este aggregate la cagas con la standard deviation 
# tienes q crear otro aggregate con la funcion sd y pegarle un solo vector (mean.U.UI) a sd
auxU.UI <- aggregate(CumMatrizU.UI[,1:(ncol(CumMatrizU.UI)-2)],
                     by = list('YDAY'= format(CumMatrizU.UI$Fecha,'%Y%m%d' )),
                     FUN = sd, na.rm=T )
sd.new=auxU.UI$Mean.U.UI
sd.new.NO=auxU.UI$Mean.U.UI.NO
sd.new.NO2=auxU.UI$Mean.U.UI.NO2

Cum.ttoU.UI_DayMean$SD.U.UI <- sd.new
Cum.ttoU.UI_DayMean$SD.U.UI.NO <- sd.new.NO
Cum.ttoU.UI_DayMean$SD.U.UI.NO2 <- sd.new.NO2
########################################################
##Urea+ DMPSA+NBPT

Cum.ttoU.2I_DayMean <- aggregate(CumMatrizU.2I[,1:(ncol(CumMatrizU.2I)-2)],
                             by = list('YDAY'= format(CumMatrizU.2I$Fecha,'%Y%m%d' )),
                             FUN = mean, na.rm=T )
# con este aggregate la cagas con la standard deviation 
# tienes q crear otro aggregate con la funcion sd y pegarle un solo vector (mean.U.2I) a sd
auxU.2I <- aggregate(CumMatrizU.2I[,1:(ncol(CumMatrizU.2I)-2)],
                     by = list('YDAY'= format(CumMatrizU.2I$Fecha,'%Y%m%d' )),
                     FUN = sd, na.rm=T )
sd.new=auxU.2I$Mean.U.2I
sd.new.NO=auxU.2I$Mean.U.2I.NO
sd.new.NO2=auxU.2I$Mean.U.2I.NO2

Cum.ttoU.2I_DayMean$SD.U.2I <- sd.new
Cum.ttoU.2I_DayMean$SD.U.2I.NO <- sd.new.NO
Cum.ttoU.2I_DayMean$SD.U.2I.NO2 <- sd.new.NO2
