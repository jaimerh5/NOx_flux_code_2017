########################################################
##CONTROL
ttoControl_DayMean <- aggregate(MatrizControl[,1:(ncol(MatrizControl)-2)],
                          by = list('YDAY'= format(MatrizControl$Fecha,'%Y%m%d' )),
                          FUN = mean, na.rm=T )
# con este aggregate la cagas con la standard deviation 
# tienes q crear otro aggregate con la funcion sd y pegarle un solo vector (mean.Control) a sd
auxControl <- aggregate(MatrizControl[,1:(ncol(MatrizControl)-2)],
                     by = list('YDAY'= format(MatrizControl$Fecha,'%Y%m%d' )),
                     FUN = sd, na.rm=T )
sd.new=auxControl$Mean.Control
sd.new.NO=auxControl$Mean.Control.NO
sd.new.NO2=auxControl$Mean.Control.NO2

ttoControl_DayMean$SD.Control <- sd.new
ttoControl_DayMean$SD.Control.NO <- sd.new.NO
ttoControl_DayMean$SD.Control.NO2 <- sd.new.NO2
########################################################
##Urea

ttoU_DayMean <- aggregate(MatrizUrea[,1:(ncol(MatrizUrea)-2)],
                          by = list('YDAY'= format(MatrizUrea$Fecha,'%Y%m%d' )),
                          FUN = mean, na.rm=T )
# con este aggregate la cagas con la standard deviation 
# tienes q crear otro aggregate con la funcion sd y pegarle un solo vector (mean.urea) a sd
auxUrea <- aggregate(MatrizUrea[,1:(ncol(MatrizUrea)-2)],
                     by = list('YDAY'= format(MatrizUrea$Fecha,'%Y%m%d' )),
                     FUN = sd, na.rm=T )
sd.new=auxUrea$Mean.Urea
sd.new.NO=auxUrea$Mean.Urea.NO
sd.new.NO2=auxUrea$Mean.Urea.NO2

ttoU_DayMean$SD.Urea <- sd.new
ttoU_DayMean$SD.Urea.NO <- sd.new.NO
ttoU_DayMean$SD.Urea.NO2 <- sd.new.NO2

########################################################
##Urea+DMPSA

ttoU.NI_DayMean <- aggregate(MatrizU.NI[,1:(ncol(MatrizU.NI)-2)],
                          by = list('YDAY'= format(MatrizU.NI$Fecha,'%Y%m%d' )),
                          FUN = mean, na.rm=T )
# con este aggregate la cagas con la standard deviation 
# tienes q crear otro aggregate con la funcion sd y pegarle un solo vector (mean.U.NI) a sd
auxU.NI <- aggregate(MatrizU.NI[,1:(ncol(MatrizU.NI)-2)],
                     by = list('YDAY'= format(MatrizU.NI$Fecha,'%Y%m%d' )),
                     FUN = sd, na.rm=T )
sd.new=auxU.NI$Mean.U.NI
sd.new.NO=auxU.NI$Mean.U.NI.NO
sd.new.NO2=auxU.NI$Mean.U.NI.NO2

ttoU.NI_DayMean$SD.U.NI <- sd.new
ttoU.NI_DayMean$SD.U.NI.NO <- sd.new.NO
ttoU.NI_DayMean$SD.U.NI.NO2 <- sd.new.NO2
########################################################
##Urea + NBPT

ttoU.UI_DayMean <- aggregate(MatrizU.UI[,1:(ncol(MatrizU.UI)-2)],
                          by = list('YDAY'= format(MatrizU.UI$Fecha,'%Y%m%d' )),
                          FUN = mean, na.rm=T )
# con este aggregate la cagas con la standard deviation 
# tienes q crear otro aggregate con la funcion sd y pegarle un solo vector (mean.U.UI) a sd
auxU.UI <- aggregate(MatrizU.UI[,1:(ncol(MatrizU.UI)-2)],
                     by = list('YDAY'= format(MatrizU.UI$Fecha,'%Y%m%d' )),
                     FUN = sd, na.rm=T )
sd.new=auxU.UI$Mean.U.UI
sd.new.NO=auxU.UI$Mean.U.UI.NO
sd.new.NO2=auxU.UI$Mean.U.UI.NO2

ttoU.UI_DayMean$SD.U.UI <- sd.new
ttoU.UI_DayMean$SD.U.UI.NO <- sd.new.NO
ttoU.UI_DayMean$SD.U.UI.NO2 <- sd.new.NO2
########################################################
##Urea+ DMPSA+NBPT

ttoU.2I_DayMean <- aggregate(MatrizU.2I[,1:(ncol(MatrizU.2I)-2)],
                          by = list('YDAY'= format(MatrizU.2I$Fecha,'%Y%m%d' )),
                          FUN = mean, na.rm=T )
# con este aggregate la cagas con la standard deviation 
# tienes q crear otro aggregate con la funcion sd y pegarle un solo vector (mean.U.2I) a sd
auxU.2I <- aggregate(MatrizU.2I[,1:(ncol(MatrizU.2I)-2)],
                     by = list('YDAY'= format(MatrizU.2I$Fecha,'%Y%m%d' )),
                     FUN = sd, na.rm=T )
sd.new=auxU.2I$Mean.U.2I
sd.new.NO=auxU.2I$Mean.U.2I.NO
sd.new.NO2=auxU.2I$Mean.U.2I.NO2

ttoU.2I_DayMean$SD.U.2I <- sd.new
ttoU.2I_DayMean$SD.U.2I.NO <- sd.new.NO
ttoU.2I_DayMean$SD.U.2I.NO2 <- sd.new.NO2
