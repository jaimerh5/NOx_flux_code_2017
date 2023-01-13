#################################################
# JAIME RECIO HUETOS 
# ETSIA DE LA UPM; DEPARTAMENTO DE QUÍMICA Y TECNOLOGIA DE ALIMENTOS 
#18/09/2017
#############################################################
# Plotear Acumulados Medios Finales
library(data.table)
RepTTOs<- factor(c('Urea','Urea','Urea','U+NI','U+NI','U+NI','U+UI','U+UI','U+UI','U+2I','U+2I','U+2I','C','C','C'))
MeanCumEnd <- c(Cum.ttoU_DayMean$Mean.U.NOx[length(Cum.ttoU_DayMean$Mean.U.NOx)],Cum.ttoU.NI_DayMean$Mean.U.NOx[length(Cum.ttoU.NI_DayMean$Mean.U.NOx)],
                Cum.ttoU.UI_DayMean$Mean.U.NOx[length(Cum.ttoU.UI_DayMean$Mean.U.NOx)], Cum.ttoU.2I_DayMean$Mean.U.NOx[length(Cum.ttoU.2I_DayMean$Mean.U.NOx)],
                Cum.ttoControl_DayMean$Mean.U.NOx[length(Cum.ttoControl_DayMean$Mean.U.NOx)])
SDCumEnd <- c(Cum.ttoU_DayMean$SD.U.NOx[length(Cum.ttoU_DayMean$SD.U.NOx)],Cum.ttoU.NI_DayMean$SD.U.NOx[length(Cum.ttoU.NI_DayMean$SD.U.NOx)],
              Cum.ttoU.UI_DayMean$SD.U.NOx[length(Cum.ttoU.UI_DayMean$SD.U.NOx)], Cum.ttoU.2I_DayMean$SD.U.NOx[length(Cum.ttoU.2I_DayMean$SD.U.NOx)],
              Cum.ttoControl_DayMean$SD.U.NOx[length(Cum.ttoControl_DayMean$SD.U.NOx)])
RepMeanCumEnd <- data.table(Cum.ttoU_DayMean[nrow(Cum.ttoU_DayMean),2:4],Cum.ttoU.NI_DayMean[nrow(Cum.ttoU.NI_DayMean),2:4],
                Cum.ttoU.UI_DayMean[nrow(Cum.ttoU.UI_DayMean),2:4], Cum.ttoU.2I_DayMean[nrow(Cum.ttoU.2I_DayMean),2:4],
                Cum.ttoControl_DayMean[nrow(Cum.ttoControl_DayMean),2:4])
setwd('C:/R/Expmto2017/MAIZ/EurochemScripts')

EndCum.U <- ttoU_Cum[nrow(ttoU_Cum),2:4]
EndCum.UNI<- ttoUNI_Cum[nrow(ttoUNI_Cum),2:4]
EndCum.UUI<- Cum.ttoU.UI_DayMean[nrow(Cum.ttoU.UI_DayMean),2:4]
EndCum.U2I <- ttoU2I_Cum[nrow(ttoU2I_Cum),2:4]
EndCum.Con <- ttoCon_Cum[nrow(ttoCon_Cum),2:4]


#############STATISTIC######################
TTO <- gl(5, 3, 15, labels = c('Urea','U+NI','U+UI','U+2I','C'))
REP <-  gl(3, 1, 15, labels = c('1','2','3'))
EndCum.Stat <- c(EndCum.U,EndCum.UNI,EndCum.UUI,EndCum.U2I,EndCum.Con)
split(EndCum.Stat,TTO)
tapply(EndCum.Stat, TTO, summary)
StatsNOxCUM <- data.frame(cbind(TTO,REP,EndCum.Stat))
lm.CUM <- lm(EndCum.Stat~TTO)
summary(lm.CUM)
plot(lm.CUM, las = 1) 
var.test(EndCum.Stat,TTO)
anova <- aov(TTO~EndCum.Stat)
summary(anova)
anv <- lm(EndCum.Stat~TTO)
anova(anv)
TukeyHSD(anova)
plot(TukeyHSD(anova))
model.tables(anova,type = "mean")

####################
mu <- mean(EndCum.Stat)
sigma <- sd(EndCum.Stat)
t.test(StatsNOxCUM$EndCum.Stat[StatsNOxCUM$TTO_==1],StatsNOxCUM$EndCum.Stat[StatsNOxCUM$TTO_==2])
var.test(StatsNOxCUM$TTO_,StatsNOxCUM$EndCum.Stat ) 
#HOMOCEDASTICIDAD
# F test to compare two variances
# 
# data:  StatsNOxCUM$TTO_ and StatsNOxCUM$EndCum.Stat
# F = 1.845e-09, num df = 14, denom df = 14, p-value < 2.2e-16
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   6.194160e-10 5.495448e-09
# sample estimates:
#   ratio of variances 
# 1.844985e-09 
ks.test(EndCum.Stat,pnorm,mu,sigma)  
#Prueba Kolmogorov-Smirnov-->Normalidad 
# One-sample Kolmogorov-Smirnov test
# 
# data:  EndCum.Stat
# D = 0.25458, p-value = 0.2409
# alternative hypothesis: two-sided
##p>0.05 --> se acepta que son muestras normalizadas
#


anova <- aov(StatsNOxCUM$EndCum.Stat~ StatsNOxCUM$TTO_)
summary(anova)
anv <- lm(StatsNOxCUM$EndCum.Stat~ StatsNOxCUM$TTO_)
anova(anv)
TukeyHSD(anova)
stripchart(StatsNOxCUM$EndCum.Stat~ StatsNOxCUM$TTO_)
plot(TukeyHSD(anova))
model.tables(anova,type = "mean")
#################################################################



StatsNOxCUM <- data.frame(cbind(stColum.tto,ndColum.rep,rdColum.EndCum))
names(StatsNOxCUM) <- c('TTO','REP', 'AcumEnd')


# StatsNOxCUM <-read.table("ResultadosNOxEnd.csv",header=T, sep = ';', dec='.')
# StatsNOxCUM <- StatsNOxCUM[,1:3]
# 
# xyplot(NOxDaily_END~TTOs,data= StatsNOxCUM, ty='h',
#      xlab='Treatments',ylab='[g-N (NOx)* ha-1]', main='Cumulative NOx Emissions',
#      col='brown1')
# StatsNOxCUM2 <- data.table(StatsNOxCUM)
# anova(StatsNOxCUM2)
