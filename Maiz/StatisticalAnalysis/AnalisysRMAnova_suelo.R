# setwd('C:/Users/JaimeRecio/Documents/Expmto2017/MAIZ/ANALISIS ESTADISTICO/ExpH')
setwd('C:/R/Expmto2017/MAIZ/EurochemScripts/StatisticalAnalysis')

RMsuelo_H<-read_excel('Rsuelo_allSin.xlsx')
RMsuelo_H$Date_AllSin <- as.POSIXct(RMsuelo_H$Date_AllSin, format='%Y/%m/%d')
class(RMsuelo_H)
summary(RMsuelo_H)
headTail(RMsuelo_H)
View(RMsuelo_H)
## Convertir las variables caracteres en factores
RMsuelo_H$TTO_AllSin<-as.factor(RMsuelo_H$TTO_AllSin)
RMsuelo_H$Rep_AllSin<-as.factor(RMsuelo_H$Rep_AllSin)
RMsuelo_H$Dia_AllSin<-as.factor(RMsuelo_H$Dia_AllSin)

str(RMsuelo_H)
summary(RMsuelo_H)
# plot3d(x=RMsuelo_H$Fecha,y=RMsuelo_H$NO3_allSin,z=RMsuelo_H$NH4_allSin,
#        type = 's', col='black')

## An?lisis para la variable NO

coplot(NH4_allSin~Dia_AllSin|TTO_AllSin,RMsuelo_H,ty='o')
coplot(NO3_allSin~Dia_AllSin|TTO_AllSin,RMsuelo_H)
xyplot(NH4_allSin~Dia_AllSin|factor(TTO_AllSin),data=RMsuelo_H,ty='o')
#coplot=conditioning plot

## Estimaci?n del modelo con los factor TTO_AllSin, Semana y su interacci?n
## Caja es un efecto aleatorio (modelo mixto)
## corAR1 implica un modelo de autocorrelacion entre los datos a lo largo de las 8 semanas
## varPower para contemplar la heterogeneidad de varianzas
## lme=Linear Mixed-Effects Models
## One approach is to define the null model as one with no fixed effects except for an intercept, indicated with a 1 on the right side of the ~
ayTT <-with(RMsuelo_H,tapply(NH4_allSin,TTO_AllSin,mean,na.rm=T))
ayTT <- as.matrix(ayTT)


mod.NH4<-lme(NH4_allSin~TTO_AllSin+Dia_AllSin+TTO_AllSin:Dia_AllSin,random=~1|Rep_AllSin,
          correlation=corAR1(form=~1|Rep_AllSin),control=list(maxIter=100),
          # weights=varPower(),
          data=RMsuelo_H)
mod.NH4
summary(mod.NH4)
Anova(mod.NH4)
########################################################
mod2.NH4<-lme(log(NH4_allSin)~TTO_AllSin+Dia_AllSin+TTO_AllSin:Dia_AllSin,random=~1|Rep_AllSin,
             correlation=corAR1(form=~1|Rep_AllSin),control=list(maxIter=100),
             # weights=varPower(),
             data=RMsuelo_H)
mod2.NH4
summary(mod2.NH4)
Anova(mod2.NH4)
res00<-residuals(mod2.NH4,type="pearson",level=0)
qqnorm(res00);qqline(res00)

plotNormalHistogram(res00)
plot(fitted(mod2.NH4),residuals(mod2.NH4))
medias2<-emmeans(mod2.NH4,~TTO_AllSin|Dia_AllSin)
medias2
plot(medias2,horizontal=FALSE)
normalityTest(~log(NH4_allSin), test="shapiro.test", data=RMsuelo_H) #si sale menor que 0.05 no son normales

########################################################

res0<-residuals(mod.NH4,type="pearson",level=0)
qqnorm(res0);qqline(res0)

plotNormalHistogram(res0)
plot(fitted(mod.NH4),residuals(mod.NH4))
if (!require(RcmdrMisc)) install.packages("RcmdrMisc") 
normalityTest(~NH4_allSin, test="lillie.test", data=RMsuelo_H) #si sale menor que 0.05 no son normales

# Lilliefors (Kolmogorov-Smirnov) normality test
## Verificamos si hemos corregido la heterogeneidad
plot(res0~fitted(mod.NH4)) #todos los valores<3
abline(h=0)
plot(res00~fitted(mod2.NH4))
## Valores medios de NO para cada semana por TTO_AllSinamiento

#Tabla for NH4_allSin 
with(RMsuelo_H,tapply(NH4_allSin,Dia_AllSin,mean))
with(RMsuelo_H,tapply(NH4_allSin,Dia_AllSin:TTO_AllSin,mean))
interaction.plot(RMsuelo_H$TTO_AllSin, RMsuelo_H$Dia_AllSin, RMsuelo_H$NH4_allSin, 
                 col = colores) #colors()[grep("red",colors())][1:8])
interaction.plot(RMsuelo_H$Dia_AllSin, RMsuelo_H$TTO_AllSin, RMsuelo_H$NH4_allSin, 
                 col = colores) #colors()[grep("red",colors())][1:8])
colores <- c('green','navyblue','red','purple','yellow','brown','orangered1','violetred4')
## Medias estimadas para cada Semana por TTO_AllSinamiento
library(emmeans); library(lattice) #Estimated marginal means (Least-squares means)
medias<-emmeans(mod.NH4,~TTO_AllSin|Dia_AllSin)
medias
contrast(medias,method="pairwise")
xyplot(NH4_allSin~Dia_AllSin|factor(TTO_AllSin),data=RMsuelo_H,ty='o')
## Gr?fico con las medias estimadas en cada TTOpor semanas
## ## 
windows()
# pdf(file='Rplot1.pdf')
plot(medias,horizontal=FALSE)
# dev.off()
#################################################################
#NO3

## Gr?fico de los valores de NO por semana y para cada TTO_AllSinamiento
coplot(NO3_allSin~Dia_AllSin|TTO_AllSin,RMsuelo_H)
coplot(NO3_allSin~TTO_AllSin|Dia_AllSin,RMsuelo_H)
xyplot(NO3_allSin~Dia_AllSin|factor(TTO_AllSin),data=RMsuelo_H,ty='o')
#coplot=conditioning plot

## Estimaci?n del modelo con los factor TTO_AllSin, Semana y su interacci?n
## Caja es un efecto aleatorio (modelo mixto)
## corAR1 implica un modelo de autocorrelacion entre los datos a lo largo de las 8 semanas
## varPower para contemplar la heterogeneidad de varianzas
## lme=Linear Mixed-Effects Models
## One approach is to define the null model as one with no fixed effects except for an intercept, indicated with a 1 on the right side of the ~
ayTT <-with(RMsuelo_H,tapply(NO3_allSin,TTO_AllSin,mean,na.rm=T))
ayTT <- as.matrix(ayTT)


mod.NO3<-lme(NO3_allSin~TTO_AllSin+Dia_AllSin+TTO_AllSin:Dia_AllSin,random=~1|Rep_AllSin,
             correlation=corAR1(form=~1|Rep_AllSin),control=list(maxIter=100),
             # weights=varPower(),
             data=RMsuelo_H)
mod.NO3
summary(mod.NO3)
Anova(mod.NO3)
res0<-residuals(mod.NO3,type="pearson",level=0)
qqnorm(res0);qqline(res0)
plotNormalHistogram(res0)
plot(fitted(mod.NO3),residuals(mod.NO3))
normalityTest(~NO3_allSin, test="shapiro.test", data=RMsuelo_H) #si sale menor que 0.05 no son normales
# Shapiro-Wilk normality test
# W = 0.59543, p-value < 2.2e-16
# Se rechaza la hipotesis nula, la muestra no es normal
## Verificamos si hemos corregido la heterogeneidad
plot(res0~fitted(mod.NO3)) #todos los valores<3
abline(h=0)

## Valores medios de NO para cada semana por TTO_AllSinamiento

#Tabla for NO3_allSin 
with(RMsuelo_H,tapply(NO3_allSin,TTO_AllSin,mean))
with(RMsuelo_H,tapply(NO3_allSin,Dia_AllSin,mean))
with(RMsuelo_H,tapply(NO3_allSin,Dia_AllSin:TTO_AllSin,mean))
interaction.plot(RMsuelo_H$TTO_AllSin, RMsuelo_H$Dia_AllSin, RMsuelo_H$NO3_allSin, 
                 col = colores) #colors()[grep("red",colors())][1:8])
interaction.plot(RMsuelo_H$Dia_AllSin, RMsuelo_H$TTO_AllSin, RMsuelo_H$NO3_allSin, 
                 col = colores) #colors()[grep("red",colors())][1:8])
colores <- c('green','navyblue','red','purple','yellow','brown','orangered1','violetred4')

###############################################################
mod2.NO3<-lme(log(NO3_allSin)~TTO_AllSin+Dia_AllSin+TTO_AllSin:Dia_AllSin,random=~1|Rep_AllSin,
             correlation=corAR1(form=~1|Rep_AllSin),control=list(maxIter=100),
             # weights=varPower(),
             data=RMsuelo_H)
mod2.NO3
summary(mod2.NO3)
Anova(mod2.NO3)
res000<-residuals(mod2.NO3,type="pearson",level=0)
qqnorm(res000);qqline(res000)
plotNormalHistogram(res000)
plot(fitted(mod2.NO3),residuals(mod2.NO3))
normalityTest(~log(NO3_allSin), test="shapiro.test", data=RMsuelo_H) #si sale menor que 0.05 no son normales
medias3<-emmeans(mod2.NO3,~TTO_AllSin|Dia_AllSin)
medias3
plot(medias3,horizontal=FALSE)
####################################################################

## Medias estimadas para cada Semana por TTO_AllSinamiento
library(emmeans) #Estimated marginal means (Least-squares means)
medias<-emmeans(mod.NO3,~TTO_AllSin|Dia_AllSin)
medias
contrast(medias,method="pairwise")
xyplot(NO3_allSin~Dia_AllSin|factor(TTO_AllSin),data=RMsuelo_H,ty='o')
## Grafico con las medias estimadas en cada TTOpor semanas
## ## 
windows()
plot(medias,horizontal=FALSE)
