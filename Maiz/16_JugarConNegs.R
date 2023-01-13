#vamos a trabajar con los valores negativos de los tratamientos
attach(MatrizU.NI)
head(MatrizU.NI)

negsUNI <- MatrizU.NI$Mean.U.NI.NO<0
length(negsUNI[negsUNI==T])
#107 valores negativos en 439 datos = 24%
MatrizU.NI[['Mean.U.NI.NO']][negsUNI]


negsUUI <- MatrizU.UI$Mean.U.UI.NO<0
length(MatrizU.UI[['Mean.U.UI.NO']][negsUUI])
MatrizU.UI[['Mean.U.UI.NO']][negsUUI]

negsU2I <- MatrizU.2I$Mean.U.2I.NO<0
MatrizU.2I[['Mean.U.2I.NO']][negsU2I]

negsCon <- MatrizControl$Mean.Control.NO<0
MatrizControl[['Mean.Control.NO']][negsCon]

MatrizU.NI[['Fecha']][negsUNI]

diaNeg.UNI <-as.numeric(format(MatrizU.NI[['Fecha']][negsUNI],format="%Y%m%d")) 
unique(diaNeg.UNI)#44dias con negativos en U+NI


MatrizControl[['Fecha']][negsCon]
diaNeg.Con <-as.numeric(format(MatrizControl[['Fecha']][negsCon],format="%Y%m%d"))
unique(diaNeg.Con)
##42dias con negativos en control
library(lattice);library(RColorBrewer); library(latticeExtra);library(ggplot2)
p1 <- xyplot(Flux.NO_ch3~HOD_ch3 |factor(YMONDOY), data=MatrizU.NI[negsUNI,],
             ylim=c(-75,10))
p2 <- xyplot(Mean.U.NI.NO~HOD_ch3 |factor(YMONDOY), data=MatrizU.NI[negsUNI,],
             col='red')
p3 <- xyplot(Flux.NO_ch10~HOD_ch10 |factor(YMONDOY), data=MatrizU.NI[negsUNI,],
             col='green')
p1+as.layer(p2)+as.layer(p3)

c1 <- xyplot(Flux.NO_ch1~HOD_ch1 |factor(YMONDOY), data=MatrizControl[negsCon,],
             ylim=c(-75,10))
c2 <- xyplot(Mean.Control.NO~HOD_ch1 |factor(YMONDOY), data=MatrizControl[negsCon,],
             col='red')
c3 <- xyplot(Flux.NO_ch8~HOD_ch8 |factor(YMONDOY), data=MatrizControl[negsCon,],
             col='green')
c1+as.layer(c2)+as.layer(c3)

NegativosC <- MatrizControl[negsCon,]
NegativosUNI <- MatrizU.NI[negsUNI,]
UreaCuandoUNI <- MatrizUrea[negsUNI,]
UreaCuandoControl <-MatrizUrea[negsCon,] 
NegativosUNI$CycleCum <- NegativosUNI$Mean.U.NI.NO*3
NegativosC$CycleCum <- NegativosC$Mean.Control.NO*3

a <- ggplot(NegativosUNI, aes(Fecha,Mean.U.NI.NO, col=PeriodDay, size=SD.U.NI.NO))+
      geom_point()+coord_cartesian()
a +theme_bw()
a+theme_bw()+scale_x_datetime(date_labels ='%m/%d',date_breaks = '5 days')
# a+theme(  panel.background = element_rect(fill = 'white'),
#           panel.grid.major = element_line(size = 0.5, linetype = 'dotted',
#                                           colour = "black") )+

#para theme()  el mejor es theme_bw()  q es el tipico
a+ ylab('Flujos(ug m-2 h-1)') + ggtitle('U+NI')+
  theme_bw()+scale_x_datetime(date_labels ='%m/%d',date_breaks = '5 days')
ggsave("plot.pdf", w=10,height = 10)


c <- ggplot(NegativosC, aes(Fecha,Mean.Control.NO, col=PeriodDay, size=SD.Control.NO))+
  geom_point()+coord_cartesian()
c+ ylab('Flujos(ug m-2 h-1)') + ggtitle('Control')+
  theme_bw()+scale_x_datetime(date_labels ='%m/%d',date_breaks = '5 days')

b <- ggplot(UreaCuandoUNI, aes(Fecha,Mean.Urea.NO, col=PeriodDay, size=SD.Urea.NO))+
     geom_point()+coord_cartesian()+theme_bw()+scale_x_datetime(date_labels ='%m/%d',date_breaks = '5 days')

a+ ylab('Flujos(ug m-2 h-1)') + ggtitle('Urea Vs U+NI negs')+
  theme_bw()+scale_x_datetime(date_labels ='%m/%d',date_breaks = '5 days')+
  geom_point(data=UreaCuandoUNI, aes(Fecha,Mean.Urea.NO,  size=SD.Urea.NO))

c+ ylab('Flujos(ug m-2 h-1)') + ggtitle('Urea Vs Conrol Neg')+
  theme_bw()+scale_x_datetime(date_labels ='%m/%d',date_breaks = '5 days')+
  geom_point(data=UreaCuandoControl, aes(Fecha,Mean.Urea.NO, size=SD.Urea.NO))

  t <- ggplot(MatrizUrea)+ geom_point(aes(x=Fecha, y=Flux.NO_ch5), col='red') +theme_bw()

  tt <- t+geom_point(aes(x=Fecha,y=Flux.NO_ch9), col='blue')
ttt <- tt+geom_point(aes(x=Fecha,y=Flux.NO_ch12), col='green')
t4 <- ttt+geom_line(aes(x=Fecha,y=Mean.Urea.NO), col='black', size=3, alpha=0.2)
t4+ylab('Flujos(ug m-2 h-1)') + ggtitle('Urea Replicates')+theme_bw()+scale_x_datetime(date_labels ='%m/%d',date_breaks = '5 days')

# plot1 <- ggplot(NULL,aes(x=Fecha, y=Mean.Urea.NO))
plot1 + geom_point(data=MatrizUrea, col='red')
plot1 + geom_point(data=MatrizUrea, col='red') + geom_line(data=ttoU_DayMean, col='blue')


plot1 + geom_point(data=MatrizUrea, col='red') + geom_line(data=ttoU_DayMean, col='blue', size=3)+
  geom_errorbar(data=ttoU_DayMean,aes(ymin = Mean.Urea.NO-SD.Urea.NO/3, ymax =  Mean.Urea.NO+SD.Urea.NO/3, width = .5))
# + geom_point(data = dataP, aes(y = value), shape = 21, size = 4, color = 'darkblue', fill = 'lightblue') +
#   labs(x= NULL, y = NULL)

