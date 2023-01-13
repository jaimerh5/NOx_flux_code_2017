#TUTORIAL MUY BUENO DE ggplot2


CajaAmbDaily
#https://cran.r-project.org/web/packages/ggplot2/ggplot2.pdf
library(ggbiplot);library(ggplot2)
data("mpg") #coches 
data('mtcars')
data('faithfuld')
windows()
View(mpg); View(mtcars);View(faithfuld)
base <- ggplot(mpg, aes(displ, hwy)) 
base+ geom_point()
base+geom_smooth()
base+ geom_point()+geom_smooth(method = 'lm')
# To override the data, you must use %+%  ## Anular =override
base %+% subset(mpg, fl == "p")
# aes() is passed to either ggplot() or specific layer. Aesthetics supplied
# to ggplot() are used as defaults for every layer
aes(mpg, wt, col = cyl)
c <- ggplot(mtcars, aes(factor(cyl)))
c+geom_bar()
c + geom_bar(fill = "white", colour = "red")
#------------------
k <- ggplot(mtcars, aes(factor(cyl), fill = factor(vs)))
k + geom_bar()
# Fill aesthetic can also be used with a continuous variable
m <- ggplot(faithfuld, aes(waiting, eruptions))
m + geom_raster()
m + geom_raster(aes(fill = density))
# For large datasets with overplotting the alpha
# aesthetic will make the points more transparent
df <- data.frame(x = rnorm(5000), y = rnorm(5000))
h <- ggplot(df, aes(x,y))
h + geom_point()
h + geom_point(alpha = 0.5)
h + geom_point(alpha = 1/10)
######################################☺
CajaAmbDaily$PeriodDay <- NA
CajaAmbDaily$PeriodDay[which(CajaAmbDaily$HOD>6 & CajaAmbDaily$HOD<=12)] <- 'morning'
CajaAmbDaily$PeriodDay[which(CajaAmbDaily$HOD>12 & CajaAmbDaily$HOD<=18)] <- 'afternoon'
CajaAmbDaily$PeriodDay[which(CajaAmbDaily$HOD>18 & CajaAmbDaily$HOD<=24)] <- 'evening'
CajaAmbDaily$PeriodDay[which(CajaAmbDaily$HOD>=0 & CajaAmbDaily$HOD<=6)] <- 'night'
try1 <- ggplot(data=CajaAmbDaily, aes(x=Fecha,y=NOx, col=PeriodDay))

# try1 <- ggplot(data=CajaAmbDaily, aes(x=Fecha,y=NOx, col=PeriodDay))
# try1 <- ggplot(data=CajaAmbDaily, aes(x=Fecha,y=NOx, shape=factor(PeriodDay)))
try1 <- ggplot(data=CajaAmbDaily, aes(x=Fecha,y=NOx, shape=factor(PeriodDay),col=HOD))

try1+geom_point()
try2 <- try1+coord_cartesian(xlim = NULL, ylim = c(0,120))
try2+geom_point()
###################################
p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
p + annotate("rect", xmin = 2, xmax = 3.5, ymin = 2, ymax = 25,
             fill = "dark grey", alpha = .5)
dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
d <- ggplot(dsamp, aes(carat, price)) +
    geom_point(aes(colour = clarity))
d + scale_colour_brewer(palette='Accent')
# scale_colour_brewer(..., type = "seq", palette = 1, direction = 1,
#                     aesthetics = "colour")
#-----------------
RColorBrewer::display.brewer.all()
CajaAmbDaily2 <- CajaAmbDaily[1:45900,]
try3 <- ggplot(data=CajaAmbDaily2, aes(x=Fecha,y=NOx,col=PeriodDay))
try3+geom_point(alpha=1/3)+coord_cartesian(xlim = NULL, ylim = c(0,120))+
  scale_colour_brewer(palette='Accent')

