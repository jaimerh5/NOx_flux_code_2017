#########################################################################
# JAIME RECIO HUETOS 
# ETSIA DE LA UPM DEPARTAMENTO DE QUÍMICA Y TECNOLOGIA DE ALIMENTOS 
# 11/09/2017
######################################################################
#Esto tienes que ir uno por uno
#j=1,2,3,....,59

library(lattice)
setwd('C:/R/ClaseJR_R/')
#fl son todos los archivos que hay en setwd() 
#Fijate antes del script ((fl <- fl[8:14])) que pueden variar el numero 
fl <- dir(); fls <- fl[7:9];
# fls= fls[1]=caja1,fls[2]=Caja2......

FileDay <- fls[1]
setwd(FileDay)
dir()
n <- dir(); summary(n)

# open <- paste(fls[1],'/csv',sep = '');open
# setwd(open)

# creo vbles de apoyo  
Ver <- rep(0,length(n)) 
NAME <- rep(0,length(n)); NAME2  <- rep(0,length(n)) 
Title <- rep(0,length(n));Title2 <- rep(0,length(n))
f <- NULL; file <- NULL
#-------------------------------------------------------------------------------------
 for (j in 1:(length(n))) {


File2Open <- n[j]; File2Open
file <- c(file,File2Open)  #chequear bucle 'for '
Chambj<- read.table(File2Open, sep=';', dec=',', header=TRUE)
# Aqui cometo error de llamarla Chambj aunque depende del archivo que coja será caja2,3,4,5,6

Ver[j] <-n[j]; Ver 
Chambj$X <- NULL
names(Chambj)[1] <- 'Fecha';
Chambj$Fecha <-as.POSIXct (Chambj$Fecha,
                           format= '%Y/%m/%d %H:%M:%S', tz = 'GMT')
# Chambj$Fecha[1:5]
GAPtime <- Chambj$Fecha  # Es mas facil crear un vector de tiempo y luego jugar con el 
#con este If quito valores erróneos 

if (any(Chambj[['NOx']]>(Chambj[['NO']]+Chambj[['NO2']]+6))) {
  idx <- which(Chambj[['NOx']]>(Chambj[['NO']]+Chambj[['NO2']]+6))
  Chambj[['NOx']][idx] <- Chambj[['NO']][idx]+Chambj[['NO2']][idx]}
############################################################################
YMONDOY  <- as.numeric(format(GAPtime,format="%Y%m%d")) ;  Chambj$YMONDOY <- YMONDOY
DOY  <- as.numeric(format(GAPtime,format="%j")) ;  Chambj$DOY <- DOY
HOD <- as.numeric(format(GAPtime,format="%H")); Chambj$HOD <- HOD
MeasureMinute <- as.numeric(format(GAPtime,format="%M")); Chambj$Minute <- MeasureMinute
#En este he creado columnas numericas para ver el tiempo 


NAME[j] <- paste('caja2_',substr(File2Open,1,8),'.pdf' ,sep='')

Title[j] <- paste('caja2',substr(File2Open,1,8))



plotLL <- xyplot(NOx~Minute |factor(HOD),data=Chambj,
           # layout=c(2, 4),
            cex=1, col='red',
            ty='o',xlab="Minute",ylab="ppb", main=Title[j],
            lwd=2,pch=18,
            grid=T)
pdf(NAME[j])
    
plotLL
    
 dev.off()
 
 setwd('C:/R/ClaseJR_R/');setwd(FileDay);}
