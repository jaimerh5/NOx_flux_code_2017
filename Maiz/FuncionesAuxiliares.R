interpolate <- function (a,b,c,d,x)
  ((d-c)/(b-a))*(x-a)+c
  show(x)
###########################################################################################
FindMissValues <- function (box,amb) {
  
  
SrchBox <- unique(box[['YMONDOY']])
length(SrchBox)
#SrchBox=59-> tengo datos todos los dias
SrchAmb <- unique(amb[['YMONDOY']])
length(SrchAmb)

#Crear matrices vacías para almacenar cosas
DatosDiaCaja <- matrix(data=NA, nrow=length(SrchBox),ncol = 2);
DatosDiaAmb <-  matrix(data=NA, nrow=length(SrchAmb),ncol = 2);

for (q in 1:length(SrchBox)) {
  ay1 <- which(box[['YMONDOY']]==SrchBox[q])  #ay=ayudante
  ay2 <- which(amb[['YMONDOY']]==SrchAmb[q])
  DatosDiaCaja[q,1] <- SrchBox[q]
  DatosDiaCaja[q,2] <- length(ay1)
  DatosDiaAmb[q,1] <- SrchBox[q]
  DatosDiaAmb[q,2] <- length(ay2)
}
Comp1 <- cbind(DatosDiaCaja,DatosDiaAmb);
Comp1 <- cbind(Comp1,DatosDiaCaja[,2]==DatosDiaAmb[,2])
print(Comp1)
}  
###########################################################################################
#•Funcion en la que dd es el vector que tengas en formato fecha 
# r es la vble que equivale a el número de división del eje Y, ed, si quieres q vaya de 1 en 1 o de 20 en 20
  
  AxisPlotsFecha <- function(dd,r) {
  axis.POSIXct(1,at=seq(dd[1],dd[length(dd)],by='week'),format='%d/%m' )
  axis(2); box()
  #Esto es para poner las líneas de grid en el eje
  
  eje.x=seq(dd[1],dd[length(dd)],by='week')
  abline(v= as.numeric(eje.x),col = "lightgray", lty = "dotted")
  abline(h=seq(-100,200,r),col = "lightgray", lty = "dotted")
  # SpecialDay1 <- as.numeric(dd[GAPDmean_Box1$YMONDOY==20160405]) 
  # arrows(SpecialDay1,150,x,100, col='black',lwd=2, lty=1)
  #   legend ('topright',
  #           legend=lly, # 
  #           pch=pch, bty='n',
  #           lty=lty,lwd=lwd,cex=1,
  #           col = col1) 
}
###########################################################################################

