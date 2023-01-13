colors=c()

attach(MatrizUrea)
plot  (Fecha,
       Flux.NO_ch5,
       xlab="Year 2017",ylab="FLUX NO [ug m-2 h-1]", main = '',
       cex.main=2, cex.lab=1,
       ty='p',col="#01665e",pch=16,cex=1,lty=1,lwd=1, axes=F,
       ylim =c(0,400))


attach(ttoU_DayMean)   #ttoU->medias diarias

points (Fecha[1:29],
        Mean.Urea.NO[1:29],
        xlab=" ",ylab=" ", main = '',
        cex.main=2, cex.lab=1,
        ty='o',col="#01665e",pch=1,cex=1.5,lty=1,lwd=1,
        ylim =c(0,500))