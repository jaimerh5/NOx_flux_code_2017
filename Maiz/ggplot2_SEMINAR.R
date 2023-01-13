library(ggplot2)
midwest <- read.csv("http://goo.gl/G1K41K") 
h <- ggplot(midwest, aes(x= state, y=popdensity, col=state, size=area))+
  geom_point()
h
install.packages('MEMSS')
# data(milk, package="robustbase")
data(Milk)
head(Milk)
ggplot(data = Milk, aes(x=Time, y=protein)) + 
  geom_point(aes(col=Diet)) +
  geom_boxplot(aes(color=Diet))
ggplot(data = Milk, aes(x=Time, y=protein, shape=Diet)) +geom_point() 

ggplot(Milk, aes(x=Time, y=protein, shape=Diet,size=3)) +
  geom_point() +
  scale_shape_manual(values=c("B","M","L"))


ggplot(data = Milk, aes(x=Time, y=protein)) + 
  geom_point() + 
  geom_line(aes(group=Cow))+
  geom_smooth()

p <- ggplot(data = Milk, aes(x=Time, y=protein)) + 
  geom_point()
p+geom_ribbon()

ggplot(Milk, aes(x=Time, y=protein)) + 
  stat_summary()
ggplot(Milk, aes(x=Time, y=protein)) + 
  stat_summary(fun.y="sd", geom="point")

fun.y="median", geom="point"
pro <- ggplot(Milk, aes(x=protein)) #es un grafico de frecuencias
 pro+geom_histogram(aes(col=Diet)))
#stat_bin() transforms a continuous variable mapped to x into bins (intervals) of counts. Its default geom is bar, producing a histogram.
 pro+geom_bar()
 pro + geom_density()
 
 dDiet <- ggplot(Milk, aes(x=protein, fill=Diet)) + 
   geom_density(alpha=1/3) # makes all colors transparent
 dDiet + scale_fill_hue()
 dDiet + scale_fill_brewer(type="div")
 
 ggplot(Milk, aes(x=Time, y=protein)) +
   geom_point() +
   lims(x=c(5,10), y=c(3,4)) +
   labs(x="Weeks", y="Protein Content") #+ guides(shape='none'); guides to remove the shape scale legend.
 
 ggplot(Milk, aes(x=protein, color=Diet)) + 
   geom_density() + 
   facet_wrap(~Time)
 
 pt <- ggplot(Milk, aes(x=Time, y=protein)) + 
   geom_point()
 pt + theme(panel.background=element_rect(fill="white"))
 ggsave("plot.pdf") 
 
 
 ggplot(Milk, aes(x=Time, y=protein, fill=Diet)) + 
   geom_bar(stat="identity", position="dodge") #position='stack' ? position='fill'
 
 ggplot(sleepstudy, aes(x=Days, y=Reaction)) + 
   geom_point() + 
   geom_smooth(span=1.5) +
   facet_wrap(~Subject)
 ###########################################
library(lme4) 
 head(sleepstudy)
 lm1 <- lm(Reaction ~ Days, sleepstudy); summary(lm1)
 sleepstudy$res <- residuals(lm1)
 sleepstudy$fit <- predict(lm1)
 #esto es plotear el modelo-_>plot(lm1)
 ggplot(sleepstudy, aes(x=fit, y=res)) +
   geom_point() +
   geom_smooth()  
 
 ggplot(sleepstudy, aes(x=Subject, y=res)) +
   geom_point() +
   stat_summary(color="red", size=1) 
 ############################################################-
 mixed <- lmer(Reaction ~ Days + (1+Days|Subject), 
               data=sleepstudy)
 summary(mixed)
 
 #############################################################
 
 ggplot(sleepstudy, aes(x=Subject, y=res_mix)) +
   geom_point() +
   stat_summary(color="red", size=1) +
   stat_summary(aes(y=res), color="blue", size=1)
 
 ###############################################################################
 logit2 <- glm(cbind(ncases, ncontrols) ~ as.numeric(agegp) + 
                 as.numeric(alcgp) + as.numeric(tobgp),
               data=esoph, family=binomial)
 summary(logit2)
 