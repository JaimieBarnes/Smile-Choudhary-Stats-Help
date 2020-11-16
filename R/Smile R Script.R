### Figuring out a 3-way interaction
library(ggplot2)

dat <- data.frame(F1=gl(n = 2,k = 50),F2=factor(rep(1:2,times=50)),X1=runif(100,-2,2))
dat
modmat <- model.matrix(~F1*F2*X1,dat)
modmat
#the coefs
betas <- runif(8,-2,2)
betas
#simulate some data
dat$y <- rnorm(100,modmat%*%betas,1)
dat$y
#fit a model
m <- lm(y ~ X1*F1*F2, dat)
summary(m)

dat$y[2]

pred <- expand.grid(X1 = c(min(dat$X1),0,max(dat$X1)),F1=factor(1:2),F2=factor(1:2))
pred
pred$y <- predict(m,pred)
pred$y

#Intercepts

coef(m)[1] # F1=1, F2=1
coef(m)[1] + coef(m)[3] # F1=2, F2=1
coef(m)[1] + coef(m)[4] # F1=1, F2=1
coef(m)[1] + coef(m)[3] + coef(m)[4] + coef(m)[7] # F1=2, F2=2

ggplot(dat,aes(x=X1,y=y,color=F1))+geom_point()+
  facet_grid(.~F2,labeller = "label_both")+
  stat_smooth(method="lm",se=FALSE)+
  geom_linerange(data=pred[c(2,5),],aes(ymin=y[1],ymax=y[2]),
                 size=2,color="orange")+
  geom_text(data=pred[2,],aes(label="F12"),vjust=4,color="black")+
  geom_linerange(data=pred[c(8,11),],aes(ymin=y[1],ymax=y[2]),
                 size=2,color="orange")+
  geom_text(data=pred[8,],aes(label="F12+\nF12:F22"),vjust=2,color="black") +
  geom_point(data=pred[pred$X==0,],color="black")

# Slopes

coef(m)[2] # F1=1, F2=1 
coef(m)[2] + coef(m)[5] # F1=2, F2=2
coef(m)[2] + coef(m)[6] # F1=1, F2=2
coef(m)[2] + coef(m)[5] + coef(m)[6] + coef(m)[8] # F1=1, F2=2

ggplot(dat,aes(x=X1,y=y,color=F1))+geom_point()+
  facet_grid(.~F2,labeller = "label_both")+
  stat_smooth(method="lm",se=FALSE)+
  geom_point(data=pred[pred$X!=0,],color="black")+
  geom_line(data=pred[c(1,4),],arrow=arrow(),color="orange")+
  geom_line(data=pred[c(3,6),],arrow=arrow(),color="orange")+
  geom_line(data=pred[c(7,10),],arrow=arrow(),color="orange")+
  geom_line(data=pred[c(9,12),],arrow=arrow(),color="orange")+
  geom_text(data=pred[1,],aes(label="X1:F12",hjust=-3,vjust=-4),color="black")+
  geom_text(data=pred[9,],aes(label="X1:F12+X1:F12:F22",hjust=1.5,vjust=3),color="black")
