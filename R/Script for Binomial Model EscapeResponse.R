Data1<-read.csv("Exp1Excluded.csv", header=TRUE)
Data1

Data1$EscapeResponse<-as.factor(Data1$EscapeResponse)
Data1$Predator<-as.factor(Data1$Predator)
Data1$Circle<-as.factor(Data1$Circle)
### Going to start with a binomial for escaperesponse

Model1<-glm(EscapeResponse~Circle*Predator, data=Data1, family=binomial(link="logit"))
Model1 ### Has interaction
Model2<-glmer(EscapeResponse~Circle+Predator+(1|Individual), data=Data1, family=binomial)
Model2 ### No interaction
anova(Model1, Model2, test = "Chi") 


summary(Model2)
### Not significant - Remove Predator Circle interaction for minimal model

Model3<-glm(EscapeResponse~Predator, data=Data1, family=binomial(link="logit"))
Model3 ### Removes circle
anova(Model2, Model3, test="Chi")

### Significant, so your minimal adequate model keeps circle

Model4<-glm(EscapeResponse~Circle, data=Data1, family=binomial(link="logit"))
Model4 ### removes Predator
anova(Model2, Model4, test="Chi")

### Significant so keep predator and circle but no interaction
### Minimal Adequate Model is EscapeResponse~Circle+Predator so Model2

### You report the vales of these anovas for to explain your model 
### simplification approach

### So this appears to be fine, using normal graphical approaches 
### to checking assumptions is bad see below

plot(Model1)

### The plots, particularly the Q-Q Normal plot look pretty bad, 
### but it is binomial so we can basically say screw it and ignore it.
### The overdispersion parameter is what we are interested in.

overdisp<-Model1$deviance/Model1$df.residual
overdisp

### According to Dave Hodgeson's infinite wisdom, if the value exceeds
### 1.5 it is bad and we need to use quasibinomial. It is only 1.2, so 
### the model checks out. Whoo! Also, if the value is below 0.75 = bad

### You can graph this model however you like. I don't know how it will 
### look since it is all categorical variables but I will make quick 
### graphs here and see.

plot(EscapeResponse~Predator, data=Data1)
plot(EscapeResponse~Circle, data=Data1)

### Tbh, I don't know how you would put the two together in a graph, maybe
### convert fast and slow to 1 and 0 then plot them as points and see how
### that looks. But up to you, if you are happy with presenting these kinds
### of graphs then cool.

### Final thing for this model is interpretation. What the model is 
### actually saying. Important note: This is on the Logit scale and it
### is important to acknowledge this when referring to these values.

summary(Model2)

### looking at the summary we can see that you have a baseline intercept at
### 0.5925. So if there is no predator (predator = N) and circle = B instead 
### Y, then you have a 0.5925 chance of being Slow compared with Fast. I 
### think this is the case as fast is first in the alphabet so it is using
### that as the base, the it gives you the log-odds (Logit) value of the 
### probability of being slow... I think. Hopefully, that makes sense.
### If there is a predator present, the log-odds of being slow, decreases
### by -1.6167 to an absolute log-odds probability value of 

SumLogit<-0.5925-1.6167 #intercept + PredatorP

### -1.0242 of being slow. So with a predator present and circle = B, you 
### have a -1.0242 log-odds probability of being slow. But you can just
### say that "If circle = B and a predator is present, the log-odds of being 
### slow decreases by 1.6167"

### You can also take it out of log-odds to give absolute values. When doing
### this, you must fist calculate the full value then convert from log-odds to
### standard probability. So you cannot say convert the intercept and then
### convert the PredatorP value and then add them together for the 
### probability. You need to add them first, then convert.

### So you would convert the value of -1.0242 to give you the probability
### of being slow if there is a predator present and circle = B.

library(boot)

Prob<-inv.logit(SumLogit)
Prob

inv.logit(0.9415-0.5098)

### The value calculate here is the probability of being slow with circle = B
### and predator is present. So 26.4% of being slow under these conditions.

### This should cover everything for this model. Hope it help and I will
### work on the other models later.

str(Data1)
relevel(Data1$Circle, ref="Y")

Data1$Predator<-as.factor(Data1$Predator)
Data1$Circle<-as.factor(Data1$Circle)


E1Model1<-glmer(EscapeResponse~Predator*Circle*Brightness.DifferencePercent + (1|Individual), data=Data1, family=binomial)
E1Model2<-glmer(EscapeResponse~Predator*Circle + Circle*Brightness.DifferencePercent + Predator*Brightness.DifferencePercent + (1|Individual), data=Data1, family=binomial, na.action="na.fail")
anova(E1Model1, E1Model2, test="Chisq")

E1Model3<-glmer(EscapeResponse~Circle*Brightness.DifferencePercent + Predator*Brightness.DifferencePercent + (1|Individual), data=Data1, family=binomial)
anova(E1Model2, E1Model3, test="Chisq") ### remove circle*brightdiff
 
E1Model4<-glmer(EscapeResponse~Predator*Circle + Brightness.DifferencePercent + (1|Individual), data=Data1, family=binomial)
anova(E1Model3, E1Model4, test="Chisq")

E1Model5<-glmer(EscapeResponse~Predator + Circle + Brightness.DifferencePercent + (1|Individual), data=Data1, family=binomial)
anova(E1Model4, E1Model5, test="Chisq")

E1Model6<-glmer(EscapeResponse~Predator + Circle  + (1|Individual), data=Data1, family=binomial)
anova(E1Model5, E1Model6, test="Chisq")

E1Model7<-glmer(EscapeResponse~Predator + (1|Individual), data=Data1, family=binomial)
anova(E1Model6, E1Model7, test="Chisq")

E1Model8<-glmer(EscapeResponse~ Circle + (1|Individual), data=Data1, family=binomial)
anova(E1Model6, E1Model8, test="Chisq")

install.packages("MuMIn")
library(MuMIn)

DM<-dredge(E1Model2)
DM

coeff<-model.avg(DM,subset=delta<20)
modav.betas<-coefTable(coeff)
modav.ci<-confint(coeff)
modav.betas
modav.ci

oldpar<-par(mar=c(5,15,4,2)+0.1,mgp=c(10,1,0))
mod.names<-rownames(modav.betas)
ddd<-barplot(modav.betas[,1],horiz=T,xlim=c(-6,5),col="white",border="white", names.arg=mod.names,las=1)
mtext("model-averaged effect size",side=1,line=3)
mtext("predictor",side=2,line=9)
arrows(modav.ci[,1],ddd,modav.ci[,2],ddd,code=3,angle=90,length=0.1)
points(modav.betas[,1],ddd,cex=1.8,pch=21,bg="black")
lines(c(0,0),c(0,20),lty=2,lwd=2)
