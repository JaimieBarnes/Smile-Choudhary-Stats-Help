#Exp2 Including Size
#Exp2

install.packages("reshape2")




library("numDeriv")
library("RCurl") ## to source() from Github
library("ggplot2")
library("reshape2")
library("plyr")
library("RColorBrewer")

bees<-read.csv("Exp2Excluded.csv")
bees$EscapeResponse<-as.factor(bees$EscapeResponse)

scale.size<-scale(bees$Size)
scale.BDP<-scale(bees$Brightness.DifferencePercent)

#Full model
Fullmodel<- glmer(EscapeResponse ~ Predator + Circle+ Size+ Brightness.DifferencePercent + Predator*Circle +
                    Predator*Brightness.DifferencePercent + Predator*Size + Circle*Brightness.DifferencePercent +Circle*Size +
                    Size*Brightness.DifferencePercent + (1|Individual) , family = "binomial", data =bees)
summary(Fullmodel)

### Scaled size model attempt

ASSmodel<- glmer(EscapeResponse ~ Predator*Circle + Predator*scale.BDP + Predator*scale.size 
                  + Circle*scale.BDP +Circle*scale.size +
                    scale.size*scale.BDP + (1|Individual) , family = "binomial", data =bees)

summary(ASSmodel)

ASSmodel.optimum <- glmer(EscapeResponse ~ Predator*Circle + Predator*scale.BDP + Predator*scale.size 
                 + Circle*scale.BDP +Circle*scale.size +
                   scale.size*scale.BDP + (1|Individual) , family = "binomial", data =bees, 
                 control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

summary(ASSmodel.optimum)



#Removing Predator*Brightness
Model1<-glmer(EscapeResponse ~ Predator + Circle + Size+ Brightness.DifferencePercent + Predator*Circle +
                Predator*Size + Circle*Brightness.DifferencePercent +Circle*Size+ Size*Brightness.DifferencePercent +(1|Individual) ,
              family = "binomial", data =bees)
summary(Model1)
anova(Model1)
anova(Fullmodel,Model1)
#Removing Predator*Size
Model2<-glmer(EscapeResponse ~ Predator + Circle + Size+ Brightness.DifferencePercent + Predator*Circle +
                Circle*Brightness.DifferencePercent +Circle*Size+ Size*Brightness.DifferencePercent + (1|Individual) , family =
                "binomial", data =bees)
summary(Model2)
anova(Model1,Model2)
#Removing Size*Brightness
Model3<-glmer(EscapeResponse ~ Predator + Circle + Size+ Brightness.DifferencePercent + Predator*Circle +
                Circle*Brightness.DifferencePercent +Circle*Size + (1|Individual) , family = "binomial", data =bees)
summary(Model3)
anova(Model2,Model3)
#Removing Circle*Brightness
Model4<-glmer(EscapeResponse ~ Predator + Circle + Size+ Brightness.DifferencePercent + Predator*Circle +Circle*Size
              + (1|Individual) , family = "binomial", data = bees)
summary(Model4)
anova(Model3,Model4,test = "Chisq")
#Removing Circle*Size
Model5<-glmer(EscapeResponse ~ Predator + Circle + Size + Predator*Circle + Brightness.DifferencePercent +(1|Individual) , family =
                "binomial", data = bees)
summary(Model5)
anova(Model4,Model5)
#Removing Circle*Pred
Model6<-glmer(EscapeResponse ~ Predator + Circle + Size + Brightness.DifferencePercent + (1|Individual) , family = "binomial", data
              = bees)
summary(Model6)
anova(Model5,Model6)
#Removing bright
Model7<-glmer(EscapeResponse ~ Predator + Circle + Size+ (1|Individual) , family = "binomial", data = bees)
summary(Model7)
anova(Model6,Model7) #Model7 is MAM?
#Removing Size
Model8<-glmer(EscapeResponse ~ Predator + Circle +(1|Individual) , family = "binomial", data = bees)
summary(Model8)
anova(Model7,Model8) # size is sig so keep

Model9<-glmer(EscapeResponse ~ Predator + Size+(1|Individual) , family = "binomial", data = bees)
anova(Model7, Model9, test="Chisq") # circle is sig so keep

Model10<-glmer(EscapeResponse ~ Size+Circle+(1|Individual) , family = "binomial", data = bees)
anova(Model7, Model10, test="Chisq") # predator is sig so keep

##With colour
#Full model
Fullmodel<- glmer(EscapeResponse ~ Predator + Circle+ Size+ ColourDiffPercent + Predator*Circle +
                    Predator*ColourDiffPercent + Predator*Size + Circle*ColourDiffPercent +Circle*Size + Size*ColourDiffPercent +
                    (1|Individual) , family = "binomial", data =bees)
summary(Fullmodel)
anova(Fullmodel)
#Removing Predator*Colour
Model1<-glmer(EscapeResponse ~ Predator + Circle + Size+ ColourDiffPercent + Predator*Circle + Predator*Size +
                Circle*ColourDiffPercent +Circle*Size + Size*ColourDiffPercent + (1|Individual) , family = "binomial", data =bees)
summary(Model1)
anova(Model1)
anova(Fullmodel,Model1)
#Removing Predator*Size
Model2<-glmer(EscapeResponse ~ Predator + Circle+ Size+ ColourDiffPercent + Predator*Circle +
                Circle*ColourDiffPercent + Circle*Size + Size*ColourDiffPercent + (1|Individual) , family = "binomial", data =bees)
summary(Model2)
anova(Model1,Model2)
#Removing Circle*Colour
Model3<-glmer(EscapeResponse ~ Predator + Circle + Size + ColourDiffPercent + Predator*Circle + Circle*Size +
                Size*ColourDiffPercent +(1|Individual) , family = "binomial", data = bees)
summary(Model3)
anova(Model2,Model3)
#Removing Colour*Size
Model4<-glmer(EscapeResponse ~ Predator + Circle + Size+ ColourDiffPercent+ Predator*Circle+ Circle*Size+
                (1|Individual) , family = "binomial", data = bees)
summary(Model4)
anova(Model3,Model4,test = "Chisq")
#Removing Circle*Size
Model5<-glmer(EscapeResponse ~ Predator + Circle + Size + ColourDiffPercent+ Predator*Circle+(1|Individual) , family
              = "binomial", data = bees)
summary(Model5)
anova(Model4,Model5)
#Removing Predator*Circle
Model6<-glmer(EscapeResponse ~ Predator + Circle + Size + ColourDiffPercent + (1|Individual) , family = "binomial",
              data = bees)
summary(Model6)
anova(Model5,Model6)
#Removing Colour


bees$Predator<-factor(bees$Predator, levels = c("N", "B", "F"))

Model7<-glmer(EscapeResponse ~ Predator + Circle + Size + (1|Individual) , family = "binomial", data = bees)
summary(Model7)
anova(Model6,Model7) #Model7 is MAM?
#Removing Size
Model8<-glmer(EscapeResponse ~ Predator + Circle +(1|Individual) , family = "binomial", data = bees)
summary(Model8)
anova(Model7,Model8)