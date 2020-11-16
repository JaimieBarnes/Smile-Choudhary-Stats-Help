install.packages("lme4")
library(lme4)

Data3<-read.csv("Exp3Excluded.csv", header=TRUE)
Data3
Data3$EscapeResponse<-as.factor(Data3$EscapeResponse)

H<-Data3[Data3$Board=="H",]
HNOP<-H[H$Predator=="N",]

No.Slow<-sum(HNOP$EscapeResponse=="SLOW")
164-No.Slow

16/164


MaxModel<-glmer(EscapeResponse~Predator*Board*ColourDiffPercent + (1|Individual), data=Data3, family=binomial)
Model1<-glmer(EscapeResponse~Predator*Board + Board*ColourDiffPercent + Predator*ColourDiffPercent + (1|Individual), data=Data3, family=binomial)
anova(MaxModel, Model1, test="Chisq") ### 3-way interaction not significant so remove

Model2<-glmer(EscapeResponse~Predator*Board + Board*ColourDiffPercent + Predator+ColourDiffPercent + (1|Individual), data=Data3, family=binomial)
anova(Model1, Model2, test="Chisq") ### predator colourdiff interaction is not significant so remove

### In the model step above there is an issue, removing predator*board here is not 
### significant but it nearly is. So I tested the other interactions first to see which
### ones had the highest p values or least significance. I found this to be predator*colourdiff
### so I removed that one first, now you will notice in the next step, predator*board 
### is significant so should be kept in the model. This is an issue in science with no
### real answer currently.

Model3<-glmer(EscapeResponse~Predator+Board+Board*ColourDiffPercent+Predator+ColourDiffPercent+(1|Individual), data=Data3, family=binomial)
anova(Model2, Model3, test="Chisq") ### predator board interaction is significant so keep

Model4<-glmer(EscapeResponse~Predator*Board + Board + ColourDiffPercent + Predator+ColourDiffPercent + (1|Individual), data=Data3, family=binomial)
anova(Model2, Model4, test="Chisq") ### board colourdiff interaction not significant so remove
### The code is a little sloppy as predator*board includes board already but I have +board in there still
### This is to show you that i am only removing interactions, it does not change the model this way
### It is also why colourdiffpercent is repeated twice

Model5<-glmer(EscapeResponse~Predator*Board + ColourDiffPercent + (1|Individual), data=Data3, family=binomial)
anova(Model4, Model5, test="Chisq")

### For model5 I cleaned it up a little bit so you can see what I mean
### The anova shows a difference of 0 because they are the same model
### This is not needed for your analysis just for the purposes of demonstration
### Model6 is the next step in simplification

Model6<-glmer(EscapeResponse~Predator*Board + (1|Individual), data=Data3, family=binomial)
anova(Model5, Model6, test="Chisq") ### Final step - removing colourdiffpercent is signifcant
### So keep it in the model, all terms are now significant - Model5 is the minimum adequate model



### Adding temp and size
Model1<-glmer(EscapeResponse~Treatment*ColourDiffPercent + Temp.*ColourDiffPercent + Temp.*Size + Temp.*Treatment + Size*Treatment + Size*ColourDiffPercent + (1|Individual), data=Data3, family=binomial)

Model2<-glmer(EscapeResponse~Treatment*ColourDiffPercent + Temp.*ColourDiffPercent + Temp.*Treatment + Size*Treatment + Size*ColourDiffPercent + (1|Individual), data=Data3, family=binomial)
anova(Model1, Model2, test="Chisq") ### Temp and size interaction removed 

Model3<-glmer(EscapeResponse~Treatment*ColourDiffPercent + Temp.*Treatment + Size*Treatment + Size*ColourDiffPercent + (1|Individual), data=Data3, family=binomial)
anova(Model2, Model3, test="Chisq") ### Temp and colourdiff intercation removed

Model4<-glmer(EscapeResponse~Treatment*ColourDiffPercent + Temp.*Treatment + Size*ColourDiffPercent + (1|Individual), data=Data3, family=binomial)
anova(Model3, Model4, test="Chisq") ### size treatment interaction removed

Model5<-glmer(EscapeResponse~Treatment*ColourDiffPercent + Temp.*Treatment + Size + (1|Individual), data=Data3, family=binomial)
anova(Model4, Model5, test="Chisq") ### Size colourdiff interaction kept - maybe covariance here or maybe size predicts colourdiff

Model6<-glmer(EscapeResponse~Treatment*ColourDiffPercent + Temp. + Size*ColourDiffPercent + (1|Individual), data=Data3, family=binomial)
anova(Model4, Model6, anova="Chisq") ### remove temp treatment interaction

Model7<-glmer(EscapeResponse~Treatment + Temp. + Size*ColourDiffPercent + (1|Individual), data=Data3, family=binomial)
anova(Model6, Model7, anova="Chisq") ### remove treatment colourdiff

Model8<-glmer(EscapeResponse~Treatment + Size*ColourDiffPercent + (1|Individual), data=Data3, family=binomial)
anova(Model7, Model8, test="Chisq") ### remove temp.

Model9<-glmer(EscapeResponse~ Size*ColourDiffPercent + (1|Individual), data=Data3, family=binomial)
anova(Model8, Model9, test="Chisq") ### remove treatment

### This is minimal model - However, may need to retry using scaled variables cos R was having a fit

### Brightness


MaxModel<-glmer(EscapeResponse~Predator*Board*Brightness.DifferencePercent + (1|Individual), data=Data3, family=binomial)
Model1<-glmer(EscapeResponse~Predator*Board + Board*Brightness.DifferencePercent + Predator*Brightness.DifferencePercent + (1|Individual), data=Data3, family=binomial)
anova(MaxModel, Model1, test="Chisq") ### 3-way interaction not significant so remove

Model2<-glmer(EscapeResponse~Predator*Board + Board*ColourDiffPercent + Predator+ColourDiffPercent + (1|Individual), data=Data3, family=binomial)
anova(Model1, Model2, test="Chisq") ### predator colourdiff interaction is not significant so remove

### In the model step above there is an issue, removing predator*board here is not 
### significant but it nearly is. So I tested the other interactions first to see which
### ones had the highest p values or least significance. I found this to be predator*colourdiff
### so I removed that one first, now you will notice in the next step, predator*board 
### is significant so should be kept in the model. This is an issue in science with no
### real answer currently.

Model3<-glmer(EscapeResponse~Predator+Board+Board*ColourDiffPercent+Predator+ColourDiffPercent+(1|Individual), data=Data3, family=binomial)
anova(Model2, Model3, test="Chisq") ### predator board interaction is significant so keep

Model4<-glmer(EscapeResponse~Predator*Board + Board + ColourDiffPercent + Predator+ColourDiffPercent + (1|Individual), data=Data3, family=binomial)
anova(Model2, Model4, test="Chisq") ### board colourdiff interaction not significant so remove
### The code is a little sloppy as predator*board includes board already but I have +board in there still
### This is to show you that i am only removing interactions, it does not change the model this way
### It is also why colourdiffpercent is repeated twice

Model5<-glmer(EscapeResponse~Predator*Board + ColourDiffPercent + (1|Individual), data=Data3, family=binomial)
anova(Model4, Model5, test="Chisq")

### For model5 I cleaned it up a little bit so you can see what I mean
### The anova shows a difference of 0 because they are the same model
### This is not needed for your analysis just for the purposes of demonstration
### Model6 is the next step in simplification

Model6<-glmer(EscapeResponse~Predator*Board + (1|Individual), data=Data3, family=binomial)
anova(Model5, Model6, test="Chisq") ### Final step - removing colourdiffpercent is signifcant
### So keep it in the model, all terms are now significant - Model5 is the minimum adequate model
