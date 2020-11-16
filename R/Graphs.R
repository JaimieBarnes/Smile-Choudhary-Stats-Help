x <- 4
x
y <- "hello"
y
y <- sqrt(10)
y
zig <- x+y
#print variable zig
# remove variables x and y
rm(x,y)
ls()
seq(0,3,0.5)
seq(10,0,-0.5)
seq(10,0,0.5)
seq(2,30,4)
seq(2,30,2)
seq(-2.5,15.34)
seq(0,3,0.04)
seq(0,+0.04)
seq(101,-20,-11)
exp(sqrt(10))
x <- sqrt(10)
exp(x)
?log
set.seed(1453)
N <- 100
weight <- runif(n=N,min=60,max=100)
height <- 2.2*weight + rnorm(n=N,mean=0,sd=10)
plot(weight,height,pch=19,xlab="Weight(kg)",ylab="Height(cm)",col="blue")
fit <- lm(height~weight)
lines(weight, predict(fit), col='black', lwd=3)
print(fit)


hist(x)
10
"smile"
#common vectors c(), seq(), :
#c()concatenate{non sequential no. or other data}, seq()structured sequences,:integers
1:5
6:2
-4:1
-(4:1)
seq(0,0.04, length.out=7)
seq(-2.5,15.34, length.out=14)
seq(10,5, by=-0.5)
c(1,3,5,7)
c(0.6,1,2,0)
c("bob","tim")
c(1,-6,zig)
c(c('bob',"tim","zig"))
c(zig)
c("zig")
as.character(zig)
zig
seq(1,11)
seq(2,50,2)
seq(17,33,2)
seq("M","T","W","T","F","S","S")
c("M","T","W","T","F","S","S")
#for subsetting vectors in order to get specific elements
x <- 1:5
x[3]
x[c(2,3,4)]
x[1] <- 17
x[-3]## generate a vector x of five elements from 1 to 5
x <- 1:5

## extract the third element of x
x[3]

## extract the 2, 3 and 4th elements of x
x[c(2, 3, 4)]

## change the first element of x to 17
x[1] <- 17

## extract all elements of x except the third
x[-3]
w <- x[-3]
x <- 1:5
x<-[3]
y <- x[3]
w <- x[-3]
y <- c(1,5,2,4,7)
y[-c(3,5)]
y[c(1,4,5)]
y[c(3,5)]
i <- 1:3
y[i]
t <- 2:3
y[t]
z <- c(9,10,11)
y[i]<- z
y[i]<- z
order(y)
rm(t)
rev(y)
order(y)
sort(y)
sort(-y)
rm(-y)
#order()puts the values in increasing order but according to their positions in data
#rev()just reverses the values i.e puts the original values in reverse order.
#sort()puts the values in imcreasing order
order(y)
sort(y)
rev(y)
sort(y, decreasing = T)
rev(sort(y))
order(-y)

y <- 1:10
y^2
log(y)
exp(y)
x <- c(5,4,3,2,1,5,4,3,2,1)
x+y
x*y
z <- c(-1, 2.2, 10)
z + y
z*y
c(1:6)*c(1:2)
c(1:3)*c(1:2)
c(1:3)*2
c(1:6)*c(1:2,3)
c(1:2,3)
c(1:6)
x <- 1:10
y <- c(5,4,3,2,1,5,4,3,2,1)
x < 4
x[x<4]
y[x<4]
y>1& y<=4
y[y>1&y<=4]
z <- y[y!=3]
z <- y[3]
length(x)
names(x)
min(x)
max(x)
mean(x)
median(x)
range(x)
sum(x)
sd(x)
var(x)
diff(x)
summary(x)
seq(1,10,0.5)
log(seq(1,10,0.5))
sqrt(log(seq(1,10,0.5)))
sum(sqrt(log(seq(1,10,0.5))))
install.packages("gapminder")
install.packages("tidyverse")
library(tidyverse)
ggplot(iris)+
  geom_density(aes(x=Sepal.Length, fill=Species, alpha=0.5))+
  xlab("Sepal Length(cm)") + ylab("Density(cm)")
  ggtitle("Density plot of Sepal lengths by species")
install.packages("tidyverse")
ggplot(iris)+
  geom_point(aes(x = Sepal.Length, y = Sepal.Width, colour = Species))+
  facet_wrap(~Species)
ggplot(iris)+
  geom_point(aes(x=Sepal.Length, y=Sepal.Width))+
  stat_smooth(aes(x=Sepal.Length, y=Sepal.Width), method = "lm")+
  facet_wrap(~Species)
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width))+
  geom_point()+
  stat_smooth(method="lm")+
  facet_wrap(~Species)
library(gapminder)
ggplot(gapminder[gapminder$year==1952,],
       aes(x=gdpPercap, y=lifeExp, colour=continent, size=pop))+
  geom_point()+
  facet_wrap(~continent)
p<-ggplot(gapminder[gapminder$year==1952,],
       aes(x=log10(gdpPercap), y=lifeExp, colour=continent, size=pop, scale_x_log10))+
  geom_point()+
  facet_wrap(~continent)
p<-p+scale_x_log10()
p
p<-p+scale_size_area(max_size=10)
p
p<-p+xlab("log10(GDP per capita"))+
  ylab("Life Expectancy at birth (years)")+
  ggtitle("1952")
p
p<-p=labs(colour=continent,size=population)
p
p<-p+theme_bw()
p
gapminderNoOc<-gapminder[gapminder$continent != "Oceania",]
ggplot(gapminderNoOc[gapminderNoOc$year==1952,],
       aes(x=gdpPercap, fill=continent))+
  geom_density(position="stack")+
  scale_x_log10()+
  xlab("log10(GDP percapita)")+
  ylab("count")+
  facet_wrap(~continent)
  ggtitle("1952")
  library(tidyverse)
  head(iris)
  iris[iris$Species=="versicolor",]
filter(iris, Species=="versicolor")
iris[order(iris$Species,iris$Sepal.length),]
arrange(iris,Species,Sepal.Length)
cbind(Species=iris$Species, Sepal.Length=iris$Sepal.Length, Sepal.Width=iris$Sepal.Width)
select(iris,Species,Sepal.Length,Sepal.Width)
select(iris,-Petal.Length,-Petal.Width)
mutate(iris,Species,Sepal.Length2=Sepal.Length^2)
iris%>%filter(Species=="versicolor")%>%arrange(-Sepal.Length)
iris%>%group_by(Species)%>%summarise(mn=mean(Sepal.Length&Sepal.Width))
iris%>%group_by(Species)%>%summarise(mnL=mean(Sepal.Length),varL=var(Sepal.Length),
                                     mnW=mean(Sepal.Width),varW=var(Sepal.Width))
library(gapminder)
head(gapminder)


gp_income <- read_csv("uploadFiles/indicator gapminder gdp_per_capita_ppp.csv")
gp_income
gp_income<- gp_income%>%
  rename(country="GDP per capita")
gp_income<-gp_income%>%
  gather(key=year,value=gdp,-country)
gp_income<-gp_income%>%
  mutate(year=as.numeric(year))
sum(is.na(gp_income$country))
gp_income%>%filter(year>1990)
gp_income <- read_csv("upload")
  gp_income<-gp_income%>%filter(mean(gdp))
gp_income%>%
  group_by(year)
gp_income
gp_hiv <- read_csv("indicator hiv estimated prevalence%15-49.csv")
germany<-read_csv("Germanypop.csv")
gp<-inner_join(gp_income,gp_hiv,by=c("year","country"))

gp_hiv %>%
    filter(country == "Brazil" | country == "Uganda") %>%
                      ggplot(aes(x = year, y = prevalence, colour = country)) +
                      geom_line() + xlab("Year") + ylab("Prevalence") +
                      labs(colour = "Country")
gp_income <- read_csv("indicator gapminder gdp_per_capita_ppp.csv")
gp_income

df <- data.frame(height=height,weight=weight)
head(df)
fit <- lm(height~weight,data=df)
print(fit)
summary(fit)
glm(height~weight,data=df,family=gaussian(link=identity))

ff <- readRDS("fruitfly.rds")
head(ff)
plot(longevity~thorax, data=ff,pch=19,col="blue")
fit_lm<-lm(longevity~type+thorax,ff)
summary(fit_lm)
fit_glm <- glm(longevity~type+thorax,ff,family=gaussian)
summary(fit_glm)
library("ggplot")
fit<-lm(Beg~Mass*Species,data=cuckoo)
par(mfrow=c(2,2))
plot(fit,pch=19,col="blue")
fit<-glm(Beg~Mass*Species,data=cuckoo,family=poisson(link=log))
summary(fit)
fit <- lm(Species~Biomass*pH,data=df)
summary(fit)
fit <- glm(Species~Biomass*pH,data=df,family=poisson(link=log))
summary(fit)
library(tidyverse)
newdata <- expand.grid(Biomass=seq(min(df$Biomass),max(df$Biomass),length.out=200),pH=levels(df$pH))
newdata <- cbind(newdata,Species=predict(fit,newdata,type="response"))
ggplot(mapping=aes(x=Biomass,y=Species,colour=pH))+geom_point(data=df)+geom_line(data=newdata)
library(gapminder)
fit <- glm(Vote~Income,data=USA, family=binomial(link=logit))
summary(fit)
ggplot(wine,aes(x=TotalPhenols,y=Alcohol,colour=WineType))+geom_point()
fit <- glm(WineType~Alcohol+TotalPhenols,data=wine,family=binomial(link=logit))
summary(fit)
head(wine)
phenol <- c(1.5,2.5,3.5)
newdata <- expand.grid(Alcohol=seq(min(wine$Alcohol), max(wine$Alcohol),length.out=100),TotalPhenols=phenol)
newdata<-cbind(newdata,p=predict(fit,newdata,type="response"))
newdata$TotalPhenols<-as.factor(newdata$TotalPhenols)
ggplot(mapping=aes(x=Alcohol,y=p,colour=TotalPhenols))+geom_line(data=newdata)
install.packages("lme4")
install.packages("faraway")
install.packages("tidyverse")
library(tidyverse)
library(lme4)

bac <- readRDS("bacCabinets.rds")
bac_lm<- lm(growth~media, data=bac)
drop1(bac_lm,test="F")

bac_lm<-(growth~media+cabinet, data=bac)
drop1(bac_lm,test="F")
summary(lm(growth~media*cabinet,data=bac))
bac_lmer <- lmer(growth~media+(1|cabinet),data=bac)
summary(bac_lmer)
drop1(bac_lmer,test="Chisq")
drop1(update(bac_lmer,REML=F),test="Chisq")
confint(bac_lmer,level=0.97)
library(faraway)
data(abrasion)
abrasion_lmer <- lmer(wear~material+(1|position)+(1|run),data=abrasion)
drop1(update(abrasion_lmer,REML=F),test="Chisq")
summary(abrasion_lmer)
rats <- readRDS("rats.rds")
summary(rats)

ratsNew <- rats%>%
group_by(Rat, Treatment)%>%
summarise(Glycogen=mean(Glycogen))
ratsNew

ratsNew_lm<- lm(Glycogen~Treatment,data=ratsNew)
drop1(ratsNew_lm,test="F")

rats<- rats%>%
unite(Rat,Treatment,Rat,
sep="_",remove=F)%>%
mutate(Rat=factor(
as.numeric(factor(Rat))))

drunk <- readRDS("drunk.rds")
alc<- drunk%>%
group_by(student,freshener)%>%
summarise(alcohol=mean(alcohol))%>%
ungroup()
alc_lm<- lm(alcohol~freshener,data=alc)
drop1(alc_lm,test="Chisq")

confint(alc_lm,level=0.97)[2,]

drunk_lmer<-lmer(alcohol~freshener+(1|student/sample),data=drunk)

drop1(update(drunk_lmer,REML=F),test="Chisq")
drop1(drunk_lmer,test="Chisq")

confint(drunk_lmer,level=0.97)[5,]


splityield <- readRDS("splityield.rds")
head(splityield)
summary(splityield)

split_lmer<-lmer(yield~irrigation*density*fertilizer+(1|block/irrigation/density),data=splityield)
drop1(update(split_lmer,REML=F),test="Chisq")

split_lmer<-update(split_lmer,~.-irrigation:density:fertilizer)
drop1(update(split_lmer,REML=F),test="Chisq")

split_lmer<-update(split_lmer,~.-density:fertilizer)
drop1(update(split_lmer,REML=F),test="Chisq")

par(mfrow=c(2,2))
interaction.plot(splityield$fertilizer,splityield$density,splityield$yield)
interaction.plot(splityield$fertilizer,splityield$irrigation,splityield$yield)
interaction.plot(splityield$density,splityield$irrigation,splityield$yield)
par(mfrow=c(1,1))


split_pred<-readRDS("split_pred.rds")
library(gapminder)
ggplot(newdata,
aes(x=fertilizer,colour=density))+
geom_pointrange(
aes(y=yield,ymin=lci,ymax=uci),position=position_dodge(width=0.5))+
facet_wrap(~irrigation)+
labs(title="CIs based on fixed effects uncertainity ONLY")


hg <- readRDS("hg.rds")
hg$ldist<-log(hg$Distance)
hg_lm<-lm(ldist~Species*Number,data=hg)
drop1(hg_lm,test="F")
summary(hg_lm)

newdata<-expand.grid(Number=seq(min(hg$Number),max(hg$Number),length=100),Species=levels(hg$Species))

newdata<-newdata%>%
mutate(Distance=exp(predict(hg_lm,newdata)))

ggplot(hg,aes(x=Number,y=Distance,col=Species))+geom_point()+geom_line(data=newdata)

hg_lmer<-lmer(ldist~Species*Number+(1|Group.Name),data=hg)
summary(hg_lmer)
install.packages("tidyverse")
library(tidyverse)
install.packages("gapminder")
head(ff)
ggplot(ff)+ geom_point(aes(x=Predator,y=total(s)))
boxplot(x)
sd(initial)
attach(Crabs_stats)
boxplot(initial, colour)
sd(initial)
sd(crabs_stats)

