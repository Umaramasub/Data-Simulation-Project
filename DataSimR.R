
set.seed(101)
generateDataset<-function(N){
  data.frame(matrix(0, nrow = N, ncol = 11))
  age<-0
  canc<-0
  cvd<-0
  diab<-0
  hbp<-0
  education<-0
  smoke<-0
  functional.limitation<-0
  physical.activity<-0
  bmi<-0
  sitting<-0
  data.frame(age,canc,cvd,diab,hbp,education,smoke,functional.limitation,physical.activity,bmi,sitting)
  age<-rnorm(N,55.6,5.4)
  education<-factor(sample(0:5, N, replace=TRUE, prob=c(0.07, 0.128, 0.104, 0.172,0.21,0.317)),ordered=T, 
                    levels=c('0','1','2','3','4','5'))
  smoke<-rbinom(N,1,0.48)
  
  functional.limitation<-factor(sample(0:3, N, replace=TRUE, prob=c(0.445,0.205, 0.169, 0.181)),ordered=T,
                                levels= c('0','1','2','3'))
  no<-which(functional.limitation==0)
  length.no<-length(no)
  mild<-which(functional.limitation==1)
  length.mild<-length(mild)
  mod<-which(functional.limitation==2)
  length.mod<-length(mod)
  severe<-which(functional.limitation==3)
  length.severe<-length(severe)
  
  sitting <- factor( rep("level4",N), ordered=T,levels =c("level1", "level2", "level3", "level4"))
  mod1<-sample(mod,round(length(mod)*0.35),replace = FALSE)
  no1<-sample(no,round(length.no*0.30),replace = FALSE)
  mild1<-sample(mild,round(length.mild*0.20),replace = FALSE)
  sitting[c(no1,mild1,mod1)]<-"level1"
  
  
  no2<-sample(subset(no, !(no %in% no1)),round(length.no*0.30),replace = FALSE)
  mild2<-sample(subset(mild, !(mild %in% mild1)),round(length.mild*0.25),replace = FALSE)
  mod2<-sample(subset(mod,!(mod %in% mod1)),round(length.mod*0.10),replace = FALSE)
  sitting[c(no2,mild2,mod2)]<-"level2"
  
  mod3<-sample(subset(mod, !(mod %in% c(mod1,mod2)),round(length.mod*0.75),replace = FALSE))
  no3<-sample(subset(no, !(no %in% c(no1,no2))),round(length.no*0.35),replace = FALSE)
  sitting[c(mod3,no3)]<- "level3"
  
  logistic <- function(t) 1 / (1 + exp(-t))
  canc <- runif(length(age))< .05*logistic((age-50)/10) + .065*logistic((as.numeric(sitting)-3)/2)
  cvd<- runif(length(age))< .06*logistic((age-45)/2) + .07*logistic((as.numeric(sitting)-3)/2)
  diab <- runif(length(age))< .07*logistic((age-50)/10) + .086*logistic((as.numeric(sitting)-3)/2)
  hbp <- runif(length(age))< .4*logistic((age-45)/10) +.06*logistic((as.numeric(sitting)-3)/2)
  
  
  physical.activity <- factor( rep("veryhigh",N), ordered=T,levels =c("sed", "low", "suff", "high", "veryhigh"))
  low.mod1<-sample(mod,round(length(mod)*0.8),replace = FALSE)
  low.no1<-sample(no,round(length.no*0.03),replace = FALSE)
  low.mild1<-sample(mild,round(length.mild*0.05),replace = FALSE)
  physical.activity[c(low.no1,low.mild1,low.mod1)]<-"low"
  
  suff.no2<-sample(subset(no, !(no %in% low.no1)),round(length.no*0.28),replace = FALSE)
  suff.mild2<-sample(subset(mild, !(mild %in% low.mild1)),round(length.mild*0.45),replace = FALSE)
  suff.mod2<-sample(subset(mod,!(mod %in% low.mod1)),round(length.mod*0.05),replace = FALSE)
  physical.activity[c(suff.no2,suff.mild2,suff.mod2)]<-"suff"
  
  high.no3<-sample(subset(no, !(no %in% c(no1,no2)),round(length.no*0.1),replace = FALSE))
  high.mild3<-sample(subset(mild, !(mild %in% c(low.mild1,suff.mild2)),round(length.mild*0.1),replace = FALSE))
  physical.activity[c(high.no3,high.mild3)]<-"high"
  
  severe.sed<-sample(severe,length.severe,replace = FALSE)
  severe.no4<-sample(subset(no, !(no %in% c(low.no1,suff.no2,high.no3)),round(length.no*0.1),replace = FALSE))
  physical.activity[c(severe.sed,severe.no4)]<-"sed"
  
  
  bmi<- factor( rep("obese",N), ordered=T,levels =c("underweight","normalweight", "overweight","obese"))
  
  sed<-which(physical.activity=="sed")
  length.sed<-length(sed)
  low<-which(physical.activity=="low")
  length.low<-length(low)
  suff<-which(physical.activity=="suff")
  length.suff<-length(suff)
  high<-which(physical.activity=="high")
  length.high<-length(high)
  veryhigh<-which(physical.activity=="veryhigh")
  length.veryhigh<-length(veryhigh)
  
  high1<-sample(high,round(length.high*0.01),replace = FALSE)
  veryhigh1<-sample(veryhigh,round(length.veryhigh*0.015),replace = FALSE)
  bmi[c(high1,veryhigh1)]<-"underweight"
  
  high2<-sample(subset(high, !(high %in% high1)),round(length.high*0.75),replace = FALSE)
  veryhigh2<-sample(subset(veryhigh, !(veryhigh %in% veryhigh1)),round(length.veryhigh*0.5),replace = FALSE)
  low2<-sample(low,round(length.low*0.05),replace = FALSE)
  sed2<-sample(sed,round(length.sed*0.05),replace = FALSE)
  suff2<-sample(suff,round(length.suff*0.25),replace = FALSE)
  bmi[c(high2,veryhigh2,low2,sed2,suff2)]<-"normalweight"
  
  
  sed3<-sample(subset(sed, !(sed %in% sed2),round(length.sed*0.1),replace = FALSE))
  suff3<-sample(subset(suff, !(suff %in% c(suff2)),round(length.suff*0.10),replace = FALSE))
  bmi[c(sed3,suff3)]<-"overweight"
  
  data.frame(age,canc,cvd,diab,hbp,education,smoke,functional.limitation,physical.activity,bmi,sitting)
}

data<-generateDataset(50000)

# Add new column counting the number of chronic diseases reported
data$chronic.disease<-rowSums(data=='TRUE')


summary(data)

library(ggplot2)
p<-ggplot(data, aes(x = as.numeric(physical.activity))) + geom_histogram(aes(fill=bmi),position = 'dodge', bins = 10)
p+labs(title="Distribution of BMI with physical activity",x="Physical Activity")


p<-ggplot(data, aes(x = as.numeric(functional.limitation))) + geom_histogram(aes(fill=physical.activity),position = 'dodge', bins = 10)
p+labs(title="Distribution of physical activity with functional limitation ",x="Functional limitation")


ggplot(data = data, aes(x = functional.limitation, y = physical.activity)) +
  geom_tile(aes(fill = sitting)) 

g<-ggplot(data,aes(chronic.disease)) + stat_count(aes(y = ((..count..)/sum(..count..))))
p<-g+ facet_wrap(~sitting)
p + labs (title= 'Distribution of Chronic Disease with level of sitting',y= 'Percentage')

data$sitting<-factor(data$sitting, ordered=FALSE)
model1<-glm(formula = canc~sitting, family = binomial(),data = data)

model2<-glm(formula = diab~sitting + age, family = binomial(),data = data)
summary(model2)

model3<-glm(formula = cvd~sitting , family = binomial(),data = data)
summary(model3)

model4<-glm(formula = hbp~sitting, family = binomial(),data = data)
summary(model4)

data$physical.activity<-factor(data$physical.activity, ordered=FALSE)
data$functional.limitation<-factor(data$functional.limitation, ordered=FALSE)
data$chronic.disease<-as.factor(ifelse(data$chronic.disease==0,0,1))
model5<- glm(chronic.disease~sitting + age + physical.activity + functional.limitation,family = binomial(),data=data)
summary(model5)

generateDataset<-function(N){
  data.frame(matrix(0, nrow = N, ncol = 11))
  age<-0
  canc<-0
  cvd<-0
  diab<-0
  hbp<-0
  education<-0
  smoke<-0
  functional.limitation<-0
  physical.activity<-0
  bmi<-0
  sitting<-0
  data.frame(age,canc,cvd,diab,hbp,education,smoke,functional.limitation,physical.activity,bmi,sitting)
  
  # Age was normally distributed given the mean and standard deviation
  age<-rnorm(N,55.6,5.4)
  
  # Education was a factor variable with different levels of education 
  # 0= none, 1=School Certificate, 2= High School, 3= Trade 4= Certificate/diploma 5= University degree
  education<-factor(sample(0:5, N, replace=TRUE, prob=c(0.07, 0.128, 0.104, 0.172,0.21,0.317)),ordered=T, 
                    levels=c('0','1','2','3','4','5'))
  # Smoke as binary variable of did or did not smoke
  smoke<-rbinom(N,1,0.48)
  
  # Functional limitation as factor with 4 levels
  # 0= no limitation , 1 = mild limitation, 2= mod limitation, 3 = severe limitation
  
  functional.limitation<-factor(sample(0:3, N, replace=TRUE, prob=c(0.445,0.205, 0.169, 0.181)),ordered=T,
                                levels= c('0','1','2','3'))
  no<-which(functional.limitation==0)
  length.no<-length(no)
  mild<-which(functional.limitation==1)
  length.mild<-length(mild)
  mod<-which(functional.limitation==2)
  length.mod<-length(mod)
  severe<-which(functional.limitation==3)
  length.severe<-length(severe)
  
  # Sitting is a factor variable with 4 levels
  # Level 1 = 0 to < 4 hours of sitting
  # Level 2 = 4 to < 6 hours of sitting
  # Level 3 = 6 to < 8 hours of sitting
  # Level 4 = >= 8 hours of sitting
  
  # This variable is correalted with the amount if functional limitation as people with more limitation with sit 
  # for longer hours. This variable was simulated using the functional limitation indices and randomly assigning 
  # them to different levels based on domain knowledge
  
  sitting <- factor( rep("level4",N), ordered=T,levels =c("level1", "level2", "level3", "level4"))
  mod1<-sample(mod,round(length(mod)*0.35),replace = FALSE)
  no1<-sample(no,round(length.no*0.30),replace = FALSE)
  mild1<-sample(mild,round(length.mild*0.20),replace = FALSE)
  sitting[c(no1,mild1,mod1)]<-"level1"
  
  
  no2<-sample(subset(no, !(no %in% no1)),round(length.no*0.30),replace = FALSE)
  mild2<-sample(subset(mild, !(mild %in% mild1)),round(length.mild*0.25),replace = FALSE)
  mod2<-sample(subset(mod,!(mod %in% mod1)),round(length.mod*0.10),replace = FALSE)
  sitting[c(no2,mild2,mod2)]<-"level2"
  
  mod3<-sample(subset(mod, !(mod %in% c(mod1,mod2)),round(length.mod*0.75),replace = FALSE))
  no3<-sample(subset(no, !(no %in% c(no1,no2))),round(length.no*0.35),replace = FALSE)
  sitting[c(mod3,no3)]<- "level3"
  
  # The chronic conditions diabetes, canc, cvd and hbp wa simulated using logistic function to take into account 
  # of age and level of sitting 
  
  logistic <- function(t) 1 / (1 + exp(-t))
  canc <- runif(length(age))< .05*logistic((age-50)/10) + .065*logistic((as.numeric(sitting)-3)/2)
  cvd<- runif(length(age))< .06*logistic((age-45)/2) + .07*logistic((as.numeric(sitting)-3)/2)
  diab <- runif(length(age))< .07*logistic((age-50)/10) + .086*logistic((as.numeric(sitting)-3)/2)
  hbp <- runif(length(age))< .4*logistic((age-45)/10) +.08*logistic((as.numeric(sitting)-3)/2)
}

model6<- glm(chronic.disease~sitting + age ,family = binomial(),data=data)
summary(model6)

