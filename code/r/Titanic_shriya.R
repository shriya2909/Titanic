


train <- read.csv("raw/train.csv", stringsAsFactors=FALSE)
test <- read.csv("raw/test.csv", stringsAsFactors=FALSE)

summary(train)
str(train)


View(train)
## check NA values
sapply(train, function(x) {sum(is.na(x))})
sapply(test, function(x) {sum(is.na(x))})
unique(train$SibSp)
unique(train$Parch)

train_w_age <- subset(train, !(is.na(train$Age)))
train_wo_age <- subset(train, is.na(train$Age))
fit<-lm(Age~Pclass+Survived+SibSp,data= train)
summary(fit)

train_wo_age$Age <- round(predict(fit,train_wo_age))
updated_age<-merge(train_w_age,train_wo_age,all=TRUE)
View(updated_age)

drops <- c("Name", "Ticket", "Cabin")
Cleansed_data<- updated_age[,!(names(updated_age) %in% drops)]

Cleansed<-dummy.data.frame(Cleansed_data,sep="_")
View(Cleansed)


# lm annova using diff features 

m0 <- glm(Survived~ 1, Cleansed) #RSS - 210.73
m1 <- update(m0, ~ Pclass) #RSS - 186.58
anova(m0,m1) # Adding Pclass lowers the RSS 

m2 <- update(m1, ~ . + fSex_female+fSex_male) #RSS - 133.25
anova(m1,m2) # Adding Sex significantly lowers the RSS 

m3 <- update(m2, ~ . + Age) # RSS -  129.90
anova(m2,m3) # Adding Age lowers the RSS further

m4 <- update(m3, ~ . + SibSp) #RSS- 126.73 
anova(m3,m4) # Adding SibSp lowers the RSS further

m5 <- update(m3, ~ . + Parch) #RSS- 129.01
anova(m3,m5) # Adding Parch to m3 lowers the RSS but not more than adding SibSp

m6 <-   update(m3, ~ . + Fare) #RSS- 129.89
anova(m3,m7) # Adding Fare to m3 lowers the RSS just by 0.01

m7 <- update(m3, ~ . + Parch+ SibSp) #RSS- 126.63
anova(m4,m7) # Adding both Parch and SibSp to m3 lowers the RSS just by .10 when compared to m4

m8 <- update(m7, ~ . + Fare) #RSS- 126.44
anova(m7,m8) # Adding Fare to m8 lowers the RSS just by .19 when compared to m8

#Least RSS is given by m9 

m9 <- update(m8, ~ . +  Embarked_ + Embarked_C +  Embarked_Q +  Embarked_S) #RSS-  125.81
anova(m10,m9)
#m10 further decreases RSS by .63

summary(m9) #Multiple R-squared:  0.403,	Adjusted R-squared:  0.3969 

