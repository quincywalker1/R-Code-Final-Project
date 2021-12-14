library(faraway)
library(readr)
happy <- read_csv("happy.csv")
View(happy)
happy = happy[, -1]
happy
happy$region = factor(happy$region)
summary(happy)


### 1 Initial Data Analysis
##### a #####
par(mfrow = c(2,2))
plot(happiness~money, data=happy)
plot(happiness~love, data=happy)
plot(happiness~work, data=happy)
plot(happiness~region, data=happy)

##### b #####
happyA = happy[which(happy$region == "A"), -5]
par(mfrow = c(2,2))
plot(happiness~money, data=happyA)
plot(happiness~love, data=happyA)
plot(happiness~work, data=happyA)


##### c #####
happyB = happy[which(happy$region == "B"), -5]
par(mfrow = c(2,2))
plot(happiness~money, data=happyB)
plot(happiness~love, data=happyB)
plot(happiness~work, data=happyB)

### 2 Linear Model
lm1 = lm(happiness~.,data = happy)
summary(lm1)

par(mfrow = c(1,1))
plot(lm1$fitted.values, lm1$residuals)
abline(h=0)

qqnorm(lm1$residuals)
qqlines(lm1$residuls)

plot(lm1$residuals[1:299],lm1$ residuals[2:300])

par(mfrow = c(2,2))
termplot(lm1, partial.resid = T)


lm1A = lm(happiness~.,data = happyA)
summary(lm1A)

par(mfrow = c(1,3))
plot(lm1A$fitted.values, lm1A$residuals)
abline(h=0)

qqnorm(lm1A$residuals)
qqlines(lm1A$residuls)

plot(lm1A$residuals[1:199], lm1$residuals[2:200])

par(mfrow = c(1,1))
h= hatvalues(lm1A)
halfnorm(h)

stud = rstudent(lm1A)
sort(abs(stud))
abs(qt(0.05/200/2, df=200-4-2))

cook = cooks.distance(lm1A)
halfnorm(cook)

par(mfrow = c(2,2))
termplot(lm1A, partial.resid = T)


### 3 Generalized Linear Model
glmA1 = glm(cbind(happiness,10- happiness)~., data = happyA, family = binomial(link="logit"))
glmA2 = glm(cbind(happiness,10- happiness)~., data = happyA, family = binomial(link="probit"))
glmA3 = glm(cbind(happiness,10- happiness)~., data = happyA, family = binomial(link="cauchit"))
glmA4 = glm(cbind(happiness,10- happiness)~., data = happyA, family = binomial(link="cloglog"))
summary(glmA1)
summary(glmA2)
summary(glmA3)
summary(glmA4)

par(mfrow = c(4,4))
plot(glmA1)
plot(glmA2)
plot(glmA3)
plot(glmA4)

glmB1 = glm(cbind(happiness,10- happiness)~., data = happyB, family = binomial(link="logit"))
glmB2 = glm(cbind(happiness,10- happiness)~., data = happyB, family = binomial(link="probit"))
glmB3 = glm(cbind(happiness,10- happiness)~., data = happyB, family = binomial(link="cauchit"))
glmB4 = glm(cbind(happiness,10- happiness)~., data = happyB, family = binomial(link="cloglog"))
summary(glmB1)
summary(glmB2)
summary(glmB3)
summary(glmB4)

par(mfrow = c(4,4))
plot(glmB1)
plot(glmB2)
plot(glmB3)
plot(glmB4)

