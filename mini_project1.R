install.packages("ISLR")
library(ISLR)

View(College)
help(College)
str(College)
dim(College)
names(College)

sum(is.na(College))

class(College[,1])
summary(College)
cor(College[2:18])

###########################

# full model #

attach(College)
full.model =lm(Apps~., data= College)
summary(full.model)

predicted_y= predict(full.model); predicted_y

windows()
par(mfrow=c(2,2)) 
plot(full.model)

residuals(full.model)

install.packages("car")
library(car)
vif(full.model)

install.packages("MASS")
library(MASS)

# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)

predicted_y1= predict(step.model.reg); predicted_y1
residuals(step.model)

windows()
par(mfrow=c(2,2)) 
plot(step.model)


vif(step.model)

reg= lm(Apps~Private+Accept+Top10perc+Top25perc+P.Undergrad+Outstate+Room.Board +PhD +Expend +Grad.Rate+Enroll:F.Undergrad, data= College)
summary(reg)
vif(reg)

windows()
par(mfrow=c(2,2)) 
plot(reg)

reg1= lm(Apps~Private+Accept+Top10perc+Top25perc+P.Undergrad+Outstate+Room.Board +PhD +Expend +Grad.Rate+F.Undergrad, data= College)
summary(reg1)
vif(reg1)

windows()
par(mfrow=c(2,2)) 
plot(step.model)

reg2= lm(Apps~Private+Accept+Top10perc+Top25perc+P.Undergrad+Outstate+Room.Board +PhD +Expend +Grad.Rate+Enroll, data= College)
summary(reg2)
vif(reg2)

windows()
par(mfrow=c(2,2)) 
plot(step.model)





############################

# regsubsets()
regressionfits= regsubsets(Apps~., data=College, method ="seqrep",nvmax = 17)
regressionfits.summary = summary(regressionfits);regressionfits.summary

names(regressionfits.summary)

regressionfits.summary$which
regressionfits.summary$rsq
regressionfits.summary$rss
regressionfits.summary$adjr2
regressionfits.summary$cp
regressionfits.summary$bic

# If a model is more than 2 AIC units lower than another, then it is considered
# significantly better than that model.

regsubset.model.reg= lm(Apps~ Private+Accept+Top10perc+Expend+Outstate+Enroll+Room.Board+Top25perc+PhD+ F.Undergrad+ P.Undergrad+ S.F.Ratio+ Grad.Rate, data=College )
summary(regsubset.model.reg)
##############################

anova(reg, regsubset.model.reg )

# calculating the value of F-statistics by formula

((858690765-825317242)/4)/(825317242/764)




