
CreditCard<- read.csv(file.choose())

ccard <- CreditCard[1:100,]
ccard$income <- round(ccard$income, digits = 2)
ccard$expenditure <- round(ccard$expenditure, digits = 2)
ccard$age <- round(ccard$age + .01)
## suspicious:
CreditCard$age[CreditCard$age < 1]

ccard$age[79] <- 36

## 
ccard <- ccard[order(ccard$income),]
ccard0 <- subset(ccard, expenditure > 0)
cc_ols <- lm(expenditure ~ age + owner + income + I(income^2), data = ccard0)

## 
plot(residuals(cc_ols) ~ income, data = ccard0, pch = 19)

## 
mean(ccard$age)
prop.table(table(ccard$owner))
mean(ccard$income)

library(car)
library(lmtest)
library(zoo)
summary(cc_ols)
sqrt(diag(vcovHC(cc_ols, type = "HC0")))
sqrt(diag(vcovHC(cc_ols, type = "HC2"))) 
sqrt(diag(vcovHC(cc_ols, type = "HC1")))

bptest(cc_ols, ~ (age + income + I(income^2) + owner)^2 + I(age^2) + I(income^4), data = ccard0)
gqtest(cc_ols)
bptest(cc_ols, ~ income + I(income^2), data = ccard0, studentize = FALSE)
bptest(cc_ols, ~ income + I(income^2), data = ccard0)
