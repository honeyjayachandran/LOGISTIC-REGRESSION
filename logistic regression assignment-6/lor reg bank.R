library(AER)
deposit <- read.csv(file.choose(),sep = ";")


View(deposit)
str(deposit)
table(deposit$y) #no 39922 yes 5289
deposit$y <- as.numeric(deposit$y)
table(deposit$y)
deposit$y[deposit$y == 1] <- 1

deposit$y[deposit$y == 2] <- 0
 
table(deposit$y)

View(deposit)
 
model <- glm(formula = y ~ . , data = deposit, family = binomial())

summary(model)
model_redused <- glm(formula = y ~ job + marital + education + balance + housing + loan + contact + day + month + campaign + poutcome ,
                     data = deposit, family = binomial())

summary(model_redused)


######
 