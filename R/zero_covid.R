data("zero_covid_2021")
library(caTools)

library(randomForestSRC)
library(ggplot2)
library(dplyr)


zero_covid_2021
sample <- sample.split(zero_covid_2021$Anxiety, SplitRatio = 0.7)
train  <- subset(zero_covid_2021, sample == TRUE)
test   <- subset(zero_covid_2021, sample == FALSE)

relation <- lm(Anxiety~Baidu_Move_in_Index,Baidu_Move_out_Index,data=train)
lm_predicted = predict(relation, newdata = test, type = "response")

mean((lm_predicted-test$Anxiety)^2)
