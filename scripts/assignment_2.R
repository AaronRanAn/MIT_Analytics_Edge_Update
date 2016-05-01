library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(magrittr)



#################### Part 1: Climate Change


cc <- read_csv("../data/week 2/climate_change.csv")

cc %>% 
        filter(Year <= 2006) -> cc_train

cc %>% 
        filter(Year > 2006) -> cc_test

# Next, build a linear regression model to predict the dependent variable Temp, 
# using MEI, CO2, CH4, N2O, CFC.11, CFC.12, TSI, and Aerosols as independent variables 
# (Year and Month should NOT be used in the model). Use the training set to build the model.

cc_train %>% 
        lm(Temp ~ MEI + CO2 + CH4 + N2O + `CFC-11` + `CFC-12` + TSI + Aerosols, .) -> fit_0

fit_0 %>% summary()

cc_train %>% 
        cor(.) %>% 
        as.data.frame() %>% View()

cc_train %>% 
        lm(Temp ~ MEI + TSI + Aerosols + N2O, .) -> fit_1 

fit_1 %>% summary()

lm(Temp ~ MEI + CO2 + CH4 + N2O + `CFC-11` + `CFC-12` + TSI + Aerosols, cc_train) -> fit_0

step(fit_0)

fit_lm = lm(Temp ~ MEI + CO2 + N2O + `CFC-11` + `CFC-12` + TSI + Aerosols, data = cc_train)

fit_lm %>% summary()

y_hat = predict(fit_lm, newdata = cc_test)

y = cc_test$Temp

# calculate the R-square 

SSE = sum((y_hat - y)^2)
SST = sum((mean(cc_train$Temp) - y)^2)

1 - SSE/SST

#################### Part 2: READING TEST SCORES

pisa_train = read_csv("../data/week 2/pisa2009train.csv")
pisa_test = read.csv("../data/week 2/pisa2009test.csv")

dim(pisa_train)

pisa_train %>% 
        group_by(male) %>% summarise(mean(readingScore))

pisa_train %>% 
        summarise_each(funs(anyNA)) %>% 
        gather("col_names", "NA", grade:readingScore) %>% 
        filter(`NA`==T)

pisa_train %<>% na.omit()
pisa_test %<>% na.omit()

dim(pisa_train)
dim(pisa_test)


pisa_train$raceeth = relevel(as.factor(pisa_train$raceeth), "White")
pisa_test$raceeth = relevel(pisa_test$raceeth, "White")

lmScore = lm(readingScore~., data=pisa_train)

RMSE <- sqrt(mean(residuals(lmScore)^2))

29.542707*2

predict_test <- predict(lmScore, pisa_test)

max(predict_test, na.rm = T) - min(predict_test, na.rm = T)


sum((predict_test - pisa_test$readingScore)^2, na.rm = T) -> SSE

sqrt(mean((predict_test - pisa_test$readingScore)^2, na.rm = T))

# The answer there is wrong.... 

bias = mean(pisa_train$readingScore)

SST = sum((pisa_test$readingScore-bias)^2)

1 - SSE/SST

#################### Part 3: Fluenza

flu_train = read_csv("./Analytics/MOOC/data/week 2/FluTrain.csv")

flu_train %>% 
    top_n(1, ILI)

flu_train %>% 
    top_n(1, Queries)

flu_train %>% 
    ggplot(aes(ILI)) + 
    geom_histogram(position="identity", bins= 50, alpha=0.7)

flu_train %>% 
    ggplot(aes(x=Queries, y=log(ILI))) + 
    geom_point(colour = "#4080FF", size = 3, alpha=0.3) +
    stat_smooth() + 
    theme(panel.background = element_rect(fill = "#ECF0F7"))

ggsave("flu_trend.png")

flu_train %>% 
    lm(log(ILI)~1+Queries, .) -> fit_0

flu_train %$%
    cor(log(ILI), Queries)^2

flu_test = read_csv("./Analytics/MOOC/data/week 2/FluTest.csv")
    
pred_test_1 = exp(predict(fit_0, newdata=flu_test))

flu_test %>% 
    cbind(pred_test_1) %>% 
    filter(str_detect(Week, "2012-03-11"))

## test set RMSE 

sqrt(mean((pred_test_1 - flu_test$ILI)^2)) # test RMSE

## Time Series

install.packages("zoo")
library(zoo)

flu_train %>% 
    mutate(ILILag2 = lag(ILI, 2)) %$%
    table(is.na(ILILag2))

flu_train %<>% 
    mutate(ILILag2 = lag(ILI, 2))

flu_train %>% 
    ggplot(aes(x=log(ILI), y=log(ILILag2))) + 2
    geom_point(colour = "#4080FF", size = 3, alpha=0.3) +
    stat_smooth() + 
    theme(panel.background = element_rect(fill = "#ECF0F7"))

ggsave("log_ILI_ILI2.png")

flu_train %>% 
    lm(log(ILI) ~ 1 + Queries + log(ILILag2), .) -> fit_1

summary(fit_1)

flu_test %<>% 
    mutate(ILILag2 = lag(ILI, 2)) 

flu_test %$%
    table(is.na(ILILag2))

dim(flu_train) #417
dim(flu_test) #52

flu_test$ILILag2[1] = flu_train$ILI[416]
flu_test$ILILag2[2] = flu_train$ILI[417]

## evaluation model 

pred_test_2 = exp(predict(fit_1, newdata=flu_test))

sqrt(mean((pred_test_1 - flu_test$ILI)^2)) # test RMSE : 0.2942029 | Model 1 test RMSE: 

