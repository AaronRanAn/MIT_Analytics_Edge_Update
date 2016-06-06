# -----------------------------
# ------ Quiz 2: Anova --------
# -----------------------------

library(readxl)
library(readr)
library(dplyr)

rat = read_csv("../../../../Desktop/rat_wt.csv")

rat %>% 
        filter(SOURCE == "Cereal" & TYPE == "Low") %>% 
        summarise(mean = mean(WEIGHTGAIN), 
                  std = sd(WEIGHTGAIN))

rat %>% 
        filter(SOURCE == "Beef" & TYPE == "High") %>% 
        summarise(mean = mean(WEIGHTGAIN), 
                  std = sd(WEIGHTGAIN))


rat %>% 
        filter(SOURCE == "Beef" & TYPE == "Low") %>% 
        summarise(mean = mean(WEIGHTGAIN), 
                  std = sd(WEIGHTGAIN))


rat %>% 
        filter(SOURCE == "Cereal" & TYPE == "High") %>% 
        summarise(mean = mean(WEIGHTGAIN), 
                  std = sd(WEIGHTGAIN))


# -----------------------------
# ------ Quiz 3: Regression ---
# -----------------------------

df = data.frame(x=c(20,11,15,10,17,19), y=c(5,15,14,17,8,9))

lm(y~x, df)

library(caret)

data(cars)

fit_0 <- lm(formula = cars$Price ~ cars$Mileage + cars$convertible, data = cars)

fit_1 <- lm(formula = cars$Price ~ cars$Mileage * cars$Cylinder * cars$convertible,
            data = cars)

summary(fit_1)

fit_2 <- lm(formula = cars$Price ~ ., data = cars) 

summary(fit_2)

cor(cars$coupe, cars$sedan)

summary(lm(cars$Price ~ ., data=cars))
