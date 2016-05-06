library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(magrittr)
library(ROCR)
library(tidyr)
library(mice)
library(caTools)

#################### Part 1: song popularity 


song = read_csv("../data/week 3/songs.csv")

song %>% 
        filter(year <= 2009) -> song_train

song_train %>% nrow()

song %>% 
        filter(year > 2009) -> song_test

song_train %>% 
        select(-c(year, songtitle, artistname, songID, artistID)) %>% 
        glm(Top10 ~.,., family = "binomial") -> song_log_1

summary(song_log_1)

## holy multicollinearity batman! 

song %$%
        cor(loudness, energy)

song_train %>% 
        select(-c(year, songtitle, artistname, songID, artistID)) %>% 
        glm(Top10 ~. - loudness,., family = "binomial") -> song_log_2

song_train %>% 
        select(-c(year, songtitle, artistname, songID, artistID)) %>% 
        glm(Top10 ~. - energy,., family = "binomial") -> song_log_3

song_test %>% 
        mutate(pred_top = predict(song_log_3, ., type="response"), ### DON'T FORGET type = "response"
               pred_top_class = ifelse(pred_top >= 0.45, 1, 0), 
               pred_check = pred_top_class==Top10) %>% 
        count(pred_check == T)

328/(328+45) # accuracy 

song_test %>% 
        count(Top10) %>% 
        mutate(prop = n/sum(n))


song_test %>% 
        mutate(pred_top = predict(song_log_3, ., type="response"), ### DON'T FORGET type = "response"
               pred_top_class = ifelse(pred_top >= 0.45, 1, 0), 
               pred_check = pred_top_class==Top10) %>% 
        count(pred_check == T, Top10)

# output 

#    pred_check == T  Top10   n
# (lgl) (int) (int)
# 1           FALSE     0     5
# 2           FALSE     1    40
# 3            TRUE     0   309
# 4            TRUE     1    19

sen = 19 / (19 + 40)

spec = 309 / (309+5)

### Get sensitivity and specificity 

# Sensitivity (also called the true positive rate, or the recall in some fields) 
# measures the proportion of positives that are correctly identified as such 
# (e.g., the percentage of sick people who are correctly identified as having the condition).


# Specificity (also called the true negative rate) 
# measures the proportion of negatives that are correctly identified as such 
# (e.g., the percentage of healthy people who are correctly identified as not having the condition).

# Model 3 prefer specificity (precision) and provide a confident prediction over the one that would make top 10. 
# thus very useful for investors who would benefit from even knowing one songs to be in the Top 10. 

# In this case, the cost of false positive is low, the gain of true positive is really high. 

#################### Part 2: Parole Prediction

prl = read_csv("../data/week 3/parole.csv")

nrow(prl)

prl %>% 
        count(violator)

prl$state %<>% as.factor()
prl$crime %<>% as.factor()

# Spliting train & test

set.seed(144)
split = sample.split(prl$violator, SplitRatio = 0.7)
prl_train = subset(prl, split == TRUE)
prl_test = subset(prl, split == FALSE)

nrow(prl_train) / nrow(prl)

prl_train %>% 
        glm(violator ~ ., ., family = "binomial") -> prl_log_1

summary(prl_log_1)

log_odss_diff = 1.6119919

odds = exp(log_odss_diff) # note that we are interpret an coeff for binary

# to convert coef to odds, exp() that coeff 
# to convert odds back to prob, follow the func below

# odds = pi / (1-pi)
# pi = odds / (1+odds)

coeff_vec = as.vector(prl_log_1$coefficients)
one_point = c(1, 1, 1, 50, 0, 0, 0, 3, 12, 0, 1, 0, 0)

odds_pred = exp(sum(coeff_vec*one_point))
prob_pred = odds_pred / (1+odds_pred)

prl_test %<>% 
        mutate(pred_vio = predict(prl_log_1, ., type="response"), 
               pred_class = ifelse(pred_vio > 0.5, 1,0), 
               pred_check = pred_class == violator
               )

prl_test %>% 
        top_n(1, pred_vio)

prl_test %>% 
        count(violator, pred_class)

sen = 12 / (12 + 11)
spec = 167 / (167+12)
acc = (167+12) / nrow(prl_test)

# in this case, the false negative, predicted non-violator ends up violates, is more cost. 
# thus we should be more conservative with cutoff point 

vio_pred_obj = prediction(prl_test$pred_vio, prl_test$violator)

performance(vio_pred_obj, "auc")@y.values # performance return S4 object 

# check this out: http://mlwiki.org/index.php/ROC_Analysis#AUC:_Area_Under_ROC_Curve

#################### Part 3: Lending Club 

loan = read_csv("../../data/week 3/loans.csv")

loan %>% 
        count(not.fully.paid) %$%
        prop.table(n)

loan %>% 
        summarise_each(funs(na_cnt = sum(is.na(.)))) %>% 
        gather("var_name", "na_cnt", 1:ncol(loan)) %>% 
        arrange(desc(na_cnt))

loan %>% 
        rowwise() %>% 
        mutate(na_check = apply(., 2, function(x) sum(is.na(x)))) %>% View()


loan %>% 
        filter(complete.cases(.)) %>% dim()

## imputation


library(mice)
set.seed(144)

vars.for.imputation = setdiff(names(loan), "not.fully.paid")
imputed = complete(mice(loan[vars.for.imputation]))
loan[vars.for.imputation] = imputed

# Split train and test

set.seed(144)
split = sample.split(loan$not.fully.paid, SplitRatio = 0.7)
loan_train = subset(loan, split == TRUE)
loan_test = subset(loan, split == FALSE)

loan_train %>% 
        glm(not.fully.paid ~., ., family="binomial") -> loan_log_1

# O(a)/O(b) = exp(logit(a)-logit(b))

loan_test %<>% 
        mutate(pred_notfully = predict(loan_log_1, ., type="response"), 
               pred_class = ifelse(pred_notfully > 0.5, 1,0), 
               pred_check = pred_class == not.fully.paid
        )

loan_test %>% 
        count(not.fully.paid, pred_class)

# not.fully.paid      pred_class n
# (int)      (dbl)             (int)
# 1              0          0  2400
# 2              0          1    13
# 3              1          0   457
# 4              1          1     3

accuracy = (2400+3) / (2400+13+457+3) # 0.8364079

accu_base = (2400) / (2400+13+457+3)

loan_pred_obj = prediction(loan_test$pred_notfully, loan_test$not.fully.paid)

performance(loan_pred_obj, "auc")@y.values # performance return S4 object 

## Smart baseline 

loan_train %>% 
        glm(not.fully.paid ~ int.rate, ., family="binomial") -> loan_log_0

loan_test %<>% 
        mutate(pred_notfully_0 = predict(loan_log_0, ., type="response"), 
               pred_class_0 = ifelse(pred_notfully_0 > 0.5, 1,0), 
               pred_check_0 = pred_class_0 == not.fully.paid
        )

max(exp(loan_test$pred_notfully_0)/(1+exp(loan_test$pred_notfully_0)))

max(loan_test$pred_notfully_0)

loan_test %>% 
        count(not.fully.paid, pred_class_0)

loan_pred_obj_0 = prediction(loan_test$pred_notfully_0, loan_test$not.fully.paid)

performance(loan_pred_obj_0, "auc")@y.values # performance return S4 object 


### investment

10 * exp(0.06*3)

loan_test$profit = exp(loan_test$int.rate*3) - 1

loan_test$profit[loan_test$not.fully.paid == 1] = -1

max(loan_test$profit)*10

loan_test %>% 
        filter(int.rate>=0.15) %$% 
        mean(profit)

loan_test %>% 
        filter(int.rate>=0.15) %>% 
        count(not.fully.paid)

max(loan_test$pred_notfully)

loan_test %>% 
        filter(int.rate>=0.15) %>% 
        arrange(pred_notfully) %>% 
        head(100) %>%
        # sum(profit)
        count(not.fully.paid)
        






