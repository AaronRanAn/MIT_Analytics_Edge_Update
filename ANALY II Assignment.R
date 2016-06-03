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

library(lubridate)

ymd("2016-01-04") - ymd("2014-08-17")

ymd("2014-08-17") + days(127)
201