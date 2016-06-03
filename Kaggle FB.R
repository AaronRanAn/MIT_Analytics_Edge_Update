# -----------------------------------
# ------ EDA For Kaggle's FB --------
# -----------------------------------

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
library(viridis)
library(MASS)
library(pander)


fb_train <- read_csv("../../../Kaggle/FB Location/data/train.csv")
fb_test <- read_csv("../../../Kaggle/FB Location/data/test.csv") # for memory, haven't read it

fb_train %>% 
        sample_frac(0.01) -> fb_train_sample

dim(fb_train)

fb_train %>% 
        head(200) %>% View()

fb_train_sample %$% range(y)

# this is the global timestamp trend

fb_train_sample %>% 
        ggplot(aes(x = time)) +
        geom_density()

# what about the individual places? 

fb_train_sample %>% 
        group_by(place_id) %>% 
        summarise(freq = n()) %>% 
        top_n(10) %>% 
        arrange(desc(freq))

