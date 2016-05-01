### Assignment 1

######################### Part 1: Chicago Crime

library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(magrittr)

mvt <- read_csv("../data/mvtWeek1.csv")

dim(mvt)
max(mvt$ID)
min(mvt$Beat)

View(mvt)

mvt %>% count(Arrest==T)
mvt %>% count(LocationDescription == "ALLEY")

head(mvt$Date)

DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))

median(DateConvert)

mvt$Month = months(DateConvert)

mvt$Weekday = weekdays(DateConvert)

mvt$Date = DateConvert

mvt %>% 
        count(Month) %>% 
        filter(n==min(n))

mvt %>% 
        count(Weekday) %>% 
        filter(n==max(n))

mvt %>% 
        filter(Arrest==T) %>% 
        group_by(Month) %>% 
        summarise(n=n()) %>% 
        filter(n==max(n))

mvt %>% 
  ggplot(aes(Date)) + 
        geom_histogram(bins = 100, alpha = 0.8) +
        fte_theme() 

mvt %>% 
        mutate(half = Year>2007) %>% 
        group_by(half) %>% 
        summarise(n=n())

mvt %>% 
        group_by(Year, Arrest) %>%
        filter(Year==2012) %>% 
        summarise(n=n())

mvt %>% 
        group_by(LocationDescription) %>% 
        summarise(n=n()) %>% 
        arrange(desc(n)) %>% 
        head(6)

mvt %>% 
        filter(LocationDescription %in% c("STREET", "PARKING LOT/GARAGE(NON.RESID.)", "ALLEY",
                                          "GAS STATION", "DRIVEWAY - RESIDENTIAL")) %>% 
        dim()

mvt %>% 
        filter(LocationDescription %in% c("STREET", "PARKING LOT/GARAGE(NON.RESID.)", "ALLEY",
                                          "GAS STATION", "DRIVEWAY - RESIDENTIAL")) %>% 
        group_by(LocationDescription, Arrest) %>% 
        summarise(n=n()) %>% 
        left_join(
                mvt %>% 
                        filter(LocationDescription %in% c("STREET", "PARKING LOT/GARAGE(NON.RESID.)", "ALLEY",
                                                          "GAS STATION", "DRIVEWAY - RESIDENTIAL")) %>% 
                        group_by(LocationDescription) %>% 
                        summarise(n_sub = n())
        ) %>% 
        filter(Arrest==T) %>% 
        mutate(arst_rt = n/n_sub)

# Gas Station

mvt %>% 
        filter(LocationDescription == "GAS STATION") %>% 
        group_by(Weekday) %>% 
        summarise(n = n()) %>% 
        arrange(desc(n))

# Saturday

mvt %>% 
        filter(LocationDescription == "DRIVEWAY - RESIDENTIAL") %>% 
        group_by(Weekday) %>% 
        summarise(n = n()) %>% 
        arrange(n)

# Saturday

######################### Part 2: Stock Market

library(lubridate)

ibm = read_csv("../data/week 1/IBMStock.csv", col_types = "cd")
ge = read_csv("../data/week 1/GEStock.csv", col_types = "cd")
pg = read_csv("../data/week 1/ProcterGambleStock.csv", col_types = "cd")
cc = read_csv("../data/week 1/CocaColaStock.csv", col_types = "cd")
bi = read_csv("../data/week 1/BoeingStock.csv", col_types = "cd")


ibm$brand = "ibm"; ge$brand = "ge"; pg$brand = "pg"; cc$brand = "cc"; bi$brand = "bi"

st = rbind(ibm, ge, pg, cc, bi)
st$Date = mdy(st$Date)

st %>% 
        group_by(brand) %>% 
        summarise(n=n()) # 480

min(st$Date); max(st$Date); # 1970 and 2009

st %>% 
       group_by(brand) %>% 
       summarise(avg_st = mean(StockPrice), # 144.37503
                 min_st = min(StockPrice), # 9.293636
                 max_st = max(StockPrice), # 146.5843
                 md_st = median(StockPrice), # 44.88340
                 sd_st = sd(StockPrice)) # 18.19414

st %>% 
        filter(brand == "cc") %>% 
        filter(StockPrice == max(StockPrice) | StockPrice == min(StockPrice))

# max 
# min


st %>% 
        ggplot(aes(x = Date, y = StockPrice, colour = brand)) +
        geom_line() + 
        geom_vline(xintercept = as.numeric(as.POSIXct("2000-03-01")), color = "grey", lwd = 3, alpha= 0.3)

ggsave("stock.png")

st %>% 
        filter(brand %in% c("cc", "pg")) %>% 
        ggplot(aes(x = Date, y = StockPrice, colour = brand)) +
        geom_line() + 
        # geom_rect(aes(xmin=as.POSIXct("1983-01-01"), 
        #            xmax=as.POSIXct("1983-12-01"), ymin=0, ymax=Inf, alpha = 0.1), size = 0, fill = "green")
        annotate("rect", xmin=as.POSIXct("1983-01-01"), xmax=as.POSIXct("1983-12-01"), ymin=0, ymax=Inf, alpha=0.2, fill="red")   

ggsave("stock_zoom.png")

st %>% 
        ggplot(aes(x = Date, y = StockPrice, colour = brand)) +
        geom_line() + 
        geom_vline(xintercept = as.numeric(as.POSIXct("2000-03-01")), color = "blue", lwd = 3, alpha= 0.3)

st %>% 
        ggplot(aes(x = Date, y = StockPrice, colour = brand)) +
        geom_line() + 
        # geom_rect(aes(xmin=as.POSIXct("1983-01-01"), 
        #            xmax=as.POSIXct("1983-12-01"), ymin=0, ymax=Inf, alpha = 0.1), size = 0, fill = "green")
        annotate("rect", xmin=as.POSIXct("1995-01-01"), xmax=as.POSIXct("2005-12-01"), ymin=0, ymax=Inf, alpha=0.2, fill="red")

# use group_by and filter to filter within each group 

st %>% 
        group_by(brand) %>% 
        filter(StockPrice == max(StockPrice))
        

st %>%  filter(Date > ymd("1995-01-01") & Date < ymd("2001-01-01")) %>% 
        ggplot(aes(x = Date, y = StockPrice, colour = brand)) +
        geom_line() + 
        geom_vline(xintercept = as.numeric(as.POSIXct("1997-09-01")), color = "blue", lwd = 1, alpha= 0.3) + 
        geom_vline(xintercept = as.numeric(as.POSIXct("1997-11-01")), color = "blue", lwd = 1, alpha= 0.3)

ggsave("stock_zoom_2.png")

st %>%  filter(Date > ymd("2003-01-01") & Date < ymd("2006-01-01")) %>% 
        ggplot(aes(x = Date, y = StockPrice, colour = brand)) +
        geom_line() 

st %>% 
        group_by(brand, month(Date)) %>% 
        summarise(mon_st = mean(StockPrice)) %>% 
        left_join(
                
                st %>% group_by(brand) %>% 
                        summarise(ov_mean = mean(StockPrice))
        ) %>% 
        mutate(grt_avg = mon_st > ov_mean) %>% 
        filter(brand == "ibm")
        

st %>% 
        group_by(brand, month(Date)) %>% 
        summarise(mon_st = mean(StockPrice)) %>%
        mutate(rank = min_rank(mon_st)) %>% 
        arrange(brand, rank) %>% 
        filter(rank == 12)
        

st %>% 
        group_by(brand, month(Date)) %>% 
        summarise(mon_st = mean(StockPrice)) %>%
        mutate(rank = min_rank(mon_st)) %>%
        left_join(
                
                st %>%  group_by(brand, month = month(Date)) %>% 
                        summarise(mon_st = mean(StockPrice)) %>% 
                        mutate(dec_st = ifelse(month==12, mon_st, 0), 
                               jan_st = ifelse(month==1, mon_st, 0)) %>% 
                        filter(month %in% c(1, 12)) %>% 
                        group_by(brand) %>% 
                        summarise(dec_st = sum(dec_st), 
                                  jan_st = sum(jan_st))
        ) %>% 
        mutate(check = (mon_st > dec_st & mon_st > jan_st))


st %>% 
        group_by(brand, month = month(Date)) %>% 
        summarise(mon_st = mean(StockPrice)) %>%
        mutate(rank = min_rank(mon_st)) %>%
        filter(month %in% c(12, 1))

## Learn to use the window function 

######################### Part 3: EMOGRAPHICS AND EMPLOYMENT IN THE UNITED STATES


cps = read_csv("../data/week 1/CPSData.csv")

View(cps)

dim(cps)

table(cps$Industry) %>% 
        as.data.frame() %>% 
        arrange(desc(Freq)) %>% 
        head()

table(cps$Citizenship=="Non-Citizen") %>% prop.table() %>% sort()

table(cps$Race, cps$Hispanic)

cps %>% 
        summarise_each(funs(anyNA(.))) %>% 
        select(which(as.numeric(.)==1))

table(cps$Region, is.na(cps$Married))
table(cps$Sex, is.na(cps$Married))
table(cps$Age, is.na(cps$Married))
table(cps$Citizenship, is.na(cps$Married))

cps %>% 
        filter(is.na(MetroAreaCode)==T) %>% count(State) %>% dim(.)    # table() is hard to work with dplyr, use count()

cps %>% 
        filter(is.na(MetroAreaCode)==F) %>% count(State) %>% dim(.)

# minus that with all states 

cps %>% 
        filter(is.na(MetroAreaCode)==T) %>% count(Region) %>% 
        mutate(prop = n/sum(n))


# MetroAreaCode is missing if an interviewee does not live in a metropolitan area.


cps %>% 
        filter(is.na(MetroAreaCode)==T) %>% 
        group_by(State) %>% 
        summarise(n = n()) %>% 
        left_join(
                cps %>% 
                        group_by(State) %>% 
                        summarise(n_tot = n())
        ) %>% 
        mutate(prop = n/n_tot) %>% 
        arrange(abs(prop - 0.3))

cps %>% 
        filter(is.na(MetroAreaCode)==T) %>% 
        group_by(State) %>% 
        summarise(n = n()) %>% 
        left_join(
                cps %>% 
                        group_by(State) %>% 
                        summarise(n_tot = n())
        ) %>% 
        mutate(prop = n/n_tot) %>% 
        arrange(desc(prop))

ma_code <- read_csv("../data/week 1/MetroAreaCodes.csv")

cy_code <- read_csv("../data/week 1/CountryCodes.csv")

dim(ma_code)
dim(cy_code)

ma_code$Code %<>% as.numeric()

cps %>% 
        left_join(ma_code, c("MetroAreaCode"="Code")) %>% count(is.na(MetroArea))


cps %<>% 
        left_join(ma_code, c("MetroAreaCode"="Code"))

cps %>% 
        filter(MetroArea %in% c("Atlanta-Sandy Springs-Marietta, GA",
                                    "Baltimore-Towson, MD", 
                                    "Boston-Cambridge-Quincy, MA-NH", 
                                    "San Francisco-Oakland-Fremont, CA")) %>% 
        count(MetroArea) %>%
        top_n(2, n)

cps %>% 
        filter(Hispanic==1) %>% 
        count(MetroArea) %>% 
        left_join(
                cps %>% 
                        count(MetroArea) %>% 
                        select(MetroArea, n_sub = n)) %>% 
        mutate(n_prop = n/n_sub) %>% 
        top_n(1, n_prop)

cps %>% 
        group_by(MetroArea) %>% 
        summarise(asian_prop = mean(Race == "Asian", na.rm = T)) %>% 
        filter(asian_prop > 0.2)

cps %>% 
        group_by(MetroArea) %>% 
        summarise(edu_prop = mean(Education == "No high school diploma", na.rm = T)) %>% 
        arrange(edu_prop)


cy_code$Code %<>% as.numeric()

cps %<>% 
        left_join(cy_code, c("CountryOfBirthCode"="Code"))

cps %>% 
        filter(is.na(Country)==T) %>% 
        count()

cps %>% 
        filter(!(Country %in% c("Mexico", "United States", "Canada"))) %>% 
        count(Country) %>% 
        top_n(1)

cps %>% 
        filter(MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA" & is.na(Country)==F) %>% 
        count(Country!="United States") %>% prop.table(.)

cps %>% 
        filter(Country=="India") %>% 
        count(MetroArea) %>% 
        top_n(1)

cps %>% 
        filter(Country=="Somalia") %>% 
        count(MetroArea) %>% 
        top_n(2)
        


