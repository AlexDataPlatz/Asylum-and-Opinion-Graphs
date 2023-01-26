library(eurostat)
library(tidyverse)
library(tseries)
library(aTSA)
library(urca)
library(lmtest)
library(sandwich)
library(dynlm)
library(broom)
library(gtsummary)
library(writexl)
library(readxl)
library(sjmisc)
library(sjlabelled)
library(dLagM)
library(lmtest)
library(stargazer)
library(zoo)
library(lubridate)

# download and clean decisions data, calculate asylum recognition rates
# first 2008 to present data
# data id available from:
# https://ec.europa.eu/eurostat/web/main/data/database
# download can take a while (is > 4.5GB, may take ~ 30 minutes)
dec_all <- get_eurostat("migr_asydcfstq",
                        time_format = "raw")
head(dec_all)

# list all unique decision and country values 
unique(dec_all$decision)
unique(dec_all$geo)

# filter decisions by value
# first select all total decision values
dectot <- dec_all %>%
  filter(decision == "TOTAL")

# group total decisions by country and date
dectot1 <- dectot %>% 
  group_by(geo, time, decision) %>% 
  summarise(decq = sum(values))

# next do the same with all positive decisions
decpos <- dec_all %>%
  filter(decision == "TOTAL_POS")

decpos1 <- decpos %>% 
  group_by(geo, time, decision) %>% 
  summarise(posq = sum(values))

# create new db to calculate quarterly asylum recognition rate (pospq) 
dec_ctry <- dectot1
dec_ctry$posq <- decpos1$posq

# asylum recognition rate is (total positive/ total decisions)*100
dec_ctry$pospq <- dec_ctry$posq / dec_ctry$decq * 100

# new db now includes pospq value
head(dec_ctry)

# for clarity, rename time variable --> date
dec_ctry <- dec_ctry %>% rename(date = time)

# reformat date variable from quarters to year-month-day
dec_ctry$date <- as.Date(as.yearqtr(dec_ctry$date, format = "%YQ%q"),
                         frac = 1)


# filter to exclude more recent data
# (excludes possibly incomplete data and pandemic effects)
dec_ctry <- dec_ctry %>%
  filter( date < "2020-03-31")

# save this db to wd (then can import next time)
# set working directory to preferred location, e.g...
setwd("~/Desktop/Data")
# then save db there
write.csv(dec_ctry, "dec_ctry0819.csv")


# create line graph for top 5 asylum receiving EU28 countries
# import previously created db
ctry <- read.csv("dec_ctry0819.csv")

# calculate annual decisions/ positive decisions by country and year
ctry1 <- ctry %>% 
  group_by(geo, year) %>% 
  summarise(dec3y = sum(decq),
            pos3y = sum(posq))  

# filter 2010-2019
ctry2 <- ctry1 %>%
  filter(year > 2009 & year < 2020)

# rename for next step
ctry3 <- ctry2

# calculate asylum recognition rates per year
ctry3$posp3y <- ctry3$pos3y / ctry3$dec3y * 100

# filter to top 5 asylum receiving countries (2010-19)
target <- c("DE", "SE", "IT", "FR", "UK")

ctry4 <- ctry3 %>%
  filter(geo %in% target)

# create line graph with results
# graph output will be saved to working directory
tiff("EUARR.tiff", units="in", width=10, height=10, res=450) 
ggplot(ctry4, aes(year, posp3y)) +
  geom_line(aes(colour = geo)) +
  theme_minimal() +
  theme(
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 25))  +
  labs(y= "Asylum Recognition Rates, 2010-19 (%)", x = NULL) +
  scale_fill_manual(values=c("#003f5c",
   "#58508d",
    "#bc5090",
    "#ff6361",
    "#ffa600"))
dev.off()         


tiff("EU5.tiff", units="in", width=20, height=10, res=100)
ggplot(ctry4, aes(x=year, y=posp3y, line = geo, color = geo)) + 
  geom_line(position = position_dodge(width = 0), size = 3) +
  scale_color_manual(values = c("#055e8a",
    "#7262aa",
    "#cd589a",
    "#fe6560",
    "#f09c00"), 
                     name = "", 
                     breaks=c("DE","FR", "IT", "SE", "UK"), 
                     labels = c("DE","FR", "IT", "SE", "UK")) +
  theme_bw() +
  labs(y= "Asylum Recognition Rate (%)", x = NULL) +
  scale_x_continuous(breaks = c(2012, 2016)) +
  scale_y_continuous(limits = c(0, 85), 
                     breaks = c(0, 50)) +
  theme(text = element_text(size = 40),
        plot.title = element_text(hjust = 0.5),
  )
dev.off()
