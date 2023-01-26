library(tidyverse)
library(ggrepel)
library(data.table)
library(convertr)
library(readxl)

# Creat dual y axis plots

# 
DEUKDB2 <- read_excel("DEUKDB_JPP.xlsx")


# see "UK Asylum Data.R" script to create ukdech dataframe

ukdec0119 <- ukdech %>%
  slice(5:76)

# Graph for the UK
mydata <- DEUKDB2 %>%
  filter(COUNTRY2 == "UK")

mydata0119 <- select(mydata, 2,3)

mydata0119[, 2:ncol(mydata0119)] <- lapply(2:ncol(mydata0119), 
                                           function(x) as.numeric(mydata0119[[x]]))




mydata0119$TIMM2 <- as.Date(mydata0119$TIMM2)

head(mydata0119)

mydata0119 <- na.omit(mydata0119)


# select data
uk0219 <- mydata0119 %>% slice(4:75)

uk0219$POSPH <- ukdec0119$posph

newdata1 <- uk0219

newdata2 <- uk0219



#remove missing values
newdata1 <- na.omit(newdata1)
newdata2 <- na.omit(newdata2)

# create mean values
X <- newdata1 %>% 
  group_by(TIMM2) %>% 
  summarise(sum_asap = mean(POSPH))

Y <- newdata2 %>%
  group_by(TIMM2) %>% 
  summarise(mean_sal = mean(POLSALH))

# bind 4 variables into dataframe, reatining character and continuous variables
Z <- cbind.data.frame(Y$TIMM2, X$sum_asap, Y$mean_sal, stringsAsFactors = FALSE)

Z <- Z %>% rename(date = 'Y$TIMM2', 
                  posph = 'X$sum_asap',
                  polsalh = 'Y$mean_sal')

Z$date <- as.Date(Z$date)


# build dual y axis variables and plots
shade1 <- Z %>%  
  mutate(from = as.Date("2001-03-01"),
         to  = as.Date("2016-06-01"))

puk <- ggplot() +
  geom_rect(data = shade1, 
                 aes(xmin = from, xmax = to, 
                     ymin = -Inf, ymax = Inf),
                 alpha = 0.002)
puk <- puk + (geom_line(data = Z, 
                        aes(x = date, y = posph, 
                            colour = "ARR"), size = 1.5)) 

puk <- puk + (geom_line(data = Z, 
                        aes(x = date, y = polsalh/1, 
                        colour = "Salience"),  size = 1.5))


puk <- puk + scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Salience"))

puk <- puk + labs(y = "Asylum Recognition Rate",
              x = "",
              colour = "",
              title = "UK")
puk <- puk + theme_bw() +
  theme(axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 10),
        legend.position = c(0.92, 0.1),
        legend.title = element_blank(),
        plot.title = element_text(vjust = - 6, hjust = .01, 
                                  size = 20)) +
  scale_colour_manual(values=c("red3",
                               "dodgerblue3"))

puk <- puk + expand_limits(y = 0)
puk <- puk + scale_x_date(expand=c(0,0))


tiff("UK Plot.tiff", units="cm", width=20, height=14, res=450) 
puk
dev.off()


###########################################################################

# Graph for DE
mydata <- DEUKDB2 %>%
  filter(COUNTRY2 == "DE")

mydata0119 <- select(mydata, 2,3)

mydata0119[, 2:ncol(mydata0119)] <- lapply(2:ncol(mydata0119), 
                                           function(x) as.numeric(mydata0119[[x]]))

mydata0119$TIMM2 <- as.Date(mydata0119$TIMM2)

mydata0119 <- na.omit(mydata0119)


# select data
de0219 <- mydata0119 %>% slice(2:72)


# Create deallh dataframe

deallh <- read_xlsx("deallh.xlsx")

deallh <- na.omit(deallh)

de0219$POSPH <- deallh$posph

newdata1 <- de0219

newdata2 <- de0219

#remove missing values
newdata1 <- na.omit(newdata1)
newdata2 <- na.omit(newdata2)

# create mean values
X <- newdata1 %>% 
  group_by(TIMM2) %>% 
  summarise(sum_asap = mean(POSPH))

Y <- newdata2 %>%
  group_by(TIMM2) %>% 
  summarise(mean_sal = mean(POLSALH))

# bind 4 variables into dataframe, reatining character and continuous variables
Z <- cbind.data.frame(Y$TIMM2, X$sum_asap, Y$mean_sal, stringsAsFactors = FALSE)

Z <- Z %>% rename(date = 'Y$TIMM2', 
                  posph = 'X$sum_asap',
                  polsalh = 'Y$mean_sal')

Z$date <- as.Date(Z$date)


# build dual y axis variables and plots
shade <- Z %>%  
  mutate(from = as.Date("2002-06-01"),
         to  = as.Date("2013-03-01"))

p <- ggplot() + 
  geom_rect(data = shade, 
              aes(xmin = from, xmax = to, 
                  ymin = -Inf, ymax = Inf), alpha = 0.002)


p <- p + (geom_line(data = Z,
                    aes(x = date, y = posph, 
                        colour = "ARR"), size = 1.5)) 

p <- p + (geom_line(data = Z,
                    aes(x = date, y = polsalh/1.19, 
                        colour = "Salience"),  size = 1.5))


p <- p + scale_y_continuous(sec.axis = sec_axis(~.*1.19, name = "Salience"))

p <- p + labs(y = "Asylum Recognition Rate",
              x = "",
              colour = "",
              title = "Germany")

p <- p + theme_bw() +
  theme(axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 10),
        legend.position = c(0.92, 0.1),
        legend.title = element_blank(),
        plot.title = element_text(vjust= -6, hjust = 0.01,
                                  size = 20))  +
  scale_colour_manual(values=c("red3",
                               "dodgerblue3"))

p <- p + expand_limits(y = 0)
p <- p + scale_x_date(expand=c(0,0))



tiff("DE2 Plot.tiff", units="cm", width=20, height=14, res=450) 
p 

dev.off()





# Graph for the UK
mydata <- DEUKDB2 %>%
  filter(COUNTRY2 == "UK")
head(mydata)

mydata0119 <- select(mydata, 2,3,6)

mydata0119[, 2:ncol(mydata0119)] <- lapply(2:ncol(mydata0119), 
                                           function(x) as.numeric(mydata0119[[x]]))




mydata0119$TIMM2 <- as.Date(mydata0119$TIMM2)

head(mydata0119)

mydata0119 <- na.omit(mydata0119)


# select data
mydata0119 <- mydata0119 %>% slice(4:76)

newdata1 <- mydata0119

newdata2 <- mydata0119



#remove missing values
newdata1 <- na.omit(newdata1)
newdata2 <- na.omit(newdata2)

# create mean values
X <- newdata1 %>% 
  group_by(TIMM2) %>% 
  summarise(sum_asap = mean(POSPH))

Y <- newdata2 %>%
  group_by(TIMM2) %>% 
  summarise(mean_sal = mean(POLSALH))

# bind 4 variables into dataframe, reatining character and continuous variables
Z <- cbind.data.frame(Y$TIMM2, X$sum_asap, Y$mean_sal, stringsAsFactors = FALSE)

Z <- Z %>% rename(date = 'Y$TIMM2', 
                          posph = 'X$sum_asap',
                          polsalh = 'Y$mean_sal')

Z$date <- as.Date(Z$date)


# build dual y axis variables and plots
p <- ggplot(Z, aes(x = date))
p <- p + (geom_line(aes(y = posph, linetype = "ARR"), size = 1.5)) 

p <- p + (geom_line(aes(y = polsalh/1, 
                        linetype = "Salience"),  size = 1.5))
                   

p <- p + scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Salience"))

p <- p + labs(y = "Asylum Recognition Rate",
              x = "",
              colour = "")
p <- p + theme(legend.position = c(0.1, 0.9))
p <- p + theme_bw() +
  theme(axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 15),
        legend.position = "right")

p <- p + expand_limits(y = 0)
p <- p + scale_x_date(expand=c(0,0))


shade <- Z %>%  
  mutate(from = as.Date("2001-03-01"),
          to  = as.Date("2017-03-01"))
  
p + geom_rect(data = shade, 
              aes(xmin = from, xmax = to, 
                  ymin = -Inf, ymax = Inf), alpha = 0.002)


# Graph for the DE
mydata <- DEUKDB2 %>%
  filter(COUNTRY2 == "DE")
head(mydata)

mydata0119 <- select(mydata, 2,3,6)

mydata0119[, 2:ncol(mydata0119)] <- lapply(2:ncol(mydata0119), 
                                           function(x) as.numeric(mydata0119[[x]]))


mydata0119$TIMM2 <- as.Date(mydata0119$TIMM2)

head(mydata0119)

mydata0119 <- na.omit(mydata0119)


newdata1 <- mydata0119

newdata2 <- mydata0119



#remove missing values
newdata1 <- na.omit(newdata1)
newdata2 <- na.omit(newdata2)

# create mean values
X <- newdata1 %>% 
  group_by(TIMM2) %>% 
  summarise(sum_asap = mean(POSPH))

Y <- newdata2 %>%
  group_by(TIMM2) %>% 
  summarise(mean_sal = mean(POLSALH))

# bind 4 variables into dataframe, reatining character and continuous variables
Z <- cbind.data.frame(Y$TIMM2, X$sum_asap, Y$mean_sal, stringsAsFactors = FALSE)

Z <- Z %>% rename(date = 'Y$TIMM2', 
                  posph = 'X$sum_asap',
                  polsalh = 'Y$mean_sal')

Z$date <- as.Date(Z$date)


# build dual y axis variables and plots
p <- ggplot(Z, aes(x = date))
p <- p + (geom_line(aes(y = posph, linetype = "ARR"), size = 1.5)) 

p <- p + (geom_line(aes(y = polsalh/1, 
                        linetype = "Salience"),  size = 1.5))


p <- p + scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Salience"))

p <- p + labs(y = "Asylum Recognition Rate",
              x = "",
              colour = "")
p <- p + theme(legend.position = c(0.1, 0.9))
p <- p + theme_bw() +
  theme(axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 15),
        legend.position = "right")

p <- p + expand_limits(y = 0)
p <- p + scale_x_date(expand=c(0,0))


shade <- Z %>%  
  mutate(from = as.Date("2002-06-01"),
         to  = as.Date("2013-03-01"))

tiff("UK Plot.tiff", units="in", width=10, height=10, res=450) 
p + geom_rect(data = shade, 
              aes(xmin = from, xmax = to, 
                  ymin = -Inf, ymax = Inf), alpha = 0.002)
dev.off()



