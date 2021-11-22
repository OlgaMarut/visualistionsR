library(utils)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(ggtext)
library(ggrepel)
library(ggpubr)
library(stringr)
library(gridExtra)
library(DT)

setwd("C:/Users/Lenovo/Documents/Advanced Visualisation in R/Project/data")

# data preparation

audi <- read.csv('audi.csv', header = TRUE)
audi$brand <- 'audi'
bmw <- read.csv('bmw.csv', header = TRUE)
bmw$brand <- 'bmw'
cclass <- read.csv('cclass.csv', header = TRUE)
cclass$brand <- 'cclass'
focus <- read.csv('focus.csv', header = TRUE)
focus$brand <- 'focus'
ford <- read.csv('ford.csv', header = TRUE)
ford$brand <- 'ford'
hyundai <- read.csv('hyundi.csv', header = TRUE)
hyundai$brand <- 'hyundai'
mercedes <- read.csv('merc.csv', header = TRUE)
mercedes$brand <- 'mercedes'
skoda <- read.csv('skoda.csv', header = TRUE)
skoda$brand <- 'skoda'
toyota <- read.csv('toyota.csv', header = TRUE)
toyota$brand <- 'toyota'
vauxhall <- read.csv('vauxhall.csv', header = TRUE)
vauxhall$brand <- 'vauxhall'
volkswagen <- read.csv('vw.csv', header = TRUE)
volkswagen$brand <- 'volkswagen'

hyundai <- hyundai %>%
  rename(tax = tax.£.)

# merging data

cars1 <- data.frame(bind_rows(audi, bmw, cclass, focus, ford, hyundai,
                              mercedes, skoda, toyota, vauxhall, volkswagen))

# getting rid of cclass and focus data as they lack many observations

cars2 <- data.frame(bind_rows(audi, bmw, ford, hyundai,
                              mercedes, skoda, toyota, vauxhall, volkswagen))

# choosing data period - getting rid of outliers
cars2 <- cars2 %>%
  filter(year > 1995 & year < 2025)

# 1st plot
# engine sive vs decade vs brans

# adding dacade variable
cars2$decade <- cut(cars2$year, breaks = seq(1995, 2025, by = 10), dig.lab = 4)

cars2$decade <- ifelse(cars2$year > 1995 & cars2$year <= 2005, '1995-2005',
                       ifelse(cars2$year > 2005 & cars2$year <= 2015, '2005-2015',
                              ifelse(cars2$year > 2015, '2015-now', '<1995')))

# adding mean engine size variable
cars2 %>% 
  group_by(brand, decade) %>% 
  summarise(N = mean(engineSize)) %>% 
  mutate(brand = factor(brand)) -> cars_mutate

# dotchart

cars_mutate %>%
  mutate(brand = as.character(brand))

ggdotchart(cars_mutate, x = 'brand', y = 'N', color = 'decade',                                
           palette = c("#A1D99B", "#FC9272", "#BCBDDC"), 
           sorting = "descending",                       
           rotate = TRUE,                                
           dot.size = 2,                                
           y.text.col = TRUE,
           col.axis = 'black') +
  labs(y  = 'Size in liters') +
  theme(axis.text.y = element_text(color = '#F0F0F0', size = 10),
        axis.text.x = element_text(color = '#F0F0F0', size = 10),
        axis.title = element_text(color = '#252525', size = 11),
        legend.title = element_text(color = '#252525', size = 10),
        legend.background = element_rect(fill = '#D9D9D9'),
        plot.background = element_rect(fill = '#737373'),
        panel.background = element_rect(fill = '#F0F0F0'),
        legend.position = c(0.85, 0.24)) +
  scale_y_continuous(limits = c(1.25,3),
                     breaks = c(1.25, 1.5, 1.75, 2, 2.25, 2.5, 2.75, 3))+
  labs(title = 'Engine size') +
  theme_cleveland()  

cars_mutate5 <- cars2 %>%
  filter(brand == 'audi')

# mean engine sizes for all brand by decade
mean(cars_mutate5$engineSize[cars_mutate5$decade == '2015-now'])
mean(cars_mutate5$engineSize[cars_mutate5$decade == '2005-2015'])
mean(cars_mutate5$engineSize[cars_mutate5$decade == '1995-2005'])

# color preparation
# display.brewer.all(n = 12, exact.n = FALSE)
# brewer.pal(9,'Greys')[c(5)]
# brewer.pal(9,'Greens')[c(4)]
# brewer.pal(9,'Reds')[c(4)]
# brewer.pal(9,'Purples')[c(4)]
# brewer.pal(9,'Oranges')[c(3)]

# 2nd plot

# summarizing data by mean price and mean mileage for transmission type and 
# fuel type

cars2 %>% 
  group_by(fuelType, year, transmission) %>% 
  summarise(N = mean(price), M = mean(mileage)) %>% 
  filter(transmission != 'Other') -> cars_mutate2

# getting rid of outliers to make plot more informative
cars_mutate2 %>%
  filter(N <= 5000) -> cars_mutate2_encircle

# choosing colors
pastel <- c("#A1D99B", "#FDD0A2", "#FC9272", "#969696", "#BCBDDC")

# multiplot
p <- ggplot(cars_mutate2, aes(x = N, y = M)) +
  geom_point(aes(color = fuelType), size = 1.9) +
  geom_smooth(color = '#FFFFFF', alpha = 0.2, size = 0.8) +
  facet_wrap(~transmission) +
  scale_colour_manual(values = pastel) +
  labs(title = 'Quality vs Price',
       x = 'Price [GBP]' , y = 'Mileage [mi.]')
p <- p + labs(color = 'Fuel type') +
  theme(axis.text.y = element_text(color = '#F0F0F0', size = 8),
        axis.text.x = element_text(color = '#F0F0F0', size = 8),
        axis.title = element_text(color = '#252525', size = 11),
        legend.title = element_text(color = '#252525', size = 10),
        legend.background = element_rect(fill = '#D9D9D9'),
        legend.key = element_rect(fill = '#D9D9D9'),
        plot.background = element_rect(fill = '#737373'),
        panel.background = element_rect(fill = '#F0F0F0')) +
  guides(colour = guide_legend(override.aes = list(shape = 15, alpha = 1, size = 8),
                               label.position = 'left'))
p


# 3rd plot

# summarizing mean tax value and fuel efficiency by car models

cars2 %>%
  summarise(brand, model, year, tax, mpg) %>%
  filter(mpg <= 300) %>%
  group_by(model, brand) %>%
  summarise(N = mean(tax), M = mean(mpg))-> cars_mutate3

# setting lines position for doubled mean values of tax value and fuel efficiency

x2mean <- (2 * mean(cars_mutate3$M))
y2mean <- (2 * mean(cars_mutate3$N))

# finding maximum values for tax and fuel efficiency

maxM <- max(cars_mutate3$M)
maxN <- max(cars_mutate3$N)

# finding best and worst model for potential buyer

best <-cars_mutate3 %>%
  filter(M == maxM)
worst <-cars_mutate3 %>%
  filter(N == maxN)

# scatterplot

p2 <- ggplot(cars_mutate3, aes(x = M, y = N)) +
  geom_jitter(width = 0.5, alpha = 0.2, size = 1) +
  labs(title = 'Efficency vs taxation', color = 'Brand',
       x = 'Fuel efficency' , y = 'Road tax', size = 'Population') +
  geom_vline(xintercept = x2mean, linetype = 'longdash') + 
  geom_hline(yintercept = y2mean, linetype = 'longdash') +
  annotate("rect", xmin = x2mean, xmax = Inf, ymin = y2mean, ymax = Inf, alpha = 0.1, fill = "white") +
  annotate("rect", xmin = x2mean, xmax = Inf, ymin = -Inf, ymax = y2mean, alpha = 0.1, fill = "green") +
  annotate("rect", xmin = -Inf, xmax = x2mean, ymin = y2mean, ymax = Inf, alpha = 0.1, fill = "red") +
  annotate("rect", xmin = -Inf, xmax = x2mean, ymin = -Inf, ymax = y2mean, alpha = 0.1, fill = "grey") +
  geom_text(data = best, aes(label = model),
            hjust = 1, vjust = 0) +
  geom_text(data = worst, aes(label = model),
            hjust = 1, vjust = 0) +
  theme_bw()

p2 + theme(axis.text.y = element_text(color = '#F0F0F0', size = 10),
             axis.text.x = element_text(color = '#F0F0F0', size = 10),
             axis.title = element_text(color = '#252525', size = 11),
             legend.title = element_text(color = '#252525', size = 10),
             legend.background = element_rect(fill = '#D9D9D9'),
             legend.key = element_rect(fill = '#D9D9D9'),
             plot.background = element_rect(fill = '#737373'),
             panel.background = element_rect(fill = '#F0F0F0')) +
  guides(colour = guide_legend(override.aes = list(shape = 15, alpha = 1, size = 10),
                               label.position = 'left'))

# adding text labels for best and worst model

geom_text_repel(aes(label = model))
theme(legend.position = 'top') 

# 4th plot

# assigning rank values for brands by price and year

cars2 %>% 
  filter(year >= 2000) %>%
  summarise(year = as.character(year), brand = as.character(brand), price) %>%
  group_by(year, brand) %>% 
  summarise(N = mean(price)) %>%
  arrange(year, N) %>%
  mutate(rank = row_number(desc(N))) %>%
  summarise(year, brand, N, rank, letters = str_length(brand)) -> cars_mutate4

# rank plot

p1 <- ggplot(data = cars_mutate4, aes(x = year, y = rank, group = brand)) +
  geom_line(aes(color = brand, alpha =(0.5 + 1/rank)), size = 1.6) +
  geom_point(aes(color = brand, alpha = 1/rank), size = 2.3, shape = 21, fill = 'white') +
  geom_text(data = cars_mutate4 %>% filter(year == "2000", rank <= 15),
            aes(label = brand, x = year, hjust = (1 + 1/letters)), color = "#888888", size = 3.4) + 
  geom_text(data = cars_mutate4 %>% filter(year == "2020", rank <= 15),
            aes(label = brand, x = year, hjust = -1/letters), color = "#888888", size = 3.4) +
  scale_x_discrete(expand = c(0.08, .2), breaks = seq(2000, 2020, 2)) +
  scale_y_reverse(breaks = seq(1, 9)) +
  labs(title = "British second-hand cars market", x = "Year", y = "Price rank") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none") + 
  labs(subtitle = '1 - most expensive, 9 - least expensive brand')

# setting colors
pastel_long <- c("#A1D99B", "#FC9272", "#BCBDDC", "#FDD0A2", "#FA9FB5",
                 "#969696", "#7FCDBB", "#F4A582", "#FED976")

p1 <- p1 + scale_colour_manual(values = pastel_long) +
  theme(axis.text.y = element_text(color = '#F0F0F0', size = 10),
        axis.text.x = element_text(color = '#F0F0F0', size = 10),
        axis.title = element_text(color = '#252525', size = 11),
        legend.title = element_text(color = '#252525', size = 10),
        legend.background = element_rect(fill = '#D9D9D9'),
        legend.key = element_rect(fill = '#D9D9D9'),
        plot.background = element_rect(fill = '#737373'),
        panel.background = element_rect(fill = '#F0F0F0')) +
  guides(colour = guide_legend(override.aes = list(shape = 15, alpha = 1, size = 10),
                               label.position = 'left'))
p1

# finding colors
# display.brewer.all(n = 12, exact.n = FALSE)
# brewer.pal(9,'RdPu')[c(4)]
# brewer.pal(9,'YlGnBu')[c(4)]
# brewer.pal(9,'RdBu')[c(3)]
# brewer.pal(9,'YlOrRd')[c(3)]
# brewer.pal(9,'Oranges')[c(3)]

# 5th plot

# grouping data by brand and decade

most_popular_cars <- cars2 %>% 
  group_by(brand, decade) %>% 
  count(model)

# finding most popular model for each brand in each of decades

most_popular_cars %>% 
  group_by(brand, decade) %>% 
  filter(n == max(n)) -> data

head(data)

# replacing two Hyundai models which occured both as most popular with 
# names of both model

data <- data[-10,] 
data[10,3] <- "Terrecan/Getz"

# setting colors

pastel <- c("#A1D99B", "#FC9272", "#BCBDDC", "#FDD0A2", "#48D1CC")

pastel_long <- c("#A1D99B", "#FC9272", "#BCBDDC", "#FDD0A2", "#FA9FB5",
                 "#969696", "#7FCDBB", "#F4A582", "#FED976", "#FC9272", "#BCBDDC", "#FDD0A2", "#969696")

# table

ggplot(data, aes(x = decade, y = brand)) + 
  geom_tile(aes(fill = model), color = 'black', show.legend = F) +
  theme_minimal() + 
  geom_text(aes(label = model), size = 3, fontface = 'bold', color = 'white') +
  labs(title = 'The most popular cars by decade') +
  scale_fill_manual(values =  c("#FC9272", "#A1D99B", "#F4A582", "#87CEFA", "#FC9272", 
                                "#BCBDDC", "#FDD0A2", "#7FCDBB", "#FA9FB5", "#BCBDDC", "#FED976", "#48D1CC", "#F1B6DA", "#90EE90")) +
  theme(axis.text.y = element_text(color = '#F0F0F0', size = 10, face = 'bold'),
        axis.text.x = element_text(color = '#F0F0F0', size = 10, face = 'bold'),
        axis.title = element_text(color = '#252525', size = 12, face = 'bold'),
        axis.title.y = element_blank(),
        plot.background = element_rect(fill = '#737373'),
        panel.background = element_blank()) +
  guides(colour = guide_legend(override.aes = list(shape = 15, alpha = 1, size = 10),
                               label.position = 'left'))

# finding colors
# display.brewer.all(n = 12, exact.n = FALSE)
# brewer.pal(9,'YlGnBu')[c(4)]

# 6th plot

# preparing quantile table by price

qt <- c(quantile(cars2$price, 0.25),
        quantile(cars2$price, 0.5),
        quantile(cars2$price, 0.75))

# assigning cars to quantiles

cars2$price_range <- ifelse(cars2$price > qt[1] & cars2$price <= qt[2], "Second quantile",
                            ifelse( cars2$price > qt[2] & cars2$price <= qt[3], "Third Quantile",
                                    ifelse(cars2$price > qt[3], "Fourth quantile", "First quantile")))

# assigning data from each quantile to new data frame which will be used
# for each quantile plot preparation

q1 <- cars2 %>% 
  filter(price_range == "First quantile")

q2 <- cars2 %>% 
  filter(price_range == "Second quantile")

q3 <- cars2 %>% 
  filter(price_range == "Third Quantile")

q4 <- cars2 %>% 
  filter(price_range == "Fourth quantile")

# counting means of mileage for each quantile

means <- c(mean(q1$mileage),
           mean(q2$mileage),
           mean(q3$mileage),
           mean(q4$mileage))

# choosing colors

pastel <- c("#A1D99B", "#FC9272", "#BCBDDC", "#FDD0A2", "#48D1CC")
pastel2 <- c("#A1D99B", "#48D1CC", "#FC9272", "#BCBDDC", "#FDD0A2" )

# 1st quantile of price scatterplot

options(scipen=999)

q1_plot <- ggplot(q1) +
  geom_point(aes(x = price, y = mileage, color = fuelType)) +
  geom_hline(yintercept  = means[1])  +
  labs(title = 'First quantile') +
  scale_colour_manual(values = pastel) +
  theme(axis.text.y = element_text(color = '#F0F0F0', size = 8),
        axis.text.x = element_text(color = '#F0F0F0', size = 8),
        axis.title = element_text(color = '#252525', size = 10),
        legend.title = element_text(color = '#252525', size = 10),
        legend.background = element_rect(fill = '#D9D9D9'),
        legend.key = element_rect(fill = '#D9D9D9'),
        plot.background = element_rect(fill = '#737373'),
        panel.background = element_rect(fill = '#F0F0F0')) +
  guides(colour = guide_legend(override.aes = list(shape = 15, alpha = 1, size = 10),
                               label.position = 'left'))

# q1_plot

# 2nd quantile of price scatterplot

q2_plot <- ggplot(q2) +
  geom_point(aes(x = price, y = mileage, color = fuelType)) +
  geom_hline(yintercept  = means[2])  +
  labs(title = 'Second quantile') +
  scale_colour_manual(values = pastel2) +
  theme(axis.text.y = element_text(color = '#F0F0F0', size = 8),
        axis.text.x = element_text(color = '#F0F0F0', size = 8),
        axis.title = element_text(color = '#252525', size = 10),
        legend.title = element_text(color = '#252525', size = 10),
        legend.background = element_rect(fill = '#D9D9D9'),
        legend.key = element_rect(fill = '#D9D9D9'),
        plot.background = element_rect(fill = '#737373'),
        panel.background = element_rect(fill = '#F0F0F0')) +
  guides(colour = guide_legend(override.aes = list(shape = 15, alpha = 1, size = 10),
                               label.position = 'left'))

# q2_plot

# 3rd quantile of price scatterplot

q3_plot <- ggplot(q3) +
  geom_point(aes(x = price, y = mileage, color = fuelType)) +
  geom_hline(yintercept  = means[3])  +
  labs(title = 'Third quantile') +
  scale_colour_manual(values = pastel2) +
  theme(axis.text.y = element_text(color = '#F0F0F0', size = 8),
        axis.text.x = element_text(color = '#F0F0F0', size = 8),
        axis.title = element_text(color = '#252525', size = 10),
        legend.title = element_text(color = '#252525', size = 10),
        legend.background = element_rect(fill = '#D9D9D9'),
        legend.key = element_rect(fill = '#D9D9D9'),
        plot.background = element_rect(fill = '#737373'),
        panel.background = element_rect(fill = '#F0F0F0')) +
  guides(colour = guide_legend(override.aes = list(shape = 15, alpha = 1, size = 10),
                               label.position = 'left'))

# q3_plot

# 4th quantile of price scatterplot

q4_plot <- ggplot(q4) +
  geom_point(aes(x = price, y = mileage, color = fuelType)) + 
  geom_hline(yintercept  = means[4])  +
  labs(title = 'Fourth quantile', color = 'Fuel type') +
  scale_colour_manual(values = pastel) +
  theme(axis.text.y = element_text(color = '#F0F0F0', size = 8),
        axis.text.x = element_text(color = '#F0F0F0', size = 8),
        axis.title = element_text(color = '#252525', size = 10),
        legend.title = element_text(color = '#252525', size = 10),
        legend.background = element_rect(fill = '#D9D9D9'),
        legend.key = element_rect(fill = '#D9D9D9', size = 8),
        plot.background = element_rect(fill = '#737373'),
        panel.background = element_rect(fill = '#F0F0F0'),
        legend.position = "top") +
  guides(colour = guide_legend(override.aes = list(shape = 15, alpha = 1, size = 5),
                               label.position = 'left'))

# q4_plot

# adding functions

getwd()
source('multiplot.R')
source('get_legend.R')

# creating multiplot of 4 scatterplots

multiplot(q1_plot, q2_plot, q3_plot, q4_plot, 
           cols = 2)

# adding legend from 4th quantile plot

q4_plot <- q4_plot + theme(legend.position = "top") 
legend <- get_legend(q4_plot)

# arranging plots from multiplot in a panel

p <- grid.arrange(arrangeGrob(q1_plot + theme(legend.position = "none"), 
                              q2_plot + theme(legend.position = "none"), 
                              q3_plot + theme(legend.position = "none"), 
                              q4_plot + theme(legend.position = "none"),
                              ncol = 2), # number of columns
                  legend, 
                  nrow = 2, 
                  top = "Miles vs price for each quantile", 
                  heights = c(17, 1) )

# 7th 'plot' - information table

# summarizing means of variables

cars2 %>% 
  group_by(brand) %>% 
  summarise(price = mean(price),
            mile = mean(mileage),
            tax = mean(tax),
            mpg = mean(mpg),
            eng_size = mean(engineSize)) %>% 
  mutate(brand = factor(brand)) -> cars_table

# descending order by price

cars_table <-cars_table %>% 
  arrange(desc(price))

# rounding values
cars_table <- cars_table %>% 
  summarise(Brand = brand,
            Price = round(price, 2),
         Miles = round(mile),
         Tax = round(tax),
         Miles_per_Gallon = round(mpg),
         Engine_size = round(eng_size, 2) )

# table

datatable(cars_table)

# summary and savinf table
head(cars_table)
htmltools::save_html(datatable(cars_table), file = 'test.html')
