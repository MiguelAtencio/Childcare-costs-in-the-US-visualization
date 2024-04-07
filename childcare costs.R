
library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2023, week = 19)

childcare_costs <- tuesdata$childcare_costs

counties <- tuesdata$counties

map <- read.csv("us_county_latlng.csv")

glimpse(childcare_costs)

head(counties)

head(map)

colnames(childcare_costs)[1] <- "fips_code"

childcare_costs_pos <- left_join(map, childcare_costs, by="fips_code")

glimpse(childcare_costs_pos)

cor(childcare_costs, use="pairwise.complete.obs")

childcare_costs %>% ggplot(aes(mhi_2018, mcsa)) + geom_point() +
  scale_x_continuous(trans="log2") + geom_smooth(method="lm")

price_vs_median_hi <- lm(mcsa ~ log2(mhi_2018), data=childcare_costs_pos)

summary(price_vs_median_hi)

#emp_m business, science, arts, and management percentage
childcare_costs %>% ggplot(aes(emp_m, mcsa)) + geom_point() +
  geom_smooth(method='lm')


model <- lm(mcsa ~ emp_m + emp_n + emp_p, data=childcare_costs)

childcare_costs$residuals <- residuals(model) #problem

summary(model)

childcare_costs_pos %>% filter(lng>-125, lat>24, lat<50) %>%
  ggplot(aes(lng, lat, color=emp_m)) + geom_point() +
  scale_color_gradient(low='blue', high='red')

childcare_costs_pos %>% filter(lng>-125, lat>24, lat<50) %>%
  ggplot(aes(lng, lat, color=mcsa)) + geom_point() +
  scale_color_gradient(low="blue", high="red") + 
  labs(x=element_blank(), y=element_blank())



