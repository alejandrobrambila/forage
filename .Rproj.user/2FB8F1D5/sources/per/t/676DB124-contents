recoveryupdated<-read.csv("recoveryupdated.csv")
library(tidyverse)
ggplot(recoveryupdated, aes(month_season, recovery_days))+
  geom_point(aes(color=disturbance_type))
# Source - https://stackoverflow.com/a
# Posted by Metrics
# Retrieved 2025-11-17, License - CC BY-SA 3.0

#lists months in calendar order instead of alphabetical
recoveryupdated$month_season <- factor(recoveryupdated$month_season, levels = c("April", "May", "June", "July", "August", "September", "October", "November"))
install.packages("lubridate")
library("lubridate")
#creates test column that measures days from 4/15(start of season). For columns with a disturbance #1, 0 is returned forrecoverydays
recovery2<-recoveryupdated|>
  mutate(day_in_date=mdy(day_in_date))|>
  mutate(totaldays=day_in_date-mdy("4/15/2025"))|>
  mutate(recovery_days=ifelse(is.na(recovery_days), 0, recovery_days))|>
  mutate(test=ifelse(recovery_days==0, totaldays, recovery_days))
glimpse(recovery2)

ggplot(recovery2, aes(month_season, test))+
  geom_point(aes(color=disturbance_type))+
  geom_text(aes(label=field))
recovery2$month_season <- factor(recovery2$month_season, levels = c("April", "May", "June", "July", "August", "September", "October", "November"))

ggplot(recovery2, aes(day_in_date, test))+
  geom_point(aes(color=disturbance_type, group=field))

ggplot(recovery2, aes(month_season))+
  geom_bar(aes(color=disturbance_type))

library(dplyr)

recovery2 <- recovery2 %>%
  mutate(target = case_when(
    month_season == "April" ~ 15,
    month_season == "May" ~ 18,
    month_season == "June" ~ 24,
    month_season == "July" ~ 30,
    month_season == "August" ~ 36,
    month_season == "September" ~ 42))

?case_when()

ggplot(recovery2, aes(x=field, y=test))+
         geom_point()

  