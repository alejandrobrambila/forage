###### READ ME############################################
#this script looks at recovery days (how long we let fields rest inbetween grazes)
#for the 2025 grazing season at Appleton Farms. The column "disturbance_number" 
#refers to the number of disturbance events (graze, mow, hay) that occured in a field.
#"day_in" date refers to either the day the cows were moved into a field if the disturbance
#was a graze, or the day that a mow or hay event occured.
#the "target" column is based on what month the disturbance occurred. Each month is assigned
#a target number of days to recover (ex: April has a 15 day target recovery period).
#the "recovery_days" column is calculated by taking the difference between the day_in date
#for the previous disturbance in a given field and the associated day_in date for the next
#disturbance.
#-----------------------------------------------------------------------------------------

recovery<-read.csv("recoveryupdated.csv")

#cleaning data----------------------------------------------------------------------

recovery <- recovery %>%
  mutate(field = if_else(field == "GP3**",
                               "GP3",
                               field))
recovery <- recovery %>%
  mutate(field = if_else(field == "GP7*",
                         "GP7",
                         field))
recovery <- recovery %>%
  mutate(field = if_else(field == "*GP7",
                         "GP7",
                         field))

#lists months in calendar order instead of alphabetical
recovery$month_season <- factor(recovery$month_season, levels = c("April", "May", "June", "July", "August", "September", "October", "November"))

#creates "totaldays" column that measures days from 4/15(start of season). 
#For columns with a disturbance #1, 0 is returned for recoverydays
#the column "test" now displays the recovery day value as the #days since the beginning
#of the season (4/15 for brood, 5/1 for feeders) instead of 0.
recovery2<-recoveryupdated|>
  mutate(day_in_date=mdy(day_in_date))|>
  mutate(totaldays=day_in_date-mdy("4/15/2025"))|>
  mutate(recovery_days=ifelse(is.na(recovery_days), 0, recovery_days))|>
  mutate(test=ifelse(recovery_days==0, totaldays, recovery_days))

#adds the "target" column which asigns a target amount of days per recovery period
#in months apr-sept.

#(cleaning data, replacing inaccurate month_season column with accurate month column)
recovery2 <- recovery2 %>%
  mutate(month = month(day_in_date, label = TRUE, abbr = FALSE))
recovery2$month_season <-NULL

#October and November do not have target recovery days because the growing season is over
recovery2 <- recovery2 %>%
  mutate(target = case_when(
    month == "April" ~ 15,
    month == "May" ~ 18,
    month == "June" ~ 24,
    month == "July" ~ 30,
    month == "August" ~ 36,
    month == "September" ~ 42,
  ))
#----------------------------plotting----------------------------------------
ggplot(recovery2, aes(month_season, test))+
  geom_point(aes(color=disturbance_type))+
  geom_text(aes(label=field))
recovery2$month_season <- factor(recovery2$month_season, levels = c("April", "May", "June", "July", "August", "September", "October", "November"))

ggplot(recovery2, aes(day_in_date, test))+
  geom_point(aes(color=disturbance_type, group=field))

ggplot(recovery2, aes(month_season))+
  geom_bar(aes(color=disturbance_type))








  