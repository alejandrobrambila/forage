#test

library(tidyverse)
library(forcats)
library(readxl)
library(lubridate)

platemeter<-read_csv('/Users/alejandro/Library/CloudStorage/OneDrive-SharedLibraries-TTOR/Farm Team - Agroecology Initiative/Monitoring/Production/Data/spreadsheets/2022/appleton/platemeter_analysis_2022.csv')%>%
  mutate(date=ymd(date1))

ggplot(platemeter, aes(date, cover, group=paddock_id)) + geom_point() + 
  geom_line() + facet_wrap(~field) + xlab("") + ylab("estimated dm kg/ha")+theme_classic()


platemetermax<-platemeter%>%
  group_by(field)%>%
  summarize(cover=max(cover))

platemetermax1<-left_join(platemetermax, platemeter)%>%
  ungroup()%>%
  mutate(name=as.factor(field))%>%
  mutate(name=fct_reorder(field, desc(cover)))


ggplot(platemetermax1, aes(as.factor(name), cover, color=date)) +geom_point()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

       