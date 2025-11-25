library(tidyverse)

apr21<-read.csv("./platemeter/2025-Apr-21-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("4/21/25"))

apr24<-read.csv("./platemeter/2025-Apr-24-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("4/24/25"))

jun10<-read.csv("./platemeter/2025-Jun-10-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("6/10/25"))

allplatemeter<-rbind(apr21,apr24,apr25,apr30,aug15,aug25,aug26,aug28,jul11,jul14,jul15, jul31,jun02,jun05,jun10,jun11,jun23,jun27,may09,may12,may13,may14,may19,may21,may30,oct01,oct06,sep12,sep17)
                                         

intersect(colnames(sep12), colnames(may21), colnames(oct01))




apr25<-read.csv("./platemeter/2025-Apr-25-00-00-00(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("4/25/25"))

apr30<-read.csv("./platemeter/2025-Apr-30-00-00-00(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("4/30/25"))

aug15<-read.csv("./platemeter/2025-Aug-15-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("8/15/25"))
##
aug25<-read.csv("./platemeter/2025-Aug-25-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("8/25/25"))

aug26<-read.csv("./platemeter/2025-Aug-26-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("8/26/25"))

aug28<-read.csv("./platemeter/2025-Aug-28-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("8/28/25"))

jul11<-read.csv("./platemeter/2025-jul-11-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("7/11/25"))

jul14<-read.csv("./platemeter/2025-jul-14-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("7/14/25"))

jul15<-read.csv("./platemeter/2025-jul-15-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("7/15/25"))

jul31<-read.csv("./platemeter/2025-jul-31-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("7/31/25"))

jun02<-read.csv("./platemeter/2025-jun-02-00-00-00(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("6/2/25"))

jun05<-read.csv("./platemeter/2025-jun-05-00-00-00 PLOTS.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("6/5/25"))

jun11<-read.csv("./platemeter/2025-jun-11-00-00-00(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("6/11/25"))

jun23<-read.csv("./platemeter/2025-jun-23-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("6/23/25"))

jun27<-read.csv("./platemeter/2025-jun-27-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("6/27/25"))

may09<-read.csv("./platemeter/2025-may-09-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("5/09/25"))

may12<-read.csv("./platemeter/2025-may-12-00-00-00(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("5/12/25"))

may13<-read.csv("./platemeter/2025-may-13-00-00-00(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("5/13/25"))

may14<-read.csv("./platemeter/2025-may-14-00-00-00 PLOTS(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("5/14/25"))

may19<-read.csv("./platemeter/2025-may-19-00-00-00(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("5/19/25"))

may21<-read.csv("./platemeter/2025-may-21-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("5/21/25"))

may30<-read.csv("./platemeter/2025-may-30-00-00-00(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("5/30/25"))

oct01<-read.csv("./platemeter/2025-oct-01-zone3(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("10/01/25"))

oct06<-read.csv("./platemeter/2025-oct-06-zone1zone2(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("10/06/25"))

sep12<-read.csv("./platemeter/2025-sep-12-00-00-00(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("09/12/25"))

sep17<-read.csv("./platemeter/2025-sep-17-00-00-00(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("09/17/25"))


# Remove column 'B'
jun02$X <- NULL
print(jun02)

jun10$X <- NULL

may12$lbs.hectre <- NULL
may12$into.maia <- NULL

may13$X <- NULL
may13$X.1 <- NULL

may14$X.3 <- NULL
may19$X.3 <- NULL
may30$X.2 <- NULL


allplatemeter<-rbind(apr21,apr24,apr25,apr30,aug15,aug25,aug26,aug28,jul11,jul14,jul15, jul31,jun02,jun05,jun10,jun11,jun23,jun27,may09,may12,may13,may14,may19,may21,may30,oct01,oct06,sep12,sep17)

ggplot(allplatemeter, aes(date, total))+
  geom_col()

glimpse(allplatemeter)
?geom_bar


library(dplyr)


allplatemeter <- allplatemeter %>%
  mutate(
    paddock_name = case_when(
      date == "2025-04-25" & paddock_name == "Paddock 1" ~ "GP1A",
      date == "2025-04-25" & paddock_name == "Paddock 2" ~ "GP1A",
      date == "2025-04-25" & paddock_name == "Paddock 3" ~ "GP2A",
      date == "2025-04-25" & paddock_name == "Paddock 4" ~ "GP2B",
      date == "2025-04-25" & paddock_name == "Paddock 5" ~ "GP2B",
      date == "2025-04-25" & paddock_name == "Paddock 6" ~ "GP3",
      date == "2025-04-25" & paddock_name == "Paddock 7" ~ "GP3",
      date == "2025-04-25" & paddock_name == "Paddock 8" ~ "GP3",
      date == "2025-04-25" & paddock_name == "Paddock 9" ~ "GP4A",
      date == "2025-04-25" & paddock_name == "Paddock 10" ~ "GP4B",
      date == "2025-04-25" & paddock_name == "Paddock 11" ~ "GP5D",
      date == "2025-04-25" & paddock_name == "Paddock 12" ~ "GP5B",
      date == "2025-04-25" & paddock_name == "Paddock 13" ~ "GP5A",
      date == "2025-04-25" & paddock_name == "Paddock 14" ~ "GP6A",
      date == "2025-04-25" & paddock_name == "Paddock 15" ~ "GP6A",
      date == "2025-04-25" & paddock_name == "Paddock 16" ~ "GP6C",
      date == "2025-04-25" & paddock_name == "Paddock 17" ~ "GP7",
      date == "2025-04-25" & paddock_name == "Paddock 18" ~ "GP7",
      date == "2025-04-25" & paddock_name == "Paddock 19" ~ "GP7",
      date == "2025-04-25" & paddock_name == "Paddock 20" ~ "GP8A",
      date == "2025-04-25" & paddock_name == "Paddock 21" ~ "GP8B",
      date == "2025-04-25" & paddock_name == "Paddock 22" ~ "GP8B",
      
      date == "2025-04-21" & paddock_name == "Paddock 53" ~ "drainage",
      date == "2025-04-21" & paddock_name == "Paddock 55" ~ "lower_underhill",
      date == "2025-04-21" & paddock_name == "Paddock 56" ~ "barberry_south",
      date == "2025-04-21" & paddock_name == "Paddock 57" ~ "barberry_north",
      date == "2025-04-21" & paddock_name == "Paddock 58" ~ "wilson",
      date == "2025-04-21" & paddock_name == "Paddock 59" ~ "underhill",
      date == "2025-04-21" & paddock_name == "Paddock 60" ~ "underhill_wet",
      date == "2025-04-21" & paddock_name == "Paddock 61" ~ "bull",
      date == "2025-04-21" & paddock_name == "Paddock 38" ~ "crabby",
      date == "2025-04-21" & paddock_name == "Paddock 39" ~ "flush",
      date == "2025-04-21" & paddock_name == "Paddock 40" ~ "upper_home",
      date == "2025-04-21" & paddock_name == "Paddock 41" ~ "railroad",
      date == "2025-04-21" & paddock_name == "Paddock 42" ~ "lower_home",
      date == "2025-04-21" & paddock_name == "Paddock 43" ~ "triangle_lower",
      date == "2025-04-21" & paddock_name == "Paddock 45" ~ "horse",
      date == "2025-04-21" & paddock_name == "Paddock 46" ~ "pond",
      date == "2025-04-21" & paddock_name == "Paddock 47" ~ "upper_sunset",
      date == "2025-04-21" & paddock_name == "Paddock 48" ~ "sunset_field",
      date == "2025-04-21" & paddock_name == "Paddock 49" ~ "lower_sunset",
      date == "2025-04-21" & paddock_name == "Paddock 50" ~ "williams_west",
      date == "2025-04-21" & paddock_name == "Paddock 51" ~ "williams_1a",
      
      
      TRUE ~ paddock_name  # keep original if no condition matches
    )
  )
allplatemeter <- allplatemeter %>%
  mutate(
    paddock_name = case_when(
      date %in% c("2025-04-25", "2025-04-26", "2025-04-27") &
        paddock_name == "Paddock 1" ~ "GP1A",
      
      Date %in% c("2025-04-25", "2025-04-26", "2025-04-27") &
        paddock_name == "Paddock 2" ~ "GP1A",
      
      Date %in% c("2025-04-25", "2025-04-26", "2025-04-27") &
        paddock_name == "Paddock 3" ~ "GP2A",
      
      TRUE ~ paddock_name
    )
  )


str(allplatemeter$date)
class(allplatemeter$date)



