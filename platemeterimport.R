##### READ ME #######################
#this script imports and combines the platemetering done by the Appleton 
#Farms agroecology team during the 2025 growing season with the platemetering done
#by the livestock team.

#First, the agroecology data. These data were taken about every other week, april 
#to october 2025.

#--------reading in each platemeter csv and naming them by date---------
library(tidyverse)


apr21<-read.csv("./platemeter/2025-Apr-21-00-00-00 (1).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("4/21/25"))%>%
  select(-contains("X"))

apr24<-read.csv("./platemeter/2025-Apr-24-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("4/24/25"))%>%
  select(-contains("X"))

jun10<-read.csv("./platemeter/2025-Jun-10-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("6/10/25"))%>%
  select(-contains("X"))
apr25<-read.csv("./platemeter/2025-Apr-25-00-00-00(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("4/25/25"))%>%
  select(-contains("X"))

apr30<-read.csv("./platemeter/2025-Apr-30-00-00-00(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("4/30/25"))%>%
  select(-contains("X"))

aug15<-read.csv("./platemeter/2025-Aug-15-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("8/15/25"))%>%
  select(-contains("X"))
##
aug25<-read.csv("./platemeter/2025-Aug-25-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("8/25/25"))%>%
  select(-contains("X"))

aug26<-read.csv("./platemeter/2025-Aug-26-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("8/26/25"))%>%
  select(-contains("X"))

aug28<-read.csv("./platemeter/2025-Aug-28-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("8/28/25"))%>%
  select(-contains("X"))

jul11<-read.csv("./platemeter/2025-jul-11-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("7/11/25"))%>%
  select(-contains("X"))

jul14<-read.csv("./platemeter/2025-jul-14-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("7/14/25"))%>%
  select(-contains("X"))

jul15<-read.csv("./platemeter/2025-jul-15-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("7/15/25"))%>%
  select(-contains("X"))

jul31<-read.csv("./platemeter/2025-jul-31-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("7/31/25"))

jun02<-read.csv("./platemeter/2025-jun-02-00-00-00(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("6/2/25"))%>%
  select(-contains("X"))

jun05<-read.csv("./platemeter/2025-jun-05-00-00-00 PLOTS.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("6/5/25"))%>%
  select(-contains("X"))

jun11<-read.csv("./platemeter/2025-jun-11-00-00-00(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("6/11/25"))%>%
  select(-contains("X"))

jun27<-read.csv("./platemeter/2025-jun-27-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("6/27/25"))%>%
  select(-contains("X"))

may09<-read.csv("./platemeter/2025-may-09-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("5/09/25"))%>%
  select(-contains("X"))

may12<-read.csv("./platemeter/2025-may-12-00-00-00(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("5/12/25"))%>%
  select(-10, -11)

may13<-read.csv("./platemeter/2025-may-13-00-00-00(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("5/13/25"))%>%
  select(-contains("X"))

may14<-read.csv("./platemeter/2025-may-14-00-00-00 PLOTS(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("5/14/25"))%>%
  select(-contains("X"))

may19<-read.csv("./platemeter/2025-may-19-00-00-00(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("5/19/25"))%>%
  select(-contains("X"))

may21<-read.csv("./platemeter/2025-may-21-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("5/21/25"))%>%
  select(-contains("X"))

may30<-read.csv("./platemeter/2025-may-30-00-00-00(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("5/30/25"))%>%
  select(-contains("X"))

oct01<-read.csv("./platemeter/2025-oct-01-zone3(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("10/01/25"))%>%
  select(-contains("X"))

oct06<-read.csv("./platemeter/2025-oct-06-zone1zone2(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("10/06/25"))

sep12<-read.csv("./platemeter/2025-sep-12-00-00-00(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("09/12/25"))

sep17<-read.csv("./platemeter/2025-sep-17-00-00-00(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("09/17/25"))
#CLEANING UP THE DATA-------------------------------------------------
#-------------------removing random extra columns from platemeter------------

#----------r bind combines all of the separate csvs into one------------------

# april 21 does not exist, run without april 21
# allplatemeter<-rbind(apr21,apr24,apr25,apr30,aug15,aug25,aug26,aug28,jul11,jul14,jul15, jul31,jun02,jun05,jun10,jun11,jun27,may09,may12,may13,may14,may19,may21,may30,oct01,oct06,sep12,sep17)

allplatemeter<-rbind(apr24,apr25,apr30,aug15,aug25,aug26,aug28,jul11,jul14,jul15, jul31,jun02,jun05,jun10,jun11,jun27,may09,may12,may13,may14,may19,may21,may30,oct01,oct06,sep12,sep17)
rm(apr24,apr25,apr30,aug15,aug25,aug26,aug28,jul11,jul14,jul15, jul31,jun02,jun05,jun10,jun11,jun27,may09,may12,may13,may14,may19,may21,may30,oct01,oct06,sep12,sep17)

#removing uneccessary columns
allplatemeter2 <- allplatemeter %>% select(-paddock_area, -equation_multiplier_a, -equation_constant_b, -residual_constant, -residual_cover, -total)



---------------------------------------------------------------------------------
#because of discrepancies in field naming, I assigned the appropriate
#paddock name to each row
---------------------------------------------------------------------------------
#first, the code corrects the paddock names for the files apr21, apr24, apr25, apr30,
#5/9, 5/14, 5/19, 5/30, 6/5, 6/10, 7/31
#since the platemetering done on these days references either old zones or are named 
#according to the bird experiment plots.
---------------------------------------------------------------------------------
#then I am naming each paddock (1-48) according to the names from the new/current
#plate metering map as a default for other dates.

 
allplatemeter2 <- allplatemeter2 %>%
mutate(
field = case_when(
      
      # ---------- SPECIAL DATE: 2025-04-21 ----------
      date == as.Date("2025-04-21") & paddock_name == "Paddock 53" ~ "drainage",
      date == as.Date("2025-04-21") & paddock_name == "Paddock 55" ~ "lower_underhill",
      date == as.Date("2025-04-21") & paddock_name == "Paddock 56" ~ "barberry_south",
      date == as.Date("2025-04-21") & paddock_name == "Paddock 57" ~ "barberry_north",
      date == as.Date("2025-04-21") & paddock_name == "Paddock 58" ~ "wilson",
      date == as.Date("2025-04-21") & paddock_name == "Paddock 59" ~ "underhill",
      date == as.Date("2025-04-21") & paddock_name == "Paddock 60" ~ "underhill_wet",
      date == as.Date("2025-04-21") & paddock_name == "Paddock 61" ~ "bull",
      
      # ---------- SPECIAL DATE: 2024-04-24 ----------
      date == as.Date("2025-04-24") & paddock_name == "Paddock 38" ~ "crabby",
      date == as.Date("2025-04-24") & paddock_name == "Paddock 39" ~ "flush",
      date == as.Date("2025-04-24") & paddock_name == "Paddock 40" ~ "upper_home",
      date == as.Date("2025-04-24") & paddock_name == "Paddock 41" ~ "railroad",
      date == as.Date("2025-04-24") & paddock_name == "Paddock 42" ~ "lower_home",
      date == as.Date("2025-04-24") & paddock_name == "Paddock 43" ~ "triangle_lower",
      date == as.Date("2025-04-24") & paddock_name == "Paddock 45" ~ "horse",
      date == as.Date("2025-04-24") & paddock_name == "Paddock 46" ~ "pond",
      date == as.Date("2025-04-24") & paddock_name == "Paddock 47" ~ "upper_sunset",
      date == as.Date("2025-04-24") & paddock_name == "Paddock 48" ~ "sunset_field",
      date == as.Date("2025-04-24") & paddock_name == "Paddock 49" ~ "lower_sunset",
      date == as.Date("2025-04-24") & paddock_name == "Paddock 50" ~ "williams_west",
      date == as.Date("2025-04-24") & paddock_name == "Paddock 51" ~ "williams_1a",
      date == as.Date("2025-04-24") & paddock_name == "Paddock 53" ~ "drainage",
      
      # ---------- SPECIAL DATE: 2024-04-25 ----------
      date == as.Date("2025-04-25") & paddock_name == "Paddock 1"  ~ "GP1A",
      date == as.Date("2025-04-25") & paddock_name == "Paddock 2"  ~ "GP1A",
      date == as.Date("2025-04-25") & paddock_name == "Paddock 3"  ~ "GP2A",
      date == as.Date("2025-04-25") & paddock_name == "Paddock 4"  ~ "GP2B",
      date == as.Date("2025-04-25") & paddock_name == "Paddock 5"  ~ "GP2B",
      date == as.Date("2025-04-25") & paddock_name == "Paddock 6"  ~ "GP3",
      date == as.Date("2025-04-25") & paddock_name == "Paddock 7"  ~ "GP3",
      date == as.Date("2025-04-25") & paddock_name == "Paddock 8"  ~ "GP3",
      date == as.Date("2025-04-25") & paddock_name == "Paddock 9"  ~ "GP4A",
      date == as.Date("2025-04-25") & paddock_name == "Paddock 10" ~ "GP4B",
      date == as.Date("2025-04-25") & paddock_name == "Paddock 11" ~ "GP5D",
      date == as.Date("2025-04-25") & paddock_name == "Paddock 12" ~ "GP5B",
      date == as.Date("2025-04-25") & paddock_name == "Paddock 13" ~ "GP5A",
      date == as.Date("2025-04-25") & paddock_name == "Paddock 14" ~ "GP6A",
      date == as.Date("2025-04-25") & paddock_name == "Paddock 15" ~ "GP6A",
      date == as.Date("2025-04-25") & paddock_name == "Paddock 16" ~ "GP6C",
      date == as.Date("2025-04-25") & paddock_name == "Paddock 17" ~ "GP7",
      date == as.Date("2025-04-25") & paddock_name == "Paddock 18" ~ "GP7",
      date == as.Date("2025-04-25") & paddock_name == "Paddock 19" ~ "GP7",
      date == as.Date("2025-04-25") & paddock_name == "Paddock 20" ~ "GP8A",
      date == as.Date("2025-04-25") & paddock_name == "Paddock 21" ~ "GP8B",
      date == as.Date("2025-04-25") & paddock_name == "Paddock 22" ~ "GP8B",
      
      # ---------- SPECIAL DATE: 2025-04-30 ----------
      date == as.Date("2025-04-30") & paddock_name == "Paddock 23" ~ "briar",
      date == as.Date("2025-04-30") & paddock_name == "Paddock 24" ~ "plains",
      date == as.Date("2025-04-30") & paddock_name == "Paddock 25" ~ "plains",
      date == as.Date("2025-04-30") & paddock_name == "Paddock 26" ~ "plains",
      date == as.Date("2025-04-30") & paddock_name == "Paddock 27" ~ "playground",
      date == as.Date("2025-04-30") & paddock_name == "Paddock 28" ~ "playground",
      date == as.Date("2025-04-30") & paddock_name == "Paddock 29" ~ "playground",
      date == as.Date("2025-04-30") & paddock_name == "Paddock 30" ~ "leos",
      date == as.Date("2025-04-30") & paddock_name == "Paddock 31" ~ "lamson_north",
      date == as.Date("2025-04-30") & paddock_name == "Paddock 32" ~ "lamson_middle",
      date == as.Date("2025-04-30") & paddock_name == "Paddock 33" ~ "lamson_hill",
      
      # ---------- SPECIAL DATE: 2025-05-14 ----------
      date == as.Date("2025-05-14") & paddock_name == "Paddock 1" ~ "GP3",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 2" ~ "GP3",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 3" ~ "GP3",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 4" ~ "GP3",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 5" ~ "GP3",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 6" ~ "GP3",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 7" ~ "GP3",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 8" ~ "GP3",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 9" ~ "GP4A",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 10" ~ "GP4A",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 11" ~ "GP4B",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 12" ~ "GP4C",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 13" ~ "GP4C",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 14" ~ "GP5D",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 15" ~ "GP5B",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 16" ~ "GP6A",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 17" ~ "GP6A",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 19" ~ "GP6C",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 20" ~ "GP6C",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 21" ~ "GP7",
      
      # ---------- SPECIAL DATE: 2025-05-30 ----------
      date == as.Date("2025-05-30") & paddock_name == "Paddock 1" ~ "GP3",
      date == as.Date("2025-05-30") & paddock_name == "Paddock 2" ~ "GP3",
      date == as.Date("2025-05-30") & paddock_name == "Paddock 3" ~ "GP3",
      date == as.Date("2025-05-30") & paddock_name == "Paddock 4" ~ "GP3",
      date == as.Date("2025-05-30") & paddock_name == "Paddock 5" ~ "GP3",
      date == as.Date("2025-05-30") & paddock_name == "Paddock 6" ~ "GP3",
      date == as.Date("2025-05-30") & paddock_name == "Paddock 7" ~ "GP3",
      date == as.Date("2025-05-30") & paddock_name == "Paddock 8" ~ "GP3",
      date == as.Date("2025-05-30") & paddock_name == "Paddock 9" ~ "GP4A",
      date == as.Date("2025-05-30") & paddock_name == "Paddock 10" ~ "GP4A",
      date == as.Date("2025-05-30") & paddock_name == "Paddock 11" ~ "GP4B",
      date == as.Date("2025-05-30") & paddock_name == "Paddock 12" ~ "GP4C",
      date == as.Date("2025-05-30") & paddock_name == "Paddock 13" ~ "GP4C",
      date == as.Date("2025-05-30") & paddock_name == "Paddock 14" ~ "GP5D",
      date == as.Date("2025-05-30") & paddock_name == "Paddock 15" ~ "GP5B",
      date == as.Date("2025-05-30") & paddock_name == "Paddock 16" ~ "GP6A",
      date == as.Date("2025-05-30") & paddock_name == "Paddock 17" ~ "GP6A",
      date == as.Date("2025-05-30") & paddock_name == "Paddock 19" ~ "GP6C",
      date == as.Date("2025-05-30") & paddock_name == "Paddock 20" ~ "GP6C",
      date == as.Date("2025-05-30") & paddock_name == "Paddock 21" ~ "GP7",
      
      # ---------- SPECIAL DATE: 2025-05-19 ----------
      date == as.Date("2025-05-19") & paddock_name == "Paddock 1" ~ "GP3",
      date == as.Date("2025-05-19") & paddock_name == "Paddock 2" ~ "GP3",
      date == as.Date("2025-05-19") & paddock_name == "Paddock 3" ~ "GP3",
      date == as.Date("2025-05-19") & paddock_name == "Paddock 4" ~ "GP3",
      date == as.Date("2025-05-19") & paddock_name == "Paddock 5" ~ "GP3",
      date == as.Date("2025-05-19") & paddock_name == "Paddock 6" ~ "GP3",
      date == as.Date("2025-05-19") & paddock_name == "Paddock 7" ~ "GP3",
      date == as.Date("2025-05-19") & paddock_name == "Paddock 8" ~ "GP3",
      date == as.Date("2025-05-19") & paddock_name == "Paddock 9" ~ "GP4A",
      date == as.Date("2025-05-19") & paddock_name == "Paddock 10" ~ "GP4A",
      date == as.Date("2025-05-19") & paddock_name == "Paddock 11" ~ "GP4B",
      date == as.Date("2025-05-19") & paddock_name == "Paddock 12" ~ "GP4C",
      date == as.Date("2025-05-19") & paddock_name == "Paddock 13" ~ "GP4C",
      date == as.Date("2025-05-19") & paddock_name == "Paddock 14" ~ "GP5D",
      date == as.Date("2025-05-19") & paddock_name == "Paddock 15" ~ "GP5B",
      date == as.Date("2025-05-19") & paddock_name == "Paddock 16" ~ "GP6A",
      date == as.Date("2025-05-19") & paddock_name == "Paddock 17" ~ "GP6A",
      date == as.Date("2025-05-19") & paddock_name == "Paddock 19" ~ "GP6C",
      date == as.Date("2025-05-19") & paddock_name == "Paddock 20" ~ "GP6C",
      date == as.Date("2025-05-19") & paddock_name == "Paddock 21" ~ "GP7",
      
      # ---------- SPECIAL DATE: 2025-06-05 ----------
      date == as.Date("2025-06-05") & paddock_name == "GP1A" ~ "GP3",
      date == as.Date("2025-06-05") & paddock_name == "GP1B" ~ "GP3",
      date == as.Date("2025-06-05") & paddock_name == "GP2A" ~ "GP3",
      date == as.Date("2025-06-05") & paddock_name == "GP2B" ~ "GP3",
      date == as.Date("2025-06-05") & paddock_name == "GP3S" ~ "GP3",
      date == as.Date("2025-06-05") & paddock_name == "GP3N" ~ "GP3",
      date == as.Date("2025-06-05") & paddock_name == "GP4A" ~ "GP3",
      date == as.Date("2025-06-05") & paddock_name == "GP4B" ~ "GP3",
      date == as.Date("2025-06-05") & paddock_name == "GP4C" ~ "GP4A",
      date == as.Date("2025-06-05") & paddock_name == "GP5D" ~ "GP4A",
      date == as.Date("2025-06-05") & paddock_name == "GP5C" ~ "GP4B",
      date == as.Date("2025-06-05") & paddock_name == "GP5B" ~ "GP4C",
      date == as.Date("2025-06-05") & paddock_name == "GP5A" ~ "GP4C",
      date == as.Date("2025-06-05") & paddock_name == "GP6A" ~ "GP5D",
      date == as.Date("2025-06-05") & paddock_name == "GP6B" ~ "GP5B",
      date == as.Date("2025-06-05") & paddock_name == "GP6C" ~ "GP6A",
      date == as.Date("2025-06-05") & paddock_name == "GP7S" ~ "GP6A",
      date == as.Date("2025-06-05") & paddock_name == "GP8A" ~ "GP6C",
      date == as.Date("2025-06-05") & paddock_name == "GP8B" ~ "GP6C",
      date == as.Date("2025-06-05") & paddock_name == "GP Woods" ~ "GP7",
      
      # ---------- SPECIAL DATE: 2025-07-31 ----------
      date == as.Date("2025-07-31") & paddock_name == "Paddock 44" ~ "williams_1A",
      date == as.Date("2025-07-31") & paddock_name == "Paddock 45" ~ "pond",
      date == as.Date("2025-07-31") & paddock_name == "Paddock 46" ~ "horse",
      date == as.Date("2025-07-31") & paddock_name == "Paddock 47" ~ "triangle_lower",
      date == as.Date("2025-07-31") & paddock_name == "Paddock 48" ~ "triangle_upper",
      date == as.Date("2025-07-31") & paddock_name == "Paddock 49" ~ "railroad",

      # ---------- SPECIAL DATE: 2025-05-09 ----------
      date == as.Date("2025-05-09") & paddock_name == "Drainage" ~ "drainage",
      date == as.Date("2025-05-09") & paddock_name == "Lower Barberry" ~ "lower_barberry",
      date == as.Date("2025-05-09") & paddock_name == "Wilson's" ~ "wilson",
      date == as.Date("2025-05-09") & paddock_name == "Underhill" ~ "underhill",
      date == as.Date("2025-05-09") & paddock_name == "Underhill Wet" ~ "underhill_wet",
      date == as.Date("2025-05-09") & paddock_name == "Lower Underhill" ~ "lower_underhill",
      date == as.Date("2025-05-09") & paddock_name == "Sunset Hill" ~ "sunset_hill",
      date == as.Date("2025-05-09") & paddock_name == "Sunset Field" ~ "sunset_field",
      date == as.Date("2025-05-09") & paddock_name == "Lower Sunset" ~ "lower_sunset",
      date == as.Date("2025-05-09") & paddock_name == "Pond" ~ "pond",
      date == as.Date("2025-05-09") & paddock_name == "Horse" ~ "horse",
      date == as.Date("2025-05-09") & paddock_name == "Triangle Lower" ~ "triangle_lower",
      date == as.Date("2025-05-09") & paddock_name == "Triangle Upper" ~ "triangle_upper",
      date == as.Date("2025-05-09") & paddock_name == "Railroad" ~ "railroad",
      
      # ---------- SPECIAL DATE: 2025-06-10 ----------
      date == as.Date("2025-06-10") & paddock_name == "Wilson's" ~ "wilson",
      date == as.Date("2025-06-10") & paddock_name == "Underhill" ~ "underhill",
      date == as.Date("2025-06-10") & paddock_name == "Underhill Wet" ~ "underhill_wet",
      date == as.Date("2025-06-10") & paddock_name == "Lower Underhill" ~ "lower_underhill",
      date == as.Date("2025-06-10") & paddock_name == "Sunset Hill" ~ "sunset_hill",
      date == as.Date("2025-06-10") & paddock_name == "Sunset Field" ~ "sunset_field",
      date == as.Date("2025-06-10") & paddock_name == "Lower Sunset" ~ "lower_sunset",
      date == as.Date("2025-06-10") & paddock_name == "Pond" ~ "pond",
      date == as.Date("2025-06-10") & paddock_name == "Horse" ~ "horse",
      date == as.Date("2025-06-10") & paddock_name == "Triangle Lower" ~ "triangle_lower",
      date == as.Date("2025-06-10") & paddock_name == "Triangle Upper" ~ "triangle_upper",
      date == as.Date("2025-06-10") & paddock_name == "Railroad" ~ "railroad",
    
      
       #------default naming-----------------------------
      paddock_name == "Paddock 1"  ~ "GP1A",
      paddock_name == "Paddock 2"  ~ "GP1B",
      paddock_name == "Paddock 3"  ~ "GP2A",
      paddock_name == "Paddock 4"  ~ "GP2B",
      paddock_name == "Paddock 5"  ~ "GP3",
      paddock_name == "Paddock 6"  ~ "GP3",
      paddock_name == "Paddock 7"  ~ "GP4A",
      paddock_name == "Paddock 8"  ~ "GP4B",
      paddock_name == "Paddock 9"  ~ "GP4C",
      paddock_name == "Paddock 10" ~ "GP5D",
      paddock_name == "Paddock 11" ~ "GP5C",
      paddock_name == "Paddock 12" ~ "GP5B",
      paddock_name == "Paddock 13" ~ "GP5A",
      paddock_name == "Paddock 14" ~ "GP6A",
      paddock_name == "Paddock 15" ~ "GP6B",
      paddock_name == "Paddock 16" ~ "GP6C",
      paddock_name == "Paddock 17" ~ "GP7",
      paddock_name == "Paddock 18" ~ "GP7A",
      paddock_name == "Paddock 19" ~ "GP8A",
      paddock_name == "Paddock 20" ~ "GP8B",
      paddock_name == "Paddock 21" ~ "GP_woods",
      paddock_name == "Paddock 22" ~ "briar",
      paddock_name == "Paddock 23" ~ "plains",
      paddock_name == "Paddock 24" ~ "playground",
      paddock_name == "Paddock 25" ~ "Leos",
      paddock_name == "Paddock 26" ~ "lamson_north",
      paddock_name == "Paddock 27" ~ "lamson_middle",
      paddock_name == "Paddock 28" ~ "lamson_hill",
      paddock_name == "Paddock 29" ~ "jimmys",
      paddock_name == "Paddock 30" ~ "white_cottage",
      paddock_name == "Paddock 31" ~ "POW_west",
      paddock_name == "Paddock 32" ~ "POW_east",
      paddock_name == "Paddock 33" ~ "drainage",
      paddock_name == "Paddock 34" ~ "lower_barberry",
      paddock_name == "Paddock 35" ~ "wilson",
      paddock_name == "Paddock 36" ~ "underhill",
      paddock_name == "Paddock 37" ~ "underhill_wet",
      paddock_name == "Paddock 38" ~ "lower_underhill",
      paddock_name == "Paddock 39" ~ "sunset_field",
      paddock_name == "Paddock 40" ~ "sunset_hill",
      paddock_name == "Paddock 41" ~ "lower_sunset",
      paddock_name == "Paddock 42" ~ "williams",
      paddock_name == "Paddock 43" ~ "williams_1A",
      paddock_name == "Paddock 44" ~ "pond",
      paddock_name == "Paddock 45" ~ "horse",
      paddock_name == "Paddock 46" ~ "triangle_lower",
      paddock_name == "Paddock 47" ~ "triangle_upper",
      paddock_name == "Paddock 48" ~ "railroad",
      
      TRUE ~ paddock_name
      # ---------- DEFAULT (KEEP ORIGINAL) ----------
    )
  )

allplatemeter2 <- allplatemeter2 %>% 
  rename(cover = Cover)

#---------------------------------------------------------------------------------------------
#SECOND, reading in the livestock recorded platemetering (from Maia grazing software)
#------------------------------reading in maia data----------------------------
maia_cover <- read.csv("./platemeter/maia cover.csv")%>%
  select(-contains("X"))


#converting date to right data type
head(maia_cover$date)
str(maia_cover$date)
maia_cover$date <- as.Date(maia_cover$date, format = "%m/%d/%Y")

#getting rid of commas, setting data in cover to numeric
maia_cover$cover <- gsub(",", "", maia_cover$cover)
maia_cover$cover <- as.numeric(maia_cover$cover)

#combines allplatemeter2 and maia_cover
combined <- bind_rows(allplatemeter2, maia_cover)

#assigning the agroecology platemeter rows a "move_type"
combined$move_type[is.na(combined$move_type)] <- "agroecology"

#gets rid of irrelevant paddock_name column
combined <- combined %>% select(-paddock_name)

#--------------------------------------------------------------------------------
#plotting the data
ggplot(combined, aes(x=field, y=cover, color=move_type))+
  geom_point()

ggplot(filter(combined, field=="GP1A"), aes(x=date, y=cover, color=move_type))+
  geom_point()


#----------------exporting out of R-----------------------------------
install.packages("writexl")
library(writexl)

write_xlsx(allplatemeter2, "allplatemeter2.xlsx")


write_xlsx(maia_cover, "maiaclean.xlsx")
