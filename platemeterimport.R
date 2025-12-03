
#--------reading in each platemeter csv and naming them by date---------

apr21<-read.csv("./platemeter/2025-Apr-21-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("4/21/25"))

apr24<-read.csv("./platemeter/2025-Apr-24-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("4/24/25"))

jun10<-read.csv("./platemeter/2025-Jun-10-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("6/10/25"))

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

#-------------------------------------removing random extra columns from platemeter------------

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

#----------r bind combines all of the separate csvs into one------------------

allplatemeter<-rbind(apr21,apr24,apr25,apr30,aug15,aug25,aug26,aug28,jul11,jul14,jul15, jul31,jun02,jun05,jun10,jun11,jun23,jun27,may09,may12,may13,may14,may19,may21,may30,oct01,oct06,sep12,sep17)

---------------------------------------------------------------------------------
#because of discrepancies in field naming, I assigned the appropriate
#paddock name to each row
---------------------------------------------------------------------------------
#first I am naming each paddock (1-48) according to the names from the new 
#plate metering map

---------------------------------------------------------------------------------
#the code below corrects the paddock names for the files apr21, apr24, apr25, apr30
#since the platemetering done on these days references the old zones
#and names the rest of the paddocks using the new platemetering zones
 
allplatemeter <- allplatemeter %>%
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
      date == as.Date("2025-05-14") & paddock_name == "Paddock 1" ~ "GP1B",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 2" ~ "GP2A",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 3" ~ "GP2A",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 4" ~ "GP2B",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 5" ~ "GP3",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 6" ~ "GP3",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 7" ~ "GP3",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 8" ~ "GP3",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 9" ~ "GP1A",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 10" ~ "GP4A",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 11" ~ "GP4A",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 12" ~ "GP4B",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 13" ~ "GP4C",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 14" ~ "GP5D",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 15" ~ "GP5A",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 16" ~ "GP6A",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 17" ~ "GP6A",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 19" ~ "GP6C",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 20" ~ "GP6C",
      date == as.Date("2025-05-14") & paddock_name == "Paddock 21" ~ "GP7",
      
      
      
      
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






