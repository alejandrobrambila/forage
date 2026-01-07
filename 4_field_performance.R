### field performance
### Question 4: How did fields respond to use (Alejandro wrote this script)
### with reference to the past and outside references
## 4a: Total productivity
### 4b: Time to recovery/speed of recovery
### 4c: max volume/total harvest
## 4d: forage quality (hay and forage)

#source data ----

# import record of moves for both herd, as well as hay and mow events.  exported in 'plan_v_actual.R' (Saskia)
record_actual <-read_csv("2025 record actual (new).csv")%>% 
  select(1, 2, 3, 4, 6, 7)  %>% # this new version has maia plameter already in it! 
  mutate(date=mdy(date))%>%
  mutate(field=ifelse(field=="pond_field", "Pond", field))%>%
  mutate(field=case_when(
    field == "gp_woods" ~ "GP Woods",
    field == "leos" ~ "Leos",
    field == "playground" ~ "Playground",
    field == "sunset_field" ~ "Sunset Field",
    field == "sunset hill" ~ "Sunset Hill",
    field == "lower_Sunset" ~ "Lower_sunset",
    field == "horse" ~ "Horse",
    field == "lower_home" ~ "Lower Home",
    field == "pow_east" ~ "POW East",
    field == "triangle_upper"~ "Upper Triangle",
    field == "triangle_lower"~ "Lower Triangle",
    field == "lower_barberry"~ "Lower Barberry",
    field == "lower_underhill"~ "Lower Underhill",
    field == "Underhill Veg Perimeter" ~ "underhill_veg",
    field == "barberry_woods" ~ "Barberry Woods",
    field == "underhill_wet" ~ "Underhill Wet Meadow",
    field == "sunset_hill" ~ "Sunset Hill",
    field == "lower_sunset" ~ "Lower Sunset",
    field == "williams_west" ~ "Williams West",
    field == "Leos" ~ "Leo's",
    field == "GP1B" ~ "gp1b",
    field == "GP1" ~ "GP1A",
    field == "GP6" ~ "GP6A",
    field == "jimmys" ~ "Jimmy's",
    T~field))%>%
    mutate(field=str_replace_all(field, "_", " "))%>%
  mutate(field=str_replace_all(field, "Gp", "GP"))%>%
    mutate(field=str_to_title(field))%>%
  mutate(field=ifelse(str_starts(field, "Gp"), toupper(field), field))%>%
  mutate(field=ifelse(field=="GP WOODS", "GP Woods", field))%>%
  mutate(field=ifelse(field=="Upper Sunset", "Sunset Hill", field))
  

# import all platemeter data. exported from "platemeterimport.R" (Saskia)
all_platemeter <- read_excel("all_platemeter.xlsx") %>%
  mutate(date=ymd(date))%>%
  mutate(field=str_replace_all(field, "_", " "))%>%
  mutate(field=str_to_title(field))%>%
  mutate(field=ifelse(str_starts(field, "Gp"), toupper(field), field))%>%
  mutate(field=case_when(
    field == "GP WOODS" ~ "GP Woods",
    field == "Pow West" ~ "POW West",
    field == "Pow East" ~ "POW EAST",
    field == "Williams 1a" ~ "Williams 1A",
    field == "Triangle Lower" ~ "Lower Triangle",
    field == "Triangle Upper" ~ "Upper Triangle",
    field == "Leos" ~ "Leo's",
    field == "GP1" ~ "GP1A",
    field == "GP6" ~ "GP6A",
    field == "Jimmys" ~ "Jimmy's",
    field == "Williams" ~ "Williams West",
    T~field))
  
  
  
# import all hay data
hay_production <-read_excel("2025 Hay Production.xlsx") %>%
  filter(!is.na(Field_name))%>%
  rename(field=Field_name)%>%
  mutate(field=case_when(
    field == "Broad Meadow 1A" ~ "Broad Meadow",
    field == "Broad Meadows CSA" ~ "Broad Meadow",
    field == "Lamson Field Middle" ~ "Lamson Middle",
    field == "Leahs" ~ "White Cottage",
    field == "Underhill Wet" ~ "Underhill Wet Meadow",
    field == "Wilson's" ~ "Wilson",
    field == "Leos" ~ "Leo's",
    field == "Triangle Upper" ~ "Upper Triangle",
    T~field))

hay_quality <-read_excel("Dairy One Results.xlsx") 

names_acres<-read_csv("field_key.csv")%>%
  rename(field=`2025_Fields`)%>%
  rename(acres=`2025_acreage`)%>%
  select(field, acres)%>%
  unique()
  


#4a What was total productivity of each field. ----
# THIS IS THE SAME QUESTION AS 3C!!!

#get all platemeters from maia and agroecology into one file
actuals_platemeter<-all_platemeter%>%
  select(-paddock)%>%
  mutate(old_cover=cover)%>%
  select(-cover)%>%
  full_join(record_actual)%>%
  mutate(cover=ifelse(is.na(cover), old_cover, cover))%>%
  mutate(field=case_when(
    field == "POW EAST" ~ "POW East",
    field == "Underhill Wet" ~ "Underhill Wet Meadow",
    field == "Upper Sunset" ~ "Sunset Hill",
    field == "Plains Wet" ~ "Plains Woods North",
    field == "Leos" ~ "Leo's",
    field == "Triangle Upper" ~ "Upper Triangle",
    T~field))%>%
  left_join(names_acres)%>%
  filter(!is.na(acres))%>%
  mutate(acres_grazed=ifelse(is.na(acres_grazed), acres, acres_grazed))%>%
  rename(acres_impacted=acres_grazed)

#fill missing in and out values from nearest platemeter

ap_arranged<- arrange(actuals_platemeter, field, date)

ap_arranged_ins<-filter(ap_arranged, move_type!="day_out")%>%
  mutate(filled=cover)%>%
  group_by(field)%>%
  fill(filled)%>%
  mutate(interpolated=ifelse(is.na(cover)&!is.na(filled), 1, NA))

ap_arranged_outs<-filter(ap_arranged, move_type!="day_in")%>%
  mutate(filled=cover)%>%
  group_by(field)%>%
  fill(filled, .direction="up")%>%
  mutate(cover_copied=ifelse(is.na(cover)&!is.na(filled), 1, NA))%>%
  filter(move_type=="day_out")


#hay harvest (by platemeter)
hay_dates<-select(hay_production, field, `Cut date`, 5)%>%
  rename(date=`Cut date`, acres_impacted=`Acres hayed`)%>%
  mutate(move_type="hay_in")

hay_dates2<-hay_dates%>%
  mutate(date=date+days(2), move_type="hay_out")

hay_dates<-bind_rows(hay_dates, hay_dates2)%>%
  left_join(names_acres)

#mow dates
rec<-read_csv("recovery_clean.csv")%>%
  mutate(field=ifelse(field=="pond_field", "Pond", field))%>%
  mutate(field=case_when(
    field == "gp_woods" ~ "GP Woods",
    field == "leos" ~ "Leos",
    field == "playground" ~ "Playground",
    field == "sunset_field" ~ "Sunset Field",
    field == "sunset hill" ~ "Sunset Hill",
    field == "lower_Sunset" ~ "Lower_sunset",
    field == "horse" ~ "Horse",
    field == "lower_home" ~ "Lower Home",
    field == "pow_east" ~ "POW East",
    field == "triangle_upper"~ "Upper Triangle",
    field == "triangle_lower"~ "Lower Triangle",
    field == "lower_barberry"~ "Lower Barberry",
    field == "lower_underhill"~ "Lower Underhill",
    field == "Underhill Veg Perimeter" ~ "underhill_veg",
    field == "barberry_woods" ~ "Barberry Woods",
    field == "underhill_wet" ~ "Underhill Wet Meadow",
    field == "sunset_hill" ~ "Sunset Hill",
    field == "lower_sunset" ~ "Lower Sunset",
    field == "williams_west" ~ "Williams West",
    field == "Broad Meadow Dry" ~ "Broad Meadow",
    field == "Broad Meadow 1a" ~ "Broad Meadow",
    field == "Leos" ~ "Leo's",
    field == "GP1B" ~ "gp1b",
    field == "GP1" ~ "GP1A",
    field == "GP6" ~ "GP6A",
    field == "jimmys" ~ "Jimmy's",
    T~field))%>%
  mutate(field=str_replace_all(field, "_", " "))%>%
  mutate(field=str_replace_all(field, "Gp", "GP"))%>%
  mutate(field=str_to_title(field))%>%
  mutate(field=ifelse(str_starts(field, "Gp"), toupper(field), field))%>%
  mutate(field=ifelse(field=="GP WOODS", "GP Woods", field))%>%
  mutate(field=ifelse(field=="Upper Sunset", "Sunset Hill", field))%>%
  mutate(disturbance_type=case_when(
    disturbance_type == "graze" ~ "day_in",
    disturbance_type == "mow" ~ "mow_in",
    disturbance_type == "hay" ~ "hay_in",T~disturbance_type))%>%
  rename(move_type=disturbance_type)%>%
  rename(date=day_in_date)%>%select(1:4)

  
library(zoo)

actuals_platemeter1<-bind_rows(ap_arranged_ins, ap_arranged_outs, hay_dates)%>%
  unique()%>%
  full_join(rec)%>%

  arrange(field, date)%>%
  
  #calculate actual amount grazed
  group_by(field, herd)%>%
  mutate(frac_acres=acres_impacted/acres, frac_max_acres=acres_impacted/max(acres_impacted))%>%
  ungroup()%>%
  mutate(acres_impacted=ifelse(move_type=="agroecology", NA, acres_impacted))%>%
  mutate(frac_acres=ifelse(move_type=="agroecology", NA, frac_acres))%>%
  mutate(frac_max_acres=ifelse(move_type=="agroecology", NA, frac_max_acres))%>%
  
  #set up cycles (hay)
  group_by(field, move_type)%>%
  mutate(hay_cycle=row_number())%>%
  mutate(hay_cycle=ifelse(move_type=="hay_in", hay_cycle, NA))%>%
  group_by(field)%>%
  fill(hay_cycle)%>%
  mutate(hay_cycle=ifelse(is.na(hay_cycle), 0, hay_cycle))%>%
  
  #set up cycles (graze)
  group_by(field, move_type)%>%
  mutate(graze_cycle0=row_number())%>%
  mutate(graze_cycle0=ifelse(move_type=="agroecology"|move_type=="hay_out"|move_type=="mow_in", NA, graze_cycle0))%>%
  mutate(graze_cycle0=ifelse(move_type=="hay_in", NA, graze_cycle0))%>%
  group_by(field)%>%
  fill(graze_cycle0)%>% 
  mutate(graze_cycle0=ifelse(is.na(graze_cycle0), 0, graze_cycle0))%>%
  
  #indicate skips based on field acreage
  group_by(field, herd, graze_cycle0)%>%
  mutate(graze_cycle_skip=sum(frac_max_acres))%>%
  mutate(graze_cycle_skip=ifelse(sum(graze_cycle_skip<1),"skip", "dont_skip"))%>%
  mutate(graze_cycle_skip = ifelse (field=="GP3"&(move_type=="day_in"|move_type=="day_out")&herd=="brood"&(graze_cycle0==1|graze_cycle0==3),"dont_skip", graze_cycle_skip))%>%


  group_by(field, move_type, graze_cycle_skip)%>%
  mutate(graze_cycle=row_number())%>%
  mutate(graze_cycle=ifelse(move_type=="agroecology"|move_type=="hay_out"|move_type=="mow_in"|graze_cycle_skip=="skip", NA, graze_cycle))%>%
  mutate(graze_cycle=ifelse(move_type=="hay_in", NA, graze_cycle))%>%
  group_by(field)%>%
  fill(graze_cycle)%>% 
  mutate(graze_cycle=ifelse(is.na(graze_cycle), 0, graze_cycle))%>%
  

  group_by(field, move_type)%>%
  mutate(mow_cycle=row_number())%>%
  mutate(mow_cycle=ifelse(move_type!="mow_in", NA, mow_cycle))%>%
  group_by(field)%>%
  fill(mow_cycle)%>% 
  mutate(mow_cycle=ifelse(is.na(mow_cycle), 0, mow_cycle))%>%
  
  #aggregated hay harvest impact, impact, recovery cycles
  mutate(harvest_cycle=graze_cycle+hay_cycle)%>%
  mutate(harvest_cycle0=graze_cycle0+hay_cycle)%>%
  
  mutate(impact_cycle=graze_cycle+hay_cycle+mow_cycle)%>%
  
  mutate(int_cycle=ifelse(move_type=="day_in"|move_type=="hay_in"|move_type=="mow_in", impact_cycle-1, impact_cycle))%>%
  mutate(int_cycle=ifelse(move_type=="agroecology", NA, int_cycle))%>%
  
  group_by(field)%>%
  #this cycle resets on the move out.  it will allow interpolation per field per cycle to estimate what the forage would have been
  fill(int_cycle, .direction="up")%>% 
  fill(int_cycle)%>%
  
  mutate(int_cover=cover)%>%
  group_by(field, int_cycle)%>%
  mutate(int_cover=na.approx(int_cover, na.rm=F, rule=2))%>% 
  select(date, field, acres, acres_impacted, frac_max_acres, move_type, herd, int_cover, hay_cycle, graze_cycle, mow_cycle, impact_cycle, harvest_cycle, harvest_cycle0, int_cycle)%>%

  rename(cover=int_cover, regrowth_cycle=int_cycle)%>%
  mutate(herd=ifelse(move_type=="hay_in"|move_type=="hay_out", "hay", herd))
  #clean up columns
 
  
#calculate dates and join in
calc_dates<-actuals_platemeter1%>%
  ungroup()%>%
  filter(move_type=="day_in"|move_type=="day_out")%>%
  select(field, move_type, herd, harvest_cycle0, date)%>%
  pivot_wider(names_from = move_type, values_from = date)%>%
  mutate(impact_period=day_out-day_in)%>%
  mutate(impact_period=ifelse(is.na(impact_period), 2 , impact_period))%>%
  select(field, herd, harvest_cycle0, impact_period)

#calc_recoveryactuals_platemeter1%>%
#    ungroup()%>%
#    filter(move_type!="agroecology")%>%
#    select(field, move_type, herd, regrowth_cycle, date)%>%
#    pivot_wider(names_from = move_type, values_from = date)%>%
#   mutate(rest_period=day_out-day_in)%>%
#   mutate(impact_period=ifelse(is.na(impact_period), 2 , impact_period))%>%
#   select(field, herd, harvest_cycle, impact_period)

#export a clean version for others. with platemeters filled in as much as possible and acreages
final_actuals_export<-left_join(actuals_platemeter1, calc_dates)
#calculate rest period




rm(all_platemeter, ap_arranged, ap_arranged_ins, ap_arranged_outs, calc_dates, record_actual, hay_dates, hay_dates2)

write_csv(final_actuals_export, "actuals_platemeter_final.csv")

ggplot(final_actuals_export, aes(date, cover, color=move_type)) +
  geom_point()+ facet_wrap(~field) + xlab("") +ylab("lb Dry Matter / Acre (measured by Platemeter)")


# filter down to just ins and outs (now filled), spread, and subtract to get total

in_out <- filter(ungroup(final_actuals_export), move_type!='agroecology')%>%
  select(field, harvest_cycle, move_type, herd, acres_impacted, cover)%>%
  group_by(field, harvest_cycle, move_type, herd)%>%
  dplyr::summarize(cover=mean(cover, na.rm=T), acres_impacted=sum(acres_impacted, na.rm=T))%>%
  pivot_wider(names_from=move_type, values_from=cover)%>%
 
  mutate(cowharvest=as.numeric(day_in)-as.numeric(day_out), hayharvest=as.numeric(hay_in)-as.numeric(hay_out))%>%
  group_by(field, harvest_cycle)%>%
  mutate(harvest=sum(cowharvest, hayharvest, na.rm=T))%>%
  mutate(harvest=ifelse(harvest<0, 0, harvest))%>%
  group_by(field)%>%
  mutate(sum=sum(harvest, na.rm=T))%>%
  mutate(harvest_acres=harvest*acres_impacted, sum_acres=sum(harvest_acres))

wet<-read_csv("field_key.csv")%>%
  select(2,4, 5)%>%
  rename(field=`2025_Fields`)%>%
  rename(type=`2025_type`)
in_out_wet<-left_join(in_out, wet)

# 2c ----
totals_by_herd<-in_out%>%
  ungroup()%>%
  group_by(herd)%>%
  dplyr::summarize(harvest=sum(harvest_acres, na.rm=T))


## THESE SHOW CUMULATIVE LIVESTOCK HARVEST BASED ON IN/OUT PLATEMETER

# by acre
ggplot(filter(in_out, harvest_acres>0), aes(reorder(field, desc(sum_acres)), harvest_acres, color=as.factor(herd))) +
  geom_point()+theme_bw()+ geom_point(aes(y=sum_acres))+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 8))+
  ylab("Total Forage Harvested per Cycle (Platemeter) lb DM")+xlab("")+labs(color="")

# 3c ----
ggplot(filter(in_out, harvest_acres>0), aes(reorder(field, desc(sum_acres)), sum_acres)) +geom_point()+theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 8))+ geom_point(aes(y=harvest_acres, color=herd), size=.8)+
  ylab("Total Forage Harvested (Platemeter) lb DM")+xlab("")+labs(color="")

#hay only (by platemeter)
ggplot(filter(in_out, herd=="hay"), aes(reorder(field, desc(sum_acres)), harvest_acres, color=as.factor(herd))) +geom_point()+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6))

# there is some missing values still - those are represented as zero
ggplot(filter(in_out, harvest_acres>0), aes(reorder(field, desc(sum)), harvest, color=as.factor(herd))) +geom_point()+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6))+
  ylab("Total Forage Harvested per Cycle (Platemeter) lb DM / Acre")

# 3c ----
ggplot(filter(in_out, harvest_acres>0), aes(reorder(field, desc(sum)), sum)) +
  geom_point()+ theme_bw()+ geom_point(aes(y=harvest, color=herd), size=.8)+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 8))+
  ylab("Total Forage Harvested (Platemeter) lb DM / Acre")+xlab("")+labs(color="")

#wet
ggplot(filter(in_out_wet, harvest_acres>0), aes(reorder(field, desc(sum)), sum, fill=type) ) +
  geom_rect(aes(width=.5, ymin=0, ymax=1000))+
  geom_point()+ theme_bw()+ geom_point(aes(y=harvest, color=herd), size=.8)+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 8))+
  ylab("Total Forage Harvested (Platemeter) lb DM / Acre")+xlab("")+labs(color="")

#bird
ggplot(filter(in_out_wet, harvest_acres>0), aes(reorder(field, desc(sum)), sum, fill=`2025_bird`) ) +
  geom_rect(aes(width=.5, ymin=0, ymax=1000))+
  geom_point()+ theme_bw()+ geom_point(aes(y=harvest, color=herd), size=.8)+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 8))+
  ylab("Total Forage Harvested (Platemeter) lb DM / Acre")+xlab("")+labs(color="")


# hay harvest (by bales)
hay_lbs<-hay_production%>%
  mutate(`Round bales`=ifelse(is.na(`Round bales`), 0, `Round bales`))%>%
mutate(`small rounds`=ifelse(is.na(`small rounds`), 0, `small rounds`))%>%
mutate(`Square bales`=ifelse(is.na(`Square bales`), 0, `Square bales`))%>%
  mutate(harvest=.85*(`Round bales`*750+`small rounds`*375+`Square bales`*55))

ggplot(hay_lbs, aes(reorder(`field`,desc(harvest)), harvest)) +geom_point()+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6))

join<-select(in_out, field,herd, harvest_acres)%>%
  filter(herd=="hay")%>%
  rename(harvest=harvest_acres)%>%
  select(-herd)%>%
  mutate(source="platemeter")

calibrate_hay<-select(hay_lbs, field, harvest)%>%
  mutate(source="bales")%>%
  bind_rows(join)%>%
  group_by(field, source)%>%
  dplyr::summarize(harvest=sum(harvest))%>%
  pivot_wider(names_from = source, values_from=harvest)%>%
  mutate(prop=(platemeter-bales)/bales*100)%>%
  mutate(diff=platemeter-bales)

sumsum<-calibrate_hay%>%
  ungroup()%>%
  dplyr::summarize(bales=sum(bales, na.rm=T), platemeter=sum(platemeter, na.rm=T))

ggplot(calibrate_hay, aes(bales, platemeter))+ 
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
  geom_point(aes(color=field))


ggplot(calibrate_hay, aes(bales, diff))+ 
  #geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
  geom_point(aes(color=field))



# 4a. Time to recovery ----

recovery<-final_actuals_export%>%
  group_by(field, regrowth_cycle)%>%
  mutate(recdays=difftime(date, min(date), units="days"))

ggplot(recovery, aes(recdays, cover, color=as.factor(regrowth_cycle))) +geom_line()+facet_wrap(~field)
ggplot(filter(recovery, recdays<40), aes(recdays, cover, color=as.factor(regrowth_cycle))) +geom_line()+facet_wrap(~field)
ggplot(filter(recovery, recdays<40), aes(recdays, cover, color=as.factor(regrowth_cycle))) +geom_smooth(method="lm",se=F)+geom_point()+facet_wrap(~field)


rec_slopes<-filter(recovery, recdays<40)%>%
  mutate(month=month(date))%>%
  mutate(season=ifelse(month<7, 1, ifelse(month<9, 2, 3)))%>%
  group_by(field, regrowth_cycle)%>%
  dplyr::summarize(slope=max(cover)/max(as.integer(recdays)), month=min(month), season=min(season), resid=min(cover), herd=max(herd, na.rm=T))%>%
  filter(!is.na(slope)&slope!=Inf&slope<500&!is.na(regrowth_cycle)&regrowth_cycle>-1)%>%
  mutate(season=ifelse(season==1, "spring", ifelse(season==2, "summer", "x_fall")))

ggplot(filter(rec_slopes), aes(reorder(field, desc(slope)), slope, color=as.factor(regrowth_cycle))) +
  geom_point()+theme_bw()+ xlab("")+ylab("Regrowth lb/acre/day")+
 theme( axis.text.x = element_text(angle = 90, hjust = 1, size = 8))+labs(color="Regrowth Cycle")

#ggplot(filter(rec_slopes), aes(reorder(field, desc(slope)), slope, color=as.factor(season))) +
#  geom_point()+theme_bw()+ xlab("")+ylab("Regrowth lb/acre/day")+
#  theme( axis.text.x = element_text(angle = 90, hjust = 1, size = 8))+labs(color="Regrowth Cycle")


# 4b recovey reasons ---- 

#cycle
ggplot(filter(rec_slopes), aes(as.factor(regrowth_cycle), slope, )) +
  geom_boxplot()+theme_bw()+ xlab("")+ylab("Regrowth lb/acre/day")+geom_jitter(width=.25, color="grey", size=.8)+
  theme( axis.text.x = element_text(hjust = 1, size = 8))+xlab("Regrowth Cycle")

#season
ggplot(filter(rec_slopes), aes(as.factor(season), slope, )) +
  geom_boxplot()+theme_bw()+ xlab("")+ylab("Regrowth lb/acre/day")+geom_jitter(width=.25, color="grey", size=.8)+
  theme( axis.text.x = element_text(hjust = 1, size = 8))+xlab("Season")

#month
ggplot(filter(rec_slopes), aes(as.factor(month), slope, )) +
  geom_boxplot()+theme_bw()+ xlab("")+ylab("Regrowth lb/acre/day")+geom_jitter(width=.25, color="grey", size=.8)+
  theme( axis.text.x = element_text(hjust = 1, size = 8))+xlab("Month")

#residual
ggplot(filter(rec_slopes, resid>0, regrowth_cycle>0), aes(resid, slope)) +
  geom_point()+theme_bw()+ xlab("")+ylab("Regrowth lb/acre/day")+
  theme( axis.text.x = element_text(hjust = 1, size = 8))+xlab("Starting Residual")

  
##### add mow dates for GP2a? 6b? lamson hill? uwm? w1A
         