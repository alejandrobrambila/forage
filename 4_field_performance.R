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

  
library(zoo)

actuals_platemeter1<-bind_rows(ap_arranged_ins, ap_arranged_outs, hay_dates)%>%
  arrange(field, date)%>%
  group_by(field, move_type)%>%
  mutate(hay_cycle=row_number())%>%
  mutate(hay_cycle=ifelse(move_type=="hay_in", hay_cycle, NA))%>%
  group_by(field)%>%
  fill(hay_cycle)%>%
  mutate(hay_cycle=ifelse(is.na(hay_cycle), 0, hay_cycle))%>%
  group_by(field, move_type)%>%
  mutate(graze_cycle=row_number())%>%
  mutate(graze_cycle=ifelse(move_type=="agroecology"|move_type=="hay_out", NA, graze_cycle))%>%
  mutate(graze_cycle=ifelse(move_type=="hay_in", NA, graze_cycle))%>%
  group_by(field)%>%
  #this cycle column resets when move in happens, and the subsequent move out and following AE readings are on that cycle
  fill(graze_cycle)%>% 
  mutate(graze_cycle=ifelse(is.na(graze_cycle), 0, graze_cycle))%>%
  mutate(harvest_cycle=graze_cycle+hay_cycle)%>%
  mutate(int_cycle=ifelse(move_type=="day_in"|move_type=="hay_in", harvest_cycle-1, harvest_cycle))%>%
  mutate(int_cycle=ifelse(move_type=="agroecology", NA, int_cycle))%>%
  group_by(field)%>%
  #this cycle resets on the move out.  it will allow interpolation per field per cycle to estimate what the forage would have been
  fill(int_cycle, .direction="up")%>% 
  fill(int_cycle)%>%
  mutate(int_cover=cover)%>%
  group_by(field, int_cycle)%>%
  mutate(int_cover=na.approx(int_cover, na.rm=F, rule=2))%>% 
  group_by(field, herd)%>%
  mutate(frac_acres=acres_impacted/acres, frac_max_acres=acres_impacted/max(acres_impacted))%>%
  ungroup()%>%
  select(date, field, acres, acres_impacted, frac_acres, frac_max_acres, move_type, herd, int_cover, hay_cycle, graze_cycle, harvest_cycle, int_cycle)%>%
  rename(cover=int_cover, regrowth_cycle=int_cycle)%>%
  mutate(herd=ifelse(move_type=="hay_in"|move_type=="hay_out", "hay", herd))%>%
  mutate(acres_impacted=ifelse(move_type=="agroecology", NA, acres_impacted))%>%
  mutate(frac_acres=ifelse(move_type=="agroecology", NA, frac_acres))%>%
  mutate(frac_max_acres=ifelse(move_type=="agroecology", NA, frac_max_acres))%>%
  group_by(field, herd, harvest_cycle)%>%
  mutate(harvest_cycle_skip=sum(frac_max_acres))%>%
  mutate(harvest_cycle_skip=ifelse(sum(harvest_cycle_skip<1),"skip", "dont_skip"))%>%
  mutate(harvest_cycle_skip = ifelse (field=="GP3"&(move_type=="day_in"|move_type=="day_out")&herd=="brood"&(harvest_cycle==1|harvest_cycle==3),"dont_skip", harvest_cycle_skip))

#calculate dates and join in
calc_dates<-actuals_platemeter1%>%
  ungroup()%>%
  filter(move_type!="agroecology")%>%
  select(field, move_type, herd, harvest_cycle, date)%>%
  pivot_wider(names_from = move_type, values_from = date)%>%
  mutate(impact_period=day_out-day_in)%>%
  mutate(impact_period=ifelse(is.na(impact_period), 2 , impact_period))%>%
  select(field, herd, harvest_cycle, impact_period)

#calculate rest period
#calc_rest<-actuals_platemeter1%>%
#  ungroup()%>%
#  filter(move_type!="agroecology")%>%
#  select(field, move_type, herd, regrowth_cycle, date)%>%
#  pivot_wider(names_from = move_type, values_from = date)%>%
 # mutate(impact_period=day_out-day_in)%>%
 # mutate(impact_period=ifelse(is.na(impact_period), 2 , impact_period))%>%
 # select(field, herd, harvest_cycle, impact_period)


#export a clean version for others. with platemeters filled in as much as possible and acreages
final_actuals_export<-left_join(actuals_platemeter1, calc_dates)

#write_csv(final_actuals_export, "actuals_platemeter_final")

ggplot(final_actuals_export, aes(date, cover, color=move_type)) +geom_point()+ facet_wrap(~field)


# filter down to just ins and outs (now filled), spread, and subtract to get total

in_out <- filter(ungroup(final_actuals_export), move_type!='agroecology')%>%
  select(field, harvest_cycle, move_type, herd, acres_impacted, cover)%>%
  pivot_wider(names_from=move_type, values_from=cover)%>%
  mutate(cowharvest=day_in-day_out, hayharvest=hay_in-hay_out)%>%
  group_by(field, harvest_cycle)%>%
  mutate(harvest=sum(cowharvest, hayharvest, na.rm=T))%>%
  mutate(harvest=ifelse(harvest<0, 0, harvest))%>%
  group_by(field)%>%
  mutate(sum=sum(harvest, na.rm=T))%>%
  mutate(harvest_acres=harvest*acres_impacted, sum_acres=sum(harvest_acres))


## THESE SHOW CUMULATIVE LIVESTOCK HARVEST BASED ON IN/OUT PLATEMETER
# there is some missing values still - those are represented as zero
ggplot(in_out, aes(reorder(field, desc(sum)), harvest, color=as.factor(harvest_cycle))) +geom_point()+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6))
ggplot(in_out, aes(reorder(field, desc(sum)), sum)) +geom_point()+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6))

# by acre
ggplot(in_out, aes(reorder(field, desc(sum_acres)), harvest_acres, color=as.factor(herd))) +geom_point()+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6))
ggplot(in_out, aes(reorder(field, desc(sum_acres)), sum_acres)) +geom_point()+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6))

#hay only (by platemeter)
ggplot(filter(in_out, herd=="hay"), aes(reorder(field, desc(sum_acres)), harvest_acres, color=as.factor(herd))) +geom_point()+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6))

# hay harvest (by bales)
hay_lbs<-hay_production%>%
  mutate(`Round bales`=ifelse(is.na(`Round bales`), 0, `Round bales`))%>%
mutate(`small rounds`=ifelse(is.na(`small rounds`), 0, `small rounds`))%>%
mutate(`Square bales`=ifelse(is.na(`Square bales`), 0, `Square bales`))%>%
  mutate(harvest=.85*(`Round bales`*750+`small rounds`*375+`Square bales`*55))

ggplot(hay_lbs, aes(reorder(`field`,desc(harvest)), harvest)) +geom_point()+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6))



