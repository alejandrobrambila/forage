### field performance
### Question 4: How did fields respond to use (Alejandro wrote this script)
### with reference to the past and outside references
## 4a: Total productivity
### 4b: Time to recovery/speed of recovery
### 4c: max volume/total harvest
## 4d: forage quality (hay and forage)

#source data ----

# import record of moves for both herd, as well as hay and mow events.  exported in 'plan_v_actual.R' (Saskia)
record_actual <- read_excel("record_actual.xlsx")  %>%
  mutate(date=mdy(date))%>%
  mutate(field=tolower(field))%>%
  mutate(field=ifelse(field=="pond_field", "pond", field))

# import all platemeter data. exported from "platemeterimport.R" (Saskia)
all_platemeter <- read_excel("all_platemeter.xlsx") %>%
  mutate(date=ymd(date))%>%
  mutate(field=tolower(field))

# import all hay data
hay_production <-read_excel("2025 Hay Production.xlsx") %>%
  filter(!is.na(Field_name))
hay_quality <-read_excel("Dairy One Results.xlsx") 


#4a What was total productivity of each field. ----
# THIS IS THE SAME QUESTION AS 3C!!!

actuals_platemeter<-all_platemeter%>%
  select(-paddock)%>%
  full_join(record_actual)


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
hay_dates<-select(hay_production, Field_name, `Cut date`)%>%
  mutate(field=case_when(
    Field_name == "Briar" ~ "briar",
    Field_name == "Broad Meadow 1A" ~ "broad_meadow",
    Field_name == "Broad Meadows CSA" ~ "broad_meadow",
    Field_name == "Jimmy's" ~ "jimmys",
    Field_name == "Lamson Field Middle" ~ "lamson_middle",
    Field_name == "Lamson North" ~ "lamson_north",
    Field_name == "Leos" ~ "leos",
    Field_name == "Lower Sunset" ~ "lower_sunset",
    Field_name == "Plains" ~ "plains",
    Field_name == "Playground" ~ "playground",
    Field_name == "POW East" ~ "pow_east",
    Field_name == "Triangle Upper" ~ "triangle_upper",
    Field_name == "Underhill Veg Perimeter" ~ "underhill_veg",
    Field_name == "Underhill Wet" ~ "underhill_wet",
    Field_name == "Leahs" ~ "white_cottage",
    Field_name == "Williams 1A" ~ "williams 1a",
    Field_name == "Williams West" ~ "williams_west",
    Field_name == "Wilson's" ~ "wilson"))%>%
  select(-Field_name)%>%
  mutate(filled=NA, date=`Cut date`)%>%
  select(-1)%>%
  mutate(move_type="hay")


actuals_platemeter1<-bind_rows(ap_arranged_ins, ap_arranged_outs, hay_dates)%>%
  arrange(field, date)%>%
  group_by(field, move_type)%>%
  mutate(hay_cycle=row_number())%>%
  mutate(hay_cycle=ifelse(move_type=="hay", hay_cycle, NA))%>%
  group_by(field)%>%
  fill(hay_cycle)%>%
  mutate(hay_cycle=ifelse(is.na(hay_cycle), 0, hay_cycle))%>%
  group_by(field, move_type)%>%
  mutate(graze_cycle=row_number())%>%
  mutate(graze_cycle=ifelse(move_type=="agroecology", NA, graze_cycle))%>%
  mutate(graze_cycle=ifelse(move_type=="hay", NA, graze_cycle))%>%
  group_by(field)%>%
  fill(graze_cycle)%>% #this cycle column resets when move in happens, and the subsequent move out and following AE readings are on that cycle
  mutate(graze_cycle=ifelse(is.na(graze_cycle), 0, graze_cycle))%>%
  mutate(harvest_cycle=graze_cycle+hay_cycle)%>%
  mutate(int_cycle=ifelse(move_type=="day_in"|move_type=="hay", harvest_cycle-1, harvest_cycle))%>%
  mutate(int_cycle=ifelse(move_type=="agroecology", NA, int_cycle))%>%
  group_by(field)%>%
  fill(int_cycle, .direction="up")%>% #this cycle resets on the move out.  it will allow interpolation per field per cycle to estimate what the forage would have been
  fill(int_cycle)
  

ggplot(actuals_platemeter, aes(date, cover, color=move_type)) +geom_point()+ facet_wrap(~field)
ggplot(actuals_platemeter1, aes(date, filled, color=move_type)) +geom_point()+ facet_wrap(~field)


# filter down to just ins and outs (now filled), spread, and subtract to get total

in_out <- filter(actuals_platemeter1, move_type!='agroecology')%>%
  select(-cover, -date, -days, -interpolated)%>%
  pivot_wider(names_from=move_type, values_from=filled)%>%
  mutate(harvest=day_in-day_out)%>%
  mutate(harvest=ifelse(harvest<0, 0, harvest))%>%
  group_by(field)%>%
  mutate(sum=sum(harvest, na.rm=T))


## THESE SHOW CUMULATIVE LIVESTOCK HARVEST BASED ON IN/OUT PLATEMETER
# there is some missing values still - those are represented as zero
ggplot(in_out, aes(reorder(field, desc(sum)), harvest, color=as.factor(cycle))) +geom_point()
ggplot(in_out, aes(reorder(field, desc(sum)), sum)) +geom_point()


# hay harvest (by bales)
hay_lbs<-hay_production%>%
  mutate(`Round bales`=ifelse(is.na(`Round bales`), 0, `Round bales`))%>%
mutate(`small rounds`=ifelse(is.na(`small rounds`), 0, `small rounds`))%>%
mutate(`Square bales`=ifelse(is.na(`Square bales`), 0, `Square bales`))%>%
  mutate(harvest=.85*(`Round bales`*750+`small rounds`*375+`Square bales`*55))

ggplot(hay_lbs, aes(reorder(`Field_name`,desc(harvest)), harvest)) +geom_point()


