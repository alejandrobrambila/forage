################### READ ME ###################
#This script analyses and compares the 2025 grazing plan and actual record
#from Appleton Farms. Data was pulled from the maia grazing software
#and physical and digital records of the 2025 grazing season.Herd is either brood (cow-calf) or
#feeders. Fields are named by the 2025 platemetering system. Move_type indicates
#whether the cows moved into or out of a field on the associated date.
#the days column is calculated by taking the difference between a day_in
#date and a day_out date. There are only "days" values associated with 
#day_in dates.
library(tidyverse)
library(readxl)

#reading in and cleaning up data

record_actual_new<-read_csv("2025 record actual (new).csv")%>% # this version will have acreages! 
  select(1, 2, 3, 4, 5, 6, 7)

plan<-read_csv("plan.csv")%>%
  select(-7,-8,-9,-10,-11,-12,-13, -paddock_plot)

plan <- plan[-c(183:201), ]

plan <- plan %>% 
  rename(move_type = "move _type")

plan$field <- plan$field |> 
  trimws()     # removes leading/trailing spaces

record_actual_new$field <- record_actual_new$field |> 
  trimws()  

##############################################################################
#fix names in plan to be consistent with names in actuals_platemeter_final

plan <- plan |>
  mutate(field = case_when(
    field == "GP_woods" ~ "GP Woods",
    field == "leos" ~ "Leos",
    field == "playground" ~ "Playground",
    field == "sunset_field" ~ "Sunset Field",
    field == "sunset_hill" ~ "Sunset Hill",
    field == "lower_sunset" ~ "Lower Sunset",
    field == "horse" ~ "Horse",
    field == "lower_home" ~ "Lower Home",
    field == "pow_east" ~ "POW East",
    field == "triangle_upper" ~ "Upper Triangle",
    field == "triangle_lower" ~ "Lower Triangle",
    field == "lower_barberry" ~ "Lower Barberry",
    field == "lower_underhill" ~ "Lower Underhill",
    field == "barberry_woods" ~ "Barberry Woods",
    field == "underhill_wet" ~ "Underhill Wet Meadow",
    field == "williams_west" ~ "Williams West",
    field == "Leos" ~ "Leo's",
    field == "GP1B" ~ "GP1B",
    field == "GP1" ~ "GP1A",
    field == "GP6" ~ "GP6A",
    field == "jimmys" ~ "Jimmy's",
    field == "briar" ~ "Briar",
    field == "bull"~ "Bull",
    field == "wilson" ~ "Wilson",
    field == "wilson_wet" ~ "Wilson Wet",
    field == "railroad" ~ "Railroad",
    field == "drainage" ~ "Drainage",
    field == "underhill" ~ "Underhill",
    TRUE ~ field
  ))

####################################################################################
#----------------exporting clean record_plan and record_actual-----------
# write_xlsx(record_plan, "record_plan.xlsx")

#write_xlsx(record_actual, "record_actual.xlsx")
#write_xlsx(record_actual_new, "record_actual_new.xlsx")

#record_actual <- read_excel("record_actual.xlsx")
#record_actual_new <
#record_plan  <- read_excel("record_plan.xlsx")
#####################################################################################



#----graphing the average grazing period per field across the season-----
#the below code takes the average number of days/grazing period per field 
#grouped by herd for record_plan and record_actual. The df's are then combined into
#combined_avg and plotted 

actuals_platemeter_final <-read.csv("actuals_platemeter_final")

#assigning a herd to Leo's, GP1A, GP6A
actuals_platemeter_final <- actuals_platemeter_final %>%
  mutate(herd = if_else(
    field %in% c(
      "Leo's",
      "GP1A",
      "GP6A"
    ) &
      move_type %in% c("day_in", "day_out"),
    "brood",
    herd
  ))

#assigning a herd to Underhill wet meadow, williams west
actuals_platemeter_final <- actuals_platemeter_final %>%
  mutate(herd = if_else(
    field %in% c(
      "Underhill Wet Meadow",
      "Williams West" ) &
      move_type %in% c("day_in", "day_out") &
      is.na(herd),
    "feeders",
    herd
  ))

#create df avg_plan which gives average planned days spent per field
#and assigns a source of "Planned"
avg_plan <- plan %>%
  filter(move_type == "day_in") %>%
  group_by(herd, field) %>%
  summarise(avg_days = mean(days, na.rm = TRUE), .groups = "drop") %>%
  mutate(source = "Planned")

#create df avg_actual which gives the average days spent per field
#and assigns a source of "Actual"
avg_actual <- actuals_platemeter_final %>%
  filter(move_type == "day_out", harvest_cycle_skip == "dont_skip" ) %>%
  group_by(herd, field) %>%
  summarise(avg_days = mean(impact_period, na.rm = TRUE), .groups = "drop")%>%
  mutate(source = "Actual")

#making data types consistent
avg_plan$field   <- as.character(avg_plan$field)
avg_actual$field <- as.character(avg_actual$field)

#combining new dfs back together
combined_avg <- bind_rows(avg_plan, avg_actual)

#plotting the averages in bar chart format
ggplot(combined_avg, aes(x = field, y = avg_days, fill = source)) +
  geom_col(position = "dodge") +
  facet_wrap(~herd, scales = "free_x") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6)
  ) +
  labs(
    title = "Planned vs Actual Average Days per Field",
    x = "Field",
    y = "Average Days",
    fill = "Dataset"
  )


#plotting the difference in average planned days and average actual days 

ggplot(combined_avg, aes(x = field, y = avg_days, color = source)) +
  geom_point(size = 1) +
  geom_line(aes(group = field), alpha = 0.4) +
  facet_wrap(~herd, scales = "free_x") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6)
  ) +
  labs(
    title = "Planned vs Actual Average Days per Field",
    x = "Field",
    y = "Average Days",
    color = "Dataset"
  )


###################################################################################
#QUESTION 3A
#Moving on from grazing periods, now we will count the
#number of times a field was grazed

#counts the number of times a field was planned to be
#grazed throughout the whole season
filter(plan, move_type == "day_in")|>
  count(field)

#counts the number of times a field was actually grazed throughout the whole
#season
filter(record_actual_new, move_type == "day_in")|>
  count(field)

#This plot shows the planned number of times a field was grazed for brood
ggplot(
  filter(plan, move_type == "day_in", herd == "brood")|>
    count(field)%>%
    mutate(field = reorder(field, n)),
  aes(x = field, y = n, fill=n)
) +
 geom_col()+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6)
  )+
  labs(
    x = "Field",
    y = "Number of Times Grazed",
    title = "Planned Number of Grazes by Field - Brood"
  )

#This plot shows the actual number of times a field was grazed for brood

ggplot(
  filter(record_actual_new, move_type == "day_in", herd == "brood")|>
    count(field)%>%
    mutate(field = reorder(field, n)),
  aes(x = field, y = n, fill=n)
) +
  geom_col() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6)
  ) +
  labs(
    x = "Field",
    y = "Number of Times Grazed",
    title = "Actual Number of Grazes by Field - Brood")


#This plot shows the planned number of times a field was grazed for feeders
ggplot(
  filter(plan, move_type == "day_in", herd == "feeders")|>
    count(field)%>%
    mutate(field = reorder(field, n)),
  aes(x = field, y = n, fill=n)
) +
  geom_col()+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6)
  )

#This plot shows the actual number of times a field was grazed for feeders
ggplot(
  filter(record_actual_new, move_type == "day_in", herd == "feeders")|>
    count(field)%>%
    mutate(field = reorder(field, n)),
  aes(x = field, y = n, fill=n)
) +
  geom_col()+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6)
  )

####################################################################################
####################################################################################
#below demonstrates a different way to visualize the difference between 
#the average planned days per field and the average actual days per field

#plan_days and actual_days eliminates move_type>day_out dates (they are redundant
#due to calculated "days column) and assigned a source column (plan or actual)
plan_days <- plan|>
  filter(move_type == "day_in")|>
  mutate(source="plan")


actual_days1 <- actuals_platemeter_final %>%
  filter(move_type == "day_out", harvest_cycle_skip == "dont_skip" ) %>%
  group_by(herd, field) %>%
  mutate(source = "Actual")

#"days" first combines plan_days and actual_days. Then pivot_wider
#creates two new columns ("plan" and "actual") from the source column
#It also creates a third new column, "avg_difference" which takes avg day values
#from plan and actual columns.
#mutate(prop = avg_difference/plan) displays values that allow us to see by what proportion
#the difference in planned vs actual days is.

#making the columns in actual_days1 and plan_days the same for rbind
actual_days1 <- actual_days1 |>
  select(-date, -acres,-acres_impacted,-frac_acres,-frac_max_acres,-hay_cycle,
         -graze_cycle,-harvest_cycle, -harvest_cycle_skip, -regrowth_cycle,
         -cover)
actual_days1 <- actual_days1 %>% 
  rename(days = "impact_period")

plan_days <- plan_days|>
  select(-date)

#creating "days" df
days_df<-rbind(plan_days, actual_days1)|>
  filter(!is.na(days))|>
  group_by(field, herd, source)|>
  summarize(avg_days = mean(days))|>
  pivot_wider(names_from = source,
              values_from = avg_days)|>
  mutate(avg_difference = Actual-plan)|>
  mutate(prop = avg_difference/plan)


#ANSWERS QUESTION 3B - length of time spent in each field, by field. 
  ggplot(
    days_df |> filter(!is.na(avg_difference)),
    aes(x = field, y = avg_difference)
  ) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6)
  ) +
  labs(
    title = "Difference in Field Use",
    x = "Field",
    y = "Average Difference"
  )
  
  
#plots the same but difference is a proportion  
ggplot(
  days_df |> filter(!is.na(prop)),
  aes(x = field, y = prop)
) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6)
  ) +
  labs(
    title = "Difference in Field Use",
    x = "Field",
    y = "Proportional Difference"
  )


####################################################################################################
####################################################################################################
#QUESTION 2B - TOTAL DURATION
#planned average time spent in fields overall

overall_avg_plan_days <- plan %>%
  summarise(
    overall_avg_plan_days = mean(days, na.rm = TRUE)
  )

#actual average time spent in fields overall 
overall_avg_actual_days <- filter(actuals_platemeter_final, move_type == "day_out") %>%
  summarise(
    overall_avg_actual_days = mean(impact_period, na.rm = TRUE)
  )

#total planned days grazed
total_plan_days <- plan %>%
  summarise(
    total_plan_days = sum(days, na.rm = TRUE)
  )

#total days grazed 
total_days <- filter(actuals_platemeter_final, move_type == "day_out") %>%
  summarise(
    total_days = sum(impact_period, na.rm = TRUE)
  )

  




