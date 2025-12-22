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

#record_actual<-read_csv("2025 record actual.csv")%>%
 # select(1, 2, 4, 5, 6)
record_actual_new<-read_csv("2025 record actual (new).csv")%>% # this version will have acreages! 
  select(1, 2, 3, 4, 5, 6, 7)

plan<-read_csv("plan.csv")

#view(record_actual)
#view(record_plan)

#-----getting rid of extraneous rows and columns---------------------------------------------------
plan$...7 <- NULL
plan$...8 <- NULL
plan$...9 <- NULL
plan$...10 <- NULL
plan$...11 <- NULL
plan$...12 <- NULL
plan$...13 <- NULL
plan$paddock_plot<- NULL
plan <- plan[-c(183:201), ] 

plan <- plan %>% 
  rename(move_type = "move _type")

plan$field <- plan$field |> 
  trimws()     # removes leading/trailing spaces

#fix names in plan to be consistent with names in actuals_platemeter_final
library(dplyr)

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


record_actual$field <- record_actual$field |> 
  trimws()    
record_actual_new$field <- record_actual_new$field |> 
  trimws()    

#----------------exporting clean record_plan and record_actual-----------
# write_xlsx(record_plan, "record_plan.xlsx")

#write_xlsx(record_actual, "record_actual.xlsx")
#write_xlsx(record_actual_new, "record_actual_new.xlsx")

#record_actual <- read_excel("record_actual.xlsx")
#record_actual_new <
record_plan  <- read_excel("record_plan.xlsx")

#the below plots take the plan and actual records and plot the "days"/grazing period
#for each cow herd, by field
#--------------------------plot for brood grazing periods-------------
#df_brood_plan and df_brood_actual take separate parts of record_plan and 
#record_actual and add a column ("source"). They are then combined in "record_brood"
#with each row assigned either planned or actual under the column source.
df_brood_plan <- plan %>% 
  filter(move_type == "day_in", herd == "brood") %>%
  mutate(
    source = "Planned",
   )
  
record_actual<-record_actual_new

df_brood_actual <- record_actual_new %>% 
  filter(move_type == "day_in", herd == "brood") %>%
  mutate(
    source = "Actual",
  )

record_brood <- bind_rows(df_brood_plan, df_brood_actual)


ggplot(record_brood, aes(x = field, y = days, color = source)) +
  geom_point(size = 1.5, position = position_jitter(width = 0.2)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6)
  ) +
  labs(color = "Dataset",
title = "Brood Planned vs. Actual Grazing Periods",
x = "Field",
y= "Grazing Period")

#-------------plot for feeders grazing periods-----------------------------
#df_feeders_plan and df_feeders_actual take separate parts of record_plan and 
#record_actual and add a column ("source"). They are then combined in "record_brood"
#with each row assigned either planned or actual under the column source.

df_feeders_plan <- plan %>% 
  filter(move_type == "day_in", herd == "feeders") %>%
  mutate(
    source = "Planned")
    
#paddock_plot = as.character(paddock_plot)
 # )

df_feeders_actual <- record_actual_new %>% 
  filter(move_type == "day_in", herd == "feeders") %>%
  mutate(
    source = "Actual")#,
   # paddock_plot = as.character(paddock_plot)
 # )

record_feeders <- bind_rows(df_feeders_plan, df_feeders_actual)


ggplot(record_feeders, aes(x = field, y = days, color = source)) +
  geom_point(size = 1.5, position = position_jitter(width = 0.2)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6)
  ) +
  labs(color = "Dataset",
       title = "Feeders Planned vs. Actual Grazing Periods",
       x = "Field",
       y= "Grazing Period")


#----now graphing the average grazing period per field across the season-----
#the below code takes the average number of days/grazing period per field 
#grouped by herd for record_plan and record_actual. The df's are then combined into
#combined_avg and plotted 
library(dplyr)

library(dplyr)

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

avg_plan <- plan %>%
  filter(move_type == "day_in") %>%
  group_by(herd, field) %>%
  summarise(avg_days = mean(days, na.rm = TRUE), .groups = "drop") %>%
  mutate(source = "Planned")

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



#Moving on from grazing periods, now we will count the 
#number of times a field was grazed

#counts the number of times a field was planned to be
#grazed throughout the whole season
filter(plan, move_type == "day_in")|>
  count(field)

#counts the number of times a field was actually grazed throughout the whole
#season
filter(record_actual, move_type == "day_in")|>
  count(field)

#This plot shows the planned number of times a field was grazed for brood
ggplot(
  filter(plan, move_type == "day_in", herd == "brood")|>
    count(field)%>%
    mutate(field = reorder(field, n)),
  aes(x = field, y = n)
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
  filter(record_actual, move_type == "day_in", herd == "brood")|>
    count(field)%>%
    mutate(field = reorder(field, n)),
  aes(x = field, y = n)
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
  aes(x = field, y = n)
) +
  geom_col()+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6)
  )

#This plot shows the actual number of times a field was grazed for feeders
ggplot(
  filter(record_actual, move_type == "day_in", herd == "feeders")|>
    count(field)%>%
    mutate(field = reorder(field, n)),
  aes(x = field, y = n)
) +
  geom_col()+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6)
  )

####################################################################################
#below demonstrates a different way to visualize the difference between 
#the average planned days per field and the average actual days per field

#plan_days and actual_days eliminates move_type>day_out dates (they are redundant
#due to calculated "days column) and assigned a source column (plan or actual)
plan_days <- plan|>
  filter(move_type == "day_in")|>
  mutate(source="plan")

actual_days <- record_actual|>
  filter(move_type == "day_in")|>
  mutate(source="actual")

#"days" first combines plan_days and actual_days. Then pivot_wider
#creates two new columns ("plan" and "actual") from the source column
#It also creates a third new column, "avg_difference" which takes avg day values
#from plan and actual columns.
#mutate(prop = avg_difference/plan) displays values that allow us to see by what proportion
#the difference in planned vs actual days is.
days<-rbind(plan_days, actual_days)|>
  select(-date)|>
  filter(!is.na(days))|>
  group_by(field, herd, source)|>
  summarize(avg_days = mean(days))|>
  pivot_wider(names_from = source,
              values_from = avg_days)|>
  mutate(avg_difference = actual-plan)|>
  mutate(prop = avg_difference/plan)

#answers question 2B 
ggplot(days, aes(x=field, y=prop))+
  geom_point()+
  geom_hline(yintercept = 0)+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6)
  )+
  labs(
    title= "Difference in planned versus actual use of fields",
    x = "Field",
    y = "Proportional Difference"
  )

  
  

###############################
avg_plan <- plan %>%
  filter(move_type == "day_in") %>%
  group_by(herd, field) %>%
  summarise(avg_days = mean(days, na.rm = TRUE), .groups = "drop") %>%
  mutate(source = "Planned")

avg_actual1 <- actuals_platemeter_final %>%
  filter(move_type == "day_out", harvest_cycle_skip == "dont_skip" ) %>%
  group_by(herd, field) %>%
  summarise(avg_days1 = mean(impact_period, na.rm = TRUE), .groups = "drop")




