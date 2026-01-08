################### READ ME ###################
#This script analyses and compares the 2025 grazing plan and actual record
#from Appleton Farms. Data was pulled from the maia grazing software
#and physical and digital records of the 2025 grazing season.Herd is either brood (cow-calf) or
#feeders. Fields are named by the 2025 platemetering system. Move_type indicates
#whether the cows moved into or out of a field on the associated date.
#the days column is calculated by taking the difference between a day_in
#date and a day_out date. There are only "days" values associated with 
#day_in dates.


#THIS SCRIPT ANSWERS QUESTIONS 3B AND 2B FOR 2025 GRAZING PLANNING

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

actuals_platemeter_final<-read.csv("actuals_platemeter_final")

##############################################################################
#fix names in plan and record_actual_new to be consistent with names in actuals_platemeter_final

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


record_actual_new <- record_actual_new |>
  mutate(field = case_when(
    field == "GP_woods" ~ "GP Woods",
    field == "leos" ~ "Leo's",
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
    field == "plains_wet" ~ "Plains Wet",
    field == "long_meadow" ~ "Long Meadow",
    field == "pond_field" ~ "Pond",
    TRUE ~ field
  ))

#need to assign herds to a few wonky data binds
actuals_platemeter_final <- actuals_platemeter_final %>%
  mutate(herd = if_else(
    field %in% c(
      "Underhill Wet Meadow",
      "Williams West"
    ) &
      is.na(herd),
    "feeders",
    herd
  ))

actuals_platemeter_final <- actuals_platemeter_final %>%
  mutate(herd = if_else(
    field %in% c(
      "GP1A",
      "GP6A",
      "Leo's"
      ) &
      is.na(herd),
    "brood",
    herd
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


####################################################################################
####################################################################################
#below demonstrates a  way to visualize the difference between 
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
  filter(!is.na(days))|> group_by(field, source, herd)|> 
  summarize(avg_days = mean(days))|> 
  pivot_wider(names_from = source, values_from = avg_days)|> 
  mutate(avg_difference = Actual-plan)|> 
  mutate(prop = avg_difference/plan)





#ANSWERS QUESTION 3B - length of time spent in each field, by field. 
   ggplot(
    days_df |> filter(!is.na(avg_difference)),
    aes(
      x = reorder(field, avg_difference),
      y = avg_difference,
      color = avg_difference > 0
    )
  ) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_color_manual(
      values = c("TRUE" = "darkgreen", "FALSE" = "blue"),
      labels = c("Over planned", "Under planned")
    ) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, size = 6)
    ) +
    labs(
      title = "Difference in Planned vs Actual Field Use",
      x = "Field",
      y = "Difference in Use",
      color = "Use relative to plan"
    )
  
  
#plots the same but difference is a proportion  
   ggplot(
     days_df |> filter(!is.na(prop)),
     aes(
       x = reorder(field, prop),
       y = prop,
       color = prop > 0
     )
   ) +
     geom_point(size = 1.5) +
     geom_hline(yintercept = 0, linetype = "dashed") +
     scale_color_manual(
       values = c("TRUE" = "darkgreen", "FALSE" = "blue"),
       labels = c("actual use less than planned use", "actual use greater than planned use")
     ) +
     theme(
       axis.text.x = element_text(
         angle = 90,      # vertical labels
         hjust = 1,       # right-align text
         vjust = 0.5,     # center labels vertically under bars
         size = 6
       )) +
     labs(
       title = "Proportional Difference in Planned vs Actual Field Use",
       x = "Field",
       y = "Proportional Difference",
       color = "Use relative to plan"
     )

      
   

####################################################################################################
####################################################################################################
#QUESTION 2B - TOTAL DURATION
#planned average time spent in fields overall
#for brood
overall_avg_plan_days_brood <- plan %>%
     filter(herd == "brood")%>%
  summarise(
    overall_avg_plan_days = mean(days, na.rm = TRUE)
  )
 #for feeders  
   overall_avg_plan_days_feeders <- plan %>%
     filter(herd == "feeders")%>%
     summarise(
       overall_avg_plan_days = mean(days, na.rm = TRUE)
     )

#actual average time spent in fields overall 
   #for brood
overall_avg_actual_days_brood <- filter(actuals_platemeter_final, move_type == "day_out", 
          herd == "brood", harvest_cycle_skip == "dont_skip") %>%
  summarise(
    overall_avg_actual_days = mean(impact_period, na.rm = TRUE)
  )
  #for feeders
overall_avg_actual_days_feeders <- filter(actuals_platemeter_final, move_type == "day_out",
                     herd == "feeders", harvest_cycle_skip == "dont_skip" ) %>%
  summarise(
    overall_avg_actual_days = mean(impact_period, na.rm = TRUE)
  )

#total planned days grazed for brood
total_plan_days_brood <- plan %>%
  filter(herd == "brood")%>%
  summarise(
    total_plan_days = sum(days, na.rm = TRUE)
  )

#total planned days grazed for feeders
total_plan_days_feeders <- plan %>%
  filter(herd == "feeders")%>%
  summarise(
    total_plan_days = sum(days, na.rm = TRUE)
  )


#total days grazed brood
total_days_brood <- filter(actuals_platemeter_final, move_type == "day_out", herd == "brood") %>%
  summarise(
    total_days = sum(impact_period, na.rm = TRUE)
  )

#total days grazed feeders
total_days_feeders <- filter(actuals_platemeter_final, move_type == "day_out", herd == "feeders") %>%
  summarise(
    total_days = sum(impact_period, na.rm = TRUE)
  )



  




