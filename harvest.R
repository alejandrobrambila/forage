#this script is to answer QUESTION 3C for 2025 grazing planning
#total forage harvested per field for grazing and haying events

library(tidyverse)
library(dplyr)

actuals_platemeter_final <-read.csv ("actuals_platemeter_final")

#calculating total harvested for grazes
cover_change_df <- actuals_platemeter_final |>
  filter(move_type %in% c("day_in", "day_out")) |>
  select(field, harvest_cycle, move_type, cover, acres, acres_impacted) |>
  pivot_wider(
    names_from = move_type,
    values_from = cover,
    values_fn = mean  
  ) |>
  mutate(
    day_in  = as.numeric(day_in),
    day_out = as.numeric(day_out),
    cover_change = day_in - day_out
  )


#take average harvests per field
avg_harvest <- cover_change_df |>
  group_by(field, acres, acres_impacted) |>
  summarize(
    avg_cover_change = mean(cover_change, na.rm = TRUE),
    .groups = "drop"
  )

#calculating avgerage acres impacted by field
avg_harvest <- avg_harvest |>
  group_by(field) |>
  summarize(
    avg_acres = mean(acres_impacted, na.rm = TRUE),
    avg_cover_change = mean(avg_cover_change, na.rm = TRUE),
    .groups = "drop"
  )



#plot average
ggplot(
    avg_harvest |> 
      filter(!is.na(avg_cover_change)), 
    aes(
      x = reorder(field, avg_cover_change), 
      y = avg_cover_change,
    )
  ) +
    geom_point()+
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, size = 6)
    ) +
  labs(
    x= "Field",
    y="lbs per acres",
    title = "Forage Harvested by Grazes"
  )


#####################
#now look at hay harvested
hay_change <- actuals_platemeter_final |>
  filter(move_type %in% c("hay_in", "hay_out")) |>
  select(field, harvest_cycle, move_type, cover, acres, acres_impacted) |>
  pivot_wider(
    names_from = move_type,
    values_from = cover,
    values_fn = mean  
  ) |>
  mutate(
    day_in  = as.numeric(hay_in),
    day_out = as.numeric(hay_out),
    cover_change = day_in - day_out
  )

#average hay harvest
avg_hay_harvest <- hay_change |>
  group_by(field, acres_impacted) |>
  summarize(
    avg_cover_change = mean(cover_change, na.rm = TRUE),
    .groups = "drop"
  )

#calculating avgerage acres impacted by field
avg_hay_harvest <- avg_hay_harvest |>
  group_by(field) |>
  summarize(
    avg_acres = mean(acres_impacted, na.rm = TRUE),
    avg_cover_change = mean(avg_cover_change, na.rm = TRUE),
    .groups = "drop"
  )

#plotting
ggplot(
  avg_hay_harvest |> 
    filter(!is.na(avg_cover_change)), 
  aes(
    x = reorder(field, avg_cover_change), 
    y = avg_cover_change,
  )
) +
  geom_point()+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6)
  ) +
  labs(
    x= "Field",
    y="lbs per acres",
    title = "Forage Harvested by Hays"
  )










  