#this script is to answer QUESTION 3C

library(tidyverse)
library(dplyr)

actuals_platemeter_final <-read.csv ("actuals_platemeter_final")

#calculating total harvested for grazes
cover_change_df <- actuals_platemeter_final |>
  filter(move_type %in% c("day_in", "day_out")) |>
  select(field, harvest_cycle, move_type, cover, acres) |>
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
  group_by(field, acres) |>
  summarize(
    avg_cover_change = mean(cover_change, na.rm = TRUE),
    .groups = "drop"
  )

#calculating lbs_per_acre
#how to use acres impacted to have an accurate lbs_per_acre???
avg_harvest_acres <- avg_harvest |>
  mutate(
    lbs_per_acre = avg_cover_change / acres
  )


#plot average

ggplot(
    avg_harvest_acres |> 
      filter(!is.na(lbs_per_acre)), 
    aes(
      x = reorder(field, lbs_per_acre), 
      y = lbs_per_acre,
    )
  ) +
    geom_point()+
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, size = 6)
    ) 
  