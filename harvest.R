#this script is to answer QUESTION 1C for 2025 grazing planning
#total forage harvested per field for grazing and haying events

library(tidyverse)
library(dplyr)

actuals_platemeter_final <-read.csv ("actuals_platemeter_final")

#calculating total harvested for grazes
cover_change_df <- actuals_platemeter_final |>
  filter(move_type %in% c("day_in", "day_out", "hay_in", "hay_out")) |>
  select(field, herd, harvest_cycle, move_type, cover, acres, acres_impacted) |>
  pivot_wider(
    names_from = move_type,
    values_from = cover,
    values_fn = mean  
  ) |>
  mutate(
    cover_change = coalesce(
      day_in - day_out,
      hay_in - hay_out
    )
  )



###########################################################
#calculating field utlization (day_out platemeter / day_in platemeter = % of forage utilized)
#question 1C
cover_change_df<-cover_change_df|>
  mutate(
    utilization = (cover_change/coalesce(day_in, hay_in))
  )

#plotting utilization 
ggplot(cover_change_df |> 
         filter(!is.na(utilization)), aes(
  x=reorder(field, utilization), y=utilization, color=herd))+
         geom_point()+
         theme(
           axis.text.x = element_text(angle = 90, hjust = 1, size = 6)
         ) +
   geom_hline(yintercept = 0.5)+
  labs( x = "Field",
       y= "% Utilization",
       title = "Percent Utilization by Graze"
       )




############################
#looking at residuals (question 1C)

ggplot(
  cover_change_df |> 
    dplyr::filter(!is.na(coalesce(day_out, hay_out)), !is.na(herd)), 
  aes(
    x = reorder(field, coalesce(day_out, hay_out)), 
    y = coalesce(day_out, hay_out),
    color = herd
  )
) +
  geom_point() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6)
  ) +
  labs(
    x = "Field",
    y = "lbs DM",
    title = "Residuals"
  )








  