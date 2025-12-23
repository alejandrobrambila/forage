#############################READ ME#############################
#this script is to examine best management practices for 2025



#Did we stick to the rule of spending <3 days per acre?

actuals_platemeter_final <-read.csv ("actuals_platemeter_final")

#create a new column in actuals_platemeter_final to calculate how many days we
#spent per acre
actuals_platemeter_final <- actuals_platemeter_final %>%
  mutate(days_per_acre = impact_period / acres_impacted)


#plotting days per acre
ggplot(actuals_platemeter_final, aes(x=field, y=days_per_acre))+
  geom_point()+
  geom_hline(yintercept = 3) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6)
  ) +
  labs(
    x = "Field",
    y = "Days per Acre"
  )

#now create a new column to look at acres/break. One break = 3 days
#acres/days x 3days/break
actuals_platemeter_final <- actuals_platemeter_final %>%
  mutate(acres_per_break = acres_impacted/impact_period * 3)

#plotting acres_per_break
ggplot(actuals_platemeter_final, aes(x=field, y=acres_per_break))+
  geom_point()+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6)
  ) +
  labs(
    x = "Field",
    y = "Acres per Break"
  )

