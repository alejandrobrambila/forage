view(record_actual)
view(record_plan)

#-----getting rid of extraneous rows and columns---------------------------------------------------
record_plan$X <- NULL
record_plan$X.1 <- NULL
record_plan$X.2 <- NULL
record_plan$X.3 <- NULL
record_plan$X.4 <- NULL
record_plan$X.5 <- NULL
record_plan$X.6 <- NULL
record_plan$paddock_plot<- NULL
record_plan <- record_plan[-c(203:223), ] 


record_actual$X <- NULL
record_actual$paddock_plot<- NULL

record_plan <- record_plan %>% 
  rename(move_type = move._type)

#----------------exporting clean record_plan and record_actual-----------
write_xlsx(record_plan, "record_plan.xlsx")

write_xlsx(record_actual, "record_actual.xlsx")


#-----------------------------comparing days plans vs actual days-----------

record_plan$field <- record_plan$field |> 
  trimws()     # removes leading/trailing spaces
 
record_actual$field <- record_actual$field |> 
  trimws()       # removes leading/trailing spaces
  

ggplot(
  filter(record_plan, move_type == "day_in", herd == "brood"),
  aes(x = field, y = days)
) +
  geom_point(size = 2) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6)
  )

ggplot(
  filter(record_actual, move_type == "day_in", herd == "brood"),
  aes(x = field, y = days)
) +
  geom_point(size = 2) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6)
  )

#--------------------------plot for brood grazing periods

df_brood_plan <- record_plan %>% 
  filter(move_type == "day_in", herd == "brood") %>%
  mutate(
    source = "Planned",
   )
  

df_brood_actual <- record_actual %>% 
  filter(move_type == "day_in", herd == "brood") %>%
  mutate(
    source = "Actual",

  )

record_brood <- bind_rows(df_brood_plan, df_brood_actual)


ggplot(record, aes(x = field, y = days, color = source)) +
  geom_point(size = 1.5, position = position_jitter(width = 0.2)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6)
  ) +
  labs(color = "Dataset",
title = "Brood Planned vs. Actual Grazing Periods",
x = "Field",
y= "Grazing Period")

#-------------plot for feeders grazing periods-----------------

df_feeders_plan <- record_plan %>% 
  filter(move_type == "day_in", herd == "feeders") %>%
  mutate(
    source = "Planned",
    paddock_plot = as.character(paddock_plot)
  )

df_feeders_actual <- record_actual %>% 
  filter(move_type == "day_in", herd == "feeders") %>%
  mutate(
    source = "Actual",
    paddock_plot = as.character(paddock_plot)
  )

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

avg_plan <- record_plan %>%
  filter(move_type == "day_in") %>%
  group_by(herd, field) %>%
  summarise(avg_days = mean(days, na.rm = TRUE), .groups = "drop") %>%
  mutate(source = "Planned")

avg_actual <- record_actual %>%
  filter(move_type == "day_in") %>%
  group_by(herd, field) %>%
  summarise(avg_days = mean(days, na.rm = TRUE), .groups = "drop") %>%
  mutate(source = "Actual")


avg_plan$field   <- as.character(avg_plan$field)
avg_actual$field <- as.character(avg_actual$field)




combined_avg <- bind_rows(avg_plan, avg_actual)


library(ggplot2)

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









