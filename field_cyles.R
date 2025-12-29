#this script is to answer 3A

library(tidyverse)
library(dplyr)
library(ggplot2)

#reading in script
actuals_platemeter_final<-read.csv("actuals_platemeter_final")

#counting harvest cycles according to <50% acreage exclusion rule
harvest_cycles_by_field <- actuals_platemeter_final |>
  filter(harvest_cycle_skip == "dont_skip") |>
  group_by(field) |>
  summarize(
    total_harvest_cycles = n_distinct(harvest_cycle),
    .groups = "drop"
  )


#plot harvest cycles by field
ggplot(
  harvest_cycles_by_field,
  aes(
    x = reorder(field, total_harvest_cycles),
    y = total_harvest_cycles,
    fill = total_harvest_cycles
  )
) +
  geom_col() +
  scale_fill_gradient(
    low = "lightblue",
    high = "darkgreen"
  ) +
  labs(
    x = "Field",
    y = "Total Harvest Cycles",
    fill = "Harvest Cycles",
    title = "Cycles per Field"
  ) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 7)
  )+
  guides(fill = "none")








