#this script is to answer 3A

library(tidyverse)
library(dplyr)
library(ggplot2)
library(forcats)
library(ggpattern)

#reading in script
actuals_platemeter_final<-read.csv("actuals_platemeter_final")


#counting harvest cycles according to <50% acreage exclusion rule
harvest_cycles_by_field <- actuals_platemeter_final |>
  filter(harvest_cycle_skip == "dont_skip") |>
  group_by(field, herd) |>
  summarize(
    total_harvest_cycles = n_distinct(harvest_cycle),
    .groups = "drop"
  )


#plot harvest cycles by field, coloring bars by herd

ggplot(
  harvest_cycles_by_field %>%
    filter(!is.na(herd)),
  aes(
    x = fct_reorder(field, total_harvest_cycles, .fun = sum),
    y = total_harvest_cycles,
    fill = herd
  )
) +
  geom_col() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5,
      size = 7
    )
  ) +
  labs(
    x = "Field",
    y = "Total Harvest Cycles",
    title = "Harvest Cycles by Field"
  )
