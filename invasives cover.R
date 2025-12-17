###############READ ME######################
#this script deals with data from the 2025 vegetation survey done at
#Appleton Farms in 2025. Each field was surveyed for % cover of invasive
#forbs, grasses, and woody plants. 

#reading in and cleaning data
invasives_by_field <- read.csv("invasives_by_field.csv")
invasives_by_field$X <- NULL
invasives_by_field <- invasives_by_field[rowSums(is.na(invasives_by_field) | invasives_by_field == "") < ncol(invasives_by_field), ]


#below graphs the average invasive GRASS by field from least to greatest
#percent (zeros removed)
#filter gets rid of fields with 0%
#reorder() sorts the fields from least to greatest percent
ggplot(
  invasives_by_field %>%
    filter(avg_invasive_grass > 0) %>%
    mutate(field = reorder(field, avg_invasive_grass)),
  aes(x = field, y = avg_invasive_grass)
) +
  geom_col() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6)
  ) +
  labs(
    x = "Field",
    y = "Average Invasive Grass (%)"
  )


#below graphs the average invasive FORBS by field from least to greatest
#percent, with zeros removed.
#filter gets rid of fields with 0%
#reorder() sorts the fields from least to greatest percent
ggplot(
  invasives_by_field %>%
    filter(avg_invasive_forb > 0) %>%
    mutate(field = reorder(field, avg_invasive_forb)),
  aes(x = field, y = avg_invasive_forb)
) +
  geom_col() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6)
  ) +
  labs(
    x = "Field",
    y = "Average Invasive Forb (%)"
  )

#below graphs the average invasive WOODIES by field from least to greatest
#percent, with zeros removed.
#filter gets rid of fields with 0%
#reorder() sorts the fields from least to greatest percent
ggplot(
  invasives_by_field %>%
    filter(avg_invasive_woody > 0) %>%
    mutate(field = reorder(field, avg_invasive_woody)),
  aes(x = field, y = avg_invasive_woody)
) +
  geom_col() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6)
  ) +
  labs(
    x = "Field",
    y = "Average Invasive Woody (%)"
  )



       
       