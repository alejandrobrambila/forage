#compaction
library(tidyverse)
compaction<-read_csv("penetrometer_2026.csv")%>%
  mutate(drainage=factor(drainage, levels=c("very poorly drained", "poorly drained", "well drained", "somewhat excessively drained", "excessively well drained")))

# Calculate both unique field names AND the local max value per drainage class

label_data <- compaction %>%
  filter(!is.na(drainage), !is.na(field)) %>%
  group_by(drainage) %>%
  summarize(
    # Stacks names vertically by separating them with a newline break
    field_labels = str_flatten(unique(field), collapse = "\n"),
    local_max = max(moisture_12, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(compaction, aes(drainage, moisture_12)) +geom_boxplot() + geom_jitter(width=.2, size=.5)+
  xlab("Field Drainage Class") + ylab("%VWC 12cm")+
  geom_text(
    data = label_data, 
    aes(y = local_max, label = field_labels),  
    vjust = -0.2,   # Positions the bottom of the text block slightly above the max point
    lineheight = 0.9, # Tightens the spacing between stacked names
    size = 3.5
  ) +
  coord_cartesian(clip = "off")

# Calculate positions for EACH unique field name separately to allow distinct coloring
label_data2 <- compaction %>%
  filter(!is.na(drainage), !is.na(field)) %>%
  group_by(drainage) %>%
  mutate(base_max = max(moisture_12, na.rm = TRUE)) %>%
  distinct(drainage, field, .keep_all = TRUE) %>%
  arrange(field) %>%
  mutate(
    row_num = row_number(),
    # --- ADJUST GAP SIZES HERE ---
    init_gap = 2.0,      # Clear spacing between the boxplot whisker and the first text label
    stack_gap = 1.5,     # Vertical spacing between the stacked words themselves
    # -----------------------------
    y_pos = base_max + init_gap + ((row_num - 1) * stack_gap)
  ) %>%
  ungroup()

ggplot(compaction, aes(x = drainage, y = moisture_12)) +
  # Standard black/grey boxplot lines and points
  geom_boxplot(color = "grey30", outlier.color = "grey50") +
  # Individual text labels colored by field name
  geom_text(
    data = label_data2, 
    aes(y = y_pos, label = field, color = field),
    vjust = 0,
    size = 3.5,show.legend = FALSE ,
    fontface = "bold"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  labs(color = "Field Names")+
  geom_jitter(
    aes(color = field), 
    width = 0.25,      # Spread points slightly horizontally so they don't overlap
    size = 1, show.legend = FALSE 
  ) +xlab("Drainage Class")+ylab("% VWC 12cm")


compaction %>%
  # 1. Select the relevant columns and pivot the two compaction readings into one column
  select(field, drainage, moisture_12, compaction_6, compaction_18) %>%
  pivot_longer(
    cols = c(compaction_6, compaction_18),
    names_to = "compaction_depth",
    values_to = "compaction_reading"
  ) %>%
  # Clean up the labels for the plot facets (optional but looks nicer)
  mutate(compaction_depth = recode(compaction_depth, 
                                   "compaction_6" = "Compaction at 6 Inches", 
                                   "compaction_18" = "Compaction at 18 Inches")) %>%
  # Filter out missing values so the trend lines calculate correctly
  filter(!is.na(moisture_12), !is.na(compaction_reading)) %>%
  
  # 2. Build the scatter plot
  ggplot(aes(x = moisture_12, y = compaction_reading)) +
  geom_point(aes(color = field), alpha = 0.6, size = 2, show.legend = FALSE) +
  # Adds a linear trend line to easily visualize the relationship direction
  geom_smooth(method = "lm", color = "black", linetype = "dashed", se = T) + 
  # Splits the plot into two panels: one for 6" and one for 18"
  facet_wrap(~compaction_depth, scales = "free_y") +
  theme_minimal() +
  labs(
    x = "Moisture (12 inches)",
    y = "Compaction Reading",
    title = "Relationship Between Moisture and Compaction Depth"
  )


compaction %>%
  select(field, drainage, moisture_12, compaction_6, compaction_18) %>%
  pivot_longer(
    cols = c(compaction_6, compaction_18),
    names_to = "compaction_depth",
    values_to = "compaction_reading"
  ) %>%
  mutate(compaction_depth = recode(compaction_depth, 
                                   "compaction_6" = "Compaction at 6 Inches", 
                                   "compaction_18" = "Compaction at 18 Inches")) %>%
  filter(!is.na(moisture_12), !is.na(compaction_reading)) %>%
  
  ggplot(aes(x = moisture_12, y = compaction_reading)) +
  geom_point(aes(color = drainage), size = 1.5, show.legend = T) +
  geom_smooth(method = "lm", color = "black", linetype = "dashed", se = FALSE) + 
  
  # Adds the R2 value to each facet panel automatically
  stat_cor(
    aes(label = ..rr.label..), # Specifies that we only want to display R^2
    label.x.npc = "middle",      # Aligns the text to the right side of the panel
    label.y.npc = "top",        # Aligns the text to the top of the panel
    size = 4,
    color = "black"
  ) +
  
  facet_wrap(~compaction_depth, scales = "free_y") +
  theme_minimal() +
  labs(
    x = "Moisture (12 cm)",
    y = "Compaction Reading",
    title = "Relationship Between Moisture and Compaction Depth"
  )
