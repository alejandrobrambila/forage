library(tidyverse, ggpubr)
calibration<-read_csv("platemeter_calibration_2026.csv")

ggplot(calibration, aes(platemeter_reading, kg_ha)) +geom_point(aes(color=Field))+
  geom_smooth(method="lm", formula = y ~ x, se=F)+
 # geom_smooth(method="lm", formula = y ~ x, se=T, aes(color=Field))
  stat_regline_equation(label.x = 3, label.y = 30)+
  geom_abline(intercept=500, slope=140, color="black")+
  annotate("text", x=10, y=4000, label = "y = 140 x + 500")+
  xlab("Platemeter Height (cm)")+ylab("Measured Kg/Ha")
