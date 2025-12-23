#herd


weights<-read_excel("all_weights.xlsx")%>%
  unique()%>%
  group_by(date, tag)%>%
  filter(weight!=0)%>%
  dplyr::summarize(weight=max(weight))%>%
  pivot_wider(names_from=tag, values_from=weight)

zeros<-read_excel("all_weights.xlsx")%>%
  unique()%>%
  group_by(date, tag)%>%
  filter(weight==0)%>%
  pivot_wider(names_from=tag, values_from=weight)



all_dates <- as.tibble(seq(from=as.Date("2025-05-15"), to=as.Date("2026-05-14"), by="day"))%>%
  rename(date=value)%>%
  full_join(weights)%>%
  arrange(date)%>%
  mutate(across(.cols=2:81, ~na.approx(.x, na.rm=F)))%>%
  bind_rows(zeros)%>%
  arrange(date)%>%
  fill(2:81, .direction="downup" )%>%
  group_by(date)%>%
  slice_tail(n=1)%>%
  ungroup()%>%
  filter(date>as.Date("2025-05-14"))%>%
  pivot_longer(names_to = "tag", values_to="weight", cols=2:81)

dmd<-all_dates%>%
  group_by(date)%>%
  dplyr::summarize(weight=sum(weight))%>%
  mutate(au=weight/1000)%>%
  mutate(dmd=au*26)%>%
  ungroup()%>%
  summarise(ad=sum(au), dmd=sum(dmd))

  


