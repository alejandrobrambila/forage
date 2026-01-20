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

classes<-read_excel("all_weights.xlsx")%>%
  group_by(tag)%>%
  filter(weight!=0)%>%
  dplyr::summarize(weight=mean(weight))%>%
  mutate(class=ifelse(startsWith(tag, "25"), "calf", NA))%>%
  mutate(class=ifelse(startsWith(tag, "24"), "feeder", class))%>%
  mutate(class=ifelse(startsWith(tag, "23"), "feeder", class))%>%
  mutate(class=ifelse(weight>1000, "cow", class))%>%
  mutate(class=ifelse(is.na(class), "cow", class))%>%
  select(tag, class)
  


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
  mutate(dmd=au*30)%>%
  ungroup()%>%
  summarise(ad=sum(au), dmd=sum(dmd))

dmd_herd<-left_join(all_dates, classes)%>%
  group_by(date, class)%>%
  dplyr::summarize(weight=sum(weight))%>%
  mutate(au=weight/1000)%>%
  mutate(dmd=au*30)%>%
  group_by(class)%>%
  summarise(ad=sum(au), dmd=sum(dmd))

dmd_herd_graze<-left_join(all_dates, classes)%>%
  group_by(date, class)%>%
  dplyr::summarize(weight=sum(weight))%>%
  mutate(au=weight/1000)%>%
  mutate(dmd=au*30)%>%
  mutate(date=ymd(date))%>%
  filter(
    (class=="cow"&(date>mdy("4/15/2025")&date<mdy("12/20/2025"))) |
           ((class=="calf"|class=="feeder")&(date>mdy("5/01/2025")&date<mdy("11/20/2025")))
  )%>%
  group_by(class)%>%
  summarise(ad=sum(au), dmd=sum(dmd))


