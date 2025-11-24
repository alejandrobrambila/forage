library(tidyverse)

apr21<-read.csv("./platemeter/2025-Apr-21-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("4/21/25"))

apr24<-read.csv("./platemeter/2025-Apr-24-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("4/24/25"))

jun10<-read.csv("./platemeter/2025-Jun-10-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("6/10/25"))

allplatemeter<-rbind(apr21,apr24,apr25,apr30,aug15,aug25,aug26,aug28,jul11,jul14,jul15, jul31,jun02,jun05,jun10,jun11,jun23,jun27,may09,may12,may13,may14,may19,may21,may30,oct01,oct06,sep12,sep17)
                                         

intersect(colnames(sep12), colnames(may21), colnames(oct01))




apr25<-read.csv("./platemeter/2025-Apr-25-00-00-00(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("4/25/25"))

apr30<-read.csv("./platemeter/2025-Apr-30-00-00-00(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("4/30/25"))

aug15<-read.csv("./platemeter/2025-Aug-15-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("8/15/25"))
##
aug25<-read.csv("./platemeter/2025-Aug-25-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("8/25/25"))

aug26<-read.csv("./platemeter/2025-Aug-26-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("8/26/25"))

aug28<-read.csv("./platemeter/2025-Aug-28-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("8/28/25"))

jul11<-read.csv("./platemeter/2025-jul-11-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("7/11/25"))

jul14<-read.csv("./platemeter/2025-jul-14-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("7/14/25"))

jul15<-read.csv("./platemeter/2025-jul-15-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("7/15/25"))

jul31<-read.csv("./platemeter/2025-jul-31-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("7/31/25"))

jun02<-read.csv("./platemeter/2025-jun-02-00-00-00(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("6/2/25"))

jun05<-read.csv("./platemeter/2025-jun-05-00-00-00 PLOTS.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("6/5/25"))

jun11<-read.csv("./platemeter/2025-jun-11-00-00-00(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("6/11/25"))

jun23<-read.csv("./platemeter/2025-jun-23-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("6/23/25"))

jun27<-read.csv("./platemeter/2025-jun-27-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("6/27/25"))

may09<-read.csv("./platemeter/2025-may-09-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("5/09/25"))

may12<-read.csv("./platemeter/2025-may-12-00-00-00(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("5/12/25"))

may13<-read.csv("./platemeter/2025-may-13-00-00-00(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("5/13/25"))

may14<-read.csv("./platemeter/2025-may-14-00-00-00 PLOTS(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("5/14/25"))

may19<-read.csv("./platemeter/2025-may-19-00-00-00(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("5/19/25"))

may21<-read.csv("./platemeter/2025-may-21-00-00-00.csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("5/21/25"))

may30<-read.csv("./platemeter/2025-may-30-00-00-00(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("5/30/25"))

oct01<-read.csv("./platemeter/2025-oct-01-zone3(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("10/01/25"))

oct06<-read.csv("./platemeter/2025-oct-06-zone1zone2(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("10/06/25"))

sep12<-read.csv("./platemeter/2025-sep-12-00-00-00(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("09/12/25"))

sep17<-read.csv("./platemeter/2025-sep-17-00-00-00(in).csv")|>
  filter(Cover>0)|>
  mutate(date=mdy("09/17/25"))


# Remove column 'B'
jun02$X <- NULL
print(jun02)

jun10$X <- NULL

may12$lbs.hectre <- NULL
may12$into.maia <- NULL

may13$X <- NULL
may13$X.1 <- NULL

may14$X.3 <- NULL
may19$X.3 <- NULL
may30$X.2 <- NULL


allplatemeter<-rbind(apr21,apr24,apr25,apr30,aug15,aug25,aug26,aug28,jul11,jul14,jul15, jul31,jun02,jun05,jun10,jun11,jun23,jun27,may09,may12,may13,may14,may19,may21,may30,oct01,oct06,sep12,sep17)

ggplot(allplatemeter, aes(date, total))+
  geom_col()

glimpse(allplatemeter)
?geom_bar


