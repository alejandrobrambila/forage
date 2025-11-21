#test

library(tidyverse)
library(forcats)

forage<-read_csv('dairyone_master.csv',skip=5)%>%
  mutate(field=as.factor(field))
   
##AVAILABLE PROTEIN###
forage<-mutate(forage, field=fct_reorder(field, desc(available_protein)))
ggplot(forage, aes(field, available_protein))+geom_point(aes(color=cut_month, shape=year))+
  ylim(min(forage$available_protein)-1,max(forage$available_protein)+1)+
  geom_hline(color="blue", yintercept=as.numeric(forage$available_protein[19]))+
  geom_hline(yintercept=as.numeric(mean(forage$available_protein[1:18])))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

### FIBER ###
forage<-mutate(forage, field=fct_reorder(field, desc(acid_detergent_fiber)))
ggplot(forage, aes(field, acid_detergent_fiber))+geom_point(aes(color=cut_month, shape=year))+
  ylim(min(forage$acid_detergent_fiber)-.5,max(forage$acid_detergent_fiber)+.5)+
  geom_hline(color="blue", yintercept=as.numeric(forage$acid_detergent_fiber[19]))+
  geom_hline(yintercept=as.numeric(mean(forage$acid_detergent_fiber[1:18])))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

###TDN###
forage<-mutate(forage, field=fct_reorder(field, desc(total_digestible_nutrient)))
ggplot(forage, aes(field, total_digestible_nutrient))+geom_point(aes(color=cut_month, shape=year))+
  ylim(min(forage$total_digestible_nutrient)-.5,max(forage$total_digestible_nutrient)+.5)+
  geom_hline(color="blue", yintercept=as.numeric(forage$total_digestible_nutrient[19]))+
  geom_hline(yintercept=as.numeric(mean(forage$total_digestible_nutrient[1:18])))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#############
allforage<-forage

###TDN###

ggplot(allforage, aes(category, total_digestible_nutrient, shape=year))+geom_jitter(width=.1)+
  ylim(min(forage$total_digestible_nutrient)-.5,max(forage$total_digestible_nutrient)+.5)+
  geom_hline(color="blue", yintercept=as.numeric(forage$total_digestible_nutrient[19]))+
  geom_hline(yintercept=as.numeric(mean(forage$total_digestible_nutrient[1:18])))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#bales over time
ggplot(subset(allforage, farm!="GP"&category!="average"&category!="baleage"&!is.na(category)&category!="first_delay"), aes(falseyear, bales, shape=year, color=category))+# geom_line(aes(group=interaction(field,year)), color="black")+
  geom_point(size=2)+
 # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +  geom_hline(color="blue", yintercept=as.numeric(forage$bales[19]))+geom_hline(color="green", yintercept=60)+
geom_text(data=subset(allforage, farm!="GP"&category!="average"&category!="baleage"&!is.na(category)&category!="first_delay"), aes(falseyear-2, bales, label=field))#+facet_wrap(~field)


#TDN over time
ggplot(subset(allforage, farm!="GP"&category!="average"&category!="baleage"&!is.na(category)&category!="first_delay"), aes(year, total_digestible_nutrient, shape=year, color=category))+# geom_line(aes(group=interaction(field,year)), color="black")+
  geom_point(size=2)+theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +  geom_hline(color="blue", yintercept=as.numeric(forage$total_digestible_nutrient[19]))+geom_hline(color="green", yintercept=60)
  geom_text(data=subset(allforage, farm!="GP"&category!="average"&category!="baleage"&!is.na(category)&category!="first_delay"&falseyear<"2023-06-02"), aes(falseyear-10, total_digestible_nutrient, label=field))#+facet_wrap(~field)

#TDN*tons
ggplot(subset(allforage, category!="forage"&category!="forage_regrowth"&is.na(synthesized)&farm!="GP"&category!="average"&category!="baleage"&!is.na(category)&category!="first_delay"), aes(falseyear, total_digestible_nutrient*bales*750/2000/100, shape=year, color=category))+ 
  geom_line(aes(group=field), color="black")+
    geom_point(size=2)+theme_classic()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +  
  geom_text(data=subset(allforage, is.na(synthesized)&farm!="GP"&category!="average"&category!="baleage"&!is.na(category)&category!="first_delay"), aes(falseyear-5, total_digestible_nutrient*bales*750/2000/100, label=field))+#+facet_wrap(~field)
  coord_trans(y = "log10")+ylab("Tons of Digestible Nutrient")

#tons
ggplot(subset(allforage, category!="forage"&category!="forage_regrowth"&is.na(synthesized)&farm!="GP"&category!="average"&category!="baleage"&!is.na(category)&category!="first_delay"), aes(falseyear, bales*750/2000, shape=year, color=category))+ 
  geom_line(aes(group=field), color="black")+
  geom_point(size=2)+theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +  
  geom_text(data=subset(allforage, is.na(synthesized)&farm!="GP"&category!="average"&category!="baleage"&!is.na(category)&category!="first_delay"), aes(falseyear-5, bales*750/2000, label=field))+#+facet_wrap(~field)
  coord_trans(y = "log10")+ylab("Tons of Hay")


#protein
ggplot(subset(allforage, farm!="GP"&category!="average"&category!="baleage"&!is.na(category)&category!="first_delay"), aes(falseyear, available_protein, shape=year, color=category))+# geom_line(aes(group=interaction(field,year)), color="black")+
    geom_point(size=2)+theme_classic()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +  geom_hline(color="blue", yintercept=as.numeric(forage$available_protein[19]))

#protein tons
    ggplot(subset(allforage, category!="forage"&category!="forage_regrowth"&is.na(synthesized)&farm!="GP"&category!="average"&category!="baleage"&!is.na(category)&category!="first_delay"), aes(falseyear, available_protein*bales*750/2000, shape=year, color=category))+ 
      geom_line(aes(group=field), color="black")+
      geom_point(size=2)+theme_classic()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +  
     # geom_hline(color="blue", yintercept=as.numeric(forage$available_protein[19]*750/2000))+
      geom_text(data=subset(allforage, is.na(synthesized)&farm!="GP"&category!="average"&category!="baleage"&!is.na(category)&category!="first_delay"), aes(falseyear-2, available_protein*bales*750/2000, label=field))+#+facet_wrap(~field)
      coord_trans(y = "log10")
    
    #ADF
ggplot(subset(allforage, farm!="GP"&category!="average"&category!="baleage"&!is.na(category)&category!="first_delay"), aes(falseyear, acid_detergent_fiber, shape=year, color=category))+# geom_line(aes(group=interaction(field,year)), color="black")+
      geom_point(size=2)+theme_classic()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +  geom_hline(color="blue", yintercept=as.numeric(forage$acid_detergent_fiber[19]))
    geom_text(data=subset(allforage, farm!="GP"&category!="average"&category!="baleage"&!is.na(category)&category!="first_delay"&falseyear<"2023-06-02"), aes(falseyear-10, total_digestible_nutrient, label=field))#+facet_wrap(~field)
##    
#ggplot(subset(allforage, farm!="GP"&category!="average"&category!="baleage"&!is.na(category)&category!="first_delay"), aes(falseyear, ne_gain_mcal.lb, shape=year, color=category))+# geom_line(aes(group=interaction(field,year)), color="black")+
#      geom_point(size=2)+
#      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +  geom_hline(color="blue", yintercept=as.numeric(forage$ne_gain_mcal.lb[19]))
#    geom_text(data=subset(allforage, farm!="GP"&category!="average"&category!="baleage"&!is.na(category)&category!="first_delay"&falseyear<"2023-06-02"), aes(falseyear-10, total_digestible_nutrient, label=field))#+facet_wrap(~field)
   
    
#categories: May23, June22, August23
#stacked bars
#total bales, tons TDN, tons protein

allforage$field=fct_reorder(allforage$field, (allforage$bales))    
allforage$cat=fct_reorder(allforage$cat, (allforage$available_protein))    

ggplot(subset(allforage, category!="forage"&category!="forage_regrowth"&is.na(synthesized)&farm!="GP"&category!="average"&category!="baleage"&!is.na(category)&category!="first_delay"), 
           aes(year, bales, fill=field))+
      geom_bar(stat="identity", position="stack")
    
ggplot(subset(allforage, category!="forage"&category!="forage_regrowth"&is.na(synthesized)&farm!="GP"&category!="average"&category!="baleage"&!is.na(category)&category!="first_delay"), 
           aes(cat, bales, fill=field))+
      geom_bar(stat="identity", position="stack")
    
ggplot(subset(allforage, category!="forage"&category!="forage_regrowth"&is.na(synthesized)&farm!="GP"&category!="average"&category!="baleage"&!is.na(category)&category!="first_delay"), 
           aes(cat, total_digestible_nutrient*bales*750/2000, fill=field))+
      geom_bar(stat="identity", position="stack")

ggplot(subset(allforage, category!="forage"&category!="forage_regrowth"&is.na(synthesized)&farm!="GP"&category!="average"&category!="baleage"&!is.na(category)&category!="first_delay"), 
       aes(year, available_protein*bales*750/2000, fill=field))+
  geom_bar(stat="identity", position="stack")

forviz<-select(filter(allforage, is.na(synthesized)&!is.na(cat)), year, field, cat, category, bales, available_protein, total_digestible_nutrient, acid_detergent_fiber)%>%
  mutate(available_protein=available_protein*bales*750/2000, total_digestible_nutrient=total_digestible_nutrient*bales*750/2000, acid_detergent_fiber=acid_detergent_fiber*bales*750/2000)%>%
  gather(var, val, 5:8)
forviz$var<-factor(forviz$var, levels= c("bales", "total_digestible_nutrient", "available_protein", "acid_detergent_fiber"))

ggplot(forviz, aes(cat, val,  fill=field))+
  geom_bar(stat="identity", position="stack")+
  facet_wrap(~var, scales="free", nrow=1)

ggplot(filter(forviz, year!=2020), aes(year, val))+
  geom_bar(stat="identity", position="stack")+
  facet_wrap(~var, scales="free", nrow=1)+theme_classic()+ylab("Tons")

ggplot(filter(forviz, year!=2020), aes(cat, val))+
  geom_bar(stat="identity", position="stack")+
  facet_wrap(~var, scales="free", nrow=1)+theme_classic()+ylab("Tons")

ggplot(filter(forviz, year!=2020), aes(year, val,  fill=field))+
  geom_bar(stat="identity", position="stack")+
  facet_wrap(~var, scales="free", nrow=1)+theme_classic()

forviz2<-select(filter(allforage, is.na(synthesized)&!is.na(cat)), year, field, cat, category, bales, available_protein, total_digestible_nutrient, acid_detergent_fiber)%>%
  gather(var, val, 5:8)
forviz2$var<-factor(forviz2$var, levels= c("bales", "total_digestible_nutrient", "available_protein", "acid_detergent_fiber"))

ggplot(filter(forviz2, year!=2020), aes(cat, val))+
  geom_boxplot()+
  facet_wrap(~var, scales="free")+theme_classic()


ggplot(filter(forviz2, year!=2020), aes(year, val))+
  geom_boxplot()+
  facet_wrap(~var, scales="free")+theme_classic()

#what if (TDN/protein losses if delayed to mid july for several of may cuts)
#if volume stayed the same/if volume increased to 2022 levels/volume by platemeter??
    
    


###FORB forage

forbs<-forage%>%
  filter(type=="forb"|type=="grass"|type=="113k")


ggplot(forbs, aes(field2, total_digestible_nutrient))+geom_point(aes(color=year))+theme_classic()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(forbs, aes(field2, acid_detergent_fiber))+geom_point(aes(color=year))+theme_classic()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(forbs, aes(field2, crude_protein))+geom_point(aes(color=year))+theme_classic()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(forbs, aes(field2, calcium))+geom_point(aes(color=year))+theme_classic()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(forbs, aes(field2, phosphorus))+geom_point(aes(color=year))+theme_classic()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(forbs, aes(field2, magnesium))+geom_point(aes(color=year))+theme_classic()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(forbs, aes(field2, potassium))+geom_point(aes(color=year))+theme_classic()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(forbs, aes(field2, zinc_ppm))+geom_point(aes(color=year))+theme_classic()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(forbs, aes(field2, iron_ppm))+geom_point(aes(color=year))+theme_classic()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(forbs, aes(field2, copper_ppm))+geom_point(aes(color=year))+theme_classic()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(forbs, aes(field2, mn_ppm))+geom_point(aes(color=year))+theme_classic()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(forbs, aes(field2, mo_ppm))+geom_point(aes(color=year))+theme_classic()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(forbs, aes(field2, sulfur_percent))+geom_point(aes(color=year))+theme_classic()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


### native warm season grasses
NWSG<-forage%>%
  filter(category=="NWSG"|type=="113k")%>%
  mutate(field2=as.factor(field2))
NWSG$field2<-fct_relevel(NWSG$field2, "DAIRYONE AVERAGE 2004-2022", "Bluestem", "Indiangrass", "Switchgrass", "Gamagrass", "Bluestem+Indiangrass", "Mixed NWSG", "Mixed NWSG w/ clover")

ggplot(NWSG, aes(field2, total_digestible_nutrient)) + geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=52, ymax=58), fill="lightgrey")+
geom_point(aes(color=year))+theme_classic()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.title = element_blank())+
  labs(caption = "Dairy One average represents typical northeast cool season hay value. Shading represents range of 'medium' quality values.")+
  xlab("")+ ylab("% Total Digestible Nutrient")
  #geom_point(data=(filter(NWSG, type!="113k")), aes(y=total_digestible_nutrient*1.2, color=year, alpha=.8))
  #geom_hline(yintercept=58)+geom_hline(yintercept=52, color="darkred")

ggplot(NWSG, aes(field2, crude_protein))+ geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=7, ymax=11), fill="lightgrey")+geom_point(aes(color=year))+theme_classic()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  #geom_point(data=(filter(NWSG, type!="113k")), aes(y=total_digestible_nutrient*1.2, color=year, alpha=.8))
  #geom_hline(yintercept=8, color="darkred")
