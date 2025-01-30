#test

library(tidyverse)
library(forcats)

forage<-read_csv('/Users/alejandro/Library/CloudStorage/OneDrive-SharedLibraries-TTOR/Farm Team - Agroecology Initiative/Monitoring/Production/Data/spreadsheets/2022/appleton/appleton_firstcut_quality_2022.csv')%>%
  mutate(field=as.factor(field))
   
##AVAILABLE PROTEIN###
forage<-mutate(forage, field=fct_reorder(field, desc(available_protein)))
ggplot(forage, aes(field, available_protein))+geom_point()+
  ylim(min(forage$available_protein)-1,max(forage$available_protein)+1)+
  geom_hline(color="blue", yintercept=as.numeric(forage$available_protein[19]))+
  geom_hline(yintercept=as.numeric(mean(forage$available_protein[1:18])))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

### FIBER ###
forage<-mutate(forage, field=fct_reorder(field, desc(acid_detergent_fiber)))
ggplot(forage, aes(field, acid_detergent_fiber))+geom_point()+
  ylim(min(forage$acid_detergent_fiber)-.5,max(forage$acid_detergent_fiber)+.5)+
  geom_hline(color="blue", yintercept=as.numeric(forage$acid_detergent_fiber[19]))+
  geom_hline(yintercept=as.numeric(mean(forage$acid_detergent_fiber[1:18])))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

###TDN###
forage<-mutate(forage, field=fct_reorder(field, desc(total_digestible_nutrient)))
ggplot(forage, aes(field, total_digestible_nutrient))+geom_point()+
  ylim(min(forage$total_digestible_nutrient)-.5,max(forage$total_digestible_nutrient)+.5)+
  geom_hline(color="blue", yintercept=as.numeric(forage$total_digestible_nutrient[19]))+
  geom_hline(yintercept=as.numeric(mean(forage$total_digestible_nutrient[1:18])))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Net Energy Lactation#
forage<-mutate(forage, field=fct_reorder(field, desc(forage[13])))
ggplot(forage, aes_string("field", names(forage[13])))+geom_point()+
  ylim(min(forage[13])*.9,max(forage[13])*1.1)+
  geom_hline(color="blue", yintercept=as.numeric(forage[19,13]))+
  geom_hline(yintercept=as.numeric(sum(forage[1:18, 13])/18))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Net Energy Maintenance" 
forage<-mutate(forage, field=fct_reorder(field, desc(forage[14])))
ggplot(forage, aes_string("field", names(forage[14])))+geom_point()+
  ylim(min(forage[14])*.9,max(forage[14])*1.1)+
  geom_hline(color="blue", yintercept=as.numeric(forage[19,14]))+
  geom_hline(yintercept=as.numeric(sum(forage[1:18, 14])/18))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Net Energy Gain#
forage<-mutate(forage, field=fct_reorder(field, desc(forage[15])))
ggplot(forage, aes_string("field", names(forage[15])))+geom_point()+
  ylim(min(forage[15])*.9,max(forage[15])*1.1)+
  geom_hline(color="blue", yintercept=as.numeric(forage[19,15]))+
  geom_hline(yintercept=as.numeric(sum(forage[1:18, 15])/18))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


###TDN###
forage<-mutate(forage, field=fct_reorder(field, desc(total_digestible_nutrient)))
ggplot(forage, aes(field, total_digestible_nutrient))+geom_point()+
  ylim(min(forage$total_digestible_nutrient)-.5,max(forage$total_digestible_nutrient)+.5)+
  geom_hline(color="blue", yintercept=as.numeric(forage$total_digestible_nutrient[19]))+
  geom_hline(yintercept=as.numeric(mean(forage$total_digestible_nutrient[1:18])))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#############
forageyears<-read_csv('/Users/alejandro/Library/CloudStorage/OneDrive-SharedLibraries-TTOR/Farm Team - Agroecology Initiative/Monitoring/Production/Data/spreadsheets/2022/appleton/all_forage_testing.csv')%>%
  mutate(field=as.factor(field))

###TDN###
allforage<-mutate(forageyears, field=fct_reorder(field, desc(total_digestible_nutrient)))%>%
  mutate(falseyear=ymd(paste("2023", month(ymd(cut_date)), day(ymd(cut_date)),  sep="_")))
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
ggplot(subset(allforage, farm!="GP"&category!="average"&category!="baleage"&!is.na(category)&category!="first_delay"), aes(falseyear, total_digestible_nutrient, shape=year, color=category))+# geom_line(aes(group=interaction(field,year)), color="black")+
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
    
    