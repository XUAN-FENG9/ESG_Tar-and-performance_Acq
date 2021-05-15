library(tidyverse)
library(writexl)
library(readxl)
library(dplyr)
library(lubridate)

#data manipulation to generate the main file for analysing
#construct the M&A data file without NA - from 2001 to 2020 without 2007 to 2009
MAD1020<-read_xlsx('D:/Zephyr_M&A1020 database.xlsx',sheet = 'Results',col_types = c("numeric","numeric","text","text","text","text","date","date","text","text","text","text","numeric","text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
MAD0106<-read_xlsx('D:/Zephyr_M&A0106 database.xlsx',sheet = 'Results',col_types = c("numeric","numeric","text","text","text","text","date","date","text","text","text","text","numeric","text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
MAD<-rbind(MAD0106,MAD1020)
MAD$`Deal value m USD`<-gsub("[*]","",MAD$`Deal value m USD`)
MAD$`Deal value m USD`<-gsub("[,]","",MAD$`Deal value m USD`)
MAD$`Deal value m USD`<-as.numeric(MAD$`Deal value m USD`)
MAD<-na.omit(MAD)
write_xlsx(MAD,'D:/Integrated M&A datebased.xlsx')

#construct the ESG data file includes total ESG score and separate scores 
ESG<-read_xlsx('D:/Original ESG database.xlsx')
MAD$year<-year(MAD$`Announced date`)
#generate total score
T<-ESG[ESG$`ESG Datatype`=="ESG Score",]
T1<-T[,c(4,18:37)]
T2<-gather(T1,key = 'year',value = 'ESGscore',-1)
T2$year<-as.numeric(T2$year)
T2$ESGscore<-as.numeric(T2$ESGscore)
tt<-MAD[,c(14,39)]
ta<-MAD[,c(15,39)]
T3<-merge(tt,T2,by.x ="Target ISIN number",by.y = "ISIN CODE" ,all = TRUE)
T4<-na.omit(T3)
T5<-T4%>%filter(T4$year.x>T4$year.y)
T6<-aggregate(T5$ESGscore,by=list(T5$`Target ISIN number`),mean)
names(T6)<-c("ISIN CODE","Total ESG score_target")
T3a<-merge(ta,T2,by.x = "Acquiror ISIN number", by.y = "ISIN CODE", all = TRUE)
T4a<-na.omit(T3a)
T5a<-T4a%>%filter(T4a$year.x>T4a$year.y)
T6a<-aggregate(T5a$ESGscore,by=list(T5a$`Acquiror ISIN number`),mean)
names(T6a)<-c("ISIN CODE", "Total ESG score_acquiror")

#generate Environment score
E<-ESG%>%filter(ESG$`ESG Datatype`%in% c("Resource Use Score","Emissions Score","Environmental Innovation Score"))
E1<-E[,c(4,18:37)]
E1[,-1]<-lapply(E1[,-1],as.numeric)
E2<-aggregate(E1,by=list(E1$`ISIN CODE`),mean)
E2<-E2[,-2]
E2<-rename(E2,c("ISIN CODE"=Group.1))
E3<-gather(E2,key='year',value = 'ESGscore',-1)
E3$year<-as.numeric(E3$year)
E4<-merge(tt,E3,by.x = "Target ISIN number",by.y = "ISIN CODE",all = TRUE)
E4<-na.omit(E4)
E5<-E4%>%filter(E4$year.x>E4$year.y)
E6<-aggregate(E5$ESGscore,by=list(E5$`Target ISIN number`),mean)
names(E6)<-c("ISIN CODE","Environment score_target")
E4a<-merge(ta,E3,by.x = "Acquiror ISIN number",by.y = "ISIN CODE",all = TRUE)
E4a<-na.omit(E4a)
E5a<-E4a%>%filter(E4a$year.x>E4a$year.y)
E6a<-aggregate(E5a$ESGscore,by=list(E5a$`Acquiror ISIN number`),mean)
names(E6a)<-c("ISIN CODE","Environment score_acquiror")

#generate Social score
S<-ESG%>%filter(ESG$`ESG Datatype`%in% c("Human Rights Score","Community Score","Product Responsibility Score"))
S1<-S[,c(4,18:37)]
S1[,-1]<-lapply(S1[,-1],as.numeric)
S2<-aggregate(S1,by=list(S1$`ISIN CODE`),mean)
S2<-S2[,-2]
S2<-rename(S2,c("ISIN CODE"=Group.1))
S3<-gather(S2,key='year',value = 'ESGscore',-1)
S3$year<-as.numeric(S3$year)
S4<-merge(tt,S3,by.x = "Target ISIN number",by.y = "ISIN CODE",all = TRUE)
S4<-na.omit(S4)
S5<-S4%>%filter(S4$year.x>S4$year.y)
S6<-aggregate(S5$ESGscore,by=list(S5$`Target ISIN number`),mean)
names(S6)<-c("ISIN CODE","Social score_target")
S4a<-merge(ta,S3,by.x = "Acquiror ISIN number",by.y = "ISIN CODE",all = TRUE)
S4a<-na.omit(S4a)
S5a<-S4a%>%filter(S4a$year.x>S4a$year.y)
S6a<-aggregate(S5a$ESGscore,by=list(S5a$`Acquiror ISIN number`),mean)
names(S6a)<-c("ISIN CODE","Social score_acquiror")

#generate governance score
G<-ESG%>%filter(ESG$`ESG Datatype`%in% c("Management Score","Shareholders Score","CSR Strategy Score","Workforce Score"))
G1<-G[,c(4,18:37)]
G1[,-1]<-lapply(G1[,-1],as.numeric)
G2<-aggregate(G1,by=list(G1$`ISIN CODE`),mean)
G2<-G2[,-2]
G2<-rename(G2,c("ISIN CODE"=Group.1))
G3<-gather(G2,key='year',value = 'ESGscore',-1)
G3$year<-as.numeric(G3$year)
G4<-merge(tt,G3,by.x = "Target ISIN number",by.y = "ISIN CODE",all = TRUE)
G4<-na.omit(G4)
G5<-G4%>%filter(G4$year.x>G4$year.y)
G6<-aggregate(G5$ESGscore,by=list(G5$`Target ISIN number`),mean)
names(G6)<-c("ISIN CODE","Governance score_target")
G4a<-merge(ta,G3,by.x = "Acquiror ISIN number",by.y = "ISIN CODE",all = TRUE)
G4a<-na.omit(G4a)
G5a<-G4a%>%filter(G4a$year.x>G4a$year.y)
G6a<-aggregate(G5a$ESGscore,by=list(G5a$`Acquiror ISIN number`),mean)
names(G6a)<-c("ISIN CODE","Governance score_acquiror")

#generate required variables in the model

main<-MAD[,-c(1,39)]
main$LnDV<-log(main$`Deal value m USD`)
main$Premium<-1+main[,12]/100
main$D_cash<-ifelse(substr(main$`Deal method of payment`,1,4)=="Cash",1,0)
main$D_crossbrd<-ifelse(main$`Acquiror country code`==main$`Target country code`,0,1)
main$D_crossind<-ifelse(main$`Target primary US SIC code`==main$`Acquiror primary US SIC code`,0,1)
main$AcqQ<-(main[,30]+main[,28]-main[,29])/main[,28]
main$AcqNPM<-main[,27]/main[,24]
main$AcqEBITM<-main[,26]/main[,24]
main$AcqEBITDAM<-main[,25]/main[,24]
main$AcqLev<-(main[,28]-main[,29])/main[,28]
main$AcqATO<-main[,24]/main[,28]
main$AcqSize<-log(main[,28])
main$TarQ<-(main[,21]+main[,17]-main[22])/main[,17]
main$TarNPM<-main[,19]/main[,18]
main$TarEBITM<-main[,20]/main[,18]
main$TarEBITDAM<-main[,23]/main[,18]
main$TarLev<-(main[,17]-main[,22])/main[,17]
main$TarATO<-main[,18]/main[,17]
main$TarSize<-log(main[,17])
main$AcqROAPre<-main[,26]/main[,28]
main$AcqROApost<-main[,35]/main[,34]
main$Complete_year<-year(main$`Completed date`)
main<-main[,c(1,2,13,3,16,4,14,5,15,59,38:58,31,32,36,37)]
main<-merge(main,T6,by.x = "Target ISIN number",by.y = "ISIN CODE",all = TRUE)
main<-merge(main,T6a,by.x = "Acquiror ISIN number",by.y = "ISIN CODE",all = TRUE)
main<-merge(main,E6,by.x = "Target ISIN number",by.y = "ISIN CODE",all = TRUE)
main<-merge(main,E6a,by.x = "Acquiror ISIN number",by.y = "ISIN CODE",all = TRUE)
main<-merge(main,S6,by.x = "Target ISIN number",by.y = "ISIN CODE",all = TRUE)
main<-merge(main,S6a,by.x = "Acquiror ISIN number",by.y = "ISIN CODE",all = TRUE)
main<-merge(main,G6,by.x = "Target ISIN number",by.y = "ISIN CODE",all = TRUE)
main<-merge(main,G6a,by.x = "Acquiror ISIN number",by.y = "ISIN CODE",all = TRUE)
main<-na.omit(main)
names(main)<-c("Acq_ISIN","Tar_ISIN","Deal_No","Acquiror_name","Acq_country_code","Acq_industry_code","Target_name","Tar_country_code","Tar_industry_code","Year_complete","LnDV","Premium","D_cash","D_crossbroad","D_crossindustry","AcqQ","AcqNPM","AcqEBITM","AcqEBITDAM","AcqLev","AcqATO","Ln_AcqSize","TarQ","TarNPM","TarEBITM","TarEBITDAM","TarLev","TarATO","Ln_Tarsize","AcqROA_pre","AcqROA_post","AcqSPrice_1month_after","AcqSPrice_3month_prior","AcqSPrice_prior_announcement","AcqSPrice_after_completion","Tar_ESG_score","Acq_ESG_score","Tar_environment_score","Acq_environment_score","Tar_social_score","Acq_social_score","Tar_governance_score","Acq_governance_score")
main<-main[,c(3,1,2,4:43)]

#print out
write_xlsx(main,"D:/main_file.xlsx")

#summary data
correl<-main[,-c(1:10,30:35)]