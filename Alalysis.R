library(tidyverse)
library(readxl)
library(writexl)
library(dplyr)
library(lubridate)
library(pheatmap)
library(plm)
library(psych)
library(car)

main<-read_xlsx("D:/main_file.xlsx")
main$AcqROA_dif<-round((main$AcqROA_post/main$AcqROA_pre)-1,5)
main$AcqStockPrice_dif3to1<-round((main$AcqSPrice_1month_after/main$AcqSPrice_3month_prior)-1,5)
main$AcqStockPrice_dif<-round((main$AcqSPrice_after_completion/main$AcqSPrice_prior_announcement)-1,5)

# variable statistics
NCrossboard<-sum(main$D_crossbroad)
NCrossindustry<-sum(main$D_crossindustry)
Ncash<-sum(main$D_cash)
main$ones<-ifelse(main$Deal_No==0,0,1)
Nyear<-aggregate(main$ones,by=list(main$Year_complete),sum)

main$AcqROA_dif<-ifelse(main$AcqROA_dif<quantile(main$AcqROA_dif,0.05),quantile(main$AcqROA_dif,0.05),main$AcqROA_dif)
main$AcqROA_dif<-ifelse(main$AcqROA_dif>quantile(main$AcqROA_dif,0.95),quantile(main$AcqROA_dif,0.95),main$AcqROA_dif)
main$AcqEBITM<-ifelse(main$AcqEBITM<quantile(main$AcqEBITM,0.05),quantile(main$AcqEBITM,0.05),main$AcqEBITM)
main$AcqEBITM<-ifelse(main$AcqEBITM>quantile(main$AcqEBITM,0.95),quantile(main$AcqEBITM,0.95),main$AcqEBITM)
main$TarEBITM<-ifelse(main$TarEBITM<quantile(main$TarEBITM,0.05),quantile(main$TarEBITM,0.05),main$TarEBITM)
main$TarEBITM<-ifelse(main$TarEBITM>quantile(main$TarEBITM,0.95),quantile(main$TarEBITM,0.95),main$TarEBITM)
main$TarQ<-ifelse(main$TarQ<quantile(main$TarQ,0.05),quantile(main$TarQ,0.05),main$TarQ)
main$TarQ<-ifelse(main$TarQ>quantile(main$TarQ,0.95),quantile(main$TarQ,0.95),main$TarQ)

mm<-main[,c(44,45,46,11:43)]
mm<-mm[,-c(10,12,17,19,23:28)]
des<-describe(mm)
des<-des[,c(3,4,5,8,9,10)]
des<-round(des,2)
des$rownames<-rownames(des)
des<-des[,c(7,1:6)]
#write_xlsx(des,"D:describe.xlsx")
correl<-round(as_tibble(cor(mm)),2)
correl$variables<-colnames(correl)
correl<-correl[,c(27,1:26)]
library(pheatmap)
pheatmap(cor(mm),cluster_rows = FALSE,cluster_cols = FALSE)
#write_xlsx(correl,"D:correlation.xlsx")

#test instrumental variables
main<-unite(main,"Tar_cty_ind",Tar_country_code,Tar_industry_code,remove = FALSE)
avgind<-aggregate(main$Tar_ESG_score,by=list(main$Tar_industry_code),mean)
avgcty<-aggregate(main$Tar_ESG_score,by=list(main$Tar_country_code),mean)
ctyind<-aggregate(main$Tar_ESG_score,by=list(main$Tar_cty_ind),mean)
main<-merge(main,avgind,by.x = "Tar_industry_code",by.y = "Group.1",all=TRUE)
main<-merge(main,avgcty,by.x = "Tar_country_code",by.y = "Group.1",all = TRUE)
main<-merge(main,ctyind,by.x = "Tar_cty_ind",by.y = "Group.1",all=TRUE)
names(main)[49]<-"Avgind"
names(main)[50]<-"Avgcty"
names(main)[51]<-"Avgindcty"
main%>%ggplot(aes(Avgind,AcqROA_dif))+geom_point()+ylim(-1,1)
main%>%ggplot(aes(Avgcty,AcqROA_dif))+geom_point()+ylim(-2,2)
main%>%ggplot(aes(Avgindcty,AcqROA_dif))+geom_point()+ylim(-1,1)

#OLS
ols<-lm(AcqStockPrice_dif3to1~Tar_ESG_score+Acq_ESG_score+LnDV+Premium+D_cash+D_crossbroad+D_crossindustry
         +AcqQ+AcqEBITM+AcqLev+AcqATO+Ln_AcqSize+TarQ+TarEBITM+TarLev+TarATO+Ln_Tarsize
         +factor(main$Year_complete)+factor(main$Acq_country_code)+factor(main$Acq_industry_code),
         data = main)

olse<-lm(AcqStockPrice_dif3to1~Tar_environment_score+Acq_environment_score+LnDV+Premium+D_cash+D_crossbroad+D_crossindustry
          +AcqQ+AcqEBITM+AcqLev+AcqATO+Ln_AcqSize+TarQ+TarEBITM+TarLev+TarATO+Ln_Tarsize
        +factor(main$Year_complete)+factor(main$Acq_country_code)+factor(main$Acq_industry_code),
         data = main)

olss<-lm(AcqStockPrice_dif3to1~Tar_social_score+Acq_social_score+LnDV+Premium+D_cash+D_crossbroad+D_crossindustry
         +AcqQ+AcqEBITM+AcqLev+AcqATO+Ln_AcqSize+TarQ+TarEBITM+TarLev+TarATO+Ln_Tarsize
         +factor(main$Year_complete)+factor(main$Acq_country_code)+factor(main$Acq_industry_code),
         data = main)

olsg<-lm(AcqStockPrice_dif3to1~Tar_governance_score+Acq_governance_score+LnDV+Premium+D_cash+D_crossbroad+D_crossindustry
         +AcqQ+AcqEBITM+AcqLev+AcqATO+Ln_AcqSize+TarQ+TarEBITM+TarLev+TarATO+Ln_Tarsize
         +factor(main$Year_complete)+factor(main$Acq_country_code)+factor(main$Acq_industry_code),
         data = main)
#main$residuals<-ols$residuals
#main%>%ggplot(aes(Avgind,residuals))+geom_point()
#main%>%ggplot(aes(Avgcty,residuals))+geom_point()
#main%>%ggplot(aes(Avgindcty,residuals))+geom_point()
rgs2<-lm(AcqROA_dif~Tar_ESG_score*Acq_ESG_score+LnDV+Premium+D_cash+D_crossbroad+D_crossindustry
        +AcqQ+AcqEBITM+AcqLev+AcqATO+Ln_AcqSize+TarQ+TarEBITM+TarLev+TarATO+Ln_Tarsize
        +factor(main$Year_complete)+factor(main$Acq_country_code)+factor(main$Acq_industry_code),
        data = main)

#dummy interaction (0 for this main analysis)
highESG0<-quantile(main$Acq_ESG_score,0.75)
lowESG0<-quantile(main$Acq_ESG_score,0.25)
DmainESG0<-main%>%filter(main$Acq_ESG_score>=highESG0|main$Acq_ESG_score<=lowESG0)
DmainESG0$DESGAcq<-ifelse(DmainESG0$Acq_ESG_score>=highESG0,1,0)
DirsgESG0<-lm(AcqROA_dif~Tar_ESG_score*DESGAcq+LnDV+Premium+D_cash+D_crossbroad+D_crossindustry
              +AcqQ+AcqEBITM+AcqLev+AcqATO+Ln_AcqSize+TarQ+TarEBITM+TarLev+TarATO+Ln_Tarsize
              +factor(DmainESG0$Year_complete),
              data = DmainESG0,singular.ok = TRUE)
DirsgSP0<-lm(AcqStockPrice_dif3to1~Tar_ESG_score*DESGAcq+LnDV+Premium+D_cash+D_crossbroad+D_crossindustry
              +AcqQ+AcqEBITM+AcqLev+AcqATO+Ln_AcqSize+TarQ+TarEBITM+TarLev+TarATO+Ln_Tarsize
              +factor(DmainESG0$Year_complete),
              data = DmainESG0,singular.ok = TRUE)

#US deals analysis
US<-main%>%filter(main$Acq_country_code=="US" & main$Tar_country_code=="US")
usols<-lm(AcqStockPrice_dif3to1~Tar_ESG_score+Acq_ESG_score+LnDV+Premium+D_cash+D_crossindustry
               +AcqQ+AcqEBITM+AcqLev+AcqATO+Ln_AcqSize+TarQ+TarEBITM+TarLev+TarATO+Ln_Tarsize
               +factor(US$Year_complete)+factor(US$Acq_industry_code),
               data = US)
usci<-lm(AcqROA_dif~Tar_ESG_score*Acq_ESG_score+LnDV+Premium+D_cash+D_crossindustry
         +AcqQ+AcqEBITM+AcqLev+AcqATO+Ln_AcqSize+TarQ+TarEBITM+TarLev+TarATO+Ln_Tarsize
         +factor(US$Year_complete)+factor(US$Acq_industry_code),
         data = US)

UShigh<-quantile(US$Acq_ESG_score,0.75)
USlow<-quantile(US$Acq_ESG_score,0.25)
DUS<-US%>%filter(US$Acq_ESG_score>=UShigh|US$Acq_ESG_score<=USlow)
DUS$DESGAcq<-ifelse(DUS$Acq_ESG_score>=UShigh,1,0)

usdi<-lm(AcqStockPrice_dif3to1~Tar_ESG_score*DESGAcq+LnDV+Premium+D_cash+D_crossindustry
              +AcqQ+AcqEBITM+AcqLev+AcqATO+Ln_AcqSize+TarQ+TarEBITM+TarLev+TarATO+Ln_Tarsize
         +factor(DUS$Year_complete),
              data = DUS,singular.ok = TRUE)

#2SLS regression
#first stage
firstreg1<-lm(Tar_ESG_score~Avgind+Avgcty+DESGAcq+LnDV+Premium+D_cash+D_crossbroad+D_crossindustry
             +AcqQ+AcqEBITM+AcqLev+AcqATO+Ln_AcqSize+TarQ+TarEBITM+TarLev+TarATO+Ln_Tarsize
             +factor(DmainESG0$Year_complete),
             data = DmainESG0)
DmainESG0$Tar_ESG_score.hat<-fitted(firstreg1)
secondreg1<-lm(AcqROA_dif~Tar_ESG_score.hat*DESGAcq+Acq_ESG_score+LnDV+Premium+D_cash+D_crossbroad+D_crossindustry
               +AcqQ+AcqEBITM+AcqLev+AcqATO+Ln_AcqSize+TarQ+TarEBITM+TarLev+TarATO+Ln_Tarsize
               +factor(DmainESG0$Year_complete), data = DmainESG0)
secondreg2<-lm(AcqStockPrice_dif3to1~Tar_ESG_score.hat*DESGAcq+Acq_ESG_score+LnDV+Premium+D_cash+D_crossbroad+D_crossindustry
               +AcqQ+AcqEBITM+AcqLev+AcqATO+Ln_AcqSize+TarQ+TarEBITM+TarLev+TarATO+Ln_Tarsize
               +factor(DmainESG0$Year_complete), data = DmainESG0)
