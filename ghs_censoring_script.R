library(survival)
library(ggfortify)
library(MASS)
library(patchwork)
library(survminer)
library(ranger)
library(tidyverse)
library(dplyr)
library(patchwork)
library(broom)
library(data.table)

#read in data
controls <- read.table('/Users/orszagl2/Library/CloudStorage/OneDrive-NationalInstitutesofHealth/Genetics/BRCA1_BRCA2/ukbb_brca2_control.txt', header=TRUE)
brca2 <- read.table('/Users/orszagl2/Library/CloudStorage/OneDrive-NationalInstitutesofHealth/Genetics/BRCA1_BRCA2/ukbb_brca2_cases.txt', header=TRUE)
pheno <- fread("/Users/orszagl2/Library/CloudStorage/OneDrive-NationalInstitutesofHealth/Genetics/BRCA1_BRCA2/UKBiobank_phenotype_update_112024_final_truncated_familyhistory.txt", sep='\t', fill = TRUE)
operations <-read.csv('/Users/orszagl2/Documents/Genetics/operations/operations.csv')

# functions
disc<-function(mydata,cols,sts){
  ITR<-length(cols)
  vec<-numeric(nrow(mydata))
  for(i in 1:ITR){
    vec<-cbind(vec,as.numeric(grepl(sts,mydata[[cols[i]]],ignore.case=TRUE)))
  }
  
  vec<-vec[,-1]
  colnames(vec)<-cols
  comb<-apply(vec,1,sum)
  comb<-as.numeric(comb>0)
  out<-list(comb,vec)
  return(out)
}

stitch <- function(dataset,g1,g2,res){
  wdata<-cbind(dataset,res)
  g1data<-cbind(wdata[is.element(wdata$eid,g1),],1)
  colnames(g1data)[ncol(g1data)]<-"GR"
  g2data<-cbind(wdata[is.element(wdata$eid,g2),],0)
  colnames(g2data)[ncol(g2data)]<-"GR"
  fdata<-rbind(g1data,g2data)
  return(fdata)
}

run_stitch_cens = function(cancers, cases, pheno){
  R1<-disc(pheno,regcodes,cancers)
  D1<-stitch(pheno,cases[,1],controls[,1],R1[[1]])
  # pull stop year if applicable
  deathyear<-as.numeric(substr(D1$p191,1,4))
  surgyear = as.numeric(substr(D1$date_op1,1,4))
  STOPYEAR = pmin(deathyear, surgyear, na.rm = TRUE) # choose whichever one comes first, death or surgery (if applicable)
  STOPYEAR[!STOPYEAR>0|is.na(STOPYEAR)]<-CONSTANTYEAR #fill w const year when not applicable
  AGEATSTART<-STARTYEAR-D1$p34 # start age
  D1<-cbind(D1,STOPYEAR)
  TE<-D1$STOPYEAR-D1$p34 # time observed
  Rset<-numeric(nrow(D1)) # for H dataframe
  # dummy var
  for(i in 1:length(regcodes)){
    Ritem<-disc(D1,c(regcodes[i],"p52"),cancers)[[1]]
    Rset<-cbind(Rset,Ritem)
  }
  
  Rset<-Rset[,-1]
  D1 = as.data.frame(D1)
  H1<-D1[,c((which(colnames(D1)=="p40008_i0")):(which(colnames(D1)=="p40008_i0")+21))]
  
  # for ICD9+10
  for(i in 1:15){
    H1[,i][Rset[,i]==0&Rset[,22+i]==0]<-1000
  }
  TE2<-apply(H1,1,min,na.rm=TRUE)
  TE[D1$res==1]<-TE2[D1$res==1]
  D1<-cbind(D1,TE,AGEATSTART)
  return(D1)
}

survival_plot = function(survres, data, title, fun, agemin, ylim){
  ggsurvplot(survres, 
             data = data ,
             fun = fun,
             censor = FALSE,
             ggtheme = theme_light(),  ##get graph with light grid
             font.x = c(18,"plain", "black"),
             font.y = c(18,"plain", "black"),
             font.legend = c(18,"plain", "black"),
             legend.labs = c('noncarrier', 'carrier'),  ##need to change per plot, will use in figures
             font.tickslab = c(18,"plain", "black"),
             title = title,  ##use to id plots, will crop out of final figures
             conf.int = 'T',           # confidence intervals for point estimates of survival 
             #conf.int.style = "step",  # customize style of confidence intervals
             xlab = "Age (Years)",    # customize X axis label.
             xlim = c(agemin ,90),
             ylim = c(0,ylim),
             break.y.by = 0.2,
             break.x.by = 10,      # break X axis in time intervals by 10
             palette = c('#E7B800', '#2E9FDF'), ## for noncarrier and 1 carrier group
             risk.table = "nrisk_cumcensor", ##keep censor table; some journals want themm we can crop out if we dont need them
             tables.height = 0.3,
             tables.y.text = TRUE,
             tables.fontsize = 2,
             tables.theme = theme_cleantable()
  )
}


# time to breast/ovarian cancer
operations2 = operations%>%filter(surgery_category == "Total hysterectomy" | surgery_category == "Bilateral salpingo_oopherectomy" | surgery_category == "Bilateral oopherectomy" | surgery_category == "Total mastectomy")
operations2$opdate = as.Date(operations2$opdate, format = '%m/%d/%y')
operations2 <- operations2 %>%
  filter(!is.na(opdate)) %>%
  group_by(eid)%>%
  summarize(date_op1 = min(opdate, na.rm = TRUE))

## set up phenotype, constant variables
pheno2 <- pheno %>% filter (p31=="Female")
pheno2 = operations2%>%
  dplyr::select(eid, date_op1)%>%
  right_join(pheno2, by = 'eid')
b_o_cancers<-"C50|^C50|^17[45]|'17[45]|C56|^C56|^1830|'1830"
STARTYEAR<-1970

## run the functions
D3_cens = run_stitch_cens(b_o_cancers, brca2, pheno2)
survres3_cens<-survfit(Surv(AGEATSTART,TE, res) ~ GR, data=D3_cens)
fit3_cens <- coxph(Surv(AGEATSTART,TE, res) ~ GR + bmis+ p34 + smokestat, data=D3_cens)
summary (fit3_cens)
survival_plot(survres3_cens, D3_cens, "Time to Breast/Ovarian Cancer with Censoring", "event", 20, .8)


# time to breast cancer
b_cancers<-"C50|^C50|^17[45]|'17[45]"
STARTYEAR<-1970
operations4= operations%>%filter(surgery_category == "Total mastectomy")
operations4$opdate = as.Date(operations4$opdate, format = '%m/%d/%y')
operations4 = operations4%>%
  filter(!is.na(opdate)) %>%
  group_by(eid)%>%
  summarize(date_op1 = min(opdate, na.rm = TRUE))
pheno4 <- pheno %>% filter (p31=="Female")
pheno4 = operations4%>%
  dplyr::select(eid, date_op1)%>%
  right_join(pheno4, by = 'eid')

## run functions
D4_cens = run_stitch_cens(b_cancers, brca2, pheno4)
survres4_cens<-survfit(Surv(AGEATSTART,TE, res) ~ GR, data=D4_cens)
fit4_cens <-coxph(Surv(AGEATSTART,TE, res) ~ GR+ bmis+ p34+ smokestat, data=D4_cens)
summary (fit4_cens)
survival_plot(survres4_cens, D4, "Time to Breast Cancer with Censoring", "event", 20, .8)

# ovarian cancer
o_cancers<-"C56|^C56|^1830|'1830"
STARTYEAR<-1970
operations5= operations%>%filter(surgery_category == "Total Hysterectomy" | surgery_category == "Bilateral salpingo_oopherectomy" | surgery_category == "Bilateral oopherectomy")
operations5$opdate = as.Date(operations5$opdate, format = '%m/%d/%y')
operations5 = operations5%>%
  filter(!is.na(opdate)) %>%
  group_by(eid)%>%
  summarize(date_op1 = min(opdate, na.rm = TRUE))
pheno5 <- pheno %>% filter (p31=="Female")
pheno5 = operations5%>%
  dplyr::select(eid, date_op1)%>%
  right_join(pheno5, by = 'eid')

## run functions
D5_cens = run_stitch_cens(o_cancers, brca2, pheno5)
survres5_cens<-survfit(Surv(AGEATSTART,TE, res) ~ GR, data=D5_cens)
fit5_cens <- coxph(Surv(AGEATSTART,TE, res) ~ GR+ bmis+ p34+ smokestat, data=D5_cens) # remove gender since only women
summary (fit5_cens)
survival_plot(survres5_cens, D5_cens, "Time to Ovarian Cancer with Censoring", "event", 40, .3)
survival_plot(survres5_cens, D5_cens, "Time to Ovarian Cancer with Censoring", "event", 20, .8)