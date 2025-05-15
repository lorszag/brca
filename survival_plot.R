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

survival_plot_fam = function(survres, data, title, fun, agemin, ylim){
  ggsurvplot(survres, 
             data = data ,
             fun = fun,
             censor = FALSE,
             ggtheme = theme_light(),  ##get graph with light grid
             font.x = c(18,"plain", "black"),
             font.y = c(18,"plain", "black"),
             font.legend = c(18,"plain", "black"),
             legend.labs = c('Carrier + History', 'Just Carrier', "Just History", "Neither"),
             font.tickslab = c(18,"plain", "black"),
             title = title,  ##use to id plots, will crop out of final figures
             conf.int = 'T',           # confidence intervals for point estimates of survival 
             #conf.int.style = "step",  # customize style of confidence intervals
             xlab = "Age (Years)",    # customize X axis label.
             xlim = c(agemin ,90),
             ylim = c(0,ylim),
             break.y.by = 0.2,
             break.x.by = 10,      # break X axis in time intervals by 10
             palette = c('#E7B800', '#2E9FDF', "orange2", "cornflowerblue"), ## for noncarrier and 1 carrier group
             risk.table = "nrisk_cumcensor", ##keep censor table; some journals want themm we can crop out if we dont need them
             tables.height = 0.3,
             tables.y.text = TRUE,
             tables.fontsize = 2,
             tables.theme = theme_cleantable()
  )
}

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

run_stitch = function(cancers, cases, pheno){
  R1<-disc(pheno,regcodes,cancers)
  D1<-stitch(pheno,cases[,1],controls[,1],R1[[1]])
  # pull stop year if applicable
  STOPYEAR<-substr(D1$p191,1,4)
  STOPYEAR[!STOPYEAR>0|is.na(STOPYEAR)]<-CONSTANTYEAR #fill w const year when not applicable
  STOPYEAR<-as.numeric(STOPYEAR)
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
