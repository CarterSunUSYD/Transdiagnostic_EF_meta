Sys.which("make")
#Step 1 Load All packages first
rm(list=ls())

## Loading library
library(robumeta)
library("readxl")
library(stringr)
library(dplyr)
library(metafor) 
library(effsize)
library("writexl")
# library("xlsx")
library(fishmethods) 
library(here)
library(janitor)
library(purrr)

#load other libraries if needed
library(ggplot2)
# remove.packages("ggplot2")
library(tidyverse)
#install.packages("ggthemes")
library(ggthemes)
library (netmeta)
library (mada)
library (mvmeta)
library (rmeta)
library (meta)
library (metaSEM)
library("pimeta")
# remove.packages("vctrs")
# install.packages("vctrs")
# install.packages("magrittr")

library(rgl)
# netgraph(m.netmeta, dim = "3d"))
library(clubSandwich)


#load other libraries if needed
library(ggplot2)
#install.packages("ggthemes")
library(ggthemes)
library (netmeta)
library (mada)
library (mvmeta)
library (rmeta)
library (meta)
library (metaSEM)

#Network Diagram
library (networkD3)
library (igraph)
library(htmlwidgets) 

## Plotting results 

library(rgl)
# netgraph(m.netmeta, dim = "3d")
library(clubSandwich)


NDDx_comparision_Rawdata <- read_excel ("PATH_TO_DATAFILE.xlsx")
View(NDDx_comparision_Rawdata)

## converting EF direction column from string to numeric type, and save data into separate files.
i<-c(115:121)
NDDx_comparision_Rawdata[,i]<-apply(NDDx_comparision_Rawdata[,i],2,function(x) as.numeric(as.character(x)))
ADHD_SLD_Rawdata <-subset(NDDx_comparision_Rawdata, `ADHD_SLD` > 0) 
ADHD_DCD_Rawdata <-subset(NDDx_comparision_Rawdata, `ADHD_DCD` > 0) 
ASD_SLD_Rawdata <-subset(NDDx_comparision_Rawdata, `ASD_SLD` > 0) 
DS_WS_Rawdata <-subset(NDDx_comparision_Rawdata, `DS_WS` > 0) 
ADHD_ASD_Rawdata <-subset(NDDx_comparision_Rawdata, `ADHD_ASD` > 0) 
ADHD_CTD_Rawdata <-subset(NDDx_comparision_Rawdata, `ADHD_CTD` > 0) 
###
nddx12_comparision<-function(df,group1,group2,ef_col_no){
  new_dataframe <-data.frame(
    "Study No."=character(),
    "Study_Year."=character(),
    "Author Name"=character(),
    "Year of Publication"=character(),
    "NDDx Groups"=character(),
    "EF Measure"=character(),
    "EF Domain"=character(),
    "EF_Domain_No"=character(),
    "Test presentation format"=character(),
    "Participant response format" = character(),
    "NDDX1"=character(),
    "NDDX1_mean"=character(),
    "NDDX1_std" = character(),
    "NDDX1_samplesize" = character(),
    "NDDX2"=character(),
    "NDDX2_mean" = character(),
    "NDDX2_std" = character(),
    "NDDX2_samplesize" = character(),
    "EF_Direction"=character(),
    "Informant_Performance"=character(),
    stringsAsFactors = FALSE)
  
  row_index <- 1  # Initialize row index for new_dataframe
  group1_flag<-0
  group2_flag<-0
  for (i in 1:nrow(df)){
    has_nddx1 <- FALSE
    has_nddx2 <- FALSE
    nddx_pairs <- character(8)  # Initialize a vector with length 8
    
    for (j in 11:ncol(df)){
      
      if(!is.na(df[[i, j]]) && as.character(df[i,j]) ==group1){#} && !is.na(ADHD_ASD_Rawdata[i,j+1])&& !is.na(ADHD_ASD_Rawdata[i,j+2])&& !is.na(ADHD_ASD_Rawdata[i,j+3])){
        nddx_pairs[1:4] <- as.character(df[i, j:(j+3)])
        group1_flag<-i
        has_nddx1<-TRUE
      }
      if (!is.na(df[[i, j]]) && as.character(df[i,j]) ==group2){#&& !is.na(ADHD_ASD_Rawdata[i,j+1])&& !is.na(ADHD_ASD_Rawdata[i,j+2])&& !is.na(ADHD_ASD_Rawdata[i,j+3])){
        nddx_pairs[5:8] <- as.character(df[i, j:(j+3)])
        has_nddx2 <- TRUE
        group2_flag<-i
      }
    }
    if ((has_nddx1 && has_nddx2) && (group1_flag ==group2_flag)&&(group1_flag ==i)){
      # print(paste("group1_flag:", group1_flag, "group2_flag:", group2_flag, "i:", i))
      # print(nddx_pairs)
      
      new_dataframe[row_index,11:18]<-nddx_pairs
      
      new_dataframe[row_index, 1:10] <- as.character(df[i, 1:10])
      
      new_dataframe[row_index, 19] <- as.character(df[i, ef_col_no])
      new_dataframe[row_index, 20]<-as.character(df[i,76])
      # print(new_dataframe[row_index,])
      
      row_index<-row_index+1
      # new_dataframe<-rbind(new_dataframe,as.data.frame(t(nddx_pairs),stringsAsFactors = FALSE))
      
    }
  }
  return (new_dataframe)
}
## To extract each NDDx comparision group data, use the nddx12_comparision function written above.
### The function requires four inputs (raw dataframe, name of the first comparision group, name of the second group, ef direction coding)
### Note that for ef direction coding, 1 - group on the left is better, 2- group on the right is better, 3- equal.
ADHD_ASD_group<-nddx12_comparision(ADHD_ASD_Rawdata,"ADHD","ASD",117)
i<-c(12:14)
ADHD_ASD_group[,i]<-apply(ADHD_ASD_group[,i],2,function(x) as.numeric(as.character(x)))
i<-c(16:19)
ADHD_ASD_group[,i]<-apply(ADHD_ASD_group[,i],2,function(x) as.numeric(as.character(x)))
# i<-c(20)
# ADHD_ASD_group[,i]<-apply(ADHD_ASD_group[,i],2,function(x) as.numeric(as.character(x)))

ADHD_SLD_group<-nddx12_comparision(ADHD_SLD_Rawdata,"ADHD","SLD",115)
i<-c(12:14)
ADHD_SLD_group[,i]<-apply(ADHD_SLD_group[,i],2,function(x) as.numeric(as.character(x)))
i<-c(16:19)
ADHD_SLD_group[,i]<-apply(ADHD_SLD_group[,i],2,function(x) as.numeric(as.character(x)))
# i<-c(20)
# ADHD_SLD_group[,i]<-apply(ADHD_SLD_group[,i],2,function(x) as.numeric(as.character(x)))


ADHD_CTD_group<-nddx12_comparision(ADHD_CTD_Rawdata,"ADHD","CTD",121)
i<-c(12:14)
ADHD_CTD_group[,i]<-apply(ADHD_CTD_group[,i],2,function(x) as.numeric(as.character(x)))
i<-c(16:19)
ADHD_CTD_group[,i]<-apply(ADHD_CTD_group[,i],2,function(x) as.numeric(as.character(x)))
# i<-c(20)
# ADHD_CTD_group[,i]<-apply(ADHD_CTD_group[,i],2,function(x) as.numeric(as.character(x)))


ADHD_DCD_group<-nddx12_comparision(ADHD_DCD_Rawdata,"ADHD","DCD",116)
i<-c(12:14)
ADHD_DCD_group[,i]<-apply(ADHD_DCD_group[,i],2,function(x) as.numeric(as.character(x)))
i<-c(16:19)
ADHD_DCD_group[,i]<-apply(ADHD_DCD_group[,i],2,function(x) as.numeric(as.character(x)))
# i<-c(20)
# ADHD_DCD_group[,i]<-apply(ADHD_DCD_group[,i],2,function(x) as.numeric(as.character(x)))


ASD_SLD_group<-nddx12_comparision(ADHD_SLD_Rawdata,"ASD","SLD",119)
i<-c(12:14)
ASD_SLD_group[,i]<-apply(ASD_SLD_group[,i],2,function(x) as.numeric(as.character(x)))
i<-c(16:19)
ASD_SLD_group[,i]<-apply(ASD_SLD_group[,i],2,function(x) as.numeric(as.character(x)))
# i<-c(20)
# ASD_SLD_group[,i]<-apply(ASD_SLD_group[,i],2,function(x) as.numeric(as.character(x)))

DS_WS_group<-nddx12_comparision(DS_WS_Rawdata,"DS","WS",118)
i<-c(12:14)
DS_WS_group[,i]<-apply(DS_WS_group[,i],2,function(x) as.numeric(as.character(x)))
i<-c(16:19)
DS_WS_group[,i]<-apply(DS_WS_group[,i],2,function(x) as.numeric(as.character(x)))

# Meta-analysis
## ADHD_ASD_group
ADHD_ASD_EffectSize<- escalc(measure="SMD",m1i=ADHD_ASD_group$NDDX1_mean, sd1i=ADHD_ASD_group$NDDX1_std,n1i=ADHD_ASD_group$NDDX1_samplesize,m2i=ADHD_ASD_group$NDDX2_mean,sd2i=ADHD_ASD_group$NDDX2_std,n2i=ADHD_ASD_group$NDDX2_samplesize
                                        ,slab=paste(ADHD_ASD_group$Study.No.,sep=""))
new_yi <- ADHD_ASD_EffectSize$yi
View(ADHD_ASD_EffectSize)

for (i in 1:length(new_yi)){
  if(ADHD_ASD_group$`EF_Direction`[i] == 1 && new_yi[i] > 0){
    new_yi[i] = -new_yi[i]
  } else if(ADHD_ASD_group$`EF_Direction`[i] == 2 && new_yi[i] < 0){
    new_yi[i] = -new_yi[i]
  }
}

ADHD_ASD_EffectSize$new_yi <- new_yi
ADHD_ASD_EffectSize$`EF_Direction` <- ADHD_ASD_group$`EF_Direction`

View(ADHD_ASD_EffectSize)

ADHD_ASD_group$yi <- ADHD_ASD_EffectSize$new_yi
ADHD_ASD_group$vi <- ADHD_ASD_EffectSize$vi

#2-Level Meta-Anlysis
study_level_metaanalysis_ADHD_ASD<- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=ADHD_ASD_group
                     ,slab=paste(ADHD_ASD_group$`Study.No.`,sep=""))
print(study_level_metaanalysis_ADHD_ASD, digits=3)
predict(study_level_metaanalysis_ADHD_ASD)


#Second level Analysis
##test for loop
perdomain_meta_results_ADHD_ASD <-c()
EF_domains_ADHD_ASD_pooled_ef <- list()
EF_domains_ADHD_ASD_pooled_ci_lb <- list()
EF_domains_ADHD_ASD_pooled_ci_ub <- list()
EF_domains_ADHD_ASD_pooled_k <- list()
EF_domains_ADHD_ASD_pooled_N <- list()
EF_domains_ADHD_ASD_pooled_p <- list()
temp <-list()
flag = 1
for (domain_label in unique(ADHD_ASD_group$EF.Domain)){
  row_basedon_ef_domain <- c()
  row_basedon_ef_domain <-ADHD_ASD_group %>% filter(ADHD_ASD_group$EF.Domain == domain_label )
  if (nrow(row_basedon_ef_domain)>=3){
    #     # print(row_basedon_ef_domain)
    print(domain_label)
    perdomain_meta_results_ADHD_ASD[[domain_label]] <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=row_basedon_ef_domain, slab=paste(row_basedon_ef_domain$`Study.No.`,sep=""))
    print(perdomain_meta_results_ADHD_ASD[[domain_label]], digits=3)
    print(metafor::forest(perdomain_meta_results_ADHD_ASD[[domain_label]],addpred=TRUE,header=TRUE))
    
    # Extract pooled effect size from each domain_label
    pooled_effect <- perdomain_meta_results_ADHD_ASD[[flag]]$b
    pooled_effect_ci_lb <- perdomain_meta_results_ADHD_ASD[[flag]]$ci.lb
    pooled_effect_ci_ub <- perdomain_meta_results_ADHD_ASD[[flag]]$ci.ub
    pooled_effect_k <- perdomain_meta_results_ADHD_ASD[[flag]]$k
    pooled_effect_N <- perdomain_meta_results_ADHD_ASD[[flag]]$s.nlevels
    pooled_effect_p <- perdomain_meta_results_ADHD_ASD[[flag]]$pval
    
    EF_domains_ADHD_ASD_pooled_ef[[flag]]<-pooled_effect
    EF_domains_ADHD_ASD_pooled_ci_lb[[flag]]<-pooled_effect_ci_lb
    EF_domains_ADHD_ASD_pooled_ci_ub[[flag]]<-pooled_effect_ci_ub
    EF_domains_ADHD_ASD_pooled_k[[flag]]<-pooled_effect_k
    EF_domains_ADHD_ASD_pooled_N[[flag]]<-pooled_effect_N
    
    pooled_effect_p <- perdomain_meta_results_ADHD_ASD[[flag]]$pval
    
    if (pooled_effect_p < 0.001) {
      result <- "<0.001"
    } else if (pooled_effect_p < 0.01) {
      result <- "<0.01"
    } else if (pooled_effect_p < 0.05) {
      result <- "<0.05"
    } else {
      result <- format(pooled_effect_p, digits = 3)
    }
    
    EF_domains_ADHD_ASD_pooled_p[[flag]] <- result
    
    # Create a data frame of pooled effects
    current <- data.frame(
      ef_domain = domain_label,
      estimate = unlist(EF_domains_ADHD_ASD_pooled_ef[[flag]][,1]),
      lower_ci = unlist(EF_domains_ADHD_ASD_pooled_ci_lb[[flag]]),
      upper_ci  = unlist(EF_domains_ADHD_ASD_pooled_ci_ub[[flag]]),
      k = unlist(EF_domains_ADHD_ASD_pooled_k[[flag]]),
      N  = unlist(EF_domains_ADHD_ASD_pooled_N[[flag]]),
      pval = unlist(EF_domains_ADHD_ASD_pooled_p[[flag]]),
      comparison ="ADHD vs ASD"
    )
    temp[[flag]]<-current
    
    flag = flag +1
    
  }
}
ADHD_ASD_efdomains_dataframe<-do.call(rbind,temp)
ADHD_ASD_efdomains_dataframe<- ADHD_ASD_efdomains_dataframe  %>% mutate(ADHD_ASD_efdomains_dataframe,
                                                                                    brackets = sprintf("%.2f (%.2f, %.2f)",estimate,lower_ci,upper_ci))

## Performance Based ADHD_ASD
ADHD_ASD_group_performance_only <-subset(ADHD_ASD_group, Informant_Performance == 0)
#ASDvsADHD Effect Size Calculations


ADHD_ASD_EffectSize_perfonly<- escalc(measure="SMD",m1i=ADHD_ASD_group_performance_only$NDDX1_mean, sd1i=ADHD_ASD_group_performance_only$NDDX1_std,n1i=ADHD_ASD_group_performance_only$NDDX1_samplesize,m2i=ADHD_ASD_group_performance_only$NDDX2_mean,sd2i=ADHD_ASD_group_performance_only$NDDX2_std,n2i=ADHD_ASD_group_performance_only$NDDX2_samplesize
                             ,slab=paste(ADHD_ASD_group_performance_only$Study.No.,sep=""))
perfonly_yi <- ADHD_ASD_EffectSize_perfonly$yi
View(ADHD_ASD_EffectSize_perfonly)

for (i in 1:length(perfonly_yi)){
  if(ADHD_ASD_group_performance_only$`EF_Direction`[i] == 1 && perfonly_yi[i] > 0){
    perfonly_yi[i] = -perfonly_yi[i]
  } else if(ADHD_ASD_group_performance_only$`EF_Direction`[i] == 2 && perfonly_yi[i] < 0){
    perfonly_yi[i] = -perfonly_yi[i]
  }
}

ADHD_ASD_EffectSize_perfonly$perfonly_yi <- perfonly_yi
ADHD_ASD_EffectSize_perfonly$`EF_Direction` <- ADHD_ASD_group_performance_only$`EF_Direction`

View(ADHD_ASD_EffectSize_perfonly)

ADHD_ASD_group_performance_only$perfonly_yi <- ADHD_ASD_EffectSize_perfonly$perfonly_yi
ADHD_ASD_group_performance_only$perfonly_vi <- ADHD_ASD_EffectSize_perfonly$vi

#2-Level Meta-Anlysis
study_level_metaanalysis_ADHD_ASD_perfonly<- rma.mv(perfonly_yi, perfonly_vi, random = ~ 1 |`Study.No.`, data=ADHD_ASD_group_performance_only
                                           ,slab=paste(ADHD_ASD_group_performance_only$`Study.No.`,sep=""))
print(study_level_metaanalysis_ADHD_ASD_perfonly, digits=3)


#Second level Analysis
##test for loop
perdomain_meta_results_ADHD_ASD_perfonly <-c()
EF_domains_ADHD_ASD_perfonly_pooled_ef <- list()
EF_domains_ADHD_ASD_perfonly_pooled_ci_lb <- list()
EF_domains_ADHD_ASD_perfonly_pooled_ci_ub <- list()
EF_domains_ADHD_ASD_perfonly_pooled_k <- list()
EF_domains_ADHD_ASD_perfonly_pooled_N <- list()
EF_domains_ADHD_ASD_perfonly_pooled_p <- list()
temp <-list()
flag = 1
for (domain_label in unique(ADHD_ASD_group_performance_only$EF.Domain)){
  row_basedon_ef_domain <- c()
  row_basedon_ef_domain <-ADHD_ASD_group_performance_only %>% filter(ADHD_ASD_group_performance_only$EF.Domain == domain_label )
  if (nrow(row_basedon_ef_domain)>=3){
    #     # print(row_basedon_ef_domain)
    print(domain_label)
    perdomain_meta_results_ADHD_ASD_perfonly[[domain_label]] <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=row_basedon_ef_domain, slab=paste(row_basedon_ef_domain$`Study.No.`,sep=""))
    print(perdomain_meta_results_ADHD_ASD_perfonly[[domain_label]], digits=3)
    print(metafor::forest(perdomain_meta_results_ADHD_ASD_perfonly[[domain_label]],addpred=TRUE,header=TRUE))
    
        # Extract pooled effect size from each domain_label
    pooled_effect <- perdomain_meta_results_ADHD_ASD_perfonly[[flag]]$b
    pooled_effect_ci_lb <- perdomain_meta_results_ADHD_ASD_perfonly[[flag]]$ci.lb
    pooled_effect_ci_ub <- perdomain_meta_results_ADHD_ASD_perfonly[[flag]]$ci.ub
    pooled_effect_k <- perdomain_meta_results_ADHD_ASD_perfonly[[flag]]$k
    pooled_effect_N <- perdomain_meta_results_ADHD_ASD_perfonly[[flag]]$s.nlevels
    pooled_effect_p <- perdomain_meta_results_ADHD_ASD_perfonly[[flag]]$pval
    
    EF_domains_ADHD_ASD_perfonly_pooled_ef[[flag]]<-pooled_effect
    EF_domains_ADHD_ASD_perfonly_pooled_ci_lb[[flag]]<-pooled_effect_ci_lb
    EF_domains_ADHD_ASD_perfonly_pooled_ci_ub[[flag]]<-pooled_effect_ci_ub
    EF_domains_ADHD_ASD_perfonly_pooled_k[[flag]]<-pooled_effect_k
    EF_domains_ADHD_ASD_perfonly_pooled_N[[flag]]<-pooled_effect_N
    
    pooled_effect_p <- perdomain_meta_results_ADHD_ASD_perfonly[[flag]]$pval
    
    if (pooled_effect_p < 0.001) {
      result <- "<0.001"
    } else if (pooled_effect_p < 0.01) {
      result <- "<0.01"
    } else if (pooled_effect_p < 0.05) {
      result <- "<0.05"
    } else {
      result <- format(pooled_effect_p, digits = 3)
    }
    
    EF_domains_ADHD_ASD_perfonly_pooled_p[[flag]] <- result
    
    # Create a data frame of pooled effects
    current <- data.frame(
      ef_domain = domain_label,
      estimate = unlist(EF_domains_ADHD_ASD_perfonly_pooled_ef[[flag]][,1]),
      lower_ci = unlist(EF_domains_ADHD_ASD_perfonly_pooled_ci_lb[[flag]]),
      upper_ci  = unlist(EF_domains_ADHD_ASD_perfonly_pooled_ci_ub[[flag]]),
      k = unlist(EF_domains_ADHD_ASD_perfonly_pooled_k[[flag]]),
      N  = unlist(EF_domains_ADHD_ASD_perfonly_pooled_N[[flag]]),
      pval = unlist(EF_domains_ADHD_ASD_perfonly_pooled_p[[flag]]),
      comparison ="ADHD vs ASD"
    )
    temp[[flag]]<-current
    
    flag = flag +1
    
  }

}
ADHD_ASD_perfonly_efdomains_dataframe<-do.call(rbind,temp)
ADHD_ASD_perfonly_efdomains_dataframe<- ADHD_ASD_perfonly_efdomains_dataframe  %>% mutate(ADHD_ASD_perfonly_efdomains_dataframe,
                                                                        brackets = sprintf("%.2f (%.2f, %.2f)",estimate,lower_ci,upper_ci))

## Informant Based ADHD_ASD
ADHD_ASD_group_informant_only <-subset(ADHD_ASD_group, Informant_Performance == 1)
#ASDvsADHD Effect Size Calculations


ADHD_ASD_EffectSize_informantonly<- escalc(measure="SMD",m1i=ADHD_ASD_group_informant_only$NDDX1_mean, sd1i=ADHD_ASD_group_informant_only$NDDX1_std,n1i=ADHD_ASD_group_informant_only$NDDX1_samplesize,m2i=ADHD_ASD_group_informant_only$NDDX2_mean,sd2i=ADHD_ASD_group_informant_only$NDDX2_std,n2i=ADHD_ASD_group_informant_only$NDDX2_samplesize
                                      ,slab=paste(ADHD_ASD_group_informant_only$Study.No.,sep=""))
informantonly_yi <- ADHD_ASD_EffectSize_informantonly$yi
View(ADHD_ASD_EffectSize_informantonly)

for (i in 1:length(informantonly_yi)){
  if(ADHD_ASD_group_informant_only$`EF_Direction`[i] == 1 && informantonly_yi[i] > 0){
    informantonly_yi[i] = -informantonly_yi[i]
  } else if(ADHD_ASD_group_informant_only$`EF_Direction`[i] == 2 && informantonly_yi[i] < 0){
    informantonly_yi[i] = -informantonly_yi[i]
  }
}

ADHD_ASD_EffectSize_informantonly$informantonly_yi <- informantonly_yi
ADHD_ASD_EffectSize_informantonly$`EF_Direction` <- ADHD_ASD_group_informant_only$`EF_Direction`

View(ADHD_ASD_EffectSize_informantonly)

ADHD_ASD_group_informant_only$informantonly_yi <- ADHD_ASD_EffectSize_informantonly$informantonly_yi
ADHD_ASD_group_informant_only$informantonly_vi <- ADHD_ASD_EffectSize_informantonly$vi

#2-Level Meta-Anlysis
study_level_metaanalysis_ADHD_ASD_informantonly<- rma.mv(informantonly_yi, informantonly_vi, random = ~ 1 |`Study.No.`, data=ADHD_ASD_group_informant_only
                                                    ,slab=paste(ADHD_ASD_group_informant_only$`Study.No.`,sep=""))
print(study_level_metaanalysis_ADHD_ASD_informantonly, digits=3)

#Second level Analysis
##test for loop
perdomain_meta_results_ADHD_ASD_informantonly <-c()
EF_domains_ADHD_ASD_informantonly_pooled_ef <- list()
EF_domains_ADHD_ASD_informantonly_pooled_ci_lb <- list()
EF_domains_ADHD_ASD_informantonly_pooled_ci_ub <- list()
EF_domains_ADHD_ASD_informantonly_pooled_k <- list()
EF_domains_ADHD_ASD_informantonly_pooled_N <- list()
EF_domains_ADHD_ASD_informantonly_pooled_p <- list()
temp <-list()
flag = 1
for (domain_label in unique(ADHD_ASD_group_informant_only$EF.Domain)){
  row_basedon_ef_domain <- c()
  row_basedon_ef_domain <-ADHD_ASD_group_informant_only %>% filter(ADHD_ASD_group_informant_only$EF.Domain == domain_label )
  if (nrow(row_basedon_ef_domain)>=3){
    #     # print(row_basedon_ef_domain)
    print(domain_label)
    perdomain_meta_results_ADHD_ASD_informantonly[[domain_label]] <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=row_basedon_ef_domain, slab=paste(row_basedon_ef_domain$`Study.No.`,sep=""))
    print(perdomain_meta_results_ADHD_ASD_informantonly[[domain_label]], digits=3)
    print(metafor::forest(perdomain_meta_results_ADHD_ASD_informantonly[[domain_label]],addpred=TRUE,header=TRUE))
    
    # Extract pooled effect size from each domain_label
    pooled_effect <- perdomain_meta_results_ADHD_ASD_informantonly[[flag]]$b
    pooled_effect_ci_lb <- perdomain_meta_results_ADHD_ASD_informantonly[[flag]]$ci.lb
    pooled_effect_ci_ub <- perdomain_meta_results_ADHD_ASD_informantonly[[flag]]$ci.ub
    pooled_effect_k <- perdomain_meta_results_ADHD_ASD_informantonly[[flag]]$k
    pooled_effect_N <- perdomain_meta_results_ADHD_ASD_informantonly[[flag]]$s.nlevels
    pooled_effect_p <- perdomain_meta_results_ADHD_ASD_informantonly[[flag]]$pval
    
    EF_domains_ADHD_ASD_informantonly_pooled_ef[[flag]]<-pooled_effect
    EF_domains_ADHD_ASD_informantonly_pooled_ci_lb[[flag]]<-pooled_effect_ci_lb
    EF_domains_ADHD_ASD_informantonly_pooled_ci_ub[[flag]]<-pooled_effect_ci_ub
    EF_domains_ADHD_ASD_informantonly_pooled_k[[flag]]<-pooled_effect_k
    EF_domains_ADHD_ASD_informantonly_pooled_N[[flag]]<-pooled_effect_N
    
    pooled_effect_p <- perdomain_meta_results_ADHD_ASD_informantonly[[flag]]$pval
    
    if (pooled_effect_p < 0.001) {
      result <- "<0.001"
    } else if (pooled_effect_p < 0.01) {
      result <- "<0.01"
    } else if (pooled_effect_p < 0.05) {
      result <- "<0.05"
    } else {
      result <- format(pooled_effect_p, digits = 3)
    }
    
    EF_domains_ADHD_ASD_informantonly_pooled_p[[flag]] <- result
    
    # Create a data frame of pooled effects
    current <- data.frame(
      ef_domain = domain_label,
      estimate = unlist(EF_domains_ADHD_ASD_informantonly_pooled_ef[[flag]][,1]),
      lower_ci = unlist(EF_domains_ADHD_ASD_informantonly_pooled_ci_lb[[flag]]),
      upper_ci  = unlist(EF_domains_ADHD_ASD_informantonly_pooled_ci_ub[[flag]]),
      k = unlist(EF_domains_ADHD_ASD_informantonly_pooled_k[[flag]]),
      N  = unlist(EF_domains_ADHD_ASD_informantonly_pooled_N[[flag]]),
      pval = unlist(EF_domains_ADHD_ASD_informantonly_pooled_p[[flag]]),
      comparison ="ADHD vs ASD"
    )
    temp[[flag]]<-current
    
    flag = flag +1
    
  }
  
}
ADHD_ASD_informantonly_efdomains_dataframe<-do.call(rbind,temp)
ADHD_ASD_informantonly_efdomains_dataframe<- ADHD_ASD_informantonly_efdomains_dataframe  %>% mutate(ADHD_ASD_informantonly_efdomains_dataframe,
                                                                                          brackets = sprintf("%.2f (%.2f, %.2f)",estimate,lower_ci,upper_ci))

## ADHD_CTD
ADHD_CTD_EffectSize<- escalc(measure="SMD",m1i=ADHD_CTD_group$NDDX1_mean, sd1i=ADHD_CTD_group$NDDX1_std,n1i=ADHD_CTD_group$NDDX1_samplesize,m2i=ADHD_CTD_group$NDDX2_mean,sd2i=ADHD_CTD_group$NDDX2_std,n2i=ADHD_CTD_group$NDDX2_samplesize
                             ,slab=paste(ADHD_CTD_group$Study.No.,sep=""))
new_yi <- ADHD_CTD_EffectSize$yi
View(ADHD_CTD_EffectSize)

for (i in 1:length(new_yi)){
  if(ADHD_CTD_group$`EF_Direction`[i] == 1 && new_yi[i] > 0){
    new_yi[i] = -new_yi[i]
  } else if(ADHD_CTD_group$`EF_Direction`[i] == 2 && new_yi[i] < 0){
    new_yi[i] = -new_yi[i]
  }
}

ADHD_CTD_EffectSize$new_yi <- new_yi
ADHD_CTD_EffectSize$`EF_Direction` <- ADHD_CTD_group$`EF_Direction`

View(ADHD_CTD_EffectSize)

ADHD_CTD_group$yi <- ADHD_CTD_EffectSize$new_yi
ADHD_CTD_group$vi <- ADHD_CTD_EffectSize$vi

#2-Level Meta-Anlysis
study_level_metaanalysis_ADHD_CTD<- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=ADHD_CTD_group
                                           ,slab=paste(ADHD_CTD_group$Study.No.,sep=""))
print(study_level_metaanalysis_ADHD_CTD, digits=3)
predict(study_level_metaanalysis_ADHD_CTD)

#Second level Analysis
##test for loop
perdomain_meta_results_ADHD_CTD <-c()
EF_domains_ADHD_CTD_pooled_ef <- list()
EF_domains_ADHD_CTD_pooled_ci_lb <- list()
EF_domains_ADHD_CTD_pooled_ci_ub <- list()
EF_domains_ADHD_CTD_pooled_k <- list()
EF_domains_ADHD_CTD_pooled_N <- list()
EF_domains_ADHD_CTD_pooled_p <- list()
temp <-list()
flag = 1
for (domain_label in unique(ADHD_CTD_group$EF.Domain)){
  row_basedon_ef_domain <- c()
  row_basedon_ef_domain <-ADHD_CTD_group %>% filter(ADHD_CTD_group$EF.Domain == domain_label )
  if (nrow(row_basedon_ef_domain)>=3){
    # print(row_basedon_ef_domain)
    print(domain_label)
    perdomain_meta_results_ADHD_CTD[[domain_label]] <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=row_basedon_ef_domain, slab=paste(row_basedon_ef_domain$Study.No.,sep=""))
    print(perdomain_meta_results_ADHD_CTD[[domain_label]], digits=3)
    print(metafor::forest(perdomain_meta_results_ADHD_CTD[[domain_label]],addpred=TRUE,header=TRUE))
    
    # Extract pooled effect size from each domain_label
    pooled_effect <- perdomain_meta_results_ADHD_CTD[[flag]]$b
    pooled_effect_ci_lb <- perdomain_meta_results_ADHD_CTD[[flag]]$ci.lb
    pooled_effect_ci_ub <- perdomain_meta_results_ADHD_CTD[[flag]]$ci.ub
    pooled_effect_k <- perdomain_meta_results_ADHD_CTD[[flag]]$k
    pooled_effect_N <- perdomain_meta_results_ADHD_CTD[[flag]]$s.nlevels
    pooled_effect_p <- perdomain_meta_results_ADHD_CTD[[flag]]$pval
    
    EF_domains_ADHD_CTD_pooled_ef[[flag]]<-pooled_effect
    EF_domains_ADHD_CTD_pooled_ci_lb[[flag]]<-pooled_effect_ci_lb
    EF_domains_ADHD_CTD_pooled_ci_ub[[flag]]<-pooled_effect_ci_ub
    EF_domains_ADHD_CTD_pooled_k[[flag]]<-pooled_effect_k
    EF_domains_ADHD_CTD_pooled_N[[flag]]<-pooled_effect_N
    
    pooled_effect_p <- perdomain_meta_results_ADHD_CTD[[flag]]$pval
    
    if (pooled_effect_p < 0.001) {
      result <- "<0.001"
    } else if (pooled_effect_p < 0.01) {
      result <- "<0.01"
    } else if (pooled_effect_p < 0.05) {
      result <- "<0.05"
    } else {
      result <- format(pooled_effect_p, digits = 3)
    }
    
    EF_domains_ADHD_CTD_pooled_p[[flag]] <- result
    
    # Create a data frame of pooled effects
    current <- data.frame(
      ef_domain = domain_label,
      estimate = unlist(EF_domains_ADHD_CTD_pooled_ef[[flag]][,1]),
      lower_ci = unlist(EF_domains_ADHD_CTD_pooled_ci_lb[[flag]]),
      upper_ci  = unlist(EF_domains_ADHD_CTD_pooled_ci_ub[[flag]]),
      k = unlist(EF_domains_ADHD_CTD_pooled_k[[flag]]),
      N  = unlist(EF_domains_ADHD_CTD_pooled_N[[flag]]),
      pval = unlist(EF_domains_ADHD_CTD_pooled_p[[flag]]),
      comparison ="ADHD vs TD"
    )
    temp[[flag]]<-current
    
    flag = flag +1
    
    
  }
}
ADHD_CTD_efdomains_dataframe<-do.call(rbind,temp)
ADHD_CTD_efdomains_dataframe<- ADHD_CTD_efdomains_dataframe  %>% mutate(ADHD_CTD_efdomains_dataframe,
                                                                        brackets = sprintf("%.2f (%.2f, %.2f)",estimate,lower_ci,upper_ci))


## Performance Based ADHD_CTD
ADHD_CTD_group_performance_only <-subset(ADHD_CTD_group, Informant_Performance == 0)
#ASDvsADHD Effect Size Calculations


ADHD_CTD_EffectSize_perfonly<- escalc(measure="SMD",m1i=ADHD_CTD_group_performance_only$NDDX1_mean, sd1i=ADHD_CTD_group_performance_only$NDDX1_std,n1i=ADHD_CTD_group_performance_only$NDDX1_samplesize,m2i=ADHD_CTD_group_performance_only$NDDX2_mean,sd2i=ADHD_CTD_group_performance_only$NDDX2_std,n2i=ADHD_CTD_group_performance_only$NDDX2_samplesize
                                       ,slab=paste(ADHD_CTD_group_performance_only$Study.No.,sep=""))
perfonly_yi <- ADHD_CTD_EffectSize_perfonly$yi
View(ADHD_CTD_EffectSize_perfonly)

for (i in 1:length(perfonly_yi)){
  if(ADHD_CTD_group_performance_only$`EF_Direction`[i] == 1 && perfonly_yi[i] > 0){
    perfonly_yi[i] = -perfonly_yi[i]
  } else if(ADHD_CTD_group_performance_only$`EF_Direction`[i] == 2 && perfonly_yi[i] < 0){
    perfonly_yi[i] = -perfonly_yi[i]
  }
}

ADHD_CTD_EffectSize_perfonly$perfonly_yi <- perfonly_yi
ADHD_CTD_EffectSize_perfonly$`EF_Direction` <- ADHD_CTD_group_performance_only$`EF_Direction`

View(ADHD_CTD_EffectSize_perfonly)

ADHD_CTD_group_performance_only$perfonly_yi <- ADHD_CTD_EffectSize_perfonly$perfonly_yi
ADHD_CTD_group_performance_only$perfonly_vi <- ADHD_CTD_EffectSize_perfonly$vi

#2-Level Meta-Anlysis
study_level_metaanalysis_ADHD_CTD_perfonly<- rma.mv(perfonly_yi, perfonly_vi, random = ~ 1 |`Study.No.`, data=ADHD_CTD_group_performance_only
                                                    ,slab=paste(ADHD_CTD_group_performance_only$Study.No.,sep=""))
print(study_level_metaanalysis_ADHD_CTD_perfonly, digits=3)


#Second level Analysis
##test for loop
perdomain_meta_results_ADHD_CTD_perfonly <-c()
EF_domains_ADHD_CTD_perfonly_pooled_ef <- list()
EF_domains_ADHD_CTD_perfonly_pooled_ci_lb <- list()
EF_domains_ADHD_CTD_perfonly_pooled_ci_ub <- list()
EF_domains_ADHD_CTD_perfonly_pooled_k <- list()
EF_domains_ADHD_CTD_perfonly_pooled_N <- list()
EF_domains_ADHD_CTD_perfonly_pooled_p <- list()
temp <-list()
flag = 1
for (domain_label in unique(ADHD_CTD_group_performance_only$EF.Domain)){
  row_basedon_ef_domain <- c()
  row_basedon_ef_domain <-ADHD_CTD_group_performance_only %>% filter(ADHD_CTD_group_performance_only$EF.Domain == domain_label )
  if (nrow(row_basedon_ef_domain)>=3){
    # print(row_basedon_ef_domain)
    print(domain_label)
    perdomain_meta_results_ADHD_CTD_perfonly[[domain_label]] <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=row_basedon_ef_domain, slab=paste(row_basedon_ef_domain$Study.No.,sep=""))
    print(perdomain_meta_results_ADHD_CTD_perfonly[[domain_label]], digits=3)
    print(metafor::forest(perdomain_meta_results_ADHD_CTD_perfonly[[domain_label]],addpred=TRUE,header=TRUE))
    # Extract pooled effect size from each domain_label
    pooled_effect <- perdomain_meta_results_ADHD_CTD_perfonly[[flag]]$b
    pooled_effect_ci_lb <- perdomain_meta_results_ADHD_CTD_perfonly[[flag]]$ci.lb
    pooled_effect_ci_ub <- perdomain_meta_results_ADHD_CTD_perfonly[[flag]]$ci.ub
    pooled_effect_k <- perdomain_meta_results_ADHD_CTD_perfonly[[flag]]$k
    pooled_effect_N <- perdomain_meta_results_ADHD_CTD_perfonly[[flag]]$s.nlevels
    pooled_effect_p <- perdomain_meta_results_ADHD_CTD_perfonly[[flag]]$pval
    
    EF_domains_ADHD_CTD_perfonly_pooled_ef[[flag]]<-pooled_effect
    EF_domains_ADHD_CTD_perfonly_pooled_ci_lb[[flag]]<-pooled_effect_ci_lb
    EF_domains_ADHD_CTD_perfonly_pooled_ci_ub[[flag]]<-pooled_effect_ci_ub
    EF_domains_ADHD_CTD_perfonly_pooled_k[[flag]]<-pooled_effect_k
    EF_domains_ADHD_CTD_perfonly_pooled_N[[flag]]<-pooled_effect_N
    
    pooled_effect_p <- perdomain_meta_results_ADHD_CTD_perfonly[[flag]]$pval
    
    if (pooled_effect_p < 0.001) {
      result <- "<0.001"
    } else if (pooled_effect_p < 0.01) {
      result <- "<0.01"
    } else if (pooled_effect_p < 0.05) {
      result <- "<0.05"
    } else {
      result <- format(pooled_effect_p, digits = 3)
    }
    
    EF_domains_ADHD_CTD_perfonly_pooled_p[[flag]] <- result
    
    # Create a data frame of pooled effects
    current <- data.frame(
      ef_domain = domain_label,
      estimate = unlist(EF_domains_ADHD_CTD_perfonly_pooled_ef[[flag]][,1]),
      lower_ci = unlist(EF_domains_ADHD_CTD_perfonly_pooled_ci_lb[[flag]]),
      upper_ci  = unlist(EF_domains_ADHD_CTD_perfonly_pooled_ci_ub[[flag]]),
      k = unlist(EF_domains_ADHD_CTD_perfonly_pooled_k[[flag]]),
      N  = unlist(EF_domains_ADHD_CTD_perfonly_pooled_N[[flag]]),
      pval = unlist(EF_domains_ADHD_CTD_perfonly_pooled_p[[flag]]),
      comparison ="ADHD vs TD"
    )
    temp[[flag]]<-current
    
    flag = flag +1
    
  }
}
ADHD_CTD_perfonly_efdomains_dataframe<-do.call(rbind,temp)
ADHD_CTD_perfonly_efdomains_dataframe<- ADHD_CTD_perfonly_efdomains_dataframe  %>% mutate(ADHD_CTD_perfonly_efdomains_dataframe,
                                                                                          brackets = sprintf("%.2f (%.2f, %.2f)",estimate,lower_ci,upper_ci))

## Informant Based ADHD_CTD
ADHD_CTD_group_Informant_only <-subset(ADHD_CTD_group, Informant_Performance == 1)
#ASDvsADHD Effect Size Calculations


ADHD_CTD_EffectSize_informantonly<- escalc(measure="SMD",m1i=ADHD_CTD_group_Informant_only$NDDX1_mean, sd1i=ADHD_CTD_group_Informant_only$NDDX1_std,n1i=ADHD_CTD_group_Informant_only$NDDX1_samplesize,m2i=ADHD_CTD_group_Informant_only$NDDX2_mean,sd2i=ADHD_CTD_group_Informant_only$NDDX2_std,n2i=ADHD_CTD_group_Informant_only$NDDX2_samplesize
                                      ,slab=paste(ADHD_CTD_group_Informant_only$Study.No.,sep=""))
informantonly_yi <- ADHD_CTD_EffectSize_informantonly$yi
View(ADHD_CTD_EffectSize_informantonly)

for (i in 1:length(informantonly_yi)){
  if(ADHD_CTD_group_Informant_only$`EF_Direction`[i] == 1 && informantonly_yi[i] > 0){
    informantonly_yi[i] = -informantonly_yi[i]
  } else if(ADHD_CTD_group_Informant_only$`EF_Direction`[i] == 2 && informantonly_yi[i] < 0){
    informantonly_yi[i] = -informantonly_yi[i]
  }
}

ADHD_CTD_EffectSize_informantonly$informantonly_yi <- informantonly_yi
ADHD_CTD_EffectSize_informantonly$`EF_Direction` <- ADHD_CTD_group_Informant_only$`EF_Direction`

View(ADHD_CTD_EffectSize_informantonly)

ADHD_CTD_group_Informant_only$informantonly_yi <- ADHD_CTD_EffectSize_informantonly$informantonly_yi
ADHD_CTD_group_Informant_only$informantonly_vi <- ADHD_CTD_EffectSize_informantonly$vi

#2-Level Meta-Anlysis
study_level_metaanalysis_ADHD_CTD_informantonly<- rma.mv(informantonly_yi, informantonly_vi, random = ~ 1 |`Study.No.`, data=ADHD_CTD_group_Informant_only
                                                    ,slab=paste(ADHD_CTD_group_Informant_only$Study.No.,sep=""))
print(study_level_metaanalysis_ADHD_CTD_informantonly, digits=3)


#Second level Analysis
##test for loop
perdomain_meta_results_ADHD_CTD_informantonly <-c()
EF_domains_ADHD_CTD_informantonly_pooled_ef <- list()
EF_domains_ADHD_CTD_informantonly_pooled_ci_lb <- list()
EF_domains_ADHD_CTD_informantonly_pooled_ci_ub <- list()
EF_domains_ADHD_CTD_informantonly_pooled_k <- list()
EF_domains_ADHD_CTD_informantonly_pooled_N <- list()
EF_domains_ADHD_CTD_informantonly_pooled_p <- list()
temp <-list()
flag = 1
for (domain_label in unique(ADHD_CTD_group_Informant_only$EF.Domain)){
  row_basedon_ef_domain <- c()
  row_basedon_ef_domain <-ADHD_CTD_group_Informant_only %>% filter(ADHD_CTD_group_Informant_only$EF.Domain == domain_label )
  if (nrow(row_basedon_ef_domain)>=3){
    # print(row_basedon_ef_domain)
    print(domain_label)
    perdomain_meta_results_ADHD_CTD_informantonly[[domain_label]] <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=row_basedon_ef_domain, slab=paste(row_basedon_ef_domain$Study.No.,sep=""))
    print(perdomain_meta_results_ADHD_CTD_informantonly[[domain_label]], digits=3)
    print(metafor::forest(perdomain_meta_results_ADHD_CTD_informantonly[[domain_label]],addpred=TRUE,header=TRUE))
    # Extract pooled effect size from each domain_label
    pooled_effect <- perdomain_meta_results_ADHD_CTD_informantonly[[flag]]$b
    pooled_effect_ci_lb <- perdomain_meta_results_ADHD_CTD_informantonly[[flag]]$ci.lb
    pooled_effect_ci_ub <- perdomain_meta_results_ADHD_CTD_informantonly[[flag]]$ci.ub
    pooled_effect_k <- perdomain_meta_results_ADHD_CTD_informantonly[[flag]]$k
    pooled_effect_N <- perdomain_meta_results_ADHD_CTD_informantonly[[flag]]$s.nlevels
    pooled_effect_p <- perdomain_meta_results_ADHD_CTD_informantonly[[flag]]$pval
    
    EF_domains_ADHD_CTD_informantonly_pooled_ef[[flag]]<-pooled_effect
    EF_domains_ADHD_CTD_informantonly_pooled_ci_lb[[flag]]<-pooled_effect_ci_lb
    EF_domains_ADHD_CTD_informantonly_pooled_ci_ub[[flag]]<-pooled_effect_ci_ub
    EF_domains_ADHD_CTD_informantonly_pooled_k[[flag]]<-pooled_effect_k
    EF_domains_ADHD_CTD_informantonly_pooled_N[[flag]]<-pooled_effect_N
    
    pooled_effect_p <- perdomain_meta_results_ADHD_CTD_informantonly[[flag]]$pval
    
    if (pooled_effect_p < 0.001) {
      result <- "<0.001"
    } else if (pooled_effect_p < 0.01) {
      result <- "<0.01"
    } else if (pooled_effect_p < 0.05) {
      result <- "<0.05"
    } else {
      result <- format(pooled_effect_p, digits = 3)
    }
    
    EF_domains_ADHD_CTD_informantonly_pooled_p[[flag]] <- result
    
    # Create a data frame of pooled effects
    current <- data.frame(
      ef_domain = domain_label,
      estimate = unlist(EF_domains_ADHD_CTD_informantonly_pooled_ef[[flag]][,1]),
      lower_ci = unlist(EF_domains_ADHD_CTD_informantonly_pooled_ci_lb[[flag]]),
      upper_ci  = unlist(EF_domains_ADHD_CTD_informantonly_pooled_ci_ub[[flag]]),
      k = unlist(EF_domains_ADHD_CTD_informantonly_pooled_k[[flag]]),
      N  = unlist(EF_domains_ADHD_CTD_informantonly_pooled_N[[flag]]),
      pval = unlist(EF_domains_ADHD_CTD_informantonly_pooled_p[[flag]]),
      comparison ="ADHD vs TD"
    )
    temp[[flag]]<-current
    
    flag = flag +1
    
  }
}
# ADHD_CTD_informantonly_efdomains_dataframe<-do.call(rbind,temp)
# ADHD_CTD_informantonly_efdomains_dataframe<- ADHD_CTD_informantonly_efdomains_dataframe  %>% mutate(ADHD_CTD_informantonly_efdomains_dataframe,
#                                                                                           brackets = sprintf("%.2f (%.2f, %.2f)",estimate,lower_ci,upper_ci))


## ADHD_SLD_group
ADHD_SLD_EffectSize<- escalc(measure="SMD",m1i=ADHD_SLD_group$NDDX1_mean, sd1i=ADHD_SLD_group$NDDX1_std,n1i=ADHD_SLD_group$NDDX1_samplesize,m2i=ADHD_SLD_group$NDDX2_mean,sd2i=ADHD_SLD_group$NDDX2_std,n2i=ADHD_SLD_group$NDDX2_samplesize
                             ,slab=paste(ADHD_SLD_group$`Study.No.`,sep=""))
new_yi <- ADHD_SLD_EffectSize$yi
View(ADHD_SLD_EffectSize)

for (i in 1:length(new_yi)){
  if(ADHD_SLD_group$`EF_Direction`[i] == 1 && new_yi[i] > 0){
    new_yi[i] = -new_yi[i]
  } else if(ADHD_SLD_group$`EF_Direction`[i] == 2 && new_yi[i] < 0){
    new_yi[i] = -new_yi[i]
  }
}

ADHD_SLD_EffectSize$new_yi <- new_yi
ADHD_SLD_EffectSize$`EF_Direction` <- ADHD_SLD_group$`EF_Direction`

# View(ADHD_SLD_EffectSize)

ADHD_SLD_group$yi <- ADHD_SLD_EffectSize$new_yi
ADHD_SLD_group$vi <- ADHD_SLD_EffectSize$vi

#2-Level Meta-Anlysis
study_level_metaanalysis_ADHD_SLD<- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=ADHD_SLD_group
                                           ,slab=paste(ADHD_SLD_group$`Study.No.`,sep=""))
print(study_level_metaanalysis_ADHD_SLD, digits=3)
predict(study_level_metaanalysis_ADHD_SLD)

#Second level Analysis
##test for loop
perdomain_meta_results_ADHD_SLD <-c()
EF_domains_ADHD_SLD_pooled_ef <- list()
EF_domains_ADHD_SLD_pooled_ci_lb <- list()
EF_domains_ADHD_SLD_pooled_ci_ub <- list()
EF_domains_ADHD_SLD_pooled_k <- list()
EF_domains_ADHD_SLD_pooled_N <- list()
EF_domains_ADHD_SLD_pooled_p <- list()
temp <-list()
flag = 1
for (domain_label in unique(ADHD_SLD_group$EF.Domain)){
  row_basedon_ef_domain <- c()
  row_basedon_ef_domain <-ADHD_SLD_group %>% filter(ADHD_SLD_group$EF.Domain == domain_label )
  if (nrow(row_basedon_ef_domain)>=3){
    #     # print(row_basedon_ef_domain)
    print(domain_label)
    perdomain_meta_results_ADHD_SLD[[domain_label]] <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=row_basedon_ef_domain, slab=paste(row_basedon_ef_domain$`Study.No.`,sep=""))
    print(perdomain_meta_results_ADHD_SLD[[domain_label]], digits=3)
    print(metafor::forest(perdomain_meta_results_ADHD_SLD[[domain_label]],addpred=TRUE,header=TRUE))
    # Extract pooled effect size from each domain_label
    pooled_effect <- perdomain_meta_results_ADHD_SLD[[flag]]$b
    pooled_effect_ci_lb <- perdomain_meta_results_ADHD_SLD[[flag]]$ci.lb
    pooled_effect_ci_ub <- perdomain_meta_results_ADHD_SLD[[flag]]$ci.ub
    pooled_effect_k <- perdomain_meta_results_ADHD_SLD[[flag]]$k
    pooled_effect_N <- perdomain_meta_results_ADHD_SLD[[flag]]$s.nlevels
    pooled_effect_p <- perdomain_meta_results_ADHD_SLD[[flag]]$pval
    
    EF_domains_ADHD_SLD_pooled_ef[[flag]]<-pooled_effect
    EF_domains_ADHD_SLD_pooled_ci_lb[[flag]]<-pooled_effect_ci_lb
    EF_domains_ADHD_SLD_pooled_ci_ub[[flag]]<-pooled_effect_ci_ub
    EF_domains_ADHD_SLD_pooled_k[[flag]]<-pooled_effect_k
    EF_domains_ADHD_SLD_pooled_N[[flag]]<-pooled_effect_N
    
    pooled_effect_p <- perdomain_meta_results_ADHD_SLD[[flag]]$pval
    
    if (pooled_effect_p < 0.001) {
      result <- "<0.001"
    } else if (pooled_effect_p < 0.01) {
      result <- "<0.01"
    } else if (pooled_effect_p < 0.05) {
      result <- "<0.05"
    } else {
      result <- format(pooled_effect_p, digits = 3)
    }
    
    EF_domains_ADHD_SLD_pooled_p[[flag]] <- result
    
    # Create a data frame of pooled effects
    current <- data.frame(
      ef_domain = domain_label,
      estimate = unlist(EF_domains_ADHD_SLD_pooled_ef[[flag]][,1]),
      lower_ci = unlist(EF_domains_ADHD_SLD_pooled_ci_lb[[flag]]),
      upper_ci  = unlist(EF_domains_ADHD_SLD_pooled_ci_ub[[flag]]),
      k = unlist(EF_domains_ADHD_SLD_pooled_k[[flag]]),
      N  = unlist(EF_domains_ADHD_SLD_pooled_N[[flag]]),
      pval = unlist(EF_domains_ADHD_SLD_pooled_p[[flag]]),
      comparison ="ADHD vs SLD"
    )
    temp[[flag]]<-current
    
    flag = flag +1
    
  }
}
ADHD_SLD_efdomains_dataframe<-do.call(rbind,temp)
ADHD_SLD_efdomains_dataframe<- ADHD_SLD_efdomains_dataframe  %>% mutate(ADHD_SLD_efdomains_dataframe,
                                                                        brackets = sprintf("%.2f (%.2f, %.2f)",estimate,lower_ci,upper_ci))



## Performance Based ADHD_SLD
ADHD_SLD_group_performance_only <-subset(ADHD_SLD_group, Informant_Performance == 0)
#ASDvsADHD Effect Size Calculations


ADHD_SLD_EffectSize_perfonly<- escalc(measure="SMD",m1i=ADHD_SLD_group_performance_only$NDDX1_mean, sd1i=ADHD_SLD_group_performance_only$NDDX1_std,n1i=ADHD_SLD_group_performance_only$NDDX1_samplesize,m2i=ADHD_SLD_group_performance_only$NDDX2_mean,sd2i=ADHD_SLD_group_performance_only$NDDX2_std,n2i=ADHD_SLD_group_performance_only$NDDX2_samplesize
                                       ,slab=paste(ADHD_SLD_group_performance_only$`Study.No.`,sep=""))
perfonly_yi <- ADHD_SLD_EffectSize_perfonly$yi
View(ADHD_SLD_EffectSize_perfonly)

for (i in 1:length(perfonly_yi)){
  if(ADHD_SLD_group_performance_only$`EF_Direction`[i] == 1 && perfonly_yi[i] > 0){
    perfonly_yi[i] = -perfonly_yi[i]
  } else if(ADHD_SLD_group_performance_only$`EF_Direction`[i] == 2 && perfonly_yi[i] < 0){
    perfonly_yi[i] = -perfonly_yi[i]
  }
}

ADHD_SLD_EffectSize_perfonly$perfonly_yi <- perfonly_yi
ADHD_SLD_EffectSize_perfonly$`EF_Direction` <- ADHD_SLD_group_performance_only$`EF_Direction`

View(ADHD_SLD_EffectSize_perfonly)

ADHD_SLD_group_performance_only$perfonly_yi <- ADHD_SLD_EffectSize_perfonly$perfonly_yi
ADHD_SLD_group_performance_only$perfonly_vi <- ADHD_SLD_EffectSize_perfonly$vi

#2-Level Meta-Anlysis
study_level_metaanalysis_ADHD_SLD_perfonly<- rma.mv(perfonly_yi, perfonly_vi, random = ~ 1 |`Study.No.`, data=ADHD_SLD_group_performance_only
                                                    ,slab=paste(ADHD_SLD_group_performance_only$`Study.No.`,sep=""))
print(study_level_metaanalysis_ADHD_SLD_perfonly, digits=3)


#Second level Analysis
##test for loop
perdomain_meta_results_ADHD_SLD_perfonly <-c()
EF_domains_ADHD_SLD_perfonly_pooled_ef <- list()
EF_domains_ADHD_SLD_perfonly_pooled_ci_lb <- list()
EF_domains_ADHD_SLD_perfonly_pooled_ci_ub <- list()
EF_domains_ADHD_SLD_perfonly_pooled_k <- list()
EF_domains_ADHD_SLD_perfonly_pooled_N <- list()
EF_domains_ADHD_SLD_perfonly_pooled_p <- list()
temp <-list()
flag = 1
for (domain_label in unique(ADHD_SLD_group_performance_only$EF.Domain)){
  row_basedon_ef_domain <- c()
  row_basedon_ef_domain <-ADHD_SLD_group_performance_only %>% filter(ADHD_SLD_group_performance_only$EF.Domain == domain_label )
  if (nrow(row_basedon_ef_domain)>=3){
    #     # print(row_basedon_ef_domain)
    print(domain_label)
    perdomain_meta_results_ADHD_SLD_perfonly[[domain_label]] <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=row_basedon_ef_domain, slab=paste(row_basedon_ef_domain$`Study.No.`,sep=""))
    print(perdomain_meta_results_ADHD_SLD_perfonly[[domain_label]], digits=3)
    print(metafor::forest(perdomain_meta_results_ADHD_SLD_perfonly[[domain_label]],addpred=TRUE,header=TRUE))
    # Extract pooled effect size from each domain_label
    pooled_effect <- perdomain_meta_results_ADHD_SLD_perfonly[[flag]]$b
    pooled_effect_ci_lb <- perdomain_meta_results_ADHD_SLD_perfonly[[flag]]$ci.lb
    pooled_effect_ci_ub <- perdomain_meta_results_ADHD_SLD_perfonly[[flag]]$ci.ub
    pooled_effect_k <- perdomain_meta_results_ADHD_SLD_perfonly[[flag]]$k
    pooled_effect_N <- perdomain_meta_results_ADHD_SLD_perfonly[[flag]]$s.nlevels
    pooled_effect_p <- perdomain_meta_results_ADHD_SLD_perfonly[[flag]]$pval
    
    EF_domains_ADHD_SLD_perfonly_pooled_ef[[flag]]<-pooled_effect
    EF_domains_ADHD_SLD_perfonly_pooled_ci_lb[[flag]]<-pooled_effect_ci_lb
    EF_domains_ADHD_SLD_perfonly_pooled_ci_ub[[flag]]<-pooled_effect_ci_ub
    EF_domains_ADHD_SLD_perfonly_pooled_k[[flag]]<-pooled_effect_k
    EF_domains_ADHD_SLD_perfonly_pooled_N[[flag]]<-pooled_effect_N
    
    pooled_effect_p <- perdomain_meta_results_ADHD_SLD_perfonly[[flag]]$pval
    
    if (pooled_effect_p < 0.001) {
      result <- "<0.001"
    } else if (pooled_effect_p < 0.01) {
      result <- "<0.01"
    } else if (pooled_effect_p < 0.05) {
      result <- "<0.05"
    } else {
      result <- format(pooled_effect_p, digits = 3)
    }
    
    EF_domains_ADHD_SLD_perfonly_pooled_p[[flag]] <- result
    
    # Create a data frame of pooled effects
    current <- data.frame(
      ef_domain = domain_label,
      estimate = unlist(EF_domains_ADHD_SLD_perfonly_pooled_ef[[flag]][,1]),
      lower_ci = unlist(EF_domains_ADHD_SLD_perfonly_pooled_ci_lb[[flag]]),
      upper_ci  = unlist(EF_domains_ADHD_SLD_perfonly_pooled_ci_ub[[flag]]),
      k = unlist(EF_domains_ADHD_SLD_perfonly_pooled_k[[flag]]),
      N  = unlist(EF_domains_ADHD_SLD_perfonly_pooled_N[[flag]]),
      pval = unlist(EF_domains_ADHD_SLD_perfonly_pooled_p[[flag]]),
      comparison ="ADHD vs SLD"
    )
    temp[[flag]]<-current
    
    flag = flag +1
    
  }
}
ADHD_SLD_perfonly_efdomains_dataframe<-do.call(rbind,temp)
ADHD_SLD_perfonly_efdomains_dataframe<- ADHD_SLD_perfonly_efdomains_dataframe  %>% mutate(ADHD_SLD_perfonly_efdomains_dataframe,
                                                                        brackets = sprintf("%.2f (%.2f, %.2f)",estimate,lower_ci,upper_ci))
## Informant Based ADHD_SLD
ADHD_SLD_group_informant_only <-subset(ADHD_SLD_group, Informant_Performance == 1)
#ASDvsADHD Effect Size Calculations


ADHD_SLD_EffectSize_informantonly<- escalc(measure="SMD",m1i=ADHD_SLD_group_informant_only$NDDX1_mean, sd1i=ADHD_SLD_group_informant_only$NDDX1_std,n1i=ADHD_SLD_group_informant_only$NDDX1_samplesize,m2i=ADHD_SLD_group_informant_only$NDDX2_mean,sd2i=ADHD_SLD_group_informant_only$NDDX2_std,n2i=ADHD_SLD_group_informant_only$NDDX2_samplesize
                                       ,slab=paste(ADHD_SLD_group_informant_only$`Study.No.`,sep=""))
informantonly_yi <- ADHD_SLD_EffectSize_informantonly$yi
View(ADHD_SLD_EffectSize_informantonly)

for (i in 1:length(informantonly_yi)){
  if(ADHD_SLD_group_informant_only$`EF_Direction`[i] == 1 && informantonly_yi[i] > 0){
    informantonly_yi[i] = -informantonly_yi[i]
  } else if(ADHD_SLD_group_informant_only$`EF_Direction`[i] == 2 && informantonly_yi[i] < 0){
    informantonly_yi[i] = -informantonly_yi[i]
  }
}

ADHD_SLD_EffectSize_informantonly$informantonly_yi <- informantonly_yi
ADHD_SLD_EffectSize_informantonly$`EF_Direction` <- ADHD_SLD_group_informant_only$`EF_Direction`

View(ADHD_SLD_EffectSize_informantonly)

ADHD_SLD_group_informant_only$informantonly_yi <- ADHD_SLD_EffectSize_informantonly$informantonly_yi
ADHD_SLD_group_informant_only$informantonly_vi <- ADHD_SLD_EffectSize_informantonly$vi

#2-Level Meta-Anlysis
study_level_metaanalysis_ADHD_SLD_informantonly<- rma.mv(informantonly_yi, informantonly_vi, random = ~ 1 |`Study.No.`, data=ADHD_SLD_group_informant_only
                                                    ,slab=paste(ADHD_SLD_group_informant_only$`Study.No.`,sep=""))
print(study_level_metaanalysis_ADHD_SLD_informantonly, digits=3)



#Second level Analysis
##test for loop
perdomain_meta_results_ADHD_SLD_informantonly <-c()
EF_domains_ADHD_SLD_informantonly_pooled_ef <- list()
EF_domains_ADHD_SLD_informantonly_pooled_ci_lb <- list()
EF_domains_ADHD_SLD_informantonly_pooled_ci_ub <- list()
EF_domains_ADHD_SLD_informantonly_pooled_k <- list()
EF_domains_ADHD_SLD_informantonly_pooled_N <- list()
EF_domains_ADHD_SLD_informantonly_pooled_p <- list()
temp <-list()
flag = 1
for (domain_label in unique(ADHD_SLD_group_informant_only$EF.Domain)){
  row_basedon_ef_domain <- c()
  row_basedon_ef_domain <-ADHD_SLD_group_informant_only %>% filter(ADHD_SLD_group_informant_only$EF.Domain == domain_label )
  if (nrow(row_basedon_ef_domain)>=3){
    #     # print(row_basedon_ef_domain)
    print(domain_label)
    perdomain_meta_results_ADHD_SLD_informantonly[[domain_label]] <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=row_basedon_ef_domain, slab=paste(row_basedon_ef_domain$`Study.No.`,sep=""))
    print(perdomain_meta_results_ADHD_SLD_informantonly[[domain_label]], digits=3)
    print(metafor::forest(perdomain_meta_results_ADHD_SLD_informantonly[[domain_label]],addpred=TRUE,header=TRUE))
    # Extract pooled effect size from each domain_label
    pooled_effect <- perdomain_meta_results_ADHD_SLD_informantonly[[flag]]$b
    pooled_effect_ci_lb <- perdomain_meta_results_ADHD_SLD_informantonly[[flag]]$ci.lb
    pooled_effect_ci_ub <- perdomain_meta_results_ADHD_SLD_informantonly[[flag]]$ci.ub
    pooled_effect_k <- perdomain_meta_results_ADHD_SLD_informantonly[[flag]]$k
    pooled_effect_N <- perdomain_meta_results_ADHD_SLD_informantonly[[flag]]$s.nlevels
    pooled_effect_p <- perdomain_meta_results_ADHD_SLD_informantonly[[flag]]$pval
    
    EF_domains_ADHD_SLD_informantonly_pooled_ef[[flag]]<-pooled_effect
    EF_domains_ADHD_SLD_informantonly_pooled_ci_lb[[flag]]<-pooled_effect_ci_lb
    EF_domains_ADHD_SLD_informantonly_pooled_ci_ub[[flag]]<-pooled_effect_ci_ub
    EF_domains_ADHD_SLD_informantonly_pooled_k[[flag]]<-pooled_effect_k
    EF_domains_ADHD_SLD_informantonly_pooled_N[[flag]]<-pooled_effect_N
    
    pooled_effect_p <- perdomain_meta_results_ADHD_SLD_informantonly[[flag]]$pval
    
    if (pooled_effect_p < 0.001) {
      result <- "<0.001"
    } else if (pooled_effect_p < 0.01) {
      result <- "<0.01"
    } else if (pooled_effect_p < 0.05) {
      result <- "<0.05"
    } else {
      result <- format(pooled_effect_p, digits = 3)
    }
    
    EF_domains_ADHD_SLD_informantonly_pooled_p[[flag]] <- result
    
    # Create a data frame of pooled effects
    current <- data.frame(
      ef_domain = domain_label,
      estimate = unlist(EF_domains_ADHD_SLD_informantonly_pooled_ef[[flag]][,1]),
      lower_ci = unlist(EF_domains_ADHD_SLD_informantonly_pooled_ci_lb[[flag]]),
      upper_ci  = unlist(EF_domains_ADHD_SLD_informantonly_pooled_ci_ub[[flag]]),
      k = unlist(EF_domains_ADHD_SLD_informantonly_pooled_k[[flag]]),
      N  = unlist(EF_domains_ADHD_SLD_informantonly_pooled_N[[flag]]),
      pval = unlist(EF_domains_ADHD_SLD_informantonly_pooled_p[[flag]]),
      comparison ="ADHD vs SLD"
    )
    temp[[flag]]<-current
    
    flag = flag +1
    
  }
}
ADHD_SLD_informantonly_efdomains_dataframe<-do.call(rbind,temp)
ADHD_SLD_informantonly_efdomains_dataframe<- ADHD_SLD_informantonly_efdomains_dataframe  %>% mutate(ADHD_SLD_informantonly_efdomains_dataframe,
                                                                                          brackets = sprintf("%.2f (%.2f, %.2f)",estimate,lower_ci,upper_ci))


##ADHD_DCD
ADHD_DCD_group<-ADHD_DCD_group[complete.cases(ADHD_DCD_group),]
ADHD_DCD_EffectSize<- escalc(measure="SMD",m1i=ADHD_DCD_group$NDDX1_mean, sd1i=ADHD_DCD_group$NDDX1_std,n1i=ADHD_DCD_group$NDDX1_samplesize,m2i=ADHD_DCD_group$NDDX2_mean,sd2i=ADHD_DCD_group$NDDX2_std,n2i=ADHD_DCD_group$NDDX2_samplesize
                             ,slab=paste(ADHD_DCD_group$Study.No.,sep=""))
new_yi <- ADHD_DCD_EffectSize$yi
View(ADHD_DCD_EffectSize)

for (i in 1:length(new_yi)){
  if(ADHD_DCD_group$`EF_Direction`[i] == 1 && new_yi[i] > 0){
    new_yi[i] = -new_yi[i]
  } else if(ADHD_DCD_group$`EF_Direction`[i] == 2 && new_yi[i] < 0){
    new_yi[i] = -new_yi[i]
  }
}

ADHD_DCD_EffectSize$new_yi <- new_yi
ADHD_DCD_EffectSize$`EF_Direction` <- ADHD_DCD_group$`EF_Direction`

View(ADHD_DCD_EffectSize)

ADHD_DCD_group$yi <- ADHD_DCD_EffectSize$new_yi
ADHD_DCD_group$vi <- ADHD_DCD_EffectSize$vi

#2-Level Meta-Anlysis
study_level_metaanalysis_ADHD_DCD<- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=ADHD_DCD_group
                                           ,slab=paste(ADHD_DCD_group$Study.No.,sep=""))
print(study_level_metaanalysis_ADHD_DCD, digits=3)
predict(study_level_metaanalysis_ADHD_DCD)

#Second level Analysis
##test for loop
perdomain_meta_results_ADHD_DCD <-c()
EF_domains_ADHD_DCD_pooled_ef <- list()
EF_domains_ADHD_DCD_pooled_ci_lb <- list()
EF_domains_ADHD_DCD_pooled_ci_ub <- list()
EF_domains_ADHD_DCD_pooled_k <- list()
EF_domains_ADHD_DCD_pooled_N <- list()
EF_domains_ADHD_DCD_pooled_p <- list()
temp <-list()
flag = 1
for (domain_label in unique(ADHD_DCD_group$EF.Domain)){
  row_basedon_ef_domain <- c()
  row_basedon_ef_domain <-ADHD_DCD_group %>% filter(ADHD_DCD_group$EF.Domain == domain_label )
  if (nrow(row_basedon_ef_domain)>=3){
        # print(row_basedon_ef_domain)
    print(domain_label)
    perdomain_meta_results_ADHD_DCD[[domain_label]] <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=row_basedon_ef_domain, slab=paste(row_basedon_ef_domain$`Study.No.`,sep=""))
    print(perdomain_meta_results_ADHD_DCD[[domain_label]], digits=3)
    print(metafor::forest(perdomain_meta_results_ADHD_DCD[[domain_label]],addpred=TRUE,header=TRUE))
    
    # Extract pooled effect size from each domain_label
    pooled_effect <- perdomain_meta_results_ADHD_DCD[[flag]]$b
    pooled_effect_ci_lb <- perdomain_meta_results_ADHD_DCD[[flag]]$ci.lb
    pooled_effect_ci_ub <- perdomain_meta_results_ADHD_DCD[[flag]]$ci.ub
    pooled_effect_k <- perdomain_meta_results_ADHD_DCD[[flag]]$k
    pooled_effect_N <- perdomain_meta_results_ADHD_DCD[[flag]]$s.nlevels
    pooled_effect_p <- perdomain_meta_results_ADHD_DCD[[flag]]$pval
    
    EF_domains_ADHD_DCD_pooled_ef[[flag]]<-pooled_effect
    EF_domains_ADHD_DCD_pooled_ci_lb[[flag]]<-pooled_effect_ci_lb
    EF_domains_ADHD_DCD_pooled_ci_ub[[flag]]<-pooled_effect_ci_ub
    EF_domains_ADHD_DCD_pooled_k[[flag]]<-pooled_effect_k
    EF_domains_ADHD_DCD_pooled_N[[flag]]<-pooled_effect_N
    
    pooled_effect_p <- perdomain_meta_results_ADHD_DCD[[flag]]$pval
    
    if (pooled_effect_p < 0.001) {
      result <- "<0.001"
    } else if (pooled_effect_p < 0.01) {
      result <- "<0.01"
    } else if (pooled_effect_p < 0.05) {
      result <- "<0.05"
    } else {
      result <- format(pooled_effect_p, digits = 3)
    }
    
    EF_domains_ADHD_DCD_pooled_p[[flag]] <- result
    
    # Create a data frame of pooled effects
    current <- data.frame(
      ef_domain = domain_label,
      estimate = unlist(EF_domains_ADHD_DCD_pooled_ef[[flag]][,1]),
      lower_ci = unlist(EF_domains_ADHD_DCD_pooled_ci_lb[[flag]]),
      upper_ci  = unlist(EF_domains_ADHD_DCD_pooled_ci_ub[[flag]]),
      k = unlist(EF_domains_ADHD_DCD_pooled_k[[flag]]),
      N  = unlist(EF_domains_ADHD_DCD_pooled_N[[flag]]),
      pval = unlist(EF_domains_ADHD_DCD_pooled_p[[flag]]),
      comparison ="ADHD vs DCD"
    )
    temp[[flag]]<-current
    
    flag = flag +1
    
  }
}
ADHD_DCD_efdomains_dataframe<-do.call(rbind,temp)
ADHD_DCD_efdomains_dataframe<- ADHD_DCD_efdomains_dataframe  %>% mutate(ADHD_DCD_efdomains_dataframe,
                                                                        brackets = sprintf("%.2f (%.2f, %.2f)",estimate,lower_ci,upper_ci))


## Performance Based ADHD_DCD (not conducted not enough data)
ADHD_DCD_group_performance_only <-subset(ADHD_DCD_group, Informant_Performance == 0)
#ASDvsADHD Effect Size Calculations


ADHD_DCD_EffectSize_perfonly<- escalc(measure="SMD",m1i=ADHD_DCD_group_performance_only$NDDX1_mean, sd1i=ADHD_DCD_group_performance_only$NDDX1_std,n1i=ADHD_DCD_group_performance_only$NDDX1_samplesize,m2i=ADHD_DCD_group_performance_only$NDDX2_mean,sd2i=ADHD_DCD_group_performance_only$NDDX2_std,n2i=ADHD_DCD_group_performance_only$NDDX2_samplesize
                                       ,slab=paste(ADHD_DCD_group_performance_only$Study.No.,sep=""))
perfonly_yi <- ADHD_DCD_EffectSize_perfonly$yi
View(ADHD_DCD_EffectSize_perfonly)

for (i in 1:length(perfonly_yi)){
  if(ADHD_DCD_group_performance_only$`EF_Direction`[i] == 1 && perfonly_yi[i] > 0){
    perfonly_yi[i] = -perfonly_yi[i]
  } else if(ADHD_DCD_group_performance_only$`EF_Direction`[i] == 2 && perfonly_yi[i] < 0){
    perfonly_yi[i] = -perfonly_yi[i]
  }
}

ADHD_DCD_EffectSize_perfonly$perfonly_yi <- perfonly_yi
ADHD_DCD_EffectSize_perfonly$`EF_Direction` <- ADHD_DCD_group_performance_only$`EF_Direction`

View(ADHD_DCD_EffectSize_perfonly)

ADHD_DCD_group_performance_only$perfonly_yi <- ADHD_DCD_EffectSize_perfonly$perfonly_yi
ADHD_DCD_group_performance_only$perfonly_vi <- ADHD_DCD_EffectSize_perfonly$vi

#2-Level Meta-Anlysis
study_level_metaanalysis_ADHD_DCD_perfonly<- rma.mv(perfonly_yi, perfonly_vi, random = ~ 1 |`Study.No.`, data=ADHD_DCD_group_performance_only
                                                    ,slab=paste(ADHD_DCD_group_performance_only$Study.No.,sep=""))
print(study_level_metaanalysis_ADHD_DCD_perfonly, digits=3)


#Second level Analysis
##test for loop
perdomain_meta_results_ADHD_DCD_perfonly <-c()
EF_domains_ADHD_DCD_perfonly_pooled_ef <- list()
EF_domains_ADHD_DCD_perfonly_pooled_ci_lb <- list()
EF_domains_ADHD_DCD_perfonly_pooled_ci_ub <- list()
EF_domains_ADHD_DCD_perfonly_pooled_k <- list()
EF_domains_ADHD_DCD_perfonly_pooled_N <- list()
EF_domains_ADHD_DCD_perfonly_pooled_p <- list()
temp <-list()
flag = 1
for (domain_label in unique(ADHD_DCD_group_performance_only$EF.Domain)){
  row_basedon_ef_domain <- c()
  row_basedon_ef_domain <-ADHD_DCD_group_performance_only %>% filter(ADHD_DCD_group_performance_only$EF.Domain == domain_label )
  if (nrow(row_basedon_ef_domain)>=3){
        # print(row_basedon_ef_domain)
    print(domain_label)
    perdomain_meta_results_ADHD_DCD_perfonly[[domain_label]] <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=row_basedon_ef_domain, slab=paste(row_basedon_ef_domain$`Study.No.`,sep=""))
    print(perdomain_meta_results_ADHD_DCD_perfonly[[domain_label]], digits=3)
    print(metafor::forest(perdomain_meta_results_ADHD_DCD_perfonly[[domain_label]],addpred=TRUE,header=TRUE))
    # Extract pooled effect size from each domain_label
    pooled_effect <- perdomain_meta_results_ADHD_DCD_perfonly[[flag]]$b
    pooled_effect_ci_lb <- perdomain_meta_results_ADHD_DCD_perfonly[[flag]]$ci.lb
    pooled_effect_ci_ub <- perdomain_meta_results_ADHD_DCD_perfonly[[flag]]$ci.ub
    pooled_effect_k <- perdomain_meta_results_ADHD_DCD_perfonly[[flag]]$k
    pooled_effect_N <- perdomain_meta_results_ADHD_DCD_perfonly[[flag]]$s.nlevels
    pooled_effect_p <- perdomain_meta_results_ADHD_DCD_perfonly[[flag]]$pval

    EF_domains_ADHD_DCD_perfonly_pooled_ef[[flag]]<-pooled_effect
    EF_domains_ADHD_DCD_perfonly_pooled_ci_lb[[flag]]<-pooled_effect_ci_lb
    EF_domains_ADHD_DCD_perfonly_pooled_ci_ub[[flag]]<-pooled_effect_ci_ub
    EF_domains_ADHD_DCD_perfonly_pooled_k[[flag]]<-pooled_effect_k
    EF_domains_ADHD_DCD_perfonly_pooled_N[[flag]]<-pooled_effect_N

    pooled_effect_p <- perdomain_meta_results_ADHD_DCD_perfonly[[flag]]$pval

    if (pooled_effect_p < 0.001) {
      result <- "<0.001"
    } else if (pooled_effect_p < 0.01) {
      result <- "<0.01"
    } else if (pooled_effect_p < 0.05) {
      result <- "<0.05"
    } else {
      result <- format(pooled_effect_p, digits = 3)
    }

    EF_domains_ADHD_DCD_perfonly_pooled_p[[flag]] <- result

    # Create a data frame of pooled effects
    current <- data.frame(
      ef_domain = domain_label,
      estimate = unlist(EF_domains_ADHD_DCD_perfonly_pooled_ef[[flag]][,1]),
      lower_ci = unlist(EF_domains_ADHD_DCD_perfonly_pooled_ci_lb[[flag]]),
      upper_ci  = unlist(EF_domains_ADHD_DCD_perfonly_pooled_ci_ub[[flag]]),
      k = unlist(EF_domains_ADHD_DCD_perfonly_pooled_k[[flag]]),
      N  = unlist(EF_domains_ADHD_DCD_perfonly_pooled_N[[flag]]),
      pval = unlist(EF_domains_ADHD_DCD_perfonly_pooled_p[[flag]]),
      comparison ="ADHD vs DCD"
    )
    temp[[flag]]<-current

    flag = flag +1

  }
}
ADHD_DCD_perfonly_efdomains_dataframe<-do.call(rbind,temp)
# ADHD_DCD_perfonly_efdomains_dataframe<- ADHD_DCD_perfonly_efdomains_dataframe  %>% mutate(ADHD_DCD_perfonly_efdomains_dataframe,
#                                                                         brackets = sprintf("%.2f (%.2f, %.2f)",estimate,lower_ci,upper_ci))

# ## Informant Based ADHD_DCD (not conducted no data)
# ADHD_DCD_group_Informant_only <-subset(ADHD_DCD_group, Informant_Performance == 1)
# #ASDvsADHD Effect Size Calculations
# 
# 
# ADHD_DCD_EffectSize_Informantonly<- escalc(measure="SMD",m1i=ADHD_DCD_group_Informant_only$NDDX1_mean, sd1i=ADHD_DCD_group_Informant_only$NDDX1_std,n1i=ADHD_DCD_group_Informant_only$NDDX1_samplesize,m2i=ADHD_DCD_group_Informant_only$NDDX2_mean,sd2i=ADHD_DCD_group_Informant_only$NDDX2_std,n2i=ADHD_DCD_group_Informant_only$NDDX2_samplesize
#                                       ,slab=paste(ADHD_DCD_group_Informant_only$Study.No.,sep=""))
# Informantonly_yi <- ADHD_DCD_EffectSize_Informantonly$yi
# View(ADHD_DCD_EffectSize_Informantonly)
# 
# for (i in 1:length(Informantonly_yi)){
#   if(ADHD_DCD_group_Informant_only$`EF_Direction`[i] == 1 && Informantonly_yi[i] > 0){
#     Informantonly_yi[i] = -Informantonly_yi[i]
#   } else if(ADHD_DCD_group_Informant_only$`EF_Direction`[i] == 2 && Informantonly_yi[i] < 0){
#     Informantonly_yi[i] = -Informantonly_yi[i]
#   }
# }
# 
# ADHD_DCD_EffectSize_Informantonly$Informantonly_yi <- Informantonly_yi
# ADHD_DCD_EffectSize_Informantonly$`EF_Direction` <- ADHD_DCD_group_Informant_only$`EF_Direction`
# 
# View(ADHD_DCD_EffectSize_Informantonly)
# 
# ADHD_DCD_group_Informant_only$Informantonly_yi <- ADHD_DCD_EffectSize_Informantonly$Informantonly_yi
# ADHD_DCD_group_Informant_only$Informantonly_vi <- ADHD_DCD_EffectSize_Informantonly$vi
# 
# #2-Level Meta-Anlysis
# study_level_metaanalysis_ADHD_DCD_Informantonly<- rma.mv(Informantonly_yi, Informantonly_vi, random = ~ 1 |`Study.No.`, data=ADHD_DCD_group_Informant_only
#                                                     ,slab=paste(ADHD_DCD_group_Informant_only$Study.No.,sep=""))
# print(study_level_metaanalysis_ADHD_DCD_Informantonly, digits=3)
# 
# 
# #Second level Analysis
# ##test for loop
# perdomain_meta_results_ADHD_DCD_Informantonly <-c()
# EF_domains_ADHD_DCD_Informantonly_pooled_ef <- list()
# EF_domains_ADHD_DCD_Informantonly_pooled_ci_lb <- list()
# EF_domains_ADHD_DCD_Informantonly_pooled_ci_ub <- list()
# EF_domains_ADHD_DCD_Informantonly_pooled_k <- list()
# EF_domains_ADHD_DCD_Informantonly_pooled_N <- list()
# EF_domains_ADHD_DCD_Informantonly_pooled_p <- list()
# temp <-list()
# flag = 1
# for (domain_label in unique(ADHD_DCD_group_Informant_only$EF.Domain)){
#   row_basedon_ef_domain <- c()
#   row_basedon_ef_domain <-ADHD_DCD_group_Informant_only %>% filter(ADHD_DCD_group_Informant_only$EF.Domain == domain_label )
#   if (nrow(row_basedon_ef_domain)>=3){
#     # print(row_basedon_ef_domain)
#     print(domain_label)
#     perdomain_meta_results_ADHD_DCD_Informantonly[[domain_label]] <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=row_basedon_ef_domain, slab=paste(row_basedon_ef_domain$`Study.No.`,sep=""))
#     print(perdomain_meta_results_ADHD_DCD_Informantonly[[domain_label]], digits=3)
#     print(metafor::forest(perdomain_meta_results_ADHD_DCD_Informantonly[[domain_label]],addpred=TRUE,header=TRUE))
#     # Extract pooled effect size from each domain_label
#     pooled_effect <- perdomain_meta_results_ADHD_DCD_Informantonly[[flag]]$b
#     pooled_effect_ci_lb <- perdomain_meta_results_ADHD_DCD_Informantonly[[flag]]$ci.lb
#     pooled_effect_ci_ub <- perdomain_meta_results_ADHD_DCD_Informantonly[[flag]]$ci.ub
#     pooled_effect_k <- perdomain_meta_results_ADHD_DCD_Informantonly[[flag]]$k
#     pooled_effect_N <- perdomain_meta_results_ADHD_DCD_Informantonly[[flag]]$s.nlevels
#     pooled_effect_p <- perdomain_meta_results_ADHD_DCD_Informantonly[[flag]]$pval
#     
#     EF_domains_ADHD_DCD_Informantonly_pooled_ef[[flag]]<-pooled_effect
#     EF_domains_ADHD_DCD_Informantonly_pooled_ci_lb[[flag]]<-pooled_effect_ci_lb
#     EF_domains_ADHD_DCD_Informantonly_pooled_ci_ub[[flag]]<-pooled_effect_ci_ub
#     EF_domains_ADHD_DCD_Informantonly_pooled_k[[flag]]<-pooled_effect_k
#     EF_domains_ADHD_DCD_Informantonly_pooled_N[[flag]]<-pooled_effect_N
#     
#     pooled_effect_p <- perdomain_meta_results_ADHD_DCD_Informantonly[[flag]]$pval
#     
#     if (pooled_effect_p < 0.001) {
#       result <- "<0.001"
#     } else if (pooled_effect_p < 0.01) {
#       result <- "<0.01"
#     } else if (pooled_effect_p < 0.05) {
#       result <- "<0.05"
#     } else {
#       result <- format(pooled_effect_p, digits = 3)
#     }
#     
#     EF_domains_ADHD_DCD_Informantonly_pooled_p[[flag]] <- result
#     
#     # Create a data frame of pooled effects
#     current <- data.frame(
#       ef_domain = domain_label,
#       estimate = unlist(EF_domains_ADHD_DCD_Informantonly_pooled_ef[[flag]][,1]),
#       lower_ci = unlist(EF_domains_ADHD_DCD_Informantonly_pooled_ci_lb[[flag]]),
#       upper_ci  = unlist(EF_domains_ADHD_DCD_Informantonly_pooled_ci_ub[[flag]]),
#       k = unlist(EF_domains_ADHD_DCD_Informantonly_pooled_k[[flag]]),
#       N  = unlist(EF_domains_ADHD_DCD_Informantonly_pooled_N[[flag]]),
#       pval = unlist(EF_domains_ADHD_DCD_Informantonly_pooled_p[[flag]]),
#       comparison ="ADHD vs DCD"
#     )
#     temp[[flag]]<-current
#     
#     flag = flag +1
#     
#   }
# }
# ADHD_DCD_Informantonly_efdomains_dataframe<-do.call(rbind,temp)
# ADHD_DCD_Informantonly_efdomains_dataframe<- ADHD_DCD_Informantonly_efdomains_dataframe  %>% mutate(ADHD_DCD_Informantonly_efdomains_dataframe,
#                                                                                           brackets = sprintf("%.2f (%.2f, %.2f)",estimate,lower_ci,upper_ci))


## ASD_SLD
ASD_SLD_EffectSize<- escalc(measure="SMD",m1i=ASD_SLD_group$NDDX1_mean, sd1i=ASD_SLD_group$NDDX1_std,n1i=ASD_SLD_group$NDDX1_samplesize,m2i=ASD_SLD_group$NDDX2_mean,sd2i=ASD_SLD_group$NDDX2_std,n2i=ASD_SLD_group$NDDX2_samplesize
                             ,slab=paste(ASD_SLD_group$Study.No.,sep=""))
new_yi <- ASD_SLD_EffectSize$yi
View(ASD_SLD_EffectSize)

for (i in 1:length(new_yi)){
  if(ASD_SLD_group$`EF_Direction`[i] == 1 && new_yi[i] > 0){
    new_yi[i] = -new_yi[i]
  } else if(ASD_SLD_group$`EF_Direction`[i] == 2 && new_yi[i] < 0){
    new_yi[i] = -new_yi[i]
  }
}

ASD_SLD_EffectSize$new_yi <- new_yi
ASD_SLD_EffectSize$`EF_Direction` <- ASD_SLD_group$`EF_Direction`

View(ASD_SLD_EffectSize)

ASD_SLD_group$yi <- ASD_SLD_EffectSize$new_yi
ASD_SLD_group$vi <- ASD_SLD_EffectSize$vi

#2-Level Meta-Anlysis
study_level_metaanalysis_ASD_SLD<- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=ASD_SLD_group
                                           ,slab=paste(ASD_SLD_group$Study.No.,sep=""))
print(study_level_metaanalysis_ASD_SLD, digits=3)
predict(study_level_metaanalysis_ASD_SLD)

#Second level Analysis
##test for loop
perdomain_meta_results_ASD_SLD <-c()
EF_domains_ASD_SLD_pooled_ef <- list()
EF_domains_ASD_SLD_pooled_ci_lb <- list()
EF_domains_ASD_SLD_pooled_ci_ub <- list()
EF_domains_ASD_SLD_pooled_k <- list()
EF_domains_ASD_SLD_pooled_N <- list()
EF_domains_ASD_SLD_pooled_p <- list()
temp <-list()
flag = 1
for (domain_label in unique(ASD_SLD_group$EF.Domain)){
  row_basedon_ef_domain <- c()
  row_basedon_ef_domain <-ASD_SLD_group %>% filter(ASD_SLD_group$EF.Domain == domain_label )
  if (nrow(row_basedon_ef_domain)>=3){
        # print(row_basedon_ef_domain)
    print(domain_label)
    perdomain_meta_results_ASD_SLD[[domain_label]] <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=row_basedon_ef_domain, slab=paste(row_basedon_ef_domain$Study.No.,sep=""))
    print(perdomain_meta_results_ASD_SLD[[domain_label]], digits=3)
    print(metafor::forest(perdomain_meta_results_ASD_SLD[[domain_label]],addpred=TRUE,header=TRUE))
    # Extract pooled effect size from each domain_label
    pooled_effect <- perdomain_meta_results_ASD_SLD[[flag]]$b
    pooled_effect_ci_lb <- perdomain_meta_results_ASD_SLD[[flag]]$ci.lb
    pooled_effect_ci_ub <- perdomain_meta_results_ASD_SLD[[flag]]$ci.ub
    pooled_effect_k <- perdomain_meta_results_ASD_SLD[[flag]]$k
    pooled_effect_N <- perdomain_meta_results_ASD_SLD[[flag]]$s.nlevels
    pooled_effect_p <- perdomain_meta_results_ASD_SLD[[flag]]$pval
    
    EF_domains_ASD_SLD_pooled_ef[[flag]]<-pooled_effect
    EF_domains_ASD_SLD_pooled_ci_lb[[flag]]<-pooled_effect_ci_lb
    EF_domains_ASD_SLD_pooled_ci_ub[[flag]]<-pooled_effect_ci_ub
    EF_domains_ASD_SLD_pooled_k[[flag]]<-pooled_effect_k
    EF_domains_ASD_SLD_pooled_N[[flag]]<-pooled_effect_N
    
    pooled_effect_p <- perdomain_meta_results_ASD_SLD[[flag]]$pval
    
    if (pooled_effect_p < 0.001) {
      result <- "<0.001"
    } else if (pooled_effect_p < 0.01) {
      result <- "<0.01"
    } else if (pooled_effect_p < 0.05) {
      result <- "<0.05"
    } else {
      result <- format(pooled_effect_p, digits = 3)
    }
    
    EF_domains_ASD_SLD_pooled_p[[flag]] <- result
    
    # Create a data frame of pooled effects
    current <- data.frame(
      ef_domain = domain_label,
      estimate = unlist(EF_domains_ASD_SLD_pooled_ef[[flag]][,1]),
      lower_ci = unlist(EF_domains_ASD_SLD_pooled_ci_lb[[flag]]),
      upper_ci  = unlist(EF_domains_ASD_SLD_pooled_ci_ub[[flag]]),
      k = unlist(EF_domains_ASD_SLD_pooled_k[[flag]]),
      N  = unlist(EF_domains_ASD_SLD_pooled_N[[flag]]),
      pval = unlist(EF_domains_ASD_SLD_pooled_p[[flag]]),
      comparison ="ASD vs SLD"
    )
    temp[[flag]]<-current
    
    flag = flag +1
    
    
  }
}
ASD_SLD_efdomains_dataframe<-do.call(rbind,temp)
ASD_SLD_efdomains_dataframe<- ASD_SLD_efdomains_dataframe  %>% mutate(ASD_SLD_efdomains_dataframe,
                                                                        brackets = sprintf("%.2f (%.2f, %.2f)",estimate,lower_ci,upper_ci))

## Performance Based ASD_SLD
ASD_SLD_group_performance_only <-subset(ASD_SLD_group, Informant_Performance == 0)
#ASDvsADHD Effect Size Calculations


ASD_SLD_EffectSize_perfonly<- escalc(measure="SMD",m1i=ASD_SLD_group_performance_only$NDDX1_mean, sd1i=ASD_SLD_group_performance_only$NDDX1_std,n1i=ASD_SLD_group_performance_only$NDDX1_samplesize,m2i=ASD_SLD_group_performance_only$NDDX2_mean,sd2i=ASD_SLD_group_performance_only$NDDX2_std,n2i=ASD_SLD_group_performance_only$NDDX2_samplesize
                                       ,slab=paste(ASD_SLD_group_performance_only$Study.No.,sep=""))
perfonly_yi <- ASD_SLD_EffectSize_perfonly$yi
View(ASD_SLD_EffectSize_perfonly)

for (i in 1:length(perfonly_yi)){
  if(ASD_SLD_group_performance_only$`EF_Direction`[i] == 1 && perfonly_yi[i] > 0){
    perfonly_yi[i] = -perfonly_yi[i]
  } else if(ASD_SLD_group_performance_only$`EF_Direction`[i] == 2 && perfonly_yi[i] < 0){
    perfonly_yi[i] = -perfonly_yi[i]
  }
}

ASD_SLD_EffectSize_perfonly$perfonly_yi <- perfonly_yi
ASD_SLD_EffectSize_perfonly$`EF_Direction` <- ASD_SLD_group_performance_only$`EF_Direction`

View(ASD_SLD_EffectSize_perfonly)

ASD_SLD_group_performance_only$perfonly_yi <- ASD_SLD_EffectSize_perfonly$perfonly_yi
ASD_SLD_group_performance_only$perfonly_vi <- ASD_SLD_EffectSize_perfonly$vi

#2-Level Meta-Anlysis
study_level_metaanalysis_ASD_SLD_perfonly<- rma.mv(perfonly_yi, perfonly_vi, random = ~ 1 |`Study.No.`, data=ASD_SLD_group_performance_only
                                                    ,slab=paste(ASD_SLD_group_performance_only$Study.No.,sep=""))
print(study_level_metaanalysis_ASD_SLD_perfonly, digits=3)


#Second level Analysis
##test for loop 
perdomain_meta_results_ASD_SLD_perfonly <-c()
EF_domains_ASD_SLD_perfonly_pooled_ef <- list()
EF_domains_ASD_SLD_perfonly_pooled_ci_lb <- list()
EF_domains_ASD_SLD_perfonly_pooled_ci_ub <- list()
EF_domains_ASD_SLD_perfonly_pooled_k <- list()
EF_domains_ASD_SLD_perfonly_pooled_N <- list()
EF_domains_ASD_SLD_perfonly_pooled_p <- list()
temp <-list()
flag = 1
for (domain_label in unique(ASD_SLD_group_performance_only$EF.Domain)){
  row_basedon_ef_domain <- c()
  row_basedon_ef_domain <-ASD_SLD_group_performance_only %>% filter(ASD_SLD_group_performance_only$EF.Domain == domain_label )
  if (nrow(row_basedon_ef_domain)>=3){
        # print(row_basedon_ef_domain)
    print(domain_label)
    perdomain_meta_results_ASD_SLD_perfonly[[domain_label]] <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=row_basedon_ef_domain, slab=paste(row_basedon_ef_domain$`Study.No.`,sep=""))
    print(perdomain_meta_results_ASD_SLD_perfonly[[domain_label]], digits=3)
    print(metafor::forest(perdomain_meta_results_ASD_SLD_perfonly[[domain_label]],addpred=TRUE,header=TRUE))
    # Extract pooled effect size from each domain_label
    pooled_effect <- perdomain_meta_results_ASD_SLD_perfonly[[flag]]$b
    pooled_effect_ci_lb <- perdomain_meta_results_ASD_SLD_perfonly[[flag]]$ci.lb
    pooled_effect_ci_ub <- perdomain_meta_results_ASD_SLD_perfonly[[flag]]$ci.ub
    pooled_effect_k <- perdomain_meta_results_ASD_SLD_perfonly[[flag]]$k
    pooled_effect_N <- perdomain_meta_results_ASD_SLD_perfonly[[flag]]$s.nlevels
    pooled_effect_p <- perdomain_meta_results_ASD_SLD_perfonly[[flag]]$pval
    
    EF_domains_ASD_SLD_perfonly_pooled_ef[[flag]]<-pooled_effect
    EF_domains_ASD_SLD_perfonly_pooled_ci_lb[[flag]]<-pooled_effect_ci_lb
    EF_domains_ASD_SLD_perfonly_pooled_ci_ub[[flag]]<-pooled_effect_ci_ub
    EF_domains_ASD_SLD_perfonly_pooled_k[[flag]]<-pooled_effect_k
    EF_domains_ASD_SLD_perfonly_pooled_N[[flag]]<-pooled_effect_N
    
    pooled_effect_p <- perdomain_meta_results_ASD_SLD_perfonly[[flag]]$pval
    
    if (pooled_effect_p < 0.001) {
      result <- "<0.001"
    } else if (pooled_effect_p < 0.01) {
      result <- "<0.01"
    } else if (pooled_effect_p < 0.05) {
      result <- "<0.05"
    } else {
      result <- format(pooled_effect_p, digits = 3)
    }
    
    EF_domains_ASD_SLD_perfonly_pooled_p[[flag]] <- result
    
    # Create a data frame of pooled effects
    current <- data.frame(
      ef_domain = domain_label,
      estimate = unlist(EF_domains_ASD_SLD_perfonly_pooled_ef[[flag]][,1]),
      lower_ci = unlist(EF_domains_ASD_SLD_perfonly_pooled_ci_lb[[flag]]),
      upper_ci  = unlist(EF_domains_ASD_SLD_perfonly_pooled_ci_ub[[flag]]),
      k = unlist(EF_domains_ASD_SLD_perfonly_pooled_k[[flag]]),
      N  = unlist(EF_domains_ASD_SLD_perfonly_pooled_N[[flag]]),
      pval = unlist(EF_domains_ASD_SLD_perfonly_pooled_p[[flag]]),
      comparison ="ASD vs SLD"
    )
    temp[[flag]]<-current
    
    flag = flag +1
    
  }
}
## Note currently not enough data for perfonly
# ASD_SLD_perfonly_efdomains_dataframe<-do.call(rbind,temp)
# ASD_SLD_perfonly_efdomains_dataframe<- ASD_SLD_perfonly_efdomains_dataframe  %>% mutate(ASD_SLD_perfonly_efdomains_dataframe,
#                                                                       brackets = sprintf("%.2f (%.2f, %.2f)",estimate,lower_ci,upper_ci))
## Informant Based ASD_SLD (not enough data)
# ASD_SLD_group_Informant_only <-subset(ASD_SLD_group, Informant_Performance == 1)
# #ASDvsADHD Effect Size Calculations
# 
# 
# ASD_SLD_EffectSize_Informantonly<- escalc(measure="SMD",m1i=ASD_SLD_group_Informant_only$NDDX1_mean, sd1i=ASD_SLD_group_Informant_only$NDDX1_std,n1i=ASD_SLD_group_Informant_only$NDDX1_samplesize,m2i=ASD_SLD_group_Informant_only$NDDX2_mean,sd2i=ASD_SLD_group_Informant_only$NDDX2_std,n2i=ASD_SLD_group_Informant_only$NDDX2_samplesize
#                                      ,slab=paste(ASD_SLD_group_Informant_only$Study.No.,sep=""))
# Informantonly_yi <- ASD_SLD_EffectSize_Informantonly$yi
# View(ASD_SLD_EffectSize_Informantonly)
# 
# for (i in 1:length(Informantonly_yi)){
#   if(ASD_SLD_group_Informant_only$`EF_Direction`[i] == 1 && Informantonly_yi[i] > 0){
#     Informantonly_yi[i] = -Informantonly_yi[i]
#   } else if(ASD_SLD_group_Informant_only$`EF_Direction`[i] == 2 && Informantonly_yi[i] < 0){
#     Informantonly_yi[i] = -Informantonly_yi[i]
#   }
# }
# 
# ASD_SLD_EffectSize_Informantonly$Informantonly_yi <- Informantonly_yi
# ASD_SLD_EffectSize_Informantonly$`EF_Direction` <- ASD_SLD_group_Informant_only$`EF_Direction`
# 
# View(ASD_SLD_EffectSize_Informantonly)
# 
# ASD_SLD_group_Informant_only$Informantonly_yi <- ASD_SLD_EffectSize_Informantonly$Informantonly_yi
# ASD_SLD_group_Informant_only$Informantonly_vi <- ASD_SLD_EffectSize_Informantonly$vi
# 
# #2-Level Meta-Anlysis
# study_level_metaanalysis_ASD_SLD_Informantonly<- rma.mv(Informantonly_yi, Informantonly_vi, random = ~ 1 |`Study.No.`, data=ASD_SLD_group_Informant_only
#                                                    ,slab=paste(ASD_SLD_group_Informant_only$Study.No.,sep=""))
# print(study_level_metaanalysis_ASD_SLD_Informantonly, digits=3)
# 
# 
# #Second level Analysis
# ##test for loop 
# perdomain_meta_results_ASD_SLD_Informantonly <-c()
# EF_domains_ASD_SLD_Informantonly_pooled_ef <- list()
# EF_domains_ASD_SLD_Informantonly_pooled_ci_lb <- list()
# EF_domains_ASD_SLD_Informantonly_pooled_ci_ub <- list()
# EF_domains_ASD_SLD_Informantonly_pooled_k <- list()
# EF_domains_ASD_SLD_Informantonly_pooled_N <- list()
# EF_domains_ASD_SLD_Informantonly_pooled_p <- list()
# temp <-list()
# flag = 1
# for (domain_label in unique(ASD_SLD_group_Informant_only$EF.Domain)){
#   row_basedon_ef_domain <- c()
#   row_basedon_ef_domain <-ASD_SLD_group_Informant_only %>% filter(ASD_SLD_group_Informant_only$EF.Domain == domain_label )
#   if (nrow(row_basedon_ef_domain)>=3){
#     # print(row_basedon_ef_domain)
#     print(domain_label)
#     perdomain_meta_results_ASD_SLD_Informantonly[[domain_label]] <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=row_basedon_ef_domain, slab=paste(row_basedon_ef_domain$`Study.No.`,sep=""))
#     print(perdomain_meta_results_ASD_SLD_Informantonly[[domain_label]], digits=3)
#     print(metafor::forest(perdomain_meta_results_ASD_SLD_Informantonly[[domain_label]],addpred=TRUE,header=TRUE))
#     # Extract pooled effect size from each domain_label
#     pooled_effect <- perdomain_meta_results_ASD_SLD_Informantonly[[flag]]$b
#     pooled_effect_ci_lb <- perdomain_meta_results_ASD_SLD_Informantonly[[flag]]$ci.lb
#     pooled_effect_ci_ub <- perdomain_meta_results_ASD_SLD_Informantonly[[flag]]$ci.ub
#     pooled_effect_k <- perdomain_meta_results_ASD_SLD_Informantonly[[flag]]$k
#     pooled_effect_N <- perdomain_meta_results_ASD_SLD_Informantonly[[flag]]$s.nlevels
#     pooled_effect_p <- perdomain_meta_results_ASD_SLD_Informantonly[[flag]]$pval
#     
#     EF_domains_ASD_SLD_Informantonly_pooled_ef[[flag]]<-pooled_effect
#     EF_domains_ASD_SLD_Informantonly_pooled_ci_lb[[flag]]<-pooled_effect_ci_lb
#     EF_domains_ASD_SLD_Informantonly_pooled_ci_ub[[flag]]<-pooled_effect_ci_ub
#     EF_domains_ASD_SLD_Informantonly_pooled_k[[flag]]<-pooled_effect_k
#     EF_domains_ASD_SLD_Informantonly_pooled_N[[flag]]<-pooled_effect_N
#     
#     pooled_effect_p <- perdomain_meta_results_ASD_SLD_Informantonly[[flag]]$pval
#     
#     if (pooled_effect_p < 0.001) {
#       result <- "<0.001"
#     } else if (pooled_effect_p < 0.01) {
#       result <- "<0.01"
#     } else if (pooled_effect_p < 0.05) {
#       result <- "<0.05"
#     } else {
#       result <- format(pooled_effect_p, digits = 3)
#     }
#     
#     EF_domains_ASD_SLD_Informantonly_pooled_p[[flag]] <- result
#     
#     # Create a data frame of pooled effects
#     current <- data.frame(
#       ef_domain = domain_label,
#       estimate = unlist(EF_domains_ASD_SLD_Informantonly_pooled_ef[[flag]][,1]),
#       lower_ci = unlist(EF_domains_ASD_SLD_Informantonly_pooled_ci_lb[[flag]]),
#       upper_ci  = unlist(EF_domains_ASD_SLD_Informantonly_pooled_ci_ub[[flag]]),
#       k = unlist(EF_domains_ASD_SLD_Informantonly_pooled_k[[flag]]),
#       N  = unlist(EF_domains_ASD_SLD_Informantonly_pooled_N[[flag]]),
#       pval = unlist(EF_domains_ASD_SLD_Informantonly_pooled_p[[flag]]),
#       comparison ="ASD vs SLD"
#     )
#     temp[[flag]]<-current
#     
#     flag = flag +1
#     
#   }
# }
# ASD_SLD_Informantonly_efdomains_dataframe<-do.call(rbind,temp)
# ASD_SLD_Informantonly_efdomains_dataframe<- ASD_SLD_Informantonly_efdomains_dataframe  %>% mutate(ASD_SLD_Informantonly_efdomains_dataframe,
#                                                                       brackets = sprintf("%.2f (%.2f, %.2f)",estimate,lower_ci,upper_ci))

## DS_WS
DS_WS_EffectSize<- escalc(measure="SMD",m1i=DS_WS_group$NDDX1_mean, sd1i=DS_WS_group$NDDX1_std,n1i=DS_WS_group$NDDX1_samplesize,m2i=DS_WS_group$NDDX2_mean,sd2i=DS_WS_group$NDDX2_std,n2i=DS_WS_group$NDDX2_samplesize
                            ,slab=paste(DS_WS_group$Study.No.,sep=""))
new_yi <- DS_WS_EffectSize$yi
View(DS_WS_EffectSize)

for (i in 1:length(new_yi)){
  if(DS_WS_group$`EF_Direction`[i] == 1 && new_yi[i] > 0){
    new_yi[i] = -new_yi[i]
  } else if(DS_WS_group$`EF_Direction`[i] == 2 && new_yi[i] < 0){
    new_yi[i] = -new_yi[i]
  }
}

DS_WS_EffectSize$new_yi <- new_yi
DS_WS_EffectSize$`EF_Direction` <- DS_WS_group$`EF_Direction`

View(DS_WS_EffectSize)

DS_WS_group$yi <- DS_WS_EffectSize$new_yi
DS_WS_group$vi <- DS_WS_EffectSize$vi

#2-Level Meta-Anlysis
study_level_metaanalysis_DS_WS<- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=DS_WS_group
                                          ,slab=paste(DS_WS_group$Study.No.,sep=""))
print(study_level_metaanalysis_DS_WS, digits=3)
predict(study_level_metaanalysis_DS_WS)

#Second level Analysis
##test for loop
perdomain_meta_results_DS_WS <-c()

EF_domains_DS_WS_pooled_ef <- list()
EF_domains_DS_WS_pooled_ci_lb <- list()
EF_domains_DS_WS_pooled_ci_ub <- list()
EF_domains_DS_WS_pooled_k <- list()
EF_domains_DS_WS_pooled_N <- list()
EF_domains_DS_WS_pooled_p <- list()
temp <-list()
flag = 1
for (domain_label in unique(DS_WS_group$EF.Domain)){
  row_basedon_ef_domain <- c()
  row_basedon_ef_domain <-DS_WS_group %>% filter(DS_WS_group$EF.Domain == domain_label )
  if (nrow(row_basedon_ef_domain)>=3){
        # print(row_basedon_ef_domain)
    print(domain_label)
    perdomain_meta_results_DS_WS[[domain_label]] <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=row_basedon_ef_domain, slab=paste(row_basedon_ef_domain$`Study_Year.`,sep=""))
    print(perdomain_meta_results_DS_WS[[domain_label]], digits=3)
    print(metafor::forest(perdomain_meta_results_DS_WS[[domain_label]],addpred=TRUE,header=TRUE))
    
    # Extract pooled effect size from each domain_label
    pooled_effect <- perdomain_meta_results_DS_WS[[flag]]$b
    pooled_effect_ci_lb <- perdomain_meta_results_DS_WS[[flag]]$ci.lb
    pooled_effect_ci_ub <- perdomain_meta_results_DS_WS[[flag]]$ci.ub
    pooled_effect_k <- perdomain_meta_results_DS_WS[[flag]]$k
    pooled_effect_N <- perdomain_meta_results_DS_WS[[flag]]$s.nlevels
    pooled_effect_p <- perdomain_meta_results_DS_WS[[flag]]$pval
    
    EF_domains_DS_WS_pooled_ef[[flag]]<-pooled_effect
    EF_domains_DS_WS_pooled_ci_lb[[flag]]<-pooled_effect_ci_lb
    EF_domains_DS_WS_pooled_ci_ub[[flag]]<-pooled_effect_ci_ub
    EF_domains_DS_WS_pooled_k[[flag]]<-pooled_effect_k
    EF_domains_DS_WS_pooled_N[[flag]]<-pooled_effect_N
    
    pooled_effect_p <- perdomain_meta_results_DS_WS[[flag]]$pval
    
    if (pooled_effect_p < 0.001) {
      result <- "<0.001"
    } else if (pooled_effect_p < 0.01) {
      result <- "<0.01"
    } else if (pooled_effect_p < 0.05) {
      result <- "<0.05"
    } else {
      result <- format(pooled_effect_p, digits = 3)
    }
    
    EF_domains_DS_WS_pooled_p[[flag]] <- result
    
    # Create a data frame of pooled effects
    current <- data.frame(
      ef_domain = domain_label,
      estimate = unlist(EF_domains_DS_WS_pooled_ef[[flag]][,1]),
      lower_ci = unlist(EF_domains_DS_WS_pooled_ci_lb[[flag]]),
      upper_ci  = unlist(EF_domains_DS_WS_pooled_ci_ub[[flag]]),
      k = unlist(EF_domains_DS_WS_pooled_k[[flag]]),
      N  = unlist(EF_domains_DS_WS_pooled_N[[flag]]),
      pval = unlist(EF_domains_DS_WS_pooled_p[[flag]]),
      comparison ="DS vs WS"
    )
    temp[[flag]]<-current
    
    flag = flag +1
    
  }
}

DS_WS_efdomains_dataframe<-do.call(rbind,temp)
DS_WS_efdomains_dataframe<- DS_WS_efdomains_dataframe  %>% mutate(DS_WS_efdomains_dataframe,
                                                                      brackets = sprintf("%.2f (%.2f, %.2f)",estimate,lower_ci,upper_ci))

## Performance Based DS_WS
DS_WS_group_performance_only <-subset(DS_WS_group, Informant_Performance == 0)
#ASDvsADHD Effect Size Calculations


DS_WS_EffectSize_perfonly<- escalc(measure="SMD",m1i=DS_WS_group_performance_only$NDDX1_mean, sd1i=DS_WS_group_performance_only$NDDX1_std,n1i=DS_WS_group_performance_only$NDDX1_samplesize,m2i=DS_WS_group_performance_only$NDDX2_mean,sd2i=DS_WS_group_performance_only$NDDX2_std,n2i=DS_WS_group_performance_only$NDDX2_samplesize
                                      ,slab=paste(DS_WS_group_performance_only$Study.No.,sep=""))
perfonly_yi <- DS_WS_EffectSize_perfonly$yi
View(DS_WS_EffectSize_perfonly)

for (i in 1:length(perfonly_yi)){
  if(DS_WS_group_performance_only$`EF_Direction`[i] == 1 && perfonly_yi[i] > 0){
    perfonly_yi[i] = -perfonly_yi[i]
  } else if(DS_WS_group_performance_only$`EF_Direction`[i] == 2 && perfonly_yi[i] < 0){
    perfonly_yi[i] = -perfonly_yi[i]
  }
}

DS_WS_EffectSize_perfonly$perfonly_yi <- perfonly_yi
DS_WS_EffectSize_perfonly$`EF_Direction` <- DS_WS_group_performance_only$`EF_Direction`

View(DS_WS_EffectSize_perfonly)

DS_WS_group_performance_only$perfonly_yi <- DS_WS_EffectSize_perfonly$perfonly_yi
DS_WS_group_performance_only$perfonly_vi <- DS_WS_EffectSize_perfonly$vi

#2-Level Meta-Anlysis
study_level_metaanalysis_DS_WS_perfonly<- rma.mv(perfonly_yi, perfonly_vi, random = ~ 1 |`Study.No.`, data=DS_WS_group_performance_only
                                                   ,slab=paste(DS_WS_group_performance_only$Study_Year.,sep=""))
print(study_level_metaanalysis_DS_WS_perfonly, digits=3)


#Second level Analysis
##test for loop
perdomain_meta_results_DS_WS_perfonly <-c()
EF_domains_DS_WS_perfonly_pooled_ef <- list()
EF_domains_DS_WS_perfonly_pooled_ci_lb <- list()
EF_domains_DS_WS_perfonly_pooled_ci_ub <- list()
EF_domains_DS_WS_perfonly_pooled_k <- list()
EF_domains_DS_WS_perfonly_pooled_N <- list()
EF_domains_DS_WS_perfonly_pooled_p <- list()
temp <-list()
flag = 1
for (domain_label in unique(DS_WS_group_performance_only$EF.Domain)){
  row_basedon_ef_domain <- c()
  row_basedon_ef_domain <-DS_WS_group_performance_only %>% filter(DS_WS_group_performance_only$EF.Domain == domain_label )
  if (nrow(row_basedon_ef_domain)>=3){
        # print(row_basedon_ef_domain)
    print(domain_label)
    perdomain_meta_results_DS_WS_perfonly[[domain_label]] <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=row_basedon_ef_domain, slab=paste(row_basedon_ef_domain$`Study.No.`,sep=""))
    print(perdomain_meta_results_DS_WS_perfonly[[domain_label]], digits=3)
    print(metafor::forest(perdomain_meta_results_DS_WS_perfonly[[domain_label]],addpred=TRUE,header=TRUE))
    if (nrow(row_basedon_ef_domain)>=3){
      # print(row_basedon_ef_domain)
      print(domain_label)
      perdomain_meta_results_DS_WS_perfonly[[domain_label]] <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=row_basedon_ef_domain, slab=paste(row_basedon_ef_domain$`Study.No.`,sep=""))
      print(perdomain_meta_results_DS_WS_perfonly[[domain_label]], digits=3)
      print(metafor::forest(perdomain_meta_results_DS_WS_perfonly[[domain_label]],addpred=TRUE,header=TRUE))
      
      # Extract pooled effect size from each domain_label
      pooled_effect <- perdomain_meta_results_DS_WS_perfonly[[flag]]$b
      pooled_effect_ci_lb <- perdomain_meta_results_DS_WS_perfonly[[flag]]$ci.lb
      pooled_effect_ci_ub <- perdomain_meta_results_DS_WS_perfonly[[flag]]$ci.ub
      pooled_effect_k <- perdomain_meta_results_DS_WS_perfonly[[flag]]$k
      pooled_effect_N <- perdomain_meta_results_DS_WS_perfonly[[flag]]$s.nlevels
      pooled_effect_p <- perdomain_meta_results_DS_WS_perfonly[[flag]]$pval
      
      EF_domains_DS_WS_perfonly_pooled_ef[[flag]]<-pooled_effect
      EF_domains_DS_WS_perfonly_pooled_ci_lb[[flag]]<-pooled_effect_ci_lb
      EF_domains_DS_WS_perfonly_pooled_ci_ub[[flag]]<-pooled_effect_ci_ub
      EF_domains_DS_WS_perfonly_pooled_k[[flag]]<-pooled_effect_k
      EF_domains_DS_WS_perfonly_pooled_N[[flag]]<-pooled_effect_N
      
      pooled_effect_p <- perdomain_meta_results_DS_WS_perfonly[[flag]]$pval
      
      if (pooled_effect_p < 0.001) {
        result <- "<0.001"
      } else if (pooled_effect_p < 0.01) {
        result <- "<0.01"
      } else if (pooled_effect_p < 0.05) {
        result <- "<0.05"
      } else {
        result <- format(pooled_effect_p, digits = 3)
      }
      
      EF_domains_DS_WS_perfonly_pooled_p[[flag]] <- result
      
      # Create a data frame of pooled effects
      current <- data.frame(
        ef_domain = domain_label,
        estimate = unlist(EF_domains_DS_WS_perfonly_pooled_ef[[flag]][,1]),
        lower_ci = unlist(EF_domains_DS_WS_perfonly_pooled_ci_lb[[flag]]),
        upper_ci  = unlist(EF_domains_DS_WS_perfonly_pooled_ci_ub[[flag]]),
        k = unlist(EF_domains_DS_WS_pooled_k[[flag]]),
        N  = unlist(EF_domains_DS_WS_perfonly_pooled_N[[flag]]),
        pval = unlist(EF_domains_DS_WS_perfonly_pooled_p[[flag]]),
        comparison ="DS vs WS"
      )
      temp[[flag]]<-current
      
      flag = flag +1
    }
  }
}
DS_WS_perfonly_efdomains_dataframe<-do.call(rbind,temp)
DS_WS_perfonly_efdomains_dataframe<- DS_WS_perfonly_efdomains_dataframe  %>% mutate(DS_WS_perfonly_efdomains_dataframe,
                                                                    brackets = sprintf("%.2f (%.2f, %.2f)",estimate,lower_ci,upper_ci))
# ## Informant Based DS_WS ( no data for analysis)
# DS_WS_group_Informant_only <-subset(DS_WS_group, Informant_Performance == 1)
# #ASDvsADHD Effect Size Calculations
# 
# 
# DS_WS_EffectSize_Informantonly<- escalc(measure="SMD",m1i=DS_WS_group_Informant_only$NDDX1_mean, sd1i=DS_WS_group_Informant_only$NDDX1_std,n1i=DS_WS_group_Informant_only$NDDX1_samplesize,m2i=DS_WS_group_Informant_only$NDDX2_mean,sd2i=DS_WS_group_Informant_only$NDDX2_std,n2i=DS_WS_group_Informant_only$NDDX2_samplesize
#                                    ,slab=paste(DS_WS_group_Informant_only$Study.No.,sep=""))
# Informantonly_yi <- DS_WS_EffectSize_Informantonly$yi
# View(DS_WS_EffectSize_Informantonly)
# 
# for (i in 1:length(Informantonly_yi)){
#   if(DS_WS_group_Informant_only$`EF_Direction`[i] == 1 && Informantonly_yi[i] > 0){
#     Informantonly_yi[i] = -Informantonly_yi[i]
#   } else if(DS_WS_group_Informant_only$`EF_Direction`[i] == 2 && Informantonly_yi[i] < 0){
#     Informantonly_yi[i] = -Informantonly_yi[i]
#   }
# }
# 
# DS_WS_EffectSize_Informantonly$Informantonly_yi <- Informantonly_yi
# DS_WS_EffectSize_Informantonly$`EF_Direction` <- DS_WS_group_Informant_only$`EF_Direction`
# 
# View(DS_WS_EffectSize_Informantonly)
# 
# DS_WS_group_Informant_only$Informantonly_yi <- DS_WS_EffectSize_Informantonly$Informantonly_yi
# DS_WS_group_Informant_only$Informantonly_vi <- DS_WS_EffectSize_Informantonly$vi
# 
# #2-Level Meta-Anlysis
# study_level_metaanalysis_DS_WS_Informantonly<- rma.mv(Informantonly_yi, Informantonly_vi, random = ~ 1 |`Study.No.`, data=DS_WS_group_Informant_only
#                                                  ,slab=paste(DS_WS_group_Informant_only$Study_Year.,sep=""))
# print(study_level_metaanalysis_DS_WS_Informantonly, digits=3)
# 
# 
# #Second level Analysis
# ##test for loop
# perdomain_meta_results_DS_WS_Informantonly <-c()
# EF_domains_DS_WS_Informantonly_pooled_ef <- list()
# EF_domains_DS_WS_Informantonly_pooled_ci_lb <- list()
# EF_domains_DS_WS_Informantonly_pooled_ci_ub <- list()
# EF_domains_DS_WS_Informantonly_pooled_k <- list()
# EF_domains_DS_WS_Informantonly_pooled_N <- list()
# EF_domains_DS_WS_Informantonly_pooled_p <- list()
# temp <-list()
# flag = 1
# for (domain_label in unique(DS_WS_group_Informant_only$EF.Domain)){
#   row_basedon_ef_domain <- c()
#   row_basedon_ef_domain <-DS_WS_group_Informant_only %>% filter(DS_WS_group_Informant_only$EF.Domain == domain_label )
#   if (nrow(row_basedon_ef_domain)>=3){
#     # print(row_basedon_ef_domain)
#     print(domain_label)
#     perdomain_meta_results_DS_WS_Informantonly[[domain_label]] <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=row_basedon_ef_domain, slab=paste(row_basedon_ef_domain$`Study.No.`,sep=""))
#     print(perdomain_meta_results_DS_WS_Informantonly[[domain_label]], digits=3)
#     print(metafor::forest(perdomain_meta_results_DS_WS_Informantonly[[domain_label]],addpred=TRUE,header=TRUE))
#     if (nrow(row_basedon_ef_domain)>=3){
#       # print(row_basedon_ef_domain)
#       print(domain_label)
#       perdomain_meta_results_DS_WS_Informantonly[[domain_label]] <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=row_basedon_ef_domain, slab=paste(row_basedon_ef_domain$`Study.No.`,sep=""))
#       print(perdomain_meta_results_DS_WS_Informantonly[[domain_label]], digits=3)
#       print(metafor::forest(perdomain_meta_results_DS_WS_Informantonly[[domain_label]],addpred=TRUE,header=TRUE))
#       
#       # Extract pooled effect size from each domain_label
#       pooled_effect <- perdomain_meta_results_DS_WS_Informantonly[[flag]]$b
#       pooled_effect_ci_lb <- perdomain_meta_results_DS_WS_Informantonly[[flag]]$ci.lb
#       pooled_effect_ci_ub <- perdomain_meta_results_DS_WS_Informantonly[[flag]]$ci.ub
#       pooled_effect_k <- perdomain_meta_results_DS_WS_Informantonly[[flag]]$k
#       pooled_effect_N <- perdomain_meta_results_DS_WS_Informantonly[[flag]]$s.nlevels
#       pooled_effect_p <- perdomain_meta_results_DS_WS_Informantonly[[flag]]$pval
#       
#       EF_domains_DS_WS_Informantonly_pooled_ef[[flag]]<-pooled_effect
#       EF_domains_DS_WS_Informantonly_pooled_ci_lb[[flag]]<-pooled_effect_ci_lb
#       EF_domains_DS_WS_Informantonly_pooled_ci_ub[[flag]]<-pooled_effect_ci_ub
#       EF_domains_DS_WS_Informantonly_pooled_k[[flag]]<-pooled_effect_k
#       EF_domains_DS_WS_Informantonly_pooled_N[[flag]]<-pooled_effect_N
#       
#       pooled_effect_p <- perdomain_meta_results_DS_WS_Informantonly[[flag]]$pval
#       
#       if (pooled_effect_p < 0.001) {
#         result <- "<0.001"
#       } else if (pooled_effect_p < 0.01) {
#         result <- "<0.01"
#       } else if (pooled_effect_p < 0.05) {
#         result <- "<0.05"
#       } else {
#         result <- format(pooled_effect_p, digits = 3)
#       }
#       
#       EF_domains_DS_WS_Informantonly_pooled_p[[flag]] <- result
#       
#       # Create a data frame of pooled effects
#       current <- data.frame(
#         ef_domain = domain_label,
#         estimate = unlist(EF_domains_DS_WS_Informantonly_pooled_ef[[flag]][,1]),
#         lower_ci = unlist(EF_domains_DS_WS_Informantonly_pooled_ci_lb[[flag]]),
#         upper_ci  = unlist(EF_domains_DS_WS_Informantonly_pooled_ci_ub[[flag]]),
#         k = unlist(EF_domains_DS_WS_pooled_k[[flag]]),
#         N  = unlist(EF_domains_DS_WS_Informantonly_pooled_N[[flag]]),
#         pval = unlist(EF_domains_DS_WS_Informantonly_pooled_p[[flag]]),
#         comparison ="DS vs WS"
#       )
#       temp[[flag]]<-current
#       
#       flag = flag +1
#     }
#   }
# }
# DS_WS_Informantonly_efdomains_dataframe<-do.call(rbind,temp)
# DS_WS_Informantonly_efdomains_dataframe<- DS_WS_Informantonly_efdomains_dataframe  %>% mutate(DS_WS_Informantonly_efdomains_dataframe,
#                                                                                     brackets = sprintf("%.2f (%.2f, %.2f)",estimate,lower_ci,upper_ci))


## combining all comparsion group results per EF domain into one
library(dplyr)
### plotting for figure 4
combined_EF_domain <- bind_rows(ADHD_ASD_efdomains_dataframe, ADHD_SLD_efdomains_dataframe, ADHD_DCD_efdomains_dataframe,ADHD_CTD_efdomains_dataframe,ASD_SLD_efdomains_dataframe,DS_WS_efdomains_dataframe)
combined_EF_domain<-subset(combined_EF_domain, N>2)
combined_EF_domain <- combined_EF_domain %>%mutate(Index = row_number())
# Install pacman package if it is not already done:
if (!require("pacman")) install.packages("pacman")
# p_load combines install.packages() and library() together:
pacman::p_load(tidyverse, ggeasy, janitor, patchwork, cowplot)
# read data
source("fplot_updated2.R") 
# load custom R function

grps <- unique(combined_EF_domain$comparison) # create list of groups from data frame

# generate all plots; for the top plot we enable data table labels:
g1 <- fplot(combined_EF_domain, grps[1], var_names = TRUE)
g2 <- fplot(combined_EF_domain, grps[2], var_names = FALSE)
g3 <- fplot(combined_EF_domain, grps[3], var_names = FALSE)
g4 <- fplot(combined_EF_domain, grps[4], var_names = FALSE)
# for the final plot we enable x-axis labels:
g5 <- fplot(combined_EF_domain, grps[5], x_axis = TRUE, var_names = FALSE)

# merge plots into one single graphic
# we use plot_layout heights to adjust height of plots based on no. of vars
#   per plot, e.g. AS vs TS has only 1 var, so it's height is 1:
finalplot <-
  
  g1 /
  g2 / g3 /
  g4 / g5 +
  
  plot_layout(heights =
                c(7,7,4, 1, 1))
Finalplot4 <- ggdraw(finalplot) +
  #  Code here creates a title at the centre of the forest plot
  draw_plot_label(x = 0.18,y = .985, label = "Favours ADHD   Favours ASD", 
                  hjust = 0, size = 10) +
  draw_plot_label(x = 0.18,y = .68, label = "Favours ADHD   Favours SLD", 
                  hjust = 0, size = 10)+
  draw_plot_label(x = 0.18,y = .39, label = "Favours ADHD   Favours TD", 
                  hjust = 0, size = 10)+
  draw_plot_label(x = 0.20,y = .2, label = "Favours ASD   Favours SLD", 
                  hjust = 0, size = 10)+
  draw_plot_label(x = 0.20,y = .12, label = "Favours DS   Favours WS", 
                  hjust = 0, size = 10)
ggsave(
  "Figure4_May2024_nesting.jpeg",
  plot = `Finalplot4`,
  # device = jpeg(),
  path = NULL,
  scale = 1,
  width = 10,
  height = 10,
  # units = c( "mm"),
  dpi = 600,
  limitsize = FALSE,
  bg = NULL,
)

## plot NDDx comparison groups performance only studies
### plotting for supplementary figure 2
combined_EF_domain_perfonly <- bind_rows(ADHD_ASD_perfonly_efdomains_dataframe, ADHD_DCD_perfonly_efdomains_dataframe,ADHD_CTD_perfonly_efdomains_dataframe,ADHD_SLD_efdomains_dataframe,DS_WS_perfonly_efdomains_dataframe)
combined_EF_domain_perfonly<-subset(combined_EF_domain_perfonly, N>2)
combined_EF_domain_perfonly <- combined_EF_domain_perfonly %>%mutate(Index = row_number())

grps <- unique(combined_EF_domain_perfonly$comparison) # create list of groups from data frame

# generate all plots; for the top plot we enable data table labels:
g1 <- fplot(combined_EF_domain_perfonly, grps[1], var_names = TRUE)
g2 <- fplot(combined_EF_domain_perfonly, grps[2], var_names = FALSE)
g3 <- fplot(combined_EF_domain_perfonly, grps[3], var_names = FALSE)
# for the final plot we enable x-axis labels:
g4 <- fplot(combined_EF_domain_perfonly, grps[4], x_axis = TRUE, var_names = FALSE)
# g5 <- fplot(combined_EF_domain_perfonly, grps[5], x_axis = TRUE, var_names = FALSE)

# merge plots into one single graphic
# we use plot_layout heights to adjust height of plots based on no. of vars
#   per plot, e.g. AS vs TS has only 1 var, so it's height is 1:
finalplot <-
  
  g1 /
  g2 / g3 /
  g4  +
  
  plot_layout(heights =
                c(7,7,5, 1))
Finalplot4 <- ggdraw(finalplot) +
  #  Code here creates a title at the centre of the forest plot
  draw_plot_label(x = 0.3,y = .985, label = "Favours ADHD   Favours ASD", 
                  hjust = 0, size = 10) +
  draw_plot_label(x = 0.3,y = .66, label = "Favours ADHD   Favours TD", 
                  hjust = 0, size = 10)+
  draw_plot_label(x = 0.3,y = .36, label = "Favours ADHD   Favours SLD", 
                  hjust = 0, size = 10)+
  # draw_plot_label(x = 0.205,y = .2, label = "Favours ASD   Favours SLD", 
  #                 hjust = 0, size = 10)+
  draw_plot_label(x = 0.305,y = .12, label = "Favours DS   Favours WS", 
                  hjust = 0, size = 10)
ggsave(
  "Supplementaryfigure2_nesting.jpeg",
  plot = `Finalplot4`,
  # device = jpeg(),
  path = NULL,
  scale = 1,
  width = 10,
  height = 10,
  # units = c( "mm"),
  dpi = 600,
  limitsize = FALSE,
  bg = NULL,
)

## plot NDDx comparison groups Informant only studies
### plotting for supplementary figure 3
combined_EF_domain_informantonly <- bind_rows(ADHD_ASD_informantonly_efdomains_dataframe,ADHD_SLD_efdomains_dataframe)
combined_EF_domain_informantonly<-subset(combined_EF_domain_informantonly, N>2)
combined_EF_domain_informantonly <- combined_EF_domain_informantonly %>%mutate(Index = row_number())

grps <- unique(combined_EF_domain_informantonly$comparison) # create list of groups from data frame

# generate all plots; for the top plot we enable data table labels:
g1 <- fplot(combined_EF_domain_informantonly, grps[1], var_names = TRUE)
g2 <- fplot(combined_EF_domain_informantonly, grps[2], x_axis = TRUE, var_names = FALSE)

# merge plots into one single graphic
# we use plot_layout heights to adjust height of plots based on no. of vars
#   per plot, e.g. AS vs TS has only 1 var, so it's height is 1:
finalplot <-
  
  g1 /g2  +
  plot_layout(heights =c(7,7,2, 1))
Finalplot5 <- ggdraw(finalplot) +
  #  Code here creates a title at the centre of the forest plot
  draw_plot_label(x = 0.245,y = 0.97, label = "Favours ADHD   Favours ASD", 
                  hjust = 0, size = 10) +
  # draw_plot_label(x = 0.3,y = .66, label = "Favours ADHD   Favours TD", 
  #                 hjust = 0, size = 10)+
  draw_plot_label(x = 0.245,y = .58, label = "Favours ADHD   Favours SLD", 
                  hjust = 0, size = 10)
  # draw_plot_label(x = 0.205,y = .2, label = "Favours ASD   Favours SLD",
  #                 hjust = 0, size = 10)+
  # draw_plot_label(x = 0.305,y = .12, label = "Favours DS   Favours WS", 
  #                 hjust = 0, size = 10)
ggsave(
  "Supplementaryfigure3_nesting.jpeg",
  plot = `Finalplot5`,
  # device = jpeg(),
  path = NULL,
  scale = 1,
  width = 10,
  height = 10,
  # units = c( "mm"),
  dpi = 600,
  limitsize = FALSE,
  bg = NULL,
)