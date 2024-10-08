Sys.which("make")
#Step 1 Load All packages first
rm(list=ls())

## Loading library
library("readxl")
library(metafor)
library (meta)

#NDDx Overall###########################################
## Loading raw data to merge NDDx vs. Control groups
NDDx_Control_Rawdata <- read_excel ("DataExtraction_NDDx_Controls.xlsx")

# # View(NDDx_Control_Rawdata)
# NDDx_Control_Clean <- na.exclude(NDDx_Control_Rawdata[,c("Study No." , "Study_Year" , "Year of Publication","EF Measure", "EF Domain", "EF_Domain_No" ,
#                                                          "a. Clinical condition", "a. NDDx 1 Mean", "a. NDDx 1 Std-Dev","a. NDDx 1 Sample size",
#                                                          "b. Clinical condition","b. NDDx 2 Mean", "b. NDDx 2 Std-Dev", "b. NDDx 2 Sample size",
#                                                          "c. Clinical condition","c. NDDx 3 Mean", "c. NDDx 3 Std-Dev", "c. NDDx 3 Sample size",
#                                                          "d. Clinical condition", "d. NDDx 4 Mean", "d. NDDx 4 Std-Dev", "d. NDDx 4 Sample size",
#                                                          "e. Clinical condition","e. NDDx 5 Mean", "e. NDDx 5 Std-Dev", "e. NDDx 5 Sample size",
#                                                          "Control Mean","Control Std-Dev","Control Sample size", "Effect_direction", "Assessment Type",
#                                                          "a. NDDx 1: Age Years (Mean)","b. NDDx 2: Age Years (Mean)","c. NDDx 3: Age Years (Mean)", "d. NDDx 4: Age Years (Mean)", "e. NDDx 5: Age Years (Mean)", "Control Mean Age",
#                                                          "DSM No","DSM edition","Gender_NDD (Percentage of Males)")])
# 
# # View(NDDx_Control_Clean)
# 
# i<-c(1)
# NDDx_Control_Clean[,i]<-apply(NDDx_Control_Clean[,i],2,function(x) as.numeric(as.character(x)))
# i<-c(3)
# NDDx_Control_Clean[,i]<-apply(NDDx_Control_Clean[,i],2,function(x) as.numeric(as.character(x)))
# i<-c(6)
# NDDx_Control_Clean[,i]<-apply(NDDx_Control_Clean[,i],2,function(x) as.numeric(as.character(x)))
# i<-c(8:10)
# NDDx_Control_Clean[,i]<-apply(NDDx_Control_Clean[,i],2,function(x) as.numeric(as.character(x))) 
# i<-c(12:14)
# NDDx_Control_Clean[,i]<-apply(NDDx_Control_Clean[,i],2,function(x) as.numeric(as.character(x)))
# i<-c(16:18)
# NDDx_Control_Clean[,i]<-apply(NDDx_Control_Clean[,i],2,function(x) as.numeric(as.character(x)))
# i<-c(20:22)
# NDDx_Control_Clean[,i]<-apply(NDDx_Control_Clean[,i],2,function(x) as.numeric(as.character(x)))
# i<-c(24:38)
# NDDx_Control_Clean[,i]<-apply(NDDx_Control_Clean[,i],2,function(x) as.numeric(as.character(x)))
# i<-c(40)
# NDDx_Control_Clean[,i]<-apply(NDDx_Control_Clean[,i],2,function(x) as.numeric(as.character(x)))
# View(NDDx_Control_Clean)
# 
# # Function to replace "Not Reported" with "Blank" ignoring case
# string_clean <- function(x) {
#   gsub("not reported", "Blank", x, ignore.case = TRUE)
# }
# 
# # Apply the function to each column of the dataframe
# NDDx_Control_Clean[] <- lapply(NDDx_Control_Clean, string_clean)

# # #separate a-e Clinical Conditions and exclude Mean/STD/sample size that has NA in.
# columns_to_include <- c("Study No.", "Study_Year", "EF Measure", "EF Domain", "EF_Domain_No", 
#                         "a. Clinical condition", "a. NDDx 1 Mean", "a. NDDx 1 Std-Dev",
#                         "a. NDDx 1 Sample size", "Control Mean", "Control Std-Dev", 
#                         "Control Sample size", "Effect_direction", "Assessment Type",
#                         "a. NDDx 1: Age Years (Mean)", "Control Mean Age", "DSM No","DSM edition","Gender_NDD (Percentage of Males)","Year of Publication")
# # Subset the data, excluding rows with NAs in specific columns
# clinicalcondition_a_control <- subset(NDDx_Control_Clean[, columns_to_include], 
#                                       !grepl("Blank", NDDx_Control_Clean$`a. Clinical condition`) &
#                                         complete.cases(NDDx_Control_Clean[, c("Study No.", "Study_Year", "EF Measure", 
#                                                                               "EF Domain", "EF_Domain_No", 
#                                                                               "a. NDDx 1 Mean", "a. NDDx 1 Std-Dev",
#                                                                               "a. NDDx 1 Sample size", 
#                                                                               "Control Mean", "Control Std-Dev", 
#                                                                               "Control Sample size", 
#                                                                               "Effect_direction", "Assessment Type")]))
# columns_to_include <- c("Study No.", "Study_Year", "EF Measure", "EF Domain", "EF_Domain_No", 
#                         "b. Clinical condition", "b. NDDx 2 Mean", "b. NDDx 2 Std-Dev",
#                         "b. NDDx 2 Sample size", "Control Mean", "Control Std-Dev", 
#                         "Control Sample size", "Effect_direction", "Assessment Type",
#                         "b. NDDx 2: Age Years (Mean)", "Control Mean Age", "DSM No","DSM edition","Gender_NDD (Percentage of Males)","Year of Publication")
# 
# clinicalcondition_b_control <- subset(NDDx_Control_Clean[, columns_to_include], 
#                                       !grepl("Blank", NDDx_Control_Clean$`b. Clinical condition`) &
#                                         complete.cases(NDDx_Control_Clean[, c("Study No.", "Study_Year", "EF Measure", 
#                                                                               "EF Domain", "EF_Domain_No", 
#                                                                               "b. NDDx 2 Mean", "b. NDDx 2 Std-Dev",
#                                                                               "b. NDDx 2 Sample size", 
#                                                                               "Control Mean", "Control Std-Dev", 
#                                                                               "Control Sample size", 
#                                                                               "Effect_direction", "Assessment Type")]))
# columns_to_include <- c("Study No.", "Study_Year", "EF Measure", "EF Domain", "EF_Domain_No", 
#                         "c. Clinical condition", "c. NDDx 3 Mean", "c. NDDx 3 Std-Dev",
#                         "c. NDDx 3 Sample size", "Control Mean", "Control Std-Dev", 
#                         "Control Sample size", "Effect_direction", "Assessment Type",
#                         "c. NDDx 3: Age Years (Mean)", "Control Mean Age", "DSM No","DSM edition","Gender_NDD (Percentage of Males)","Year of Publication")
# clinicalcondition_c_control <- subset(NDDx_Control_Clean[, columns_to_include], 
#                                       !grepl("Blank", NDDx_Control_Clean$`c. Clinical condition`) &
#                                         complete.cases(NDDx_Control_Clean[, c("Study No.", "Study_Year", "EF Measure", 
#                                                                               "EF Domain", "EF_Domain_No", 
#                                                                               "c. NDDx 3 Mean", "c. NDDx 3 Std-Dev",
#                                                                               "c. NDDx 3 Sample size", 
#                                                                               "Control Mean", "Control Std-Dev", 
#                                                                               "Control Sample size", 
#                                                                               "Effect_direction", "Assessment Type")]))
# columns_to_include <- c("Study No.", "Study_Year", "EF Measure", "EF Domain", "EF_Domain_No", 
#                         "d. Clinical condition", "d. NDDx 4 Mean", "d. NDDx 4 Std-Dev",
#                         "d. NDDx 4 Sample size", "Control Mean", "Control Std-Dev", 
#                         "Control Sample size", "Effect_direction", "Assessment Type",
#                         "d. NDDx 4: Age Years (Mean)", "Control Mean Age", "DSM No","DSM edition","Gender_NDD (Percentage of Males)","Year of Publication")
# clinicalcondition_d_control <- subset(NDDx_Control_Clean[, columns_to_include], 
#                                       !grepl("Blank", NDDx_Control_Clean$`d. Clinical condition`) &
#                                         complete.cases(NDDx_Control_Clean[, c("Study No.", "Study_Year", "EF Measure", 
#                                                                               "EF Domain", "EF_Domain_No", 
#                                                                               "d. NDDx 4 Mean", "d. NDDx 4 Std-Dev",
#                                                                               "d. NDDx 4 Sample size", 
#                                                                               "Control Mean", "Control Std-Dev", 
#                                                                               "Control Sample size", 
#                                                                               "Effect_direction", "Assessment Type")]))
# columns_to_include <- c("Study No.", "Study_Year", "EF Measure", "EF Domain", "EF_Domain_No", 
#                         "e. Clinical condition", "e. NDDx 5 Mean", "e. NDDx 5 Std-Dev",
#                         "e. NDDx 5 Sample size", "Control Mean", "Control Std-Dev", 
#                         "Control Sample size", "Effect_direction", "Assessment Type",
#                         "e. NDDx 5: Age Years (Mean)", "Control Mean Age", "DSM No","DSM edition","Gender_NDD (Percentage of Males)","Year of Publication")
# clinicalcondition_e_control <- subset(NDDx_Control_Clean[, columns_to_include], 
#                                       !grepl("Blank", NDDx_Control_Clean$`e. Clinical condition`) &
#                                         complete.cases(NDDx_Control_Clean[, c("Study No.", "Study_Year", "EF Measure", 
#                                                                               "EF Domain", "EF_Domain_No", 
#                                                                               "e. NDDx 5 Mean", "e. NDDx 5 Std-Dev",
#                                                                               "e. NDDx 5 Sample size", 
#                                                                               "Control Mean", "Control Std-Dev", 
#                                                                               "Control Sample size", 
#                                                                               "Effect_direction", "Assessment Type")]))
# #
# # #combine overall conditions
# clinicalcondition_a_control_temp = setNames(clinicalcondition_a_control, c("Study No." ,"Study_Year",'EF Measure','EF Domain','EF_Domain_No',"Clinical condition", 'NDDx Mean', 'NDDx Std-Dev', 'NDDx Sample size','Control Mean','Control Std-Dev','Control Sample size',"Effect_direction","Assessment Type","NDDx Age Years (Mean)", "Control Mean Age", "DSM No","DSM edition","NDDx_Gender_Male","Year of Publication"))
# clinicalcondition_b_control_temp = setNames(clinicalcondition_b_control, c("Study No." ,"Study_Year",'EF Measure','EF Domain','EF_Domain_No','Clinical condition','NDDx Mean', 'NDDx Std-Dev', 'NDDx Sample size','Control Mean','Control Std-Dev','Control Sample size',"Effect_direction", "Assessment Type","NDDx Age Years (Mean)", "Control Mean Age", "DSM No","DSM edition","NDDx_Gender_Male","Year of Publication"))
# clinicalcondition_c_control_temp = setNames(clinicalcondition_c_control, c("Study No." ,"Study_Year",'EF Measure','EF Domain','EF_Domain_No','Clinical condition','NDDx Mean', 'NDDx Std-Dev', 'NDDx Sample size','Control Mean','Control Std-Dev','Control Sample size',"Effect_direction", "Assessment Type","NDDx Age Years (Mean)", "Control Mean Age", "DSM No","DSM edition","NDDx_Gender_Male","Year of Publication"))
# clinicalcondition_d_control_temp = setNames(clinicalcondition_d_control, c("Study No." ,"Study_Year",'EF Measure','EF Domain','EF_Domain_No','Clinical condition','NDDx Mean', 'NDDx Std-Dev', 'NDDx Sample size','Control Mean','Control Std-Dev','Control Sample size',"Effect_direction", "Assessment Type","NDDx Age Years (Mean)", "Control Mean Age", "DSM No","DSM edition","NDDx_Gender_Male","Year of Publication"))
# clinicalcondition_e_control_temp = setNames(clinicalcondition_e_control, c("Study No." ,"Study_Year",'EF Measure','EF Domain','EF_Domain_No','Clinical condition','NDDx Mean', 'NDDx Std-Dev', 'NDDx Sample size','Control Mean','Control Std-Dev','Control Sample size',"Effect_direction", "Assessment Type","NDDx Age Years (Mean)", "Control Mean Age", "DSM No","DSM edition","NDDx_Gender_Male","Year of Publication"))
# All_clinicalcondition_with_control <- Reduce(function(x, y) merge(x, y, all=TRUE),
#                                              list(clinicalcondition_a_control_temp, clinicalcondition_b_control_temp, clinicalcondition_c_control_temp, clinicalcondition_d_control_temp, clinicalcondition_e_control_temp))
# # All_clinicalcondition_with_control[complete.cases(All_clinicalcondition_with_control), ]
# rm(clinicalcondition_a_control_temp,clinicalcondition_b_control_temp,clinicalcondition_c_control_temp,clinicalcondition_d_control_temp,clinicalcondition_e_control_temp)

# Once effect direction changed, manual load above file again
# All_clinicalcondition_with_control <- read_excel ("path_to_excelfile.xlsx")
# All_clinicalcondition_with_control <- All_clinicalcondition_with_control[, -which(names(All_clinicalcondition_with_control) == "Effect_direction")]

# ## Codes to merge manually coded EF directions to the latest 3_level file.
# common_cols <- intersect(names(All_clinicalcondition_with_control), names(All_clinicalcondition_with_control))
# common_cols <- setdiff(names(All_clinicalcondition_with_control), "Effect_direction")
# 
# # Merge based on common columns
# # merged_df <- merge(All_clinicalcondition_with_control, All_clinicalcondition_with_control[, c(common_cols, "Effect_direction")], by = common_cols, all.x = TRUE, all.y = FALSE)
# All_clinicalcondition_with_control <- merge(All_clinicalcondition_with_control, All_clinicalcondition_with_control, by = common_cols)
# 
# # #Need to hide this after saving the effect size directions information in this file.
# writexl::write_xlsx(All_clinicalcondition_with_control,"path_tosave_dataframe.xlsx",col_names = TRUE)

All_clinicalcondition_with_control <- read_excel ("3_Level_Table_NDDx_Control_v3_renamed_nesting.xlsx")
All_clinicalcondition_with_control <- All_clinicalcondition_with_control[!is.na(All_clinicalcondition_with_control$Effect_direction), ]
i<-c(5)
All_clinicalcondition_with_control[, i] <- apply(All_clinicalcondition_with_control[,i],2,function(x) as.numeric(as.character(x))) 
i<-c(7:16)
All_clinicalcondition_with_control[,i]<-apply(All_clinicalcondition_with_control[,i],2,function(x) as.numeric(as.character(x)))
i<-c(18:19)
All_clinicalcondition_with_control[,i]<-apply(All_clinicalcondition_with_control[,i],2,function(x) as.numeric(as.character(x)))
i<-c(23)
All_clinicalcondition_with_control[,i]<-apply(All_clinicalcondition_with_control[,i],2,function(x) as.numeric(as.character(x)))

# calculate yi, vi per outcome
### NDDx Effect Size Calculations 
NDDx_control_EffectSize<- escalc(measure="SMD", m1i=All_clinicalcondition_with_control$`NDDx Mean`, sd1i=All_clinicalcondition_with_control$`NDDx Std-Dev`, n1i=All_clinicalcondition_with_control$`NDDx Sample size`, m2i=All_clinicalcondition_with_control$`Control Mean`,sd2i=All_clinicalcondition_with_control$`Control Std-Dev`,n2i=All_clinicalcondition_with_control$`Control Sample size`,
                                 slab=paste(All_clinicalcondition_with_control$`Study_Year`,sep=""))

### Switch sign based on EF direction
new_yi <- NDDx_control_EffectSize$yi

# All_clinicalcondition_with_control$`Effect_direction`

# EF Direction 2 - NDDx is better, 3 - TD is better, 1- same
for (i in 1:length(new_yi)){
  # print(i)
  if(All_clinicalcondition_with_control$`Effect_direction`[i] == 2 && new_yi[i] > 0){
    new_yi[i] = -new_yi[i]
  } else if(All_clinicalcondition_with_control$`Effect_direction`[i] == 3 && new_yi[i] < 0){
    new_yi[i] = -new_yi[i]
  }
}
All_clinicalcondition_with_control$yi <- new_yi
All_clinicalcondition_with_control$vi <-NDDx_control_EffectSize$vi

##Overall random effect
NDDx_summary <- escalc(yi=yi,vi=vi,data=All_clinicalcondition_with_control)
View(NDDx_summary)
# writexl::write_xlsx(NDDx_summary,"path_to_save_effectsizes.xlsx",col_names = TRUE)
#################################################################################################################################################################
# Standard error Calculation
NDDx_summary$SE <- sqrt(NDDx_summary$vi)

# Overall Multi-variant random effect (across NDDx/TD studies) executive function
random_effect_study_ef <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=NDDx_summary,slab=paste(NDDx_summary$`Study.No.`,sep=""))
print(random_effect_study_ef, digits=3)
predict(random_effect_study_ef)

##performance only
NDDx_summary_performanceonly <-subset(NDDx_summary,NDDx_summary$`Assessment.Type`==0)
random_effect_study_ef_performanceonly <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=NDDx_summary_performanceonly,slab=paste(NDDx_summary_performanceonly$`Study.No.`,sep=""))
print(random_effect_study_ef_performanceonly, digits=3)
predict(random_effect_study_ef_performanceonly)
##Informant only
NDDx_summary_informantonly <-subset(NDDx_summary,NDDx_summary$`Assessment.Type`==1)

random_effect_study_ef_informant <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=NDDx_summary_informantonly,slab=paste(NDDx_summary_informantonly$`Study.No.`,sep=""))
print(random_effect_study_ef_informant, digits=3)
predict(random_effect_study_ef_informant)
###################################################################
# Prepare for funnel plot and trim-and-fill analysis
Data_meta_means_nddx_overall <-
  NDDx_summary %>% select(`Study.No.`, `Study_Year`, `EF.Domain`, yi, SE, vi) %>%
  group_by(`Study.No.`,) %>%
  summarise(mean_g = mean(yi), mean_SE = mean(SE), mean_vi = mean(vi))


View(Data_meta_means_nddx_overall)

writexl::write_xlsx(Data_meta_means_nddx_overall, "path_to_save_nesteddata.xlsx")


## Funnel Plot for Asymmetric Test - Univariant
eggers <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`,mods = SE, data=NDDx_summary,slab=paste(NDDx_summary$`Study.No.`,sep=""))
print(eggers)
# # Uni-variant 
random_effect_study_ef_univariant <- rma(yi = mean_g, sei = mean_SE, data =Data_meta_means_nddx_overall) 
print(random_effect_study_ef_univariant,digits=3)
predict(random_effect_study_ef_univariant)

### Funnel plots preparation (Supplementary Figure 1)
# Convert dimensions from millimeters to inches
width_mm <- 180  # width in millimeters
height_mm <- 120 # height in millimeters

width_in <- width_mm / 25.4  # Convert to inches
height_in <- height_mm / 25.4 # Convert to inches

# Plot in svg format - eps format does not recognise <= sign
svg("SupplementaryFigure1_nesting_funnelplot.svg", width = width_in, height = height_in, pointsize = 7)


par(mfrow = c(1, 2), mar = c(5, 4, 4, 2) + 0.1, oma = c(3, 1, 3, 1), 
    family = "Helvetica", cex = 0.7, cex.lab = 0.7, cex.axis = 0.7, cex.main = 0.8)
funnel(random_effect_study_ef_univariant, main = "Nested Effect Sizes Across Studies (Original)")
metabias(random_effect_study_ef_univariant$yi,random_effect_study_ef_univariant$vi)
mtext("(A)", side = 1, line = 5, at = mean(par("usr")[1:2]), cex = 0.7, family = "Helvetica")

# taf <- trimfill(random_effect_study_ef_univariant, side = "left")
# funnel(taf)
# # metabias(taf$yi,taf$vi)

## remove study no - g>2
NDDx_summary_remove_glargerthan2<-subset(NDDx_summary, NDDx_summary$Study.No.!=173)
eggers <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`,mods = SE, data=NDDx_summary_remove_glargerthan2,slab=paste(NDDx_summary_remove_glargerthan2$`Study.No.`,sep=""))

Data_meta_means_nddx_overall_remove_glargerthan2<-subset(Data_meta_means_nddx_overall, Data_meta_means_nddx_overall$Study.No.!=173)
random_effect_study_ef_univariant_remove_glargerthan2 <- rma(yi = mean_g, sei = mean_SE, data = Data_meta_means_nddx_overall_remove_glargerthan2)
print(random_effect_study_ef_univariant_remove_glargerthan2, digits=3)
predict(random_effect_study_ef_univariant_remove_glargerthan2)
# funnel(random_effect_study_ef_univariant_remove_glargerthan2)

taf_remove_glargerthan2 <- trimfill(random_effect_study_ef_univariant_remove_glargerthan2, side = "left")
funnel(taf_remove_glargerthan2, main = "Nested Effect Sizes Across Studies (Trim-and-Fill)",legend=TRUE)
metabias(taf_remove_glargerthan2$yi,taf_remove_glargerthan2$vi)
mtext("(B)", side = 1, line = 5, at = mean(par("usr")[1:2]), cex = 0.7, family = "Helvetica")

dev.off()

#Update the NDDX dataframe
NDDx_summary<-NDDx_summary_remove_glargerthan2#If outliers removed use this, and run the above multi-variant analysis again to confirm changes
random_effect_study_ef <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=NDDx_summary,slab=paste(NDDx_summary$`Study.No.`,sep=""))
print(random_effect_study_ef, digits=3)
predict(random_effect_study_ef)
##################################

# Forest plot per EF Domain
skipped_domains_norf_lessstudyonly <-c()
EF_domains<-c()
EF_domains_no_rf_pooled_ef <- list()
EF_domains_no_rf_pooled_ci_lb <- list()
EF_domains_no_rf_pooled_ci_ub <- list()
EF_domains_no_rf_pooled_k <- list()
EF_domains_no_rf_pooled_N <- list()
EF_domains_no_rf_pooled_p <- list()
temp <-list()
flag = 1
for (domain_label in unique(NDDx_summary$EF.Domain)){
  each_ef_domain_dataset <- c()
  each_ef_domain_dataset <-NDDx_summary%>% filter(NDDx_summary$EF.Domain== domain_label)
  #print(domain_label)
  if(length(unique(each_ef_domain_dataset$Study.No.)) <=2){
    skipped_domains_norf_lessstudyonly<-c(skipped_domains_norf_lessstudyonly, domain_label)
    next
  }
  #print(each_ef_domain_dataset)
  EF_domains[[flag]] <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=each_ef_domain_dataset, slab=paste(each_ef_domain_dataset$`Study_Year`,sep=""))
  
  #print(EF_domains[[domain_label]], digits=3)
  forest_plot_efdomain<-metafor::forest(EF_domains[[flag]],addpred=TRUE,header=TRUE)
  text(x = 0, y = nrow(each_ef_domain_dataset) + 1, labels = domain_label, pos = 3) 
  # print(forest_plot_efdomain, digits = 3)
  
  # Extract pooled effect size from each domain_label
  pooled_effect <- EF_domains[[flag]]$b
  pooled_effect_ci_lb <- EF_domains[[flag]]$ci.lb
  pooled_effect_ci_ub <- EF_domains[[flag]]$ci.ub
  pooled_effect_k <- EF_domains[[flag]]$k
  pooled_effect_N <- EF_domains[[flag]]$s.nlevels
  pooled_effect_p <- EF_domains[[flag]]$pval
  
  EF_domains_no_rf_pooled_ef[[flag]]<-pooled_effect
  EF_domains_no_rf_pooled_ci_lb[[flag]]<-pooled_effect_ci_lb
  EF_domains_no_rf_pooled_ci_ub[[flag]]<-pooled_effect_ci_ub
  EF_domains_no_rf_pooled_k[[flag]]<-pooled_effect_k
  EF_domains_no_rf_pooled_N[[flag]]<-pooled_effect_N
  
  pooled_effect_p <- EF_domains[[flag]]$pval
  
  if (pooled_effect_p < 0.001) {
    result <- "<0.001"
  } else {
    result <- format(pooled_effect_p, digits = 3)
  }
  
  EF_domains_no_rf_pooled_p[[flag]] <- result
  
  # Create a data frame of pooled effects
  current <- data.frame(
    ef_domain = domain_label,
    estimate = unlist(EF_domains_no_rf_pooled_ef[[flag]][,1]),
    lower_ci = unlist(EF_domains_no_rf_pooled_ci_lb[[flag]]),
    upper_ci  = unlist(EF_domains_no_rf_pooled_ci_ub[[flag]]),
    k = unlist(EF_domains_no_rf_pooled_k[[flag]]),
    N  = unlist(EF_domains_no_rf_pooled_N[[flag]]),
    pval = unlist(EF_domains_no_rf_pooled_p[[flag]]),
    comparison ="EF Domain"
  )
  temp[[flag]]<-current
  
  flag = flag +1
}

pooled_effects_efdomains_dataframe<-do.call(rbind,temp)
pooled_effects_efdomains_dataframe<- pooled_effects_efdomains_dataframe  %>% mutate(pooled_effects_efdomains_dataframe,
                                                                                    brackets = sprintf("%.2f (%.2f, %.2f)",estimate,lower_ci,upper_ci))
# writexl::write_xlsx(pooled_effects_efdomains_dataframe,"Figure3_nesting_data_NDDx_control_effectsizes_EF_no_rf_perdomain.xlsx",col_names=TRUE)

cat("less than three studies, not suitable for meta-analysis:", paste(skipped_domains_norf_lessstudyonly, collapse = ", "),"\n") 
skipped_norf_datasets <- NDDx_summary[NDDx_summary$EF.Domain %in% skipped_domains_norf_lessstudyonly, ]
# writexl::write_xlsx(skipped_norf_datasets,"NDDx_control_effectsizes_EF_no_rf_perdomain_skippedstudies.xlsx",col_names=TRUE)

# Install pacman package if it is not already done:
if (!require("pacman")) install.packages("pacman")
# p_load combines install.packages() and library() together:
pacman::p_load(tidyverse, ggeasy, janitor, patchwork, cowplot)
# read data
source("fplot_updated2.R")
grps <- unique(pooled_effects_efdomains_dataframe$comparison) # create list of groups from data frame
g1 <- fplot(pooled_effects_efdomains_dataframe, grps[1], var_names = TRUE, x_axis = TRUE, x_axis_centred = TRUE)
finalplot <- g1
FinalPlot2 <- ggdraw(finalplot) +
  draw_plot(finalplot, 0, 0, 1, 1) +
  draw_text(
    "Effect Analysis Across Executive Function Domains", 
    x = 0.5, y = 1,  # Center horizontally and place at the top
    hjust = 0.5, vjust = 1.5,  # Adjust vertical position to move title up or down
    size = 7, fontface = "bold",family="Helvetica"
  )+
  draw_plot_label(x = 0.142,y = .95, label = "Favours Clinical Groups  Favours Control", 
                  hjust = 0, size = 7, colour="#0072B2",family="Helvetica",fontface = "bold") +
  theme(
    text = element_text(family = "Helvetica", size = 7),
    # plot.margin = margin(t = 20, r = 10, b = 10, l = 10)  # Add more space at the top
  )
ggsave(
  "Figure2_Aug2024.eps",
  plot = FinalPlot2,
  #device = svg(),
  path = NULL,
  scale = 1,
  width = 180,
  height = 120,
  units = c( "mm"),
  dpi = 450,
  limitsize = FALSE,
  bg = NULL,
)
#########################################################################
# Separate Overall Effect for each NDC vs. Control pair


for (condi in unique(All_clinicalcondition_with_control$`Clinical condition`)){
  each_ndc_group <-subset(All_clinicalcondition_with_control,All_clinicalcondition_with_control$`Clinical condition`==condi)
  Number_of_study <-length(unique(each_ndc_group$`Study No.`))
  print(condi)
  
  print(Number_of_study)
  
  if (Number_of_study <=2){
    next
  }
  random_effect_study_ef_each_condition <- rma.mv(yi, vi, random = ~ 1 |`Study No.`, data=each_ndc_group,slab=paste(each_ndc_group$`Study No.`,sep=""))
  print(random_effect_study_ef_each_condition)
}


# Single NDC Group Effect
single_ndc_group <-subset(All_clinicalcondition_with_control,All_clinicalcondition_with_control$`condition type`==0)
random_effect_study_ef_single_ndc_group  <- rma.mv(yi, vi, random = ~ 1 |`Study No.`, data=single_ndc_group,slab=paste(single_ndc_group$`Study No.`,sep=""))
print(random_effect_study_ef_single_ndc_group )
### Single NDC vs. control overall EF plot
# read data
single_NDC_control_dataframe <- read_excel ("Final_figure3_singleNDDx_control_effectsizes.xlsx")
single_NDC_control_dataframe <- single_NDC_control_dataframe %>%
  mutate(pval = ifelse(pval == "<0.001", "<0.001", format(as.numeric(pval), digits = 3)))
grps <- unique(single_NDC_control_dataframe$comparison) # create list of groups from data frame
g1 <- fplot(single_NDC_control_dataframe, grps[1], var_names = TRUE, x_axis = TRUE, x_axis_centred = TRUE)
finalplot <- g1

FinalPlot2 <- ggdraw(finalplot) +
  #  Code here creates a title at the centre of the forest plot
  draw_plot(finalplot, 0, 0, 1, 1) +
  draw_text(
    "Effect Analysis For Each NDC Compared To Controls", 
    x = 0.5, y = 1,  # Center horizontally and place at the top
    hjust = 0.5, vjust = 1.5,  # Adjust vertical position to move title up or down
    size = 7, fontface = "bold",family="Helvetica"
  )+
  draw_plot_label(x = 0.173,y = .95, label = "Favours Clinical Groups  Favours Control", 
                  hjust = 0, size = 7, colour="#0072B2",family="Helvetica",fontface="bold")

ggsave(
  "Figure3_Aug2024.eps",
  plot = FinalPlot2,
  path = NULL,
  scale = 1,
  width = 180,
  height = 120,
  units = c( "mm"),
  dpi = 450,
  limitsize = FALSE,
  bg = NULL,
)


##################################################################################
# Moderator Analysis - Type of Measure
subset_all_studies_with_tom <-NDDx_summary%>% filter(!is.na(`Assessment.Type`),`Assessment.Type` !="")

random_effect_study_ef_tom <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=subset_all_studies_with_tom,slab=paste(subset_all_studies_with_tom$`Study.No.`,sep=""))
moderator_tom<-rma.mv(yi, vi, random = ~ 1 |`Study.No.`, mods = ~factor(`Assessment.Type`), data=subset_all_studies_with_tom,
       slab=paste(subset_all_studies_with_tom$`Study.No.`,sep=""))
print(moderator_tom, digits=3)

# to calcualte R^2 
#(OBSv - MODv) / OBVv
max(0,100 * (sum(random_effect_study_ef_tom$sigma2) - sum(moderator_tom$sigma2)) / sum(random_effect_study_ef_tom$sigma2))

moderator_tom_for_subgroupstats <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, mods = ~factor(`Assessment.Type`) -1, data=subset_all_studies_with_tom,
                                          slab=paste(subset_all_studies_with_tom$`Study.No.`,sep=""))
print(moderator_tom_for_subgroupstats, digits=3)

# Moderator Analysis - Age (Categorical)
## Step 1: include <5, <12, <18
for (i in 1:length(NDDx_summary$`NDDx.Age.Years..Mean.`)){
  if(NDDx_summary$`NDDx.Age.Years..Mean.`[i] <=5 & !is.na(NDDx_summary$`NDDx.Age.Years..Mean.`[i])){
    NDDx_summary$NDDx_Age_category[i] <- 1
  } else if(NDDx_summary$`NDDx.Age.Years..Mean.`[i] <=12 & !is.na(NDDx_summary$`NDDx.Age.Years..Mean.`[i])){
    NDDx_summary$NDDx_Age_category[i] <- 2
  } else if(NDDx_summary$`NDDx.Age.Years..Mean.`[i] >12 & !is.na(NDDx_summary$`NDDx.Age.Years..Mean.`[i])){
    NDDx_summary$NDDx_Age_category[i] <- 3
  }else
    NDDx_summary$NDDx_Age_category[i] <- NA
}

subset_all_studies_with_age <-NDDx_summary%>% filter(!is.na(`NDDx_Age_category`),`NDDx_Age_category` !="")

random_effect_study_ef_age<- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=subset_all_studies_with_age,slab=paste(subset_all_studies_with_age$`Study.No.`,sep=""))

moderator_age <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, mods = ~factor(`NDDx_Age_category`), data=subset_all_studies_with_age,
                        slab=paste(subset_all_studies_with_age$`Study.No.` ,sep=""))
print(moderator_age, digits=3)
moderator_age$I2
# to calcualte R^2 
#(OBSv - MODv) / OBVv
max(0,100 * (sum(random_effect_study_ef_age$sigma2) - sum(moderator_age$sigma2)) / sum(random_effect_study_ef_age$sigma2))

moderator_age_for_subgroupstats <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, mods = ~factor(`NDDx_Age_category`)-1, data=subset_all_studies_with_age,
                   slab=paste(subset_all_studies_with_age$`Study.No.` ,sep=""))
print(moderator_age_for_subgroupstats)


# Moderator Analysis - Gender Continuous
subset_all_studies_with_gender <-NDDx_summary%>% filter(!is.na(`NDDx_Gender_Male`),`NDDx_Gender_Male` !="")
random_effect_study_ef_gender <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=subset_all_studies_with_gender,slab=paste(subset_all_studies_with_gender$`Study.No.`,sep=""))
moderator_gender <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, mods = ~`NDDx_Gender_Male`, data=subset_all_studies_with_gender,
                           slab=paste(subset_all_studies_with_gender$`Study.No.`,sep=""))

print(moderator_gender, digits=3)
moderator_gender$I2

# to calcualte R^2 
#(OBSv - MODv) / OBVv
max(0,100 * (sum(random_effect_study_ef_gender$sigma2) - sum(moderator_gender$sigma2)) / sum(random_effect_study_ef_gender$sigma2))

# Moderator Analysis - Publication Year
subset_all_studies_with_YoP <-NDDx_summary%>% filter(!is.na(`Year.of.Publication`),`Year.of.Publication` !="")
random_effect_study_ef_YoP <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=subset_all_studies_with_YoP,slab=paste(subset_all_studies_with_YoP$`Study.No.`,sep=""))

moderator_YoP <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, mods = `Year.of.Publication`, data=subset_all_studies_with_YoP,
                  slab=paste(subset_all_studies_with_YoP$`Study.No.`,sep=""))
print(moderator_YoP, digits=3)

# to calcualte R^2 
#(OBSv - MODv) / OBVv
max(0,100 * (sum(random_effect_study_ef_YoP$sigma2) - sum(moderator_YoP$sigma2)) / sum(random_effect_study_ef_YoP$sigma2))


################################################################
# Moderator Analysis - DSM version
subset_all_studies_with_DSM <-NDDx_summary%>% filter(!is.na(`DSM.No`),`DSM.No` !="")
random_effect_study_ef_DSM <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=subset_all_studies_with_DSM,slab=paste(subset_all_studies_with_DSM$`Study.No.`,sep=""))

moderator_DSM <-rma.mv(yi, vi, random = ~ 1 |`Study.No.`, mods = ~factor(`DSM.No`), data=subset_all_studies_with_DSM,
                        slab=paste(subset_all_studies_with_DSM$`Study.No.`,sep=""))
print(moderator_DSM, digits=3)
moderator_DSM$I2
# to calcualte R^2 
#(OBSv - MODv) / OBVv
max(0,100 * (sum(random_effect_study_ef_DSM$sigma2) - sum(moderator_DSM$sigma2)) / sum(random_effect_study_ef_DSM$sigma2))

moderator_DSM_for_subgroupstats <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, mods = ~factor(`DSM.No`) -1, data=subset_all_studies_with_DSM,
                  slab=paste(subset_all_studies_with_DSM$`Study.No.`,sep=""))
print(moderator_DSM_for_subgroupstats, digits=3)
# DSM moderator - performance only
subset_all_studies_with_DSM_performanceonly= subset(subset_all_studies_with_DSM,subset_all_studies_with_DSM$`Assessment.Type`==0)
random_effect_study_ef_DSM_performanceonly <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=subset_all_studies_with_DSM_performanceonly,slab=paste(subset_all_studies_with_DSM_performanceonly$`Study.No.`,sep=""))

moderator_DSM_performance <-rma.mv(yi, vi, random = ~ 1 |`Study.No.`, mods = ~factor(`DSM.No`), data=subset_all_studies_with_DSM_performanceonly,
                                   slab=paste(subset_all_studies_with_DSM_performanceonly$`Study.No.`,sep=""))

print(moderator_DSM_performance, digits=3)

# to calcualte R^2 
#(OBSv - MODv) / OBVv
max(0,100 * (sum(random_effect_study_ef_DSM_performanceonly$sigma2) - sum(moderator_DSM_performance$sigma2)) / sum(random_effect_study_ef_DSM_performanceonly$sigma2))

moderator_DSM_performance_for_subgroupstats <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, mods = ~factor(`DSM.No`) -1, data=subset_all_studies_with_DSM_performanceonly,
                                                      slab=paste(subset_all_studies_with_DSM_performanceonly$`Study.No.`,sep=""))
print(moderator_DSM_performance_for_subgroupstats)
# DSM moderator - Informant only
subset_all_studies_with_DSM_informantonly= subset(subset_all_studies_with_DSM,subset_all_studies_with_DSM$`Assessment.Type`==1)
random_effect_study_ef_DSM_informantonly<- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=subset_all_studies_with_DSM_informantonly,slab=paste(subset_all_studies_with_DSM_informantonly$`Study.No.`,sep=""))

moderator_DSM_informant<-rma.mv(yi, vi, random = ~ 1 |`Study.No.`, mods = ~factor(`DSM.No`), data=subset_all_studies_with_DSM_informantonly,
                                slab=paste(subset_all_studies_with_DSM_informantonly$`Study.No.`,sep=""))
print(moderator_DSM_informant)
# to calcualte R^2 
#(OBSv - MODv) / OBVv
max(0,100 * (sum(random_effect_study_ef_DSM_informantonly$sigma2) - sum(moderator_DSM_informant$sigma2)) / sum(random_effect_study_ef_DSM_informantonly$sigma2))


moderator_DSM_informant_forsubgroupstats <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, mods = ~factor(`DSM.No`) -1, data=subset_all_studies_with_DSM_informantonly,
                                                   slab=paste(subset_all_studies_with_DSM_informantonly$`Study.No.`,sep=""))
print(moderator_DSM_informant_forsubgroupstats, digits=3)


###################################################################################################################################################################
# Single NDC Group Effect
single_ndc_group <-subset(NDDx_summary,NDDx_summary$`condition.type`==0)
random_effect_study_ef_single_ndc_group  <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=single_ndc_group,slab=paste(single_ndc_group$`Study.No.`,sep=""))
print(random_effect_study_ef_single_ndc_group )

## Single group DSM moderator
subset_single_ndc_group_with_DSM <-single_ndc_group%>% filter(!is.na(`DSM.No`),`DSM.No` !="")
random_effect_single_ndc_DSM <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=subset_single_ndc_group_with_DSM,slab=paste(subset_single_ndc_group_with_DSM$`Study.No.`,sep=""))

moderator_DSM_single_ndc_group<-rma.mv(yi, vi, random = ~ 1 |`Study.No.`, mods = ~factor(`DSM.No`), data=subset_single_ndc_group_with_DSM,
       slab=paste(subset_single_ndc_group_with_DSM$`Study.No.`,sep=""))
print(moderator_DSM_single_ndc_group, digits=3)
moderator_DSM_single_ndc_group$I2

# to calcualte R^2 
#(OBSv - MODv) / OBVv
max(0,100 * (sum(random_effect_single_ndc_DSM$sigma2) - sum(moderator_DSM_single_ndc_group$sigma2)) / sum(random_effect_single_ndc_DSM$sigma2))

moderator_DSM_single_ndc_group_forsubgroups_stats <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, mods = ~factor(`DSM.No`) -1, data=subset_single_ndc_group_with_DSM,
                                    slab=paste(subset_single_ndc_group_with_DSM$`DSM.edition`,sep=""))
print(moderator_DSM_single_ndc_group_forsubgroups_stats, digits=3)

## Single group performance effect
single_ndc_group_performanceonly = subset(single_ndc_group,single_ndc_group$`Assessment.Type`==0)
random_effect_study_ef_single_ndc_group_performanceonly  <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=single_ndc_group_performanceonly,slab=paste(single_ndc_group_performanceonly$`Study.No.`,sep=""))
print(random_effect_study_ef_single_ndc_group_performanceonly )

####################################################################
# Comorbid group effect
comorbid_ndc_group <-subset(NDDx_summary,NDDx_summary$`condition.type`==1)
random_effect_study_ef_comorbid_ndc_group <- rma.mv(yi, vi, random = ~ 1 |`Study.No.`, data=comorbid_ndc_group,slab=paste(comorbid_ndc_group$`Study.No.`,sep=""))
print(random_effect_study_ef_comorbid_ndc_group )