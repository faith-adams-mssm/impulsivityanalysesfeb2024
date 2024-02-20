###################################################################
######### Impulsivity Paper February 2024 Submission ###############
###################################################################

############## LOAD AND INSTALL PACKAGES ###############
# install.packages("dpylr")
# install.packages("lmer")
# install.packages("afex")
# install.packages("boot.pval")
# install.packages("sjPlot")
# install.packages("ggplot2")
# install.packages("table1")
# install.packages("ggpubr")
# install.packages("GGally")
# install.packages("ggeffects")
# install.packages("sjmisc")
# install.packages("modelsummary")
# install.packages("ggplot2")
# install.packages("tidyr")
# install.packages("Hmisc")
# install.packages("Matrix")


library(dplyr)       # data cleaning
library(lme4)        # running regression models 
library(afex)        # to obtain p-values
library(boot.pval)   # to obtain bootstrap models with p values - check this 
library(sjPlot)      # tabulating and plotting 
library(ggplot2)     # plot for coefficients from regression model 
library(table1)      # summary table
library(ggpubr)      # convenient summary statistics and plots
library(GGally)      # advanced plot
library(ggeffects)   # marginal effects, adjusted predictions
library(sjmisc)      # factor level variables
library(modelsummary) #plotting the coefficients
library(ggplot2)
library(tidyr)      #change the data format from long to wide 
library(Hmisc)      #used for the looking at the correlations 

########################## BUILD DATAFRAME ########################################
#BASELINE MEASURES#
ssrt <- 
  all_imaging_data %>%
  select(src_subject_id, eventname, tfmri_sst_all_beh_total_mssrt) %>%
  rename(SSRT = tfmri_sst_all_beh_total_mssrt) %>%
  filter(eventname == "baseline_year_1_arm_1")%>%
  select(-eventname)


baseline_df <- 
  `non-imaging_excluding_nt_5.0` %>%
  select(src_subject_id,interview_age, demo_prnt_empl_v2,demo_comb_income_v2, demo_prnt_ed_v2,demo_prnt_marital_v2, 
         site_id_l, demo_sex_v2.x, demo_prnt_age_v2,interview_age, race_ethnicity, rel_family_id.x, rel_relationship, eventname, cbcl_scr_syn_internal_t, cbcl_scr_syn_external_t,	famhx_ss_momdad_alc_p, upps_y_ss_negative_urgency, upps_y_ss_positive_urgency,
         upps_y_ss_lack_of_perseverance, upps_y_ss_lack_of_planning, upps_y_ss_sensation_seeking, upps_y_ss_negative_urgency, upps_y_ss_positive_urgency, upps_y_ss_lack_of_perseverance, upps_y_ss_lack_of_planning, upps_y_ss_sensation_seeking,
         upps7_y, upps11_y, upps17_y, upps20_y, upps6_y, upps16_y, upps23_y, upps28_y, 
         upps12_y, upps18_y, upps21_y, upps27_y, upps35_y, upps36_y, upps37_y, upps39_y, upps15_y, upps19_y, upps22_y, upps24_y, 
         cash_choice_task,demo_fam_exp1_v2, demo_fam_exp2_v2, demo_fam_exp3_v2, demo_fam_exp4_v2, 
         demo_fam_exp5_v2, demo_fam_exp6_v2, demo_fam_exp7_v2,  tlfb_alc_sip, tlfb_alc_use) %>%
  filter(eventname == "baseline_year_1_arm_1") %>%
  rename(age_baseline = interview_age) %>%
  mutate(age_baseline = age_baseline/12)%>%
  select(-eventname) %>%
  rename(sex_assigned = demo_sex_v2.x, parental.employment = demo_prnt_empl_v2, household.income = demo_comb_income_v2, 
         high.educ = demo_prnt_ed_v2, fam_id = rel_family_id.x, fam_relationship = rel_relationship, marital_status = demo_prnt_marital_v2, abcd_site = site_id_l, parent_age = demo_prnt_age_v2) %>%
  mutate (sex_assigned = ifelse(sex_assigned == 1, "Male", #male
                                ifelse(sex_assigned == 2, "Female", NA))) %>%
  mutate(household_parent_status = ifelse(marital_status == 1, "two parent", #married or living together
                                          ifelse(marital_status == 2, "single parent", #single parent household
                                                 ifelse(marital_status == 3, "single parent",
                                                        ifelse(marital_status == 4, "single parent",
                                                               ifelse(marital_status == 5, "single parent", 
                                                                      ifelse(marital_status == 6, "two parent", NA))))))) %>% #married or living with partner
  mutate(parental.employment = ifelse(parental.employment == 1, "Employed", #employed
                                      ifelse(parental.employment == 2, "Unemployed", #unemployed 
                                             ifelse (parental.employment == 3, "Unemployed",
                                                     ifelse(parental.employment == 4, "Unemployed",
                                                            ifelse(parental.employment == 5, "Unemployed",
                                                                   ifelse(parental.employment == 6, "Unemployed",
                                                                          ifelse(parental.employment == 7, "Unemployed",
                                                                                 ifelse(parental.employment == 8, "Unemployed",
                                                                                        ifelse(parental.employment == 9, "Employed",
                                                                                               ifelse(parental.employment == 10, "Employed", NA))))))))))) %>%
  mutate(race_ethnicity = ifelse(race_ethnicity == 1, "White",
                                 ifelse(race_ethnicity == 2, "Black",
                                        ifelse(race_ethnicity == 3, "Hispanic",
                                               ifelse(race_ethnicity == 4, "Asian",
                                                      ifelse(race_ethnicity == 5, "Other", NA)))))) %>%
  mutate(high.educ = ifelse(as.numeric(high.educ) >= 0 & as.numeric(high.educ) <= 12, "<HS_Diploma",
                            ifelse(high.educ %in% c("13", "14"), "HSDip/GED",
                                   ifelse(as.numeric(high.educ) >= 15 & as.numeric(high.educ) <= 17, "some_college",
                                          ifelse(high.educ == "18", "Bachelor",
                                                 ifelse(as.numeric(high.educ) >= 19 & as.numeric(high.educ) <= 21, "Post_Grad", NA)))))) %>%
  mutate(household.income = ifelse(as.numeric(household.income) >= 1 &as.numeric(household.income) <=6, "<50K",
                                   ifelse(household.income %in% c("7","8"), ">50K&<100K ", 
                                          ifelse(household.income %in% c("9","10"), ">100K", NA)))) %>%
  mutate(famhx_ss_momdad_alc_p = ifelse(famhx_ss_momdad_alc_p == 0, "Absent",
                                        ifelse(famhx_ss_momdad_alc_p == 1, "Present", NA)))

#remove all NAs from the dataframe 
baseline_df[baseline_df == 999] <- NA
baseline_df[baseline_df == 777] <- NA

#merge the two dataframes with the baseline measures 
baseline_df <- inner_join(baseline_df, ssrt, by = "src_subject_id")

#remove all of the youth who endorsed alcohol sipping 
baseline_df_nosip <- subset(baseline_df, tlfb_alc_sip == 0)


#FOLLOWUP MEASURES - AEs
aeq_df <-
  `non-imaging_excluding_nt_5.0` %>%
  select(src_subject_id, eventname,  aeq_positive_expectancies_ss, 
         aeq_negative_expectancies_ss, tlfb_alc_sip_l, tlfb_alc_l, interview_age,  aeq_section_q03, 
         aeq_section_q05, aeq_section_q07, aeq_section_q01, aeq_section_q02, aeq_section_q04, aeq_section_q06) %>%
  filter(eventname %in% c("1_year_follow_up_y_arm_1", "2_year_follow_up_y_arm_1", "3_year_follow_up_y_arm_1")) %>%
  mutate(nae_ss = aeq_section_q03 + aeq_section_q05 + aeq_section_q07) %>%
  mutate(pae_ss = aeq_section_q01 + aeq_section_q02 + aeq_section_q04 + aeq_section_q06) %>%
  rename(age_followup = interview_age) %>%
  mutate(age_followup = age_followup/12)

aeq_measures_nosip <- subset(aeq_df, tlfb_alc_sip_l == 0)

#some participants only have one response per AEs over the three years and in order to look at longitudinal changes, we need all two or more timepoints 
# group the data by participant ID and count the number of occurrences
counts <- aeq_measures_nosip %>%
  dplyr::group_by(src_subject_id) %>%
  dplyr::summarize(n = n())

# filter for participants that occur more than 2 or equal (they have atleast 2 time points)
repeating_participants <- counts %>%
  filter(n >=2) %>%
  select(src_subject_id)

# create a new dataframe with only the repeating participants 
repeating_data <- aeq_measures_nosip %>%
  filter(src_subject_id %in% repeating_participants$src_subject_id) 


#### merge the dataframes to create a large df ####
#merge baseline measures and repeating participants dataframe to make the final dataframe for analyses)
final_df <- inner_join(baseline_df_nosip, repeating_data, by = "src_subject_id")

final_df <- 
  final_df %>%
  mutate(time_point_yrs = age_followup - age_baseline) %>%
  mutate(time_point = ifelse(eventname == "1_year_follow_up_y_arm_1", "y1",
                             ifelse(eventname == "2_year_follow_up_y_arm_1", "y2",
                                    ifelse(eventname== "3_year_follow_up_y_arm_1", "y3", NA))))

length(unique(final_df$src_subject_id)) #7808 participants with unique data - added about 600 participants
final_df$cash_choice_task[final_df$cash_choice_task == 3] <- NA

final_df <-
  final_df %>%
  mutate(cash_choice_task = ifelse(final_df$cash_choice_task == "1", "1", #sooner reward as 1 - more impulsive 
                                   ifelse(final_df$cash_choice_task == "2", "0", NA))) #later reward as 0 - less impulsive 


# #save the data frame 
# saveRDS(final_df, file = "impdf_final.RDS")


#change variables to respective levels
final_df$high.educ <- to_factor(final_df$high.educ)
# final_df$household.income <- to_factor(final_df$household.income)
final_df$household_parent_status <- to_factor(final_df$household_parent_status)
final_df$sex_assigned <- to_factor(final_df$sex_assigned)
final_df$race_ethnicity <- to_factor(final_df$race_ethnicity)
final_df$household.income <- to_factor(final_df$household.income)

#relevel the dataframe 
final_df$race_ethnicity <- relevel(final_df$race_ethnicity, ref = "White")
final_df$high.educ <- relevel(final_df$high.educ, ref = "Post_Grad")
final_df$household_parent_status <- relevel(final_df$household_parent_status, ref = "two parent")
final_df$household.income_cat <- relevel(final_df$household.income, ref = ">100K")

###we noticed that two participants are duplicated in the data so we removed them###
#two participants are duplicated - drop them 
final_df <- final_df %>%
  group_by(src_subject_id, time_point) %>%
  slice(1) %>%  # Keeps only the first occurrence in each group
  ungroup()


#to address research question 1, we wanted to explore the differences between T2 and T3, therefore we created a dataset with only 2 timepoints
###check the data fir duplicates #####
duplicates <- final_df[duplicated(final_df[c("src_subject_id", "time_point")]) | duplicated(final_df[c("src_subject_id", "time_point")], fromLast = TRUE), ]



#this dataframe was used to compare T2 and T3 AEs#
final_df_two <- final_df[final_df$time_point %in% c("y2", "y3"), ]

##################################################### DATA ANALYSIS ####################################################################
###################
#Data Descriptives
###################

table1(~ upps_y_ss_negative_urgency + upps_y_ss_lack_of_planning + upps_y_ss_sensation_seeking + upps_y_ss_positive_urgency + upps_y_ss_lack_of_perseverance + 
         SSRT + factor(cash_choice_task) + age_followup + sex_assigned + race_ethnicity + household.income + high.educ + household_parent_status +  factor(famhx_ss_momdad_alc_p) + 
         cbcl_scr_syn_internal_t + cbcl_scr_syn_external_t + nae_ss + pae_ss | time_point, data=final_df, topclass =  "Rtable1-grid", render.missing = NULL)


##########################################################
#Cronbach's Alpha to Explore Internal Consistency
##########################################################

############ Impulsivity #########################
upps_data <- 
  final_df %>%
  select(src_subject_id, time_point, upps_y_ss_negative_urgency, upps7_y, upps11_y, upps17_y, upps20_y, upps_y_ss_positive_urgency, upps35_y, upps37_y, upps39_y, upps36_y, upps_y_ss_lack_of_perseverance, upps15_y, upps19_y, upps22_y, upps24_y, 
         upps_y_ss_lack_of_planning, upps6_y, upps16_y, upps23_y, upps28_y, upps_y_ss_sensation_seeking, upps12_y, upps18_y, upps21_y, upps27_y) 

upps_data <- upps_data %>% #ensures that there are no replicated individuals per upps score 
  group_by(src_subject_id) %>%
  slice(1)

alpha_negative_urgency <- alpha(upps_data[, 4:7])  
alpha_lack_of_planning <- alpha(upps_data[, 19:22])  
alpha_lack_of_perseverance <- alpha(upps_data[, 14:17])  
alpha_sensation_seeking <- alpha(upps_data[, 24:27])  
alpha_positive_urgency <- alpha(upps_data[, 9:12])  


############### Alcohol Expectancies ################
#Build individual dataframes for each year and then calculate the alpha for each year#
aeq_y1_data <- 
  final_df %>%
  select(src_subject_id,time_point, pae_ss, nae_ss, aeq_section_q01, aeq_section_q02, aeq_section_q04, aeq_section_q06, aeq_section_q03, aeq_section_q05, aeq_section_q07)%>%
  filter(time_point == "y1")

alpha_pae_y1 <- alpha(aeq_y1_data[, 5:8])
alpha_nae_y1 <- alpha(aeq_y1_data[, 9:11])

aeq_y2_data <- 
  final_df %>%
  select(src_subject_id,time_point, pae_ss, nae_ss, aeq_section_q01, aeq_section_q02, aeq_section_q04, aeq_section_q06, aeq_section_q03, aeq_section_q05, aeq_section_q07)%>%
  filter(time_point == "y2")

alpha_pae_y2 <- alpha(aeq_y2_data[, 5:8])
alpha_nae_y2 <- alpha(aeq_y2_data[, 9:11])

aeq_y3_data <- 
  final_df %>%
  select(src_subject_id,time_point, pae_ss, nae_ss, aeq_section_q01, aeq_section_q02, aeq_section_q04, aeq_section_q06, aeq_section_q03, aeq_section_q05, aeq_section_q07)%>%
  filter(time_point == "y3")

alpha_pae_y3 <- alpha(aeq_y3_data[, 5:8])
alpha_nae_y3 <- alpha(aeq_y3_data[, 9:11])


#################################################################################################
########## RESEARCH QUESTION 1: Assess the longitudinal change in PAE and NAE ##################
################################################################################################
#first we assessed the chnage across all three time points
summary(paeq_time_all <- lmer(pae_ss ~ 
                                time_point +
                                cbcl_scr_syn_internal_t + 
                                cbcl_scr_syn_external_t + 
                                factor(household.income) + 
                                factor(high.educ) + 
                                factor(race_ethnicity) +
                                sex_assigned +
                                factor(household_parent_status) + 
                                factor(famhx_ss_momdad_alc_p) +
                                (1|abcd_site/fam_id/src_subject_id), data = final_df, REML = TRUE, na.action="na.omit"))

summary(naeq_time_all <- lmer(nae_ss ~ 
                                time_point +
                                cbcl_scr_syn_internal_t + 
                                cbcl_scr_syn_external_t + 
                                factor(household.income) + 
                                factor(high.educ) + 
                                factor(race_ethnicity) + 
                                sex_assigned +
                                factor(household_parent_status) + 
                                factor(famhx_ss_momdad_alc_p) +
                                (1|abcd_site/fam_id/src_subject_id), data = final_df, REML = TRUE, na.action="na.omit"))

#next, since we saw that NAE did not change significantly when we looked at the means, we wanted to compare Y2 and Y3, since in our first plots we compared Y2 and Y3 to Y1
summary(paeq_time_two <- lmer(pae_ss ~ 
                                time_point +
                                cbcl_scr_syn_internal_t + 
                                cbcl_scr_syn_external_t + 
                                factor(household.income) + 
                                factor(high.educ) + 
                                factor(race_ethnicity) + 
                                sex_assigned +
                                factor(household_parent_status) + 
                                factor(famhx_ss_momdad_alc_p) +
                                (1|abcd_site/fam_id/src_subject_id), data = final_df_two, REML = TRUE, na.action="na.omit"))

summary(naeq_time_two <- lmer(nae_ss ~ 
                                time_point +
                                cbcl_scr_syn_internal_t + 
                                cbcl_scr_syn_external_t + 
                                factor(household.income) + 
                                factor(high.educ) + 
                                factor(race_ethnicity) + 
                                sex_assigned +
                                factor(household_parent_status) + 
                                factor(famhx_ss_momdad_alc_p) +
                                (1|abcd_site/fam_id/src_subject_id), data = final_df_two, REML = TRUE, na.action="na.omit"))

#Bootstrap the results#
paeq_lmer_time_two_final <- boot_summary(paeq_time_two,
                                         type = "perc",
                                         conf.level = 0.95,
                                         R = 1000)

naeq_lmer_time_two_final <- boot_summary(naeq_time_two,
                                         type = "perc",
                                         conf.level = 0.95,
                                         R = 1000)


paeq_lmer_time_all_final<- boot_summary(paeq_time_all,
                                        type = "perc",
                                        conf.level = 0.95,
                                        R = 1000)

naeq_lmer_time_all_final <- boot_summary(naeq_time_all,
                                         type = "perc",
                                         conf.level = 0.95,
                                         R = 1000)


#########################################################################################
############ RESEARCH QUESTION #2: Correlations between PAE and NAE over time  ##########
########################################################################################

#change the format of the data to look at the correlations  - make sure that there are no duplicates in the data 

wide_data <- pivot_wider(
  data = final_df,
  id_cols = src_subject_id,
  names_from = time_point,
  values_from = c(pae_ss, nae_ss),
  names_sep = "_")

important.variables <- wide_data[c("pae_ss_y1","pae_ss_y2", 
                                   "pae_ss_y3", "nae_ss_y1", 
                                   "nae_ss_y2", "nae_ss_y3")] 

#### to create a correlation matrix 
result <- rcorr(as.matrix(important.variables), type = c("spearman"))
correlation_matrix <- result$r
correlation_matrix_p <- result$P


#FDR Correction for Correlations 
adjusted_p_values <- p.adjust(correlation_matrix_p, method = "fdr")
df <- as.data.frame(adjusted_p_values)

###################################################################################
######### RESEARCH QUESTION 3: Associations between Impulsivity and AE ############
###################################################################################

summary(paeq_lmer_cct <- lmer(pae_ss ~ 
                                SSRT * time_point + 
                                factor(cash_choice_task) * time_point + 
                                upps_y_ss_negative_urgency * time_point + 
                                upps_y_ss_positive_urgency * time_point +
                                upps_y_ss_lack_of_planning * time_point + 
                                upps_y_ss_lack_of_perseverance * time_point + 
                                upps_y_ss_sensation_seeking * time_point +
                                cbcl_scr_syn_internal_t + 
                                cbcl_scr_syn_external_t + 
                                factor(household.income) +
                                factor(high.educ) +
                                factor(race_ethnicity) +
                                factor(sex_assigned)+
                                factor(household_parent_status) +
                                factor(famhx_ss_momdad_alc_p) +
                                (1|abcd_site/fam_id/src_subject_id), data = final_df, REML = TRUE, na.action="na.omit"))

confint(paeq_lmer_cct)

summary(naeq_lmer_cct <- lmer(nae_ss ~ 
                                SSRT * time_point + 
                                factor(cash_choice_task) * time_point + 
                                upps_y_ss_negative_urgency * time_point + 
                                upps_y_ss_positive_urgency * time_point +
                                upps_y_ss_lack_of_planning * time_point + 
                                upps_y_ss_lack_of_perseverance * time_point + 
                                upps_y_ss_sensation_seeking * time_point +
                                cbcl_scr_syn_internal_t + 
                                cbcl_scr_syn_external_t + 
                                factor(household.income) +  
                                factor(high.educ) + 
                                factor(race_ethnicity) + 
                                factor(sex_assigned)+
                                factor(household_parent_status) + 
                                factor(famhx_ss_momdad_alc_p) +
                                (1|abcd_site/fam_id/src_subject_id), data = final_df, REML = TRUE, na.action="na.omit"))

confint(naeq_lmer_cct)

#Bootstrap models
paeq_lmer_adjusted_cct_boot_final <- boot_summary(paeq_lmer_cct,
                                                  type = "perc",
                                                  conf.level = 0.95,
                                                  R = 1000)

naeq_lmer_adjusted_cct_boot_final <- boot_summary(naeq_lmer_cct,
                                                  type = "perc",
                                                  conf.level = 0.95,
                                                  R = 1000)

####################################################################################
###################### VISUALIZATIONS OF RESULTS ##################################
###################################################################################
#coefficient plot of all of the relevant variables
#create a coefficient map with variables you would like to plot 
cm <- c(
  "time_pointy3:factor(cash_choice_task)1" = "Impulsive Choice : Age 13",
  "time_pointy2:factor(cash_choice_task)1" = "Impulsive Choice : Age 12",
  "SSRT:time_pointy3" = "Impulsuive Action : Age 13",
  "SSRT:time_pointy2" = "Impulsive Action : Age 12", 
  "time_pointy3:upps_ss_positive_urgency" = "Positive Urgency : Age 13",
  "time_pointy2:upps_y_ss_positive_urgency" = "Positive Urgency : Age 12",
  "time_pointy3:upps_y_ss_sensation_seeking" = "Sensation Seeking : Age 13",
  "time_pointy2:upps_y_ss_sensation_seeking" = "Sensation Seeking : Age 12",
  "time_pointy3:upps_y_ss_lack_of_perseverance" = "Lack of Perseverance : Age 13",
  "time_pointy2:upps_y_ss_lack_of_perseverance" = "Lack of Perseverance : Age 12",
  "time_pointy3:upps_y_ss_lack_of_planning" = "Lack of Premeditation : Age 13",
  "time_pointy2:upps_y_ss_lack_of_planning" = "Lack of Premeditation : Age 12",
  "time_pointy3:upps_y_ss_negative_urgency" = "Negative Urgency : Age 13",
  "time_pointy2:upps_y_ss_negative_urgency" = "Negative Urgency : Age 12",
  'factor(cash_choice_task)1' = 'Impulsive Choice',
  'SSRT' = 'Impulsive Action',
  "upps_y_ss_positive_urgency" = "Positive Urgency",
  "upps_y_ss_sensation_seeking" = "Sensation Seeking",
  "upps_y_ss_lack_of_perseverance" = "Lack of Perseverance",
  "upps_y_ss_lack_of_planning" = "Lack of Premeditation",
  "upps_y_ss_negative_urgency" = "Negative Urgency")


#PAE
modelplot(paeq_lmer_cct,coef_map = cm,
          size = 0.5,
          fatten = 5) +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not Significant"),
      linetype = ifelse(p.value < 0.05, "Significant", "Not Significant")) +
  scale_linetype_manual(values = c("dashed", "solid"),
                        guide = guide_legend(override.aes = list(color = "black"))) +
  scale_color_manual(values = c("Significant" = "black", "Not Significant" = "black"),
                     guide = guide_legend(override.aes = list(linetype = c("dashed", "solid")))) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(face = "bold")) +
  geom_vline(xintercept = 0) +
  labs(x = "Coefficients") 


#NAE
modelplot(naeq_lmer_cct,
          coef_map = cm,
          size = 0.5,
          fatten = 5) +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not Significant"),
      linetype = ifelse(p.value < 0.05, "Significant", "Not Significant")) +
  scale_linetype_manual(values = c("dashed", "solid"),
                        guide = guide_legend(override.aes = list(color = "black"))) +
  scale_color_manual(values = c("Significant" = "black", "Not Significant" = "black"),
                     guide = guide_legend(override.aes = list(linetype = c("dashed", "solid")))) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(face = "bold")) +
  geom_vline(xintercept = 0) +
  labs(x = "Coefficients") 


#plot the interaction - Year 1 was removed from the plot to clearly visualize the significance changes between Year 2 and Year 3
#for clear visualization, Year 1 was removed from the final image. 
plot_model(paeq_lmer_cct, type = "pred", 
           terms = c("upps_y_ss_lack_of_perseverance", "time_point"),
           title = "Interaction between Lack of Perseverance and Time",
           axis.title = c("Lack of Perseverance", "Model Predicting Positive Alcohol Expectancies"),
           xlab = "Lack of Perseverance",
           legend.title = "Time",
           axis.labels = c("Lack of Perseverance") +
             theme_minimal() +  # Use a minimal theme
             theme(
               panel.grid.major = element_blank(),  # Remove major grid lines
               panel.grid.minor = element_blank(),  # Remove minor grid lines
               panel.background = element_rect(fill = "white", colour = NA),  # White background for the panel
               plot.background = element_rect(fill = "white", colour = "black", size = 1)))  # White background for the plot with a black border


####### SUPPLEMENTARY FIGURES #########
#################################################################
##### Distribution Plots of PAEs and NAEs at Each Timepoint######
#################################################################

long_df <- final_df %>%
  pivot_longer(
    cols = c(pae_ss, nae_ss),
    names_to = "Alcohol_Expectancy_Type",
    values_to = "Alcohol_Expectancies"
  )

ggplot(long_df, aes(x = Alcohol_Expectancies, fill = Alcohol_Expectancy_Type)) +
  geom_histogram(position = "identity", alpha = 0.5, binwidth = 1) +  # Adjust binwidth as needed
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Removes major gridlines
    panel.grid.minor = element_blank(),  # Removes minor gridlines
    panel.border = element_rect(colour = "black", fill = NA, size = 1)  # Adds a border
  ) +
  labs(title = "PAEs and NAEs Over Three Years", x = "AEs Sum Score", y = "Frequency") +
  scale_fill_manual(values = c("pae_ss" = "blue", "nae_ss" = "red"))

####Age 11####
long_df_y1 <- aeq_y1_data %>%
  pivot_longer(
    cols = c(pae_ss, nae_ss),
    names_to = "Alcohol_Expectancy_Type",
    values_to = "Alcohol_Expectancies"
  )

ggplot(long_df_y1, aes(x = Alcohol_Expectancies, fill = Alcohol_Expectancy_Type)) +
  geom_histogram(position = "identity", alpha = 0.5, binwidth = 1) +  # Adjust binwidth as needed
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Removes major gridlines
    panel.grid.minor = element_blank(),  # Removes minor gridlines
    panel.border = element_rect(colour = "black", fill = NA, size = 1)  # Adds a border
  ) +
  labs(title = "PAEs and NAEs at Age 11", x = "AEs Sum Score", y = "Frequency") +
  scale_fill_manual(values = c("pae_ss" = "blue", "nae_ss" = "red"))

####Age 12####
long_df_y2 <- aeq_y2_data %>%
  pivot_longer(
    cols = c(pae_ss, nae_ss),
    names_to = "Alcohol_Expectancy_Type",
    values_to = "Alcohol_Expectancies"
  )

ggplot(long_df_y2, aes(x = Alcohol_Expectancies, fill = Alcohol_Expectancy_Type)) +
  geom_histogram(position = "identity", alpha = 0.5, binwidth = 1) +  # Adjust binwidth as needed
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Removes major gridlines
    panel.grid.minor = element_blank(),  # Removes minor gridlines
    panel.border = element_rect(colour = "black", fill = NA, size = 1)  # Adds a border
  ) +
  labs(title = "PAEs and NAEs at Age 12", x = "AEs Sum Score", y = "Frequency") +
  scale_fill_manual(values = c("pae_ss" = "blue", "nae_ss" = "red"))


####Age 13####
long_df_y3 <- aeq_y3_data %>%
  pivot_longer(
    cols = c(pae_ss, nae_ss),
    names_to = "Alcohol_Expectancy_Type",
    values_to = "Alcohol_Expectancies"
  )

ggplot(long_df_y3, aes(x = Alcohol_Expectancies, fill = Alcohol_Expectancy_Type)) +
  geom_histogram(position = "identity", alpha = 0.5, binwidth = 1) +  # Adjust binwidth as needed
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Removes major gridlines
    panel.grid.minor = element_blank(),  # Removes minor gridlines
    panel.border = element_rect(colour = "black", fill = NA, size = 1)  # Adds a border
  ) +
  labs(title = "PAEs and NAEs at Age 13", x = "AEs Sum Score", y = "Frequency") +
  scale_fill_manual(values = c("pae_ss" = "blue", "nae_ss" = "red"))


########################### THE END ##########################################
##############################################################################
