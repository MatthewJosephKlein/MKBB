rm(list=ls())
library("sampleSelection")
library("lfe")
library("glmmML")
library("tidyverse")

# Chapter 1: Functions ####
setwd("C:/Users/mjklein2/Desktop/toot/Programming_Directory")
load("hh.df.Rda") # This data.frame is generated in "Final Cleaning for Master Panel Construction.R"

temp.df <- aggregate(hh.df$progresa_income_total[hh.df$wavenumber==2], 
                     by =list(hh.df$folio[hh.df$wavenumber==2]), FUN=mean, na.rm=T)
colnames(temp.df) <- c("folio", "progresa_income_total_in_period_2")
temp.df$treatment_household <- rep(0)
temp.df$treatment_household[temp.df$progresa_income_total_in_period_2>0] <- 1
summary(temp.df)

hh.df <- merge(hh.df, temp.df, by = c("folio"))

# A) BP Function (Hyp 1)
#    A.1) Shadow Earnings function
# B) LPM.Marginal.Fun (Hyp 2, 3)
# C) LPM.Marginal.Fun.Comparison (Hyp 4)
# D) Poisson.Marginal.Fun (Hyp 2)
# E) Generate the analog

# (A.1) Shadow Earnings (SE) Function 
SE.Fun <- function(gender_number){ #gender_number == 1 corresponds to women.
  data.df <- subset(sample.analog,  sample.analog$age > 15 & sample.analog$sex == gender_number &  sample.analog$age <= 70)
  
  selection_formula <- LFP ~ age + I(age^2) +  otherincomeval + hh_kids +  hh_young_kids + edu_yrs + literate + gov_transfer +
    indigenous_language + spanish_language + head_dummy + num_f_adults + num_m_adults + pobextre +  mpcalif  +
    number_female_kids + number_male_kids  + prop_mex_migrant + prop_usa_migrant + I(num_m_adults*prop_mex_migrant) +
    I(num_m_adults*prop_usa_migrant) + I(num_f_adults*prop_mex_migrant) + I(num_f_adults*prop_usa_migrant) +  
    as.factor(year_wave_FE) + treatment_dummy  +  # FE and Exclusion Restrictions
    ER + ER*number_female_kids +  ER*number_male_kids + ER*num_f_adults + ER*num_m_adults +
    proportion_need_permission + proportion_need_accompany  
  
  outcome_formula <-  log_wages ~ age + I(age^2) +  otherincomeval + hh_kids +  hh_young_kids + edu_yrs + literate + gov_transfer +
    indigenous_language + spanish_language + head_dummy + num_f_adults + num_m_adults + pobextre +  mpcalif  +
    number_female_kids + number_male_kids  + prop_mex_migrant + prop_usa_migrant + I(num_m_adults*prop_mex_migrant) +
    I(num_m_adults*prop_usa_migrant) + I(num_f_adults*prop_mex_migrant) + I(num_f_adults*prop_usa_migrant) +  as.factor(year_wave_FE) + treatment_dummy    
  
  if(gender_number == 1){  
    
    selection_formula <- LFP ~ age + I(age^2) +  otherincomeval + hh_kids +  hh_young_kids + edu_yrs + literate + gov_transfer +
      indigenous_language + spanish_language + head_dummy + num_f_adults + num_m_adults + pobextre +  mpcalif  +
      number_female_kids + number_male_kids  + prop_mex_migrant + prop_usa_migrant + I(num_m_adults*prop_mex_migrant) +
      I(num_m_adults*prop_usa_migrant) + I(num_f_adults*prop_mex_migrant) + I(num_f_adults*prop_usa_migrant) +  
      as.factor(year_wave_FE) + treatment_dummy  +  # FE and Exclusion Restrictions
      ER + ER*number_female_kids +  ER*number_male_kids + ER*num_f_adults + ER*num_m_adults +
      proportion_need_permission + proportion_need_accompany + progresa_income_mom
    
    outcome_formula <-  log_wages ~ age + I(age^2) +  otherincomeval + hh_kids +  hh_young_kids + edu_yrs + literate + gov_transfer +
      indigenous_language + spanish_language + head_dummy + num_f_adults + num_m_adults + pobextre +  mpcalif  +
      number_female_kids + number_male_kids  + prop_mex_migrant + prop_usa_migrant + I(num_m_adults*prop_mex_migrant) +
      I(num_m_adults*prop_usa_migrant) + I(num_f_adults*prop_mex_migrant) +
      I(num_f_adults*prop_usa_migrant) +  as.factor(year_wave_FE) + treatment_dummy  +  progresa_income_mom    # Only difference between men and women is the addition of Progresa income for female HH heads that got the transfer
    
  }
  
  reg <- selection(selection_formula, outcome_formula, data = data.df, method = "ml")
  summary(reg)
  # stargazer::stargazer(reg$probit, reg1$probit, omit = c("year_wave_FE"), single.row = T)
  # Have to generate the fitted values by hand since the canned packages aren't calculating the values for NA's.
  
  coefs <- matrix(as.numeric(reg$estimate[55:length(reg$estimate)-2]), ncol = 1)
  
  if(gender_number==1){
    coefs <- matrix(as.numeric(reg$estimate[56:length(reg$estimate)-2]), ncol = 1)
  }
  
  X <- model.matrix(outcome_formula, model.frame(outcome_formula, data.df, na.action = na.pass)) # Thanks, Travis, for the idea
  
  y_hat <- exp(X%*%coefs)
  y_hat.df <- as.data.frame(cbind(data.df$folio, data.df$ind_ID, data.df$wavenumber, y_hat))
  return(y_hat.df)
}

# hh.df <- subset(hh.df, hh.df$folio != 170466)

# (A.2) BP Function 
BP.Fun <- function(){ #Calls shadow wage function
  
  #Step 1: Call the SW function
  y_hat_men_combined <- unique(SE.Fun(gender_number = 0)) # "_Combined" references the fact that all years are used in estimation
  y_hat_women_combined <- unique(SE.Fun(gender_number = 1))
  names(y_hat_men_combined) <- c("folio", "ind_ID", "wavenumber", "y_hat_men_combined") 
  names(y_hat_women_combined) <- c("folio", "ind_ID", "wavenumber", "y_hat_women_combined")
  
  #step 2: Merging it into the sample analog. DO NOT DELETE THE LINES THAT CONVERT NA's TO 0's.
  sample.analog <- merge(sample.analog, y_hat_women_combined, by = c("folio", "ind_ID", "wavenumber"), all.x = TRUE)
  sample.analog$y_hat_women_combined[is.na(sample.analog$y_hat_women_combined)] <- 0
  sample.analog <- merge(sample.analog, y_hat_men_combined, by = c("folio", "ind_ID", "wavenumber"), all.x = TRUE)
  sample.analog$y_hat_men_combined[is.na(sample.analog$y_hat_men_combined)] <- 0
  
  #Step 3: In steps 1 and 2, every person in the HH has an estimated outside option / shadow earnings. We need to just have the mom and dad. 
  sample.analog$Mom_SW_combined_a <- sample.analog$y_hat_women_combined * sample.analog$head_dummy
  Mom_SW_combined.df <- aggregate(sample.analog$Mom_SW_combined_a, by = list(Category=sample.analog$folio), FUN=sum)
  names(Mom_SW_combined.df) <- c("folio", "Mom_SW_combined")
  sample.analog <- merge(sample.analog, Mom_SW_combined.df, by = "folio")
  
  sample.analog$Dad_SW_combined_a <- sample.analog$y_hat_men_combined * sample.analog$head_dummy
  Dad_SW_combined.df <- aggregate(sample.analog$Dad_SW_combined_a, by = list(Category=sample.analog$folio), FUN=sum)
  names(Dad_SW_combined.df) <- c("folio", "Dad_SW_combined")
  sample.analog <- merge(sample.analog, Dad_SW_combined.df, by = "folio")
  
  #Step 4: Generating the relative shadow earnings BP Proxy
  
  #BP = (\hat{E}_f + T_f) / (\hat{E}_f + T_f + \hat{E}_m + T_m)
  
  sample.analog$BP[sample.analog$wave1 == 1] <- (sample.analog$Mom_SW_combined[sample.analog$wavenumber == 1] + 
                                                   sample.analog$T_mom_total[sample.analog$wavenumber == 1] )  / 
    (sample.analog$Mom_SW_combined[sample.analog$wavenumber == 1] + 
       sample.analog$T_mom_total[sample.analog$wavenumber == 1] +
       sample.analog$Dad_SW_combined[sample.analog$wavenumber == 1] + 
       sample.analog$T_dad_total[sample.analog$wavenumber == 1])
  
  sample.analog$BP[sample.analog$wavenumber == 2] <- 
    (sample.analog$Mom_SW_combined[sample.analog$wavenumber == 2] + 
       sample.analog$T_mom_total[sample.analog$wavenumber == 2])  /
    (sample.analog$Mom_SW_combined[sample.analog$wavenumber == 2] +
       sample.analog$T_mom_total[sample.analog$wavenumber == 2] + 
       sample.analog$Dad_SW_combined[sample.analog$wavenumber == 2] + 
       sample.analog$T_dad_total[sample.analog$wavenumber == 2])
  
  sample.analog$BP[sample.analog$wavenumber == 3] <- 
    (sample.analog$Mom_SW_combined[sample.analog$wavenumber == 3] + 
       sample.analog$T_mom_total[sample.analog$wavenumber == 3])  /
    (sample.analog$Mom_SW_combined[sample.analog$wavenumber == 3] +
       sample.analog$T_mom_total[sample.analog$wavenumber == 3] + 
       sample.analog$Dad_SW_combined[sample.analog$wavenumber == 3] + 
       sample.analog$T_dad_total[sample.analog$wavenumber == 3])
  
  
  t1 <- t.test(sample.analog$BP[sample.analog$wave2 == 1 & sample.analog$treatment_dummy == "Basal"], 
               sample.analog$BP[sample.analog$wave1 == 1 & sample.analog$treatment_dummy == "Basal"])
  t2 <-  t.test(sample.analog$BP[sample.analog$wave2 == 1 & sample.analog$treatment_dummy == "Control"], 
                sample.analog$BP[sample.analog$wave1 == 1 & sample.analog$treatment_dummy == "Control"])
  
  return(list(sample.analog, t1$statistic, t2$statistic, 
              mean(sample.analog$BP[sample.analog$wave1 == 1], na.rm = T), 
              mean(sample.analog$BP[sample.analog$wave2 == 1], na.rm = T),
              mean(sample.analog$BP[sample.analog$wave3 == 1], na.rm = T)))  
} 


# Chapter 1.b: Generating the BP values and Final.df #####

sample.analog <- hh.df 

BP.Fun.Results <- BP.Fun()
sample.analog <- BP.Fun.Results[[1]]


# making a matrix of just the HH level variables: 
final.df <- aggregate(sample.analog$BP, by = list(sample.analog$folio, sample.analog$wavenumber), FUN=mean, na.rm=T)
colnames(final.df) <- c("folio", "wavenumber", "BP")

final.df <- unique(merge(sample.analog[,c("folio", "wavenumber", "loc_id", "hh_log_wages" , "hh_kids" , "hh_young_kids" , 
                                          "progresa_income_total","wave2", "wave3", "treatment_dummy_num", "seven_states", "mpio", 
                                          "accompanied", "need_permission", "AD_women_in_home",
                                          "AD_obedience", "AD_say_comm", "AD_women_job", "AD_equal_rights", "AD_women_opinions",
                                          "treatment_dummy", "treatment_household")], final.df, by =   c("folio", "wavenumber")))

# edit the variables to be dummies equal to one for agree, 0 for disagree

final.df$AD_equal_rights[final.df$AD_equal_rights != 1] <- 0
final.df$AD_women_in_home[final.df$AD_women_in_home != 1] <- 0
final.df$AD_obedience[final.df$AD_obedience != 1] <- 0
final.df$AD_say_comm[final.df$AD_say_comm != 1] <- 0
final.df$AD_women_opinions[final.df$AD_equal_rights != 1] <- 0
final.df$AD_women_job[final.df$AD_women_job != 1] <- 0


summary(felm(BP ~ accompanied + need_permission + AD_women_in_home + AD_obedience + treatment_household +
             AD_say_comm + AD_women_job +  AD_equal_rights + AD_women_opinions | wavenumber | 0 | loc_id, 
           data = final.df))



