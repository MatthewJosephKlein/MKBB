# Mjklein 
# Madison, Wisc.
# 1/11/2018

# This code takes the cleaned dataset as an input, and outputs Figure 1, Tables 1 - 7 of MK/BB
# As with "Bootstrap Function Code.R", 

# This code is split into 6 chapters:
# Chapter 1, Generating the HH Level Dataset. 
# Chapter 2: HH Summary Stats Table
# Chapter 3: Diet Summary Stats Table
# Chapter 4: Predicted Earnings Tables (Tables 4 and 5 in MK.BB)
# Chapter 5: Estimating BP for each HH and generating Figure 1
# Chapter 6: Table 7

# 7/27/2018 Adding in the tables for SE breakdowns. 1 for market earnings and 1 for transfers
# Updating the tables and figures to reflect the correction in the heckman selection code in 2019

# Bootstrap code for all three stages
# This code has three chapters: 
# 1: write the functions to be called in the bootstrap 
# 2: Block Bootstrap: 
#           2.1 Construct the bootstrap dataset 
#           2.2 Estimate the SW regressions and save average BP 
#           2.3 Run the LPM and Poisson models 
#           2.4 Generate the MEs and save to an external vector 
# 3: Conduct Inference on the Efron Interval. Save the Efron interval as a data.frame

# Note: The LHS variable in the earnings regression is labeled "wages." This is a rhetorical error - the variable is actually coded to be earnings.
# memory.limit(size = 20000)
setwd("C:/Users/mjklein2/Desktop/toot/Programming_Directory")
rm(list=ls())
library("sampleSelection")
library("lfe")
library("glmmML")
library("tidyverse")
library("ihs")

load("hh.df.Rda") # From "Final Cleaning for Master Panel Construction.R"

# Treatment households are those who received progresa in wave 2. (1999)
temp.df <- aggregate(hh.df$progresa_income_total[hh.df$wavenumber==2], 
                     by =list(hh.df$folio[hh.df$wavenumber==2]), FUN=mean, na.rm=T)
colnames(temp.df) <- c("folio", "progresa_income_total_in_period_2")
temp.df$treatment_household <- rep(0)
temp.df$treatment_household[temp.df$progresa_income_total_in_period_2>0] <- 1
summary(temp.df)

hh.df <- merge(hh.df, temp.df, by = "folio")
table(hh.df$treatment_household, hh.df$wavenumber) #  Need to have a var that delineates the T hh's in waves 1 and 2 for the DiD calc
hh.df$treatment_household_0 <- ifelse(hh.df$wavenumber > 1, hh.df$treatment_household, 0)

# Then, all eligible HH's we're folded into the program in wave 3 (2000) 
hh.df$receive_progresa <- ifelse(hh.df$progresa_income_total > 0, 1, 0)

# Drop the HH's where one partner is eligible to have their POO estimated, but the other
hh.df$drop_dummy <- 0
hh.df$drop_dummy[hh.df$head_dummy == 1 & hh.df$age < 15 ] <- 1
hh.df$drop_dummy[hh.df$head_dummy == 1 & hh.df$age > 65 ] <- 1

temp.df <- aggregate(hh.df$drop_dummy, by = list(hh.df$folio), FUN = sum, na.rm=T)
colnames(temp.df) <- c("folio", "drop_dummy_hh")

hh.df <- left_join(temp.df, hh.df)

# Also drop HH's where the partner is absent for one of the waves (cause's 1's and 0's in the BP measure)
temp.df <- aggregate(hh.df$head_dummy, by = list(hh.df$folio), FUN = sum, na.rm = T)
#summary(temp.df)
colnames(temp.df) <- c("folio", "head_dummy_aggregate")
temp.df$drop_dummy_hh_2 <- ifelse(temp.df$head_dummy_aggregate == 5, 1, 0) 

hh.df <- left_join(hh.df, select(temp.df, folio, drop_dummy_hh_2))

hh.df <- hh.df %>% filter(drop_dummy_hh == 0)
hh.df <- hh.df %>% filter(drop_dummy_hh_2 == 0)

# hh.df <- hh.df %>% filter(age < 90)

hh.df$prop_usa_migrant_dummy <- ifelse(hh.df$prop_usa_migrant > 0 & !is.na(hh.df$prop_usa_migrant), 1, 0)
hh.df$prop_mex_migrant_dummy <- ifelse(hh.df$prop_mex_migrant > 0 & !is.na(hh.df$prop_mex_migrant), 1, 0)
hh.df$otherincomeval_dummy <- ifelse(hh.df$otherincomeval > 0, 1, 0)

# A) BP Function (Hyp 1)
#    A.1) Shadow Earnings function
# B) LPM.Marginal.Fun (Hyp 2, 3)
# C) LPM.Marginal.Fun.Comparison (Hyp 4)
# D) Poisson.Marginal.Fun (Hyp 2)
# E) Accuracy Test Function 
# F) Generate the analog

# (A.1) Shadow Earnings (SE) Function 
SE.Fun <- function(gender_number){ #gender_number == 1 corresponds to women.
  
  data.df <- subset(sample.analog,  
                    sample.analog$sex == gender_number &
                      sample.analog$age > 15 &
                      sample.analog$age <= 65 &
                      sample.analog$hh_kids <= 5 & 
                      sample.analog$hh_young_kids <= 4 & 
                      sample.analog$num_m_adults <= 5 &
                      sample.analog$num_f_adults <= 5 & 
                      sample.analog$hh_young_kids <= quantile(sample.analog$hh_young_kids, 0.95))
  
  
  data.df$drop <- ifelse(!is.na(data.df$log_wages) & 
                           data.df$log_wages >=  quantile(data.df$log_wages,
                                                          c(0.99),
                                                          na.rm=T),
                         1, 0)
  
  
  data.df$drop <- ifelse(!is.na(data.df$log_wages) & 
                           data.df$log_wages <=  quantile(data.df$log_wages,
                                                          c(0.01),
                                                          na.rm=T),
                         1, data.df$drop)
  
  
  
  data.df <- data.df %>% filter(drop == 0)
  
  
  
  if(gender_number == 0){   
    
    reg <- selection(selection = LFP ~ age + I(age^2)  + otherincomeval_dummy + asinh(otherincomeval) + hh_kids + 
                       hh_young_kids + edu_yrs + literate + gov_transfer +
                       indigenous_language + spanish_language + head_dummy + num_f_adults + num_m_adults + #pobextre +  mpcalif  +
                       number_female_kids + number_male_kids   +  prop_mex_migrant_dummy + prop_usa_migrant_dummy  +
                       I(num_m_adults*prop_mex_migrant) +  prop_usa_migrant + prop_mex_migrant + 
                       I(num_m_adults*prop_usa_migrant) + I(num_f_adults*prop_mex_migrant) +
                       I(num_f_adults*prop_usa_migrant) +    
                       as.factor(year_wave_FE) + receive_progresa  +  # FE and Exclusion Restrictions
                       (ER + proportion_need_permission + proportion_need_accompany)*hh_young_kids,  
                     outcome = log_wages ~ age + I(age^2) +  otherincomeval_dummy + asinh(otherincomeval) + hh_kids +
                       hh_young_kids + edu_yrs  + literate + gov_transfer +
                       indigenous_language + spanish_language + head_dummy + num_f_adults + num_m_adults +# pobextre +  mpcalif  +
                       number_female_kids + number_male_kids  +  prop_mex_migrant_dummy + prop_usa_migrant_dummy  +
                       I(num_m_adults*prop_mex_migrant) +  prop_usa_migrant + prop_mex_migrant + 
                       I(num_m_adults*prop_usa_migrant) + I(num_f_adults*prop_mex_migrant) +
                       I(num_f_adults*prop_usa_migrant) +    
                       as.factor(year_wave_FE) + receive_progresa,
                     data = data.df,
                     method = "ml")
    
  }
  
  if(gender_number == 1){  
    # Only difference between men and women is the addition of Progresa income for female HH heads that got the transfer
    
    reg <- selection(selection = LFP ~ age + I(age^2) +  otherincomeval_dummy +  asinh(otherincomeval) + hh_kids +
                       hh_young_kids + edu_yrs  + literate + gov_transfer +
                       indigenous_language + spanish_language + head_dummy + num_f_adults + num_m_adults + #pobextre +  mpcalif  +
                       number_female_kids + number_male_kids  + 
                       prop_mex_migrant_dummy + prop_usa_migrant_dummy  +
                       I(num_m_adults*prop_mex_migrant) +   prop_usa_migrant + prop_mex_migrant +
                       I(num_m_adults*prop_usa_migrant) + I(num_f_adults*prop_mex_migrant) +
                       I(num_f_adults*prop_usa_migrant) +
                       as.factor(year_wave_FE) + receive_progresa  +  # FE and Exclusion Restrictions
                       (ER + proportion_need_permission + proportion_need_accompany)*hh_young_kids  + # ER*num_f_adults + ER*num_m_adults +
                       asinh(progresa_income_mom), 
                     # 
                     outcome = log_wages ~ age + I(age^2) +  otherincomeval_dummy +  asinh(otherincomeval) + hh_kids +
                       hh_young_kids + edu_yrs  + literate + gov_transfer +
                       indigenous_language + spanish_language + head_dummy + num_f_adults + num_m_adults + #pobextre +  mpcalif  +
                       number_female_kids + number_male_kids  +
                       prop_mex_migrant_dummy + prop_usa_migrant_dummy  +
                       I(num_m_adults*prop_mex_migrant) +  prop_usa_migrant + prop_mex_migrant +
                       I(num_m_adults*prop_usa_migrant) + I(num_f_adults*prop_mex_migrant) +
                       I(num_f_adults*prop_usa_migrant) +
                       as.factor(year_wave_FE) + receive_progresa  +  
                       asinh(progresa_income_mom),
                     data = data.df,
                     method = "ml")  
    
    summary(exp(predict(reg, newdata = data.df, type = "conditional")))
    
    
  }
  
  # Add the predicted values to data.df, conditional on LFP
  data.df <- cbind(data.df,
                   exp(predict(reg, newdata = data.df, type = "conditional"))
  )
  
  
  # Predict.selection returns two values per observation, E[y|LFP=1] and E[y|LFP=0]. These have slightly different lambda expressions since the conditions are different.
  # E[y|LFP] = Xbeta + sigma rho lambda(alpha_u) 
  # where lambda(alpha_u) = phi(alpha_u) / Phi(alpha_u) for LFP = 1
  # and lambda(alpha_u) = - phi(alpha_u) / (1- Phi(alpha_u)) for LFP = 0. 
  # See Greene Chapter 24.5, Theorems 24.2 and 24.5, and the derivation in section 24.5.4. 
  
  # Select the correct prediction based on LFP: 
  data.df$y_hat <- ifelse(data.df$LFP == 1, data.df$"E[yo|ys=1]", data.df$"E[yo|ys=0]")
  
  # y_hat <- exp(predict(reg, newdata = data.df)) # newdata means that it takes a dataframe 
  y_hat.df <- select(data.df, folio, ind_ID, wavenumber, y_hat)
  return(y_hat.df)
}


# (A.2) BP Function 
BP.Fun <- function(){ #Calls shadow wage function
  
  #Step 1: Call the SW function
  y_hat_men_combined <- unique(SE.Fun(gender_number = 0)) # "_Combined" references the fact that all years are used in estimation
  y_hat_women_combined <- unique(SE.Fun(gender_number = 1))
  names(y_hat_men_combined) <- c("folio", "ind_ID", "wavenumber", "y_hat_men_combined") 
  names(y_hat_women_combined) <- c("folio", "ind_ID", "wavenumber", "y_hat_women_combined")
  
  #step 2: Merging it into the sample analog. DO NOT DELETE THE LINES THAT CONVERT NA's TO 0's.
  sample.analog <- left_join(sample.analog, y_hat_women_combined, by = c("folio", "ind_ID", "wavenumber"), all.x = TRUE)
  sample.analog$y_hat_women_combined[is.na(sample.analog$y_hat_women_combined)] <- 0
  sample.analog <- left_join(sample.analog, y_hat_men_combined, by = c("folio", "ind_ID", "wavenumber"), all.x = TRUE)
  sample.analog$y_hat_men_combined[is.na(sample.analog$y_hat_men_combined)] <- 0
  
  #Step 3: In steps 1 and 2, every person in the HH has an estimated outside option / shadow earnings. We need to just have the mom and dad. 
  sample.analog$Mom_SW_combined_a <- sample.analog$y_hat_women_combined * sample.analog$head_dummy
  Mom_SW_combined.df <- aggregate(sample.analog$Mom_SW_combined_a, by = list(sample.analog$folio, sample.analog$wavenumber), FUN=sum)
  names(Mom_SW_combined.df) <- c("folio", "wavenumber", "Mom_SW_combined")
  sample.analog <- left_join(sample.analog, Mom_SW_combined.df)
  
  sample.analog$Dad_SW_combined_a <- sample.analog$y_hat_men_combined * sample.analog$head_dummy
  Dad_SW_combined.df <- aggregate(sample.analog$Dad_SW_combined_a, by = list(Category=sample.analog$folio, sample.analog$wavenumber), FUN=sum)
  names(Dad_SW_combined.df) <- c("folio", "wavenumber", "Dad_SW_combined")
  sample.analog <- left_join(sample.analog, Dad_SW_combined.df)
  
  #Step 4: Generating the relative shadow earnings BP Proxy
  
  #BP = (\hat{E}_f + T_f) / (\hat{E}_f + T_f + \hat{E}_m + T_m)
  
  # There must be a tidy way to complete the below process  
  # sample.analog <- sample.analog %>% group_by(wavenumber) %>%
  #    mutate()
  
  sample.analog$BP[sample.analog$wave1 == 1] <- 
    (sample.analog$Mom_SW_combined[sample.analog$wavenumber == 1] + 
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
  
  
  return(list(sample.analog,  
              mean(sample.analog$BP[sample.analog$wave1 == 1], na.rm = T), 
              mean(sample.analog$BP[sample.analog$wave2 == 1], na.rm = T),
              mean(sample.analog$BP[sample.analog$wave3 == 1], na.rm = T)))  
} 


# (B) Linear Probability Model on Whole Sample for animal 
LPM_ME_Fun_animal <- function(food_name){
  i <- which(colnames(final.df) == food_name)
  # print(i)
  p1 <- felm(final.df[,i] ~ BP + I(BP^2) + hh_log_wages + hh_kids + hh_young_kids + 
               chicken.price_hybrid +
               beef.price_hybrid + pork.price_hybrid +   
               lard.price_hybrid + sardines.price_hybrid + tuna.price_hybrid +   
               milk.price_hybrid + egg.price_hybrid + bean.price_hybrid + rice.price_hybrid |folio + wavenumber | 0 | loc_id,
             data = final.df)
  
  return(list(p1$coefficients[1] + 2*p1$coefficients[2]*mean(final.df$BP, na.rm = T),
              2*p1$coefficients[2], 
              p1$coefficients[3], 
              p1$coefficients[4]))
  
}


# (D) Poisson Model
Poisson_ME_Fun_animal <- function(food_name){
  i <- which(colnames(final.df.subset) == food_name)
  p1 <- glmmboot(final.df.subset[,i] ~ BP + BP2 + hh_log_wages +  hh_kids + hh_young_kids + wave2 + wave3 +
                   chicken.price_hybrid +
                   beef.price_hybrid + pork.price_hybrid +   
                   lard.price_hybrid + sardines.price_hybrid + tuna.price_hybrid +   
                   milk.price_hybrid + egg.price_hybrid + bean.price_hybrid + rice.price_hybrid, 
                 cluster = factor(folio),
                 data = final.df.subset, family = poisson)
  
  temp <- as.data.frame(cbind(unique(final.df.subset$folio), p1$frail), nrow = 2) 
  colnames(temp) <- c("folio", "frail")
  model.mat <- merge(final.df.subset[,c("folio", "BP", "BP2", "hh_log_wages", "hh_kids", 
                                        "hh_young_kids", "wave2", "wave3",
                                        "chicken.price_hybrid" ,
                                        "beef.price_hybrid" , "pork.price_hybrid" ,    
                                        "lard.price_hybrid" , "sardines.price_hybrid" , "tuna.price_hybrid" ,   
                                        "milk.price_hybrid" , "egg.price_hybrid" , "bean.price_hybrid" , "rice.price_hybrid")], temp, by = c("folio"))
  
  ME <- mean((p1$coefficients[1] + 
                2*p1$coefficients[2]*model.mat$BP) * 
               exp(model.mat$frail + as.matrix(model.mat[,c("BP", "BP2", "hh_log_wages",  "hh_kids", "hh_young_kids", "wave2",  "wave3", 
                                                            "chicken.price_hybrid" ,
                                                            "beef.price_hybrid" , "pork.price_hybrid" ,    
                                                            "lard.price_hybrid" , "sardines.price_hybrid" , "tuna.price_hybrid" ,   
                                                            "milk.price_hybrid" , "egg.price_hybrid" , "bean.price_hybrid" , "rice.price_hybrid")]) %*% 
                     as.numeric(p1$coefficients)), na.rm = T)
  
  Cross_Partial <- ME*as.numeric(p1$coefficients[3]) 
  
  return(list(ME, Cross_Partial)) }


#FRUITS AND VEGETABLES

LPM_ME_Fun_VF <- function(food_name){
  i <- which(colnames(final.df) == food_name)
  # print(i)
  p1 <- felm(final.df[,i] ~ BP + I(BP^2) + hh_log_wages + hh_kids + hh_young_kids + 
               onion.price_hybrid + lime.price_hybrid + apple.price_hybrid + orange.price_hybrid +
               potato.price_hybrid + banana.price_hybrid + leafy.green.price_hybrid +
               tomato.price_hybrid +  
               rice.price_hybrid + milk.price_hybrid + bean.price_hybrid + egg.price_hybrid | folio + wavenumber | 0 | loc_id,
             data = final.df)
  
  return(list(p1$coefficients[1] + 2*p1$coefficients[2]*mean(final.df$BP, na.rm = T), #ME for BP
              2*p1$coefficients[2], # second derivative of BP 
              p1$coefficients[3], # 
              p1$coefficients[4]
  ))
  
}


# (D) Poisson Model
Poisson_ME_Fun_VF <- function(food_name){
  i <- which(colnames(final.df.subset) == food_name)
  p1 <- glmmboot(final.df.subset[,i] ~ BP + BP2 + hh_log_wages + hh_kids + hh_young_kids + wave2 + wave3 + 
                   onion.price_hybrid + lime.price_hybrid + apple.price_hybrid + orange.price_hybrid +
                   potato.price_hybrid + banana.price_hybrid + leafy.green.price_hybrid +
                   tomato.price_hybrid +  
                   rice.price_hybrid + milk.price_hybrid + bean.price_hybrid + egg.price_hybrid , 
                 cluster = factor(folio), # The fixed effect
                 data = final.df.subset, family = poisson)
  
  temp <- as.data.frame(cbind(unique(final.df.subset$folio), p1$frail), nrow = 2) 
  colnames(temp) <- c("folio", "frail")
  model.mat <- merge(final.df.subset[,c("folio", "BP", "BP2", "hh_log_wages", "hh_kids", 
                                        "hh_young_kids", "wave2", "wave3",
                                        "onion.price_hybrid" , "lime.price_hybrid" , "apple.price_hybrid" , "orange.price_hybrid" ,
                                        "potato.price_hybrid" , "banana.price_hybrid" , "leafy.green.price_hybrid" ,
                                        "tomato.price_hybrid" ,  
                                        "rice.price_hybrid" , "milk.price_hybrid" , "bean.price_hybrid" , "egg.price_hybrid")], temp, by = c("folio"))
  
  ME <- mean((p1$coefficients[1] + 2*p1$coefficients[2]*model.mat$BP) * exp(model.mat$frail + 
                                                                              as.matrix(model.mat[,
                                                                                                  c("BP", "BP2", "hh_log_wages",  "hh_kids", "hh_young_kids",  "wave2", "wave3",   
                                                                                                    "onion.price_hybrid" , "lime.price_hybrid" , "apple.price_hybrid" , "orange.price_hybrid" ,
                                                                                                    "potato.price_hybrid" , "banana.price_hybrid" , "leafy.green.price_hybrid" ,
                                                                                                    "tomato.price_hybrid" ,  
                                                                                                    "rice.price_hybrid" , "milk.price_hybrid" , "bean.price_hybrid" , "egg.price_hybrid")]) %*% 
                                                                              as.numeric(p1$coefficients)), na.rm = T)
  
  Cross_Partial <- ME*as.numeric(p1$coefficients[3]) 
  
  return(list(ME, Cross_Partial)) }


#PULSES AND GRAINS

LPM_ME_Fun_grains <- function(food_name){
  i <- which(colnames(final.df) == food_name)
  #  print(i)
  p1 <- felm(final.df[,i] ~ BP + I(BP^2) + hh_log_wages + hh_kids + hh_young_kids + 
               digestive.biscuit.price_hybrid +      
               pan.blanco.price_hybrid + 
               tortilla.price_hybrid + wheat.flour.price_hybrid + 
               rice.price_hybrid + milk.price_hybrid + bean.price_hybrid + egg.price_hybrid | folio + wavenumber | 0 | loc_id,
             data = final.df)
  
  return(list(p1$coefficients[1] + 2*p1$coefficients[2]*mean(final.df$BP, na.rm = T),
              2*p1$coefficients[2],
              p1$coefficients[3], 
              p1$coefficients[4]))
  
}


# (D) Poisson Model
Poisson_ME_Fun_grains <- function(food_name){
  i <- which(colnames(final.df.subset) == food_name)
  p1 <- glmmboot(final.df.subset[,i] ~ BP + BP2 + hh_log_wages + hh_kids + hh_young_kids + wave2 + wave3 +
                   digestive.biscuit.price_hybrid +      
                   pan.blanco.price_hybrid + 
                   tortilla.price_hybrid + wheat.flour.price_hybrid + 
                   rice.price_hybrid + milk.price_hybrid + bean.price_hybrid + egg.price_hybrid, 
                 cluster = factor(folio),
                 data = final.df.subset, family = poisson)
  
  temp <- as.data.frame(cbind(unique(final.df.subset$folio), p1$frail), nrow = 2) 
  colnames(temp) <- c("folio", "frail")
  model.mat <- merge(final.df.subset[,c("folio", "BP", "BP2", "hh_log_wages", "hh_kids", 
                                        "hh_young_kids", "wave2", "wave3",
                                        "digestive.biscuit.price_hybrid" ,      
                                        "pan.blanco.price_hybrid" , 
                                        "tortilla.price_hybrid" , "wheat.flour.price_hybrid" , 
                                        "rice.price_hybrid" , "milk.price_hybrid" , "bean.price_hybrid" , "egg.price_hybrid")], temp, by = c("folio"))
  
  ME <- mean((p1$coefficients[1] + 2*p1$coefficients[2]*model.mat$BP) * exp(model.mat$frail + 
                                                                              as.matrix(model.mat[,
                                         c("BP", "BP2", "hh_log_wages",  "hh_kids", "hh_young_kids", "wave2", "wave3",   
                                           "digestive.biscuit.price_hybrid" ,      
                                           "pan.blanco.price_hybrid" , 
                                           "tortilla.price_hybrid" , "wheat.flour.price_hybrid" , 
                                           "rice.price_hybrid" , "milk.price_hybrid" , "bean.price_hybrid" , "egg.price_hybrid")]) %*% 
                     as.numeric(p1$coefficients)), na.rm = T)
  
  Cross_Partial <- ME*as.numeric(p1$coefficients[3]) 
  
  return(list(ME, Cross_Partial)) }



# OTHER 

LPM_ME_Fun_misc <- function(food_name){
  i <- which(colnames(final.df) == food_name)
  # print(i)
  p1 <- felm(final.df[,i] ~ BP + I(BP^2) + hh_log_wages + hh_kids + hh_young_kids + 
               sugar.price_hybrid + coffee.price_hybrid + soda.price_hybrid + 
               veg.oil.price_hybrid + sopa.de.pasta.price_hybrid + # breakfast.cereal.price_hybrid + 
               rice.price_hybrid + milk.price_hybrid + bean.price_hybrid + egg.price_hybrid | folio + wavenumber | 0 | loc_id,
             data = final.df)
  
  return(list(p1$coefficients[1] + 2*p1$coefficients[2]*mean(final.df$BP, na.rm = T), 
              2*p1$coefficients[2],
              p1$coefficients[3], 
              p1$coefficients[4]))
  
}


# (D) Poisson Model
Poisson_ME_Fun_misc <- function(food_name){
  i <- which(colnames(final.df.subset) == food_name)
  p1 <- glmmboot(final.df.subset[,i] ~ BP + BP2 + hh_log_wages + hh_kids + hh_young_kids +  wave2 + wave3 + 
                   sugar.price_hybrid + coffee.price_hybrid + soda.price_hybrid + 
                   veg.oil.price_hybrid + sopa.de.pasta.price_hybrid + # breakfast.cereal.price_hybrid + 
                   rice.price_hybrid + milk.price_hybrid + bean.price_hybrid + egg.price_hybrid, 
                 cluster = folio,
                 data = final.df.subset, family = poisson)
  
  temp <- as.data.frame(cbind(unique(final.df.subset$folio), p1$frail), nrow = 2) 
  colnames(temp) <- c("folio", "frail")
  model.mat <- merge(final.df.subset[,c("folio", "BP", "BP2", "hh_log_wages", "hh_kids", 
                                        "hh_young_kids",  "wave2", "wave3",
                                        "sugar.price_hybrid" , "coffee.price_hybrid" , "soda.price_hybrid" , 
                                        "veg.oil.price_hybrid" , "sopa.de.pasta.price_hybrid" , "breakfast.cereal.price_hybrid" , 
                                        "rice.price_hybrid" , "milk.price_hybrid" , "bean.price_hybrid" , "egg.price_hybrid")], temp, by = c("folio"))
  
  ME <- mean((p1$coefficients[1] + 2*p1$coefficients[2]*model.mat$BP) * exp(model.mat$frail + 
      as.matrix(model.mat[,
                          c("BP", "BP2", "hh_log_wages",  "hh_kids", "hh_young_kids",  "wave2", "wave3",   
                            "sugar.price_hybrid" , "coffee.price_hybrid" , "soda.price_hybrid" , 
                            "veg.oil.price_hybrid" , "sopa.de.pasta.price_hybrid" , # "breakfast.cereal.price_hybrid" , 
                            "rice.price_hybrid" , "milk.price_hybrid" , "bean.price_hybrid" , "egg.price_hybrid" )]) %*% 
      as.numeric(p1$coefficients)), na.rm = T)
  
  Cross_Partial <- ME*as.numeric(p1$coefficients[3]) 
  
  return(list(ME, Cross_Partial)) }


# Chapter 1: Estimating BP for each HH in each period ####

sample.analog <- hh.df  

BP.Fun.Results <- BP.Fun()
sample.analog <- BP.Fun.Results[[1]] 

# making a matrix of just the HH level variables: 
final.df <- aggregate(sample.analog$BP, by = list(sample.analog$folio, sample.analog$wavenumber), FUN=mean, na.rm=T)
colnames(final.df) <- c("folio", "wavenumber", "BP")

final.df <- 
  unique(left_join(sample.analog[,
                                 c("folio", "wavenumber", "loc_id", "hh_log_wages" , "hh_kids" , 
                                   "hh_young_kids" , "seven_states",
                                   "wave2", "wave3", 
                                   # Staples
                                   "rice.price_hybrid", "bean.price_hybrid", "egg.price_hybrid", 
                                   "milk.price_hybrid", 
                                   # ANIMAL PRODUCTS
                                   "pollo", "huevos", "leche", "pollo_num_times_consume", "leche_num_times_consume", "huevos_num_times_consume", "carne.de.res.o.puerco",
                                   "pescados.y.mariscos", "pescados.y.mariscos_num_times_consume", "sardinas.o.atun.en.lata", "sardinas.o.atun.en.lata_num_times_consume",
                                   "carne.de.res.o.puerco_num_times_consume", "manteca.de.cerdo", "manteca.de.cerdo_num_times_consume",
                                   "chicken.price_hybrid", "lard.price_hybrid" , "sardines.price_hybrid", "tuna.price_hybrid" ,
                                   "beef.price_hybrid",  "pork.price_hybrid",  "lard.price_hybrid" ,
                                   # FRUITS AND VEGETABLES
                                   "tomate.rojo" , "zanahorias", "narajas", "verdudas.de.hoja",
                                   "narajas_num_times_consume", "tomate.rojo_num_times_consume",
                                   "cebolla_num_times_consume", "cebolla",
                                   "verdudas.de.hoja", "verdudas.de.hoja_num_times_consume", "platanos", 
                                   "platanos_num_times_consume", "zanahorias_num_times_consume", "limones", "limones_num_times_consume",
                                   "papa_num_times_consume", "papa", "manzanas", "manzanas_num_times_consume",
                                   "onion.price_hybrid" , "lime.price_hybrid" , "apple.price_hybrid" , "orange.price_hybrid" ,
                                   "potato.price_hybrid" , "banana.price_hybrid" , "leafy.green.price_hybrid",
                                   "tomato.price_hybrid" ,
                                   # PULSES AND GRAINS
                                   "digestive.biscuit.price_hybrid" ,  "frijol", "frijol_num_times_consume",
                                   "pan.blanco.price_hybrid" , 
                                   "tortilla.price_hybrid" , "tortialls.de.maiz_num_times_consume", "tortialls.de.maiz",
                                   "cereales.de.caja", "cereales.de.caja_num_times_consume", "pan.blanco", "pan.de.dulce",
                                   "pan.de.dulce_num_times_consume", "galletas", "galletas_num_times_consume",
                                   "pan.blanco_num_times_consume", 
                                   "pastelillos.en.bolsa_num_times_consume", "pastelillos.en.bolsa",
                                   "maiz.en.grano_num_times_consume", "maiz.en.grano",
                                   "harina.de.trigo_num_times_consume", "harina.de.trigo",
                                   # Miscellaneous
                                   "bebidas.alcoholicas_num_times_consume", "bebidas.alcoholicas",
                                   "cafe", "cafe_num_times_consume", "arroz", "arroz_num_times_consume",
                                   "sugar.price_hybrid" , "coffee.price_hybrid" , "soda.price_hybrid" ,
                                   "azucar_num_times_consume", "azucar",
                                   "refrescos", "refrescos_num_times_consume",
                                   "sopa.de.pasta", "sopa.de.pasta_num_times_consume",
                                   "wheat.flour.price_hybrid" , "veg.oil.price_hybrid" , 
                                   "aciete.vegetal", "aciete.vegetal_num_times_consume",
                                   "sopa.de.pasta.price_hybrid", "breakfast.cereal.price_hybrid",
                                   # Treatment Vars
                                   "treatment_household", "treatment_household_0", "progresa_income_total", 
                                   "receive_progresa")],
                   final.df, by =   c("folio", "wavenumber")))

DiD <- (mean(final.df$BP[final.df$wavenumber == 2 & final.df$treatment_household == 1], na.rm=T) - 
             mean(final.df$BP[final.df$wavenumber ==  1 & final.df$treatment_household == 1], na.rm=T)) -
  (mean(final.df$BP[final.df$wavenumber == 2 & final.df$treatment_household == 0], na.rm=T) -
     mean(final.df$BP[final.df$wavenumber == 1 & final.df$treatment_household == 0], na.rm=T))

DiD_reg_summary <- summary(lm(BP ~ treatment_household + wavenumber + I(treatment_household*wavenumber), data = subset(final.df, final.df$wavenumber < 3)))
DiD_reg_summary
# sqrt(DiD_reg_summary$cov.unscaled[4,4])

# Chapter 2: Summary Stats for HH's and HH Heads #####

length(table(unique(hh.df$folio[hh.df$wavenumber == 1])))
length(table(unique(hh.df$folio[hh.df$wavenumber == 2])))
length(table(unique(hh.df$folio[hh.df$wavenumber == 3])))

summary(hh.df$wages[hh.df$sex==1 & hh.df$age > 15 & hh.df$age < 71 & hh.df$LFP ==1 & hh.df$wavenumber==1])
summary(hh.df$wages[hh.df$sex==1 & hh.df$age > 15 & hh.df$age < 71 & hh.df$LFP ==1 & hh.df$wavenumber==2])
summary(hh.df$wages[hh.df$sex==1 & hh.df$age > 15 & hh.df$age < 71 & hh.df$LFP ==1 & hh.df$wavenumber==3])

summary(hh.df$wages[hh.df$sex==0 & hh.df$age > 15 & hh.df$age < 71 & hh.df$LFP ==1 & hh.df$wavenumber==1])
summary(hh.df$wages[hh.df$sex==0 & hh.df$age > 15 & hh.df$age < 71 & hh.df$LFP ==1 & hh.df$wavenumber==2])
summary(hh.df$wages[hh.df$sex==0 & hh.df$age > 15 & hh.df$age < 71 & hh.df$LFP ==1 & hh.df$wavenumber==3])

summary(hh.df$LFP[hh.df$sex==1 & hh.df$age > 15 & hh.df$age < 71 & hh.df$wavenumber==1])
summary(hh.df$LFP[hh.df$sex==1 & hh.df$age > 15 & hh.df$age < 71 & hh.df$wavenumber==2])
summary(hh.df$LFP[hh.df$sex==1 & hh.df$age > 15 & hh.df$age < 71 & hh.df$wavenumber==3])

summary(hh.df$LFP[hh.df$sex==0 & hh.df$age > 15 & hh.df$age < 71 & hh.df$wavenumber==1])
summary(hh.df$LFP[hh.df$sex==0 & hh.df$age > 15 & hh.df$age < 71 & hh.df$wavenumber==2])
summary(hh.df$LFP[hh.df$sex==0 & hh.df$age > 15 & hh.df$age < 71 & hh.df$wavenumber==3])

summary(hh.df$progresa_income_total[hh.df$wavenumber==2 & hh.df$progresa_income_total>0])
summary(hh.df$progresa_income_total[hh.df$wavenumber==3 & hh.df$progresa_income_total>0])

summary(hh.df$edu_yrs[hh.df$sex==1 & hh.df$head_dummy==1 & hh.df$wavenumber==1])
summary(hh.df$edu_yrs[hh.df$sex==1 & hh.df$head_dummy==1 & hh.df$wavenumber==2])
summary(hh.df$edu_yrs[hh.df$sex==1 & hh.df$head_dummy==1 & hh.df$wavenumber==3])

summary(hh.df$edu_yrs[hh.df$sex==0 & hh.df$head_dummy==1 & hh.df$wavenumber==1])
summary(hh.df$edu_yrs[hh.df$sex==0 & hh.df$head_dummy==1 & hh.df$wavenumber==2])
summary(hh.df$edu_yrs[hh.df$sex==0 & hh.df$head_dummy==1 & hh.df$wavenumber==3])

summary(hh.df$age[hh.df$sex==1 & hh.df$head_dummy==1 & hh.df$wavenumber==1])
summary(hh.df$age[hh.df$sex==1 & hh.df$head_dummy==1 & hh.df$wavenumber==2])
summary(hh.df$age[hh.df$sex==1 & hh.df$head_dummy==1 & hh.df$wavenumber==3])

summary(hh.df$age[hh.df$sex==0 & hh.df$head_dummy==1 & hh.df$wavenumber==1])
summary(hh.df$age[hh.df$sex==0 & hh.df$head_dummy==1 & hh.df$wavenumber==2])
summary(hh.df$age[hh.df$sex==0 & hh.df$head_dummy==1 & hh.df$wavenumber==3])

summary(hh.df$hh_kids[hh.df$wavenumber == 1])
summary(hh.df$hh_kids[hh.df$wavenumber == 2])
summary(hh.df$hh_kids[hh.df$wavenumber == 3])

summary(hh.df$hh_wages[hh.df$wavenumber==1])
summary(hh.df$hh_wages[hh.df$wavenumber==2])
summary(hh.df$hh_wages[hh.df$wavenumber==3])

inflation_1998 <- 18.61
inflation_1999 <- 12.32
inflation_2000 <- 8.96

summary(hh.df$hh_wages[hh.df$wavenumber==2]*(1-.1861)*(1-.1232))
summary(hh.df$hh_wages[hh.df$wavenumber==3]*(1-.1861)*(1-.1232)*(1-0.0896))

summary(hh.df$indigenous_language[hh.df$wavenumber==1])
summary(hh.df$indigenous_language[hh.df$wavenumber==2])
summary(hh.df$indigenous_language[hh.df$wavenumber==3])

summary(sample.analog$y_hat_women_combined[sample.analog$wavenumber==1 & sample.analog$sex==1 & sample.analog$LFP==1 & sample.analog$age > 15 & sample.analog$age <71])
summary(sample.analog$y_hat_women_combined[sample.analog$wavenumber==2 & sample.analog$sex==1 & sample.analog$LFP==1 & sample.analog$age > 15 & sample.analog$age <71])
summary(sample.analog$y_hat_women_combined[sample.analog$wavenumber==3 & sample.analog$sex==1 & sample.analog$LFP==1 & sample.analog$age > 15 & sample.analog$age <71])

summary(sample.analog$y_hat_men_combined[sample.analog$wavenumber==1 & sample.analog$sex==0 & sample.analog$LFP==1 & sample.analog$age > 15 & sample.analog$age <71])
summary(sample.analog$y_hat_men_combined[sample.analog$wavenumber==2 & sample.analog$sex==0 & sample.analog$LFP==1 & sample.analog$age > 15 & sample.analog$age <71])
summary(sample.analog$y_hat_men_combined[sample.analog$wavenumber==3 & sample.analog$sex==0 & sample.analog$LFP==1 & sample.analog$age > 15 & sample.analog$age <71])

summary(sample.analog$y_hat_women_combined[sample.analog$wavenumber==1 & sample.analog$sex==1  & sample.analog$age > 15 & sample.analog$age <71])
summary(sample.analog$y_hat_women_combined[sample.analog$wavenumber==2 & sample.analog$sex==1  & sample.analog$age > 15 & sample.analog$age <71])
summary(sample.analog$y_hat_women_combined[sample.analog$wavenumber==3 & sample.analog$sex==1  & sample.analog$age > 15 & sample.analog$age <71])

summary(sample.analog$y_hat_men_combined[sample.analog$wavenumber==1 & sample.analog$sex==0 & sample.analog$age > 15 & sample.analog$age <71])
summary(sample.analog$y_hat_men_combined[sample.analog$wavenumber==2 & sample.analog$sex==0 & sample.analog$age > 15 & sample.analog$age <71])
summary(sample.analog$y_hat_men_combined[sample.analog$wavenumber==3 & sample.analog$sex==0 & sample.analog$age > 15 & sample.analog$age <71])

prop.table(table(hh.df$seven_states[ hh.df$wavenumber==1]))
prop.table(table(hh.df$seven_states[ hh.df$wavenumber==2]))
prop.table(table(hh.df$seven_states[ hh.df$wavenumber==3]))

# Now input that information by hand to generate Table 1: Summary Statistics. 

# Chapter 3: Summary Stats for Diet #####
library(stargazer)

food_var_names <- c('tomate.rojo', 'cebolla', 'papa', 'zanahorias', 'verdudas.de.hoja', 'narajas', 'platanos',
                    'manzanas', 'limones',
                    'tortialls.de.maiz', 'maiz.en.grano', 'pan.blanco',
                    'pan.de.dulce',
                    #' 'pan.de.caja',
                    'harina.de.trigo', 'sopa.de.pasta', 'arroz', 'galletas', 'frijol', 
                    'cereales.de.caja',
                    'pollo', 'carne.de.res.o.puerco', 'pescados.y.mariscos', 
                    'sardinas.o.atun.en.lata', 
                    'huevos', 'leche', 'manteca.de.cerdo', 'pastelillos.en.bolsa', 'refrescos', 
                    'bebidas.alcoholicas', 'cafe', 'azucar', 'aciete.vegetal')

food_var_names_num <- paste0(food_var_names, "_num_times_consume", sep="")

means_97 <- map_dbl(.x = final.df[,food_var_names][final.df$wavenumber == 1,],
                    .f = function(x){100*round(mean(x, na.rm=T), 3)})

means_99_t <- map_dbl(.x = final.df[,food_var_names][final.df$wavenumber == 2 & final.df$treatment_household == 1,],
                  .f = function(x){100*round(mean(x, na.rm=T), 3)})

means_99_c <- map_dbl(.x = final.df[,food_var_names][final.df$wavenumber == 2 & final.df$treatment_household == 0,],
                  .f = function(x){100*round(mean(x, na.rm=T), 3)})

means_99_c <- map_dbl(.x = final.df[,food_var_names][final.df$wavenumber == 2 & final.df$treatment_household == 0,],
                  .f = function(x){100*round(mean(x, na.rm=T), 3)})

means_00 <- map_dbl(.x = final.df[,food_var_names][final.df$wavenumber == 3,],
                .f = function(x){100*round(mean(x, na.rm=T), 3)}) 

means_97_p <- map_dbl(.x = final.df[,food_var_names_num][final.df$wavenumber == 1,],
                      .f = function(x){round(mean(x, na.rm=T), 2)})
means_99_t_p <- map_dbl(.x = final.df[,food_var_names_num][final.df$wavenumber == 2 & 
                                                             final.df$treatment_household == 1,],
                  .f = function(x){round(mean(x, na.rm=T), 2)})
means_99_c_p <- map_dbl(.x = final.df[,food_var_names_num][final.df$wavenumber == 2 & 
                                                             final.df$treatment_household == 0,],
                  .f = function(x){round(mean(x, na.rm=T), 2)})
means_99_c_p <- map_dbl(.x = final.df[,food_var_names_num][final.df$wavenumber == 2 & 
                                                             final.df$treatment_household == 0,],
                  .f = function(x){round(mean(x, na.rm=T), 2)})
means_00_p <- map_dbl(.x = final.df[,food_var_names_num][final.df$wavenumber == 3,],
                .f = function(x){round(mean(x, na.rm=T), 2)}) 

significant.difference <- c(unname(t.test(# Chicken
  x = final.df[,food_var_names[1]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[1]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat),
unname(t.test( # Beef Pork
  x = final.df[,food_var_names[2]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[2]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat),
unname(t.test( # Eggs
  x = final.df[,food_var_names[3]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[3]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat),
unname(t.test( # Milk
  x = final.df[,food_var_names[4]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[4]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat),
unname(t.test( # Potatoes
  x = final.df[,food_var_names[5]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[5]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat),
unname(t.test( # Chicken
  x = final.df[,food_var_names[6]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[6]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat),
unname(t.test( # Chicken
  x = final.df[,food_var_names[7]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[7]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat),
unname(t.test( # Chicken
  x = final.df[,food_var_names[8]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[8]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat),
unname(t.test( # Chicken
  x = final.df[,food_var_names[9]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[9]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat),
unname(t.test( # Chicken
  x = final.df[,food_var_names[10]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[10]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat),
unname(t.test( # Chicken
  x = final.df[,food_var_names[11]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[11]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat),
unname(t.test( # Chicken
  x = final.df[,food_var_names[12]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[12]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat),
unname(t.test( # Chicken
  x = final.df[,food_var_names[13]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[13]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat),
unname(t.test( # Chicken
  x = final.df[,food_var_names[14]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[14]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat),
unname(t.test( # Chicken
  x = final.df[,food_var_names[15]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[15]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat), 
unname(t.test( # Chicken
  x = final.df[,food_var_names[16]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[16]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat), 
unname(t.test( # Chicken
  x = final.df[,food_var_names[17]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[17]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat), 
unname(t.test( # Chicken
  x = final.df[,food_var_names[18]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[18]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat), 
unname(t.test( # Chicken
  x = final.df[,food_var_names[19]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[19]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat), 
unname(t.test( # Chicken
  x = final.df[,food_var_names[20]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[20]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat), 
unname(t.test( # Chicken
  x = final.df[,food_var_names[21]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[21]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat), 
unname(t.test( # Chicken
  x = final.df[,food_var_names[22]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[22]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat), 
unname(t.test( # Chicken
  x = final.df[,food_var_names[23]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[23]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat), 
unname(t.test( # Chicken
  x = final.df[,food_var_names[24]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[24]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat), 
unname(t.test( # Chicken
  x = final.df[,food_var_names[25]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[25]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat), 
unname(t.test( # Chicke
  x = final.df[,food_var_names[26]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[26]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat), 
unname(t.test( # Chicken
  x = final.df[,food_var_names[27]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[27]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat), 
unname(t.test( # Chicken
  x = final.df[,food_var_names[28]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[28]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat), 
unname(t.test( # Chicken
  x = final.df[,food_var_names[29]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[29]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat), 
unname(t.test( # Chicken
  x = final.df[,food_var_names[30]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[30]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat), 
unname(t.test( # Chicken
  x = final.df[,food_var_names[31]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[31]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat), 
unname(t.test( # Chicken
  x = final.df[,food_var_names[32]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[32]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat))

sig.diffs <-
  tibble('Names' = food_var_names, 
         't' = significant.difference,
       'Sig' = (abs(significant.difference) > 1.96))

means.df <- tibble('Names' = food_var_names, # Translate the spanish to english in the document. 
                   '1997' = paste0(means_97, " (", means_97_p, ")"), 
                   '1999 Control' = paste0(means_99_c, " (", means_99_c_p, ")"),
                   '1999 Treatment' = paste0(means_99_t, " (", means_99_t_p, ")"),
                   '2000' = paste0(means_00, " (", means_00_p, ")"))
means.df

stargazer(means.df, summary = FALSE, title = "Percent of Households Consuming Each Food (with Mean Days/Week Frequency)")



# Chapter 4: Generating Tables 4 and 5, the predicted earnings tables ####

# Women's Subset: 
women.sub <- subset(hh.df,  
                  hh.df$sex == 1 &
                    hh.df$age > 15 &
                    hh.df$age <= 65 &
                    hh.df$hh_kids <= 5 & 
                    hh.df$hh_young_kids <= 4 & 
                    hh.df$num_m_adults <= 5 &
                    hh.df$num_f_adults <= 5 & 
                    hh.df$hh_young_kids <= quantile(hh.df$hh_young_kids, 0.95))
# Drop the income outliers who make prediction far less accurate
women.sub$drop <- ifelse(!is.na(women.sub$log_wages) &  women.sub$log_wages >=  quantile(women.sub$log_wages, c(0.99), na.rm=T), 1, 0)
women.sub$drop <- ifelse(!is.na(women.sub$log_wages) & women.sub$log_wages <=  quantile(women.sub$log_wages, c(0.01), na.rm=T), 1, women.sub$drop)
women.sub <- women.sub %>% filter(drop == 0)

# Men's Subset: 
men.sub <- subset(hh.df,  
                    hh.df$sex == 0 &
                      hh.df$age > 15 &
                      hh.df$age <= 65 &
                      hh.df$hh_kids <= 5 & 
                      hh.df$hh_young_kids <= 4 & 
                      hh.df$num_m_adults <= 5 &
                      hh.df$num_f_adults <= 5 & 
                      hh.df$hh_young_kids <= quantile(hh.df$hh_young_kids, 0.95))
# Drop the income outliers who make prediction far less accurate
men.sub$drop <- ifelse(!is.na(men.sub$log_wages) &  men.sub$log_wages >=  quantile(men.sub$log_wages, c(0.99), na.rm=T), 1, 0)
men.sub$drop <- ifelse(!is.na(men.sub$log_wages) & men.sub$log_wages <=  quantile(men.sub$log_wages, c(0.01), na.rm=T), 1, men.sub$drop)
men.sub <- men.sub %>% filter(drop == 0)

# Earnings Models
men.reg <- selection(selection = LFP ~ age + I(age^2)  + otherincomeval_dummy + asinh(otherincomeval) + hh_kids + 
                     hh_young_kids + edu_yrs + literate + gov_transfer +
                     indigenous_language + spanish_language + head_dummy + num_f_adults + num_m_adults + #pobextre +  mpcalif  +
                     number_female_kids + number_male_kids   +  prop_mex_migrant_dummy + prop_usa_migrant_dummy  +
                     I(num_m_adults*prop_mex_migrant) +  prop_usa_migrant + prop_mex_migrant + 
                     I(num_m_adults*prop_usa_migrant) + I(num_f_adults*prop_mex_migrant) +
                     I(num_f_adults*prop_usa_migrant) +    
                     as.factor(year_wave_FE) + receive_progresa  +  # FE and Exclusion Restrictions
                     (ER + proportion_need_permission + proportion_need_accompany)*hh_young_kids,  
                   outcome = log_wages ~ age + I(age^2) +  otherincomeval_dummy + asinh(otherincomeval) + hh_kids +
                     hh_young_kids + edu_yrs  + literate + gov_transfer +
                     indigenous_language + spanish_language + head_dummy + num_f_adults + num_m_adults +# pobextre +  mpcalif  +
                     number_female_kids + number_male_kids  +  prop_mex_migrant_dummy + prop_usa_migrant_dummy  +
                     I(num_m_adults*prop_mex_migrant) +  prop_usa_migrant + prop_mex_migrant + 
                     I(num_m_adults*prop_usa_migrant) + I(num_f_adults*prop_mex_migrant) +
                     I(num_f_adults*prop_usa_migrant) +    
                     as.factor(year_wave_FE) + receive_progresa,
                   data = men.sub,
                   method = "ml")

women.reg <- selection(selection = LFP ~ age + I(age^2) +  otherincomeval_dummy +  asinh(otherincomeval) + hh_kids +
                     hh_young_kids + edu_yrs  + literate + gov_transfer +
                     indigenous_language + spanish_language + head_dummy + num_f_adults + num_m_adults + #pobextre +  mpcalif  +
                     number_female_kids + number_male_kids  + 
                     prop_mex_migrant_dummy + prop_usa_migrant_dummy  +
                     I(num_m_adults*prop_mex_migrant) +   prop_usa_migrant + prop_mex_migrant +
                     I(num_m_adults*prop_usa_migrant) + I(num_f_adults*prop_mex_migrant) +
                     I(num_f_adults*prop_usa_migrant) +
                     as.factor(year_wave_FE) + receive_progresa  +  # FE and Exclusion Restrictions
                     (ER + proportion_need_permission + proportion_need_accompany)*hh_young_kids  + # ER*num_f_adults + ER*num_m_adults +
                     asinh(progresa_income_mom), 
                   # 
                   outcome = log_wages ~ age + I(age^2) +  otherincomeval_dummy +  asinh(otherincomeval) + hh_kids +
                     hh_young_kids + edu_yrs  + literate + gov_transfer +
                     indigenous_language + spanish_language + head_dummy + num_f_adults + num_m_adults + #pobextre +  mpcalif  +
                     number_female_kids + number_male_kids  +
                     prop_mex_migrant_dummy + prop_usa_migrant_dummy  +
                     I(num_m_adults*prop_mex_migrant) +  prop_usa_migrant + prop_mex_migrant +
                     I(num_m_adults*prop_usa_migrant) + I(num_f_adults*prop_mex_migrant) +
                     I(num_f_adults*prop_usa_migrant) +
                     as.factor(year_wave_FE) + receive_progresa  +  
                     asinh(progresa_income_mom),
                   data = women.sub,
                   method = "ml")  
  

stargazer::stargazer(women.reg, men.reg, omit = c("year_wave_FE"), single.row = T,
                     covariate.labels = c("Age"  , "Age Squared" , "Other Income Dummy", "Asihn of Other Income" , "Number of Kids", 
                                          "Number of Kids Ages", "Education" , "Literate Dummy",  "Other Gov Transfer Dummy",
                                          "Indigenous Language Dummy" , "Spanish and Ind. Lang. Dummy", "Household Head Dummy" ,
                                          "Number Female Adults" , "Number Male Adults" , "Gov. Poverty Index" , "Gov. Poverty Dummy", 
                                          "Number Female Kids", "Number Male Kids" , "Prop. Village Migrates MEX"  ,
                                          "Prop. Village Migrates USA" , "Num Male Adults * Prop. MEX Mig",
                                        "Num Male Adults *  Prop. USA Mig",
                                        "Num Female Adults *  Prop. MEX Mig",
                                        "Num Male Adults * Prop. USA Mig",
                                        "Progresa Control Group Dummy" ,  "Female HH Head's Progresa Income"))

stargazer::stargazer(women.reg, men.reg, selection.equation = TRUE, omit = c("year_wave_FE"), single.row = T, 
                     covariate.labels =
                       c("Age"  , "Age Squared" , "Other Income Dummy", "Asihn of Other Income" , "Number of Kids", 
                       "Number of Kids Ages", "Education" , "Literate Dummy",  "Other Gov Transfer Dummy",
                       "Indigenous Language Dummy" , "Spanish and Ind. Lang. Dummy", "Household Head Dummy" ,
                       "Number Female Adults" , "Number Male Adults" , "Gov. Poverty Index" , "Gov. Poverty Dummy", 
                       "Number Female Kids", "Number Male Kids" , "Prop. Village Migrates MEX"  ,
                       "Prop. Village Migrates USA" , "Num Male Adults * Prop. MEX Mig",
                       "Num Male Adults *  Prop. USA Mig",
                       "Num Female Adults *  Prop. MEX Mig",
                       "Num Male Adults * Prop. USA Mig",
                       "Progresa Control Group Dummy" , "Women's Job View Proportion",
                       "Need Permission Proportion", "Need Accompaniment Proportion",
                       "Female HH Head's Progresa Income", 
                       "ER1 Interaction", "ER2 Interaction", "ER3 Interaction"))

a <- summary(men.reg)
b <- summary(women.reg)


a <- tibble()
    



t1 <- xtable::xtable(a$estimate[1:53,],  omit = c("year_wave_FE"), single.row = T)
t2 <- xtable::xtable(b$estimate[1:52,],  omit = c("year_wave_FE"), single.row = T)

t <- as.data.frame(cbind(rownames(a$estimate)[1:53],t1[1:52,"Estimate"], 
                         t1[1:52,"Std. error"], t2[1:52,"Estimate"], t2[1:52,c("Std. error")]), NCOL=5)

colnames(t) <- c("Names", "Women Estimate1", " Women Std.1", " Men Estimate2", "Men std.2")

t$`Women Estimate1` <- as.numeric(as.character(t$`Women Estimate1`))
t$` Women Std.1` <- as.numeric(as.character(t$` Women Std.1`))
t$` Men Estimate2` <- as.numeric(as.character(t$` Men Estimate2`))
t$`Men std.2` <- as.numeric(as.character(t$`Men std.2`))


stargazer::stargazer(t, summary = FALSE, omit = c("as.factor(year_wave_FE)"))

# Chapter 5: Generating Figure 1: a graph of BP over time for the treatment and control groups #####
 
par(mfrow=c(3,1))
#hist(final.df$BP[final.df$wavenumber == 1], col = rgb(1,1,1), main = "1997", xlab = "Bargaining Power", 
#     breaks = 100, xlim = c(0.15,0.5), freq=FALSE, ylab = "Percent of Households")

hist(final.df$BP[final.df$wavenumber == 1 & final.df$treatment_household == 0], col = rgb(1,1,1,0.25, 0.25), main = "1997", xlab = "Bargaining Power", 
     breaks = 100, xlim = c(0.15,0.75), freq=FALSE, ylab = "Percent of Households")
par(new = T)
hist(final.df$BP[final.df$wavenumber == 1 & final.df$treatment_household == 1], col = rgb(0,0,0,0.25, 0.25),  
     breaks = 100, xlim = c(0.15,0.75), freq=FALSE,  axes = F, xlab =NA, ylab=NA, main = NA)

legend("topright", legend = c("Untreated (white)", "Treated (grey)"), col = c("black", "black"), pch = c(0,15))

hist(final.df$BP[final.df$wavenumber == 2 & final.df$progresa_income_total == 0], col = rgb(1,1,1,0.25, 0.25), main = "1999", xlab = "Bargaining Power", 
     breaks = 100, xlim = c(0.15,0.75), freq=FALSE, ylab = "Percent of Households")
par(new = T)
hist(final.df$BP[final.df$wavenumber == 2 & final.df$progresa_income_total > 0], col = rgb(0,0,0,0.25, 0.25), 
     breaks = 100, xlim = c(0.15,0.75), freq=FALSE,  axes = F, xlab =NA, ylab=NA, main = NA)

hist(final.df$BP[final.df$wavenumber == 3 & final.df$progresa_income_total == 0], col = rgb(1,1,1, 0.25, 0.25), main = "2000", xlab = "Bargaining Power", 
     breaks = 100, xlim = c(0.15,0.75), freq=FALSE, ylab = "Percent of Households")
par(new = T)
hist(final.df$BP[final.df$wavenumber == 3 & final.df$progresa_income_total > 0], col = rgb(0,0,0, 0.25, 0.25), 
     breaks = 100, xlim = c(0.15,0.75), freq=FALSE,  axes = F, xlab =NA, ylab=NA, main = NA)


# Color Version for website and other media


par(mfrow=c(3,1))

hist(final.df$BP[final.df$wavenumber == 1 & final.df$treatment_household == 0], col = rgb(0.25,0.75,0.25,0.25, 0.25), main = "1997", xlab = "Bargaining Power", 
     breaks = 100, xlim = c(0.15,0.65), freq=FALSE, ylab = "Percent of Households")
par(new = T)
hist(final.df$BP[final.df$wavenumber == 1 & final.df$treatment_household == 1],
     col = rgb(0,0,1,0.25, 0.25),  
     breaks = 100, xlim = c(0.15,0.65), freq=FALSE,  axes = F, xlab =NA, ylab=NA, main = NA)

legend("topright", legend = c("control (green)", "treatment (blue)"), col = c("light green", "blue"), pch = c(15,15))

hist(final.df$BP[final.df$wavenumber == 2 & final.df$progresa_income_total == 0], col = rgb(0.25,0.75,0.25,0.25, 0.25), main = "1999", xlab = "Bargaining Power", 
     breaks = 100, xlim = c(0.15,0.65), freq=FALSE, ylab = "Percent of Households")
par(new = T)
hist(final.df$BP[final.df$wavenumber == 2 & final.df$progresa_income_total > 0], col = rgb(0,0,1,0.25, 0.25), 
     breaks = 100, xlim = c(0.15,0.65), freq=FALSE,  axes = F, xlab =NA, ylab=NA, main = NA)

hist(final.df$BP[final.df$wavenumber == 3 & final.df$progresa_income_total == 0], col = rgb(0.25,0.75,0.25,0.25, 0.25), main = "2000", xlab = "Bargaining Power", 
     breaks = 100, xlim = c(0.15,0.65), freq=FALSE, ylab = "Percent of Households")
par(new = T)
hist(final.df$BP[final.df$wavenumber == 3 & final.df$progresa_income_total > 0], col = rgb(0,0,1,0.25, 0.25), 
     breaks = 100, xlim = c(0.15,0.65), freq=FALSE,  axes = F, xlab =NA, ylab=NA, main = NA)


# Chapter 6: Generating the Chicken + Milk Table ####

p1 <- summary(chick_LPM <- felm(pollo ~ BP + I(BP^2) + hh_log_wages + hh_kids + hh_young_kids + 
                            chicken.price_hybrid +
                            beef.price_hybrid + pork.price_hybrid +   beef.price_hybrid + pork.price_hybrid + 
                            lard.price_hybrid + sardines.price_hybrid + tuna.price_hybrid +  
                           # orange.price_hybrid + apple.price_hybrid + lime.price_hybrid +
                            milk.price_hybrid + egg.price_hybrid + bean.price_hybrid + rice.price_hybrid 
                          | folio + wavenumber | 0 | loc_id,
           data = final.df))

p2 <- summary(milk_LPM <- felm(leche ~ BP + I(BP^2) + hh_log_wages + hh_kids + hh_young_kids + 
                            chicken.price_hybrid +
                            beef.price_hybrid + pork.price_hybrid +   beef.price_hybrid + pork.price_hybrid + 
                            lard.price_hybrid + sardines.price_hybrid + tuna.price_hybrid +  
                            # orange.price_hybrid + apple.price_hybrid + lime.price_hybrid +
                            milk.price_hybrid + egg.price_hybrid + bean.price_hybrid + rice.price_hybrid 
                          | folio + wavenumber | 0 | loc_id,
                          data = final.df))


keep.index <- with(final.df, { is.na(hh_log_wages) == FALSE & is.na(BP) == FALSE})
final.df.subset <- final.df[keep.index, ]
final.df.subset$BP2 <- final.df.subset$BP^2

summary(chick_Poisson <- glmmboot(final.df.subset[,"pollo"] ~ BP + I(BP^2) + hh_log_wages + hh_kids + hh_young_kids +
                                    chicken.price_hybrid +
                                    lard.price_hybrid + sardines.price_hybrid + tuna.price_hybrid +   
                                    beef.price_hybrid + pork.price_hybrid +   
                                    milk.price_hybrid + egg.price_hybrid + bean.price_hybrid + rice.price_hybrid +
                                    factor(wavenumber), 
               cluster = factor(folio),
               data = final.df.subset, family = poisson))


summary(milk_Poisson <- glmmboot(final.df.subset[,"leche"] ~ BP + I(BP^2) + hh_log_wages + hh_kids + hh_young_kids + 
                                    chicken.price_hybrid +
                                    lard.price_hybrid + sardines.price_hybrid + tuna.price_hybrid +   
                                    beef.price_hybrid + pork.price_hybrid +   
                                    milk.price_hybrid + egg.price_hybrid + bean.price_hybrid + rice.price_hybrid  + 
                                   factor(wavenumber), 
                                  cluster = folio,
                                  data = final.df.subset, family = poisson))

# Should I use gather to make a single matrix with the coefs and se's from all four regressions? 

coefs.df <- tibble(Names = c("$hat{mu}$", "$hat{mu}^{2}$", "HH Log Earnings",
                                "Num Kids", "Num Young Kids", "Chicken Price", 
                                "Beef Price", "Pork Price", "Lard Price", "Sardine Price", "Tuna Price", 
                                "Milk Price", "Egg Price", "Bean Price", "Rice Price"),
                      Chicken.LPM = as.numeric(unname(chick_LPM$coefficients)[1:15,1]),
                      Milk.LPM = as.numeric(unname(milk_LPM$coefficients)[1:15,1]),
                      Chicken.Poi = as.numeric(chick_Poisson$coefficients[1:15]),
                      Milk.Poi = as.numeric(milk_Poisson$coefficients[1:15]))

se.df <- tibble(Names = paste0( c("$hat{mu}$", "$hat{mu}^{2}$", "HH Log Earnings",
                                  "Num Kids", "Num Young Kids", "Chicken Price", 
                                  "Beef Price", "Pork Price", "Lard Price", "Sardine Price", "Tuna Price", 
                                  "Milk Price", "Egg Price", "Bean Price", "Rice Price"), "_se"), 
                Chicken.LPM = unname(p1$coefficients[1:15,2]),
                Milk.LPM = unname(p2$coefficients[1:15,2]),
                Chicken.Poi = unname(chick_Poisson$sd[1:15]),
                Milk.Poi = unname(milk_Poisson$sd[1:15]))

table_6 <- bind_rows(coefs.df, se.df)
table_6[,2:5] <- round(table_6[,2:5], 3) 

stargazer::stargazer(table_6, summary = FALSE, digits = 3)

stargazer::stargazer(chick_LPM, milk_LPM, chick_LPM, milk_LPM, single.row=F, 
                     covariate.labels = c("$hat{mu}$", "$hat{mu}^{2}$", "HH Log Earnings",
                                          "Num Kids", "Num Young Kids", "Chicken Price", 
                                          "Beef Price", "Pork Price", "Lard Price", "Sardine Price", "Tuna Price", 
                                          "Milk Price", "Egg Price", "Bean Price", "Rice Price"), 
                     title = "Chicken and Milk LPM and Poisson Results, FE Not Shown")

# Chapter 7: generating 62 Point Estimate Results ###### 

keep.index <- with(final.df, { is.na(hh_log_wages) == FALSE & is.na(BP) == FALSE})
final.df.subset <- final.df[keep.index, ]
final.df.subset$BP2 <- final.df.subset$BP^2

pa.chick <- Poisson_ME_Fun_animal("pollo_num_times_consume")[[1]]
pa.BPork <- Poisson_ME_Fun_animal("carne.de.res.o.puerco_num_times_consume")[[1]]
pa.eggs <- Poisson_ME_Fun_animal("huevos_num_times_consume")[[1]]
pa.milk <- Poisson_ME_Fun_animal("leche_num_times_consume")[[1]]
pa.fish <- Poisson_ME_Fun_animal("pescados.y.mariscos_num_times_consume")[[1]]
pa.tuna <- Poisson_ME_Fun_animal("sardinas.o.atun.en.lata_num_times_consume")[[1]]
pa.lard <- Poisson_ME_Fun_animal("manteca.de.cerdo_num_times_consume")[[1]]

pa2.chick <- Poisson_ME_Fun_animal("pollo_num_times_consume")[[2]]
pa2.BPork <- Poisson_ME_Fun_animal("carne.de.res.o.puerco_num_times_consume")[[2]]
pa2.eggs <- Poisson_ME_Fun_animal("huevos_num_times_consume")[[2]]
pa2.milk <- Poisson_ME_Fun_animal("leche_num_times_consume")[[2]]
pa2.fish <- Poisson_ME_Fun_animal("pescados.y.mariscos_num_times_consume")[[2]]
pa2.tuna <- Poisson_ME_Fun_animal("sardinas.o.atun.en.lata_num_times_consume")[[2]]
pa2.lard <- Poisson_ME_Fun_animal("manteca.de.cerdo_num_times_consume")[[2]]

la.chick <- LPM_ME_Fun_animal("pollo")[[1]]
la.BPork <- LPM_ME_Fun_animal("carne.de.res.o.puerco")[[1]]
la.eggs <- LPM_ME_Fun_animal("huevos")[[1]]
la.milk <- LPM_ME_Fun_animal("leche")[[1]]
la.fish <- LPM_ME_Fun_animal("pescados.y.mariscos")[[1]]
la.tuna <- LPM_ME_Fun_animal("sardinas.o.atun.en.lata")[[1]]
la.lard <- LPM_ME_Fun_animal("manteca.de.cerdo")[[1]]

la2.chick <- LPM_ME_Fun_animal("pollo")[[2]]
la2.BPork <- LPM_ME_Fun_animal("carne.de.res.o.puerco")[[2]]
la2.eggs <- LPM_ME_Fun_animal("huevos")[[2]]
la2.milk <- LPM_ME_Fun_animal("leche")[[2]]
la2.fish <- LPM_ME_Fun_animal("pescados.y.mariscos")[[2]]
la2.tuna <- LPM_ME_Fun_animal("sardinas.o.atun.en.lata")[[2]]
la2.lard <- LPM_ME_Fun_animal("manteca.de.cerdo")[[2]]


#FRUITS AND VEGETABLES

pVF.onion <- Poisson_ME_Fun_VF("cebolla_num_times_consume")[[1]]
pVF.limes <- Poisson_ME_Fun_VF("limones_num_times_consume")[[1]]
pVF.apples <- Poisson_ME_Fun_VF("manzanas_num_times_consume")[[1]]
pVF.oranges <- Poisson_ME_Fun_VF("narajas_num_times_consume")[[1]]
pVF.potato <- Poisson_ME_Fun_VF("papa_num_times_consume")[[1]]
pVF.banana <- Poisson_ME_Fun_VF("platanos_num_times_consume")[[1]]
pVF.greens <- Poisson_ME_Fun_VF("verdudas.de.hoja_num_times_consume")[[1]]
pVF.carrots <- Poisson_ME_Fun_VF("zanahorias_num_times_consume")[[1]]
pVF.tomato <- Poisson_ME_Fun_VF("tomate.rojo_num_times_consume")[[1]]

pVF2.onion <- Poisson_ME_Fun_VF("cebolla_num_times_consume")[[2]]
pVF2.limes <- Poisson_ME_Fun_VF("limones_num_times_consume")[[2]]
pVF2.apples <- Poisson_ME_Fun_VF("manzanas_num_times_consume")[[2]]
pVF2.oranges <- Poisson_ME_Fun_VF("narajas_num_times_consume")[[2]]
pVF2.potato <- Poisson_ME_Fun_VF("papa_num_times_consume")[[2]]
pVF2.banana <- Poisson_ME_Fun_VF("platanos_num_times_consume")[[2]]
pVF2.greens <- Poisson_ME_Fun_VF("verdudas.de.hoja_num_times_consume")[[2]]
pVF2.carrots <- Poisson_ME_Fun_VF("zanahorias_num_times_consume")[[2]]
pVF2.tomato <- Poisson_ME_Fun_VF("tomate.rojo_num_times_consume")[[2]]

lVF.onion  <- LPM_ME_Fun_VF("cebolla")[[1]]
lVF.limes  <- LPM_ME_Fun_VF("limones")[[1]]
lVF.apples <- LPM_ME_Fun_VF("manzanas")[[1]]
lVF.orange <- LPM_ME_Fun_VF("narajas")[[1]]
lVF.potato <- LPM_ME_Fun_VF("papa")[[1]]
lVF.banana <- LPM_ME_Fun_VF("platanos")[[1]]
lVF.greens <- LPM_ME_Fun_VF("verdudas.de.hoja")[[1]]
lVF.carrot <- LPM_ME_Fun_VF("zanahorias")[[1]]
lVF.tomato <- LPM_ME_Fun_VF("tomate.rojo")[[1]]

lVF2.onion  <- LPM_ME_Fun_VF("cebolla")[[2]]
lVF2.limes  <- LPM_ME_Fun_VF("limones")[[2]]
lVF2.apples <- LPM_ME_Fun_VF("manzanas")[[2]]
lVF2.orange <- LPM_ME_Fun_VF("narajas")[[2]]
lVF2.potato <- LPM_ME_Fun_VF("papa")[[2]]
lVF2.banana <- LPM_ME_Fun_VF("platanos")[[2]]
lVF2.greens <- LPM_ME_Fun_VF("verdudas.de.hoja")[[2]]
lVF2.carrot <- LPM_ME_Fun_VF("zanahorias")[[2]]
lVF2.tomato <- LPM_ME_Fun_VF("tomate.rojo")[[2]]

#PULSES AND GRAINS

pG.rice      <- Poisson_ME_Fun_grains("arroz_num_times_consume")[[1]]
pG.beans     <- Poisson_ME_Fun_grains("frijol_num_times_consume")[[1]]
pG.biscuits  <- Poisson_ME_Fun_grains("galletas_num_times_consume")[[1]]
pG.Cflour    <- Poisson_ME_Fun_grains("maiz.en.grano_num_times_consume")[[1]]
pG.Wbread    <- Poisson_ME_Fun_grains("pan.blanco_num_times_consume")[[1]]
pG.Pastries  <- Poisson_ME_Fun_grains("pan.de.dulce_num_times_consume")[[1]]
pG.Tortillas <- Poisson_ME_Fun_grains("tortialls.de.maiz_num_times_consume")[[1]]
pG.WFlour    <- Poisson_ME_Fun_grains("harina.de.trigo_num_times_consume")[[1]]

pG2.rice      <- Poisson_ME_Fun_grains("arroz_num_times_consume")[[2]]
pG2.beans     <- Poisson_ME_Fun_grains("frijol_num_times_consume")[[2]]
pG2.biscuits  <- Poisson_ME_Fun_grains("galletas_num_times_consume")[[2]]
pG2.Cflour    <- Poisson_ME_Fun_grains("maiz.en.grano_num_times_consume")[[2]]
pG2.Wbread    <- Poisson_ME_Fun_grains("pan.blanco_num_times_consume")[[2]]
pG2.Pastries  <- Poisson_ME_Fun_grains("pan.de.dulce_num_times_consume")[[2]]
pG2.Tortillas <- Poisson_ME_Fun_grains("tortialls.de.maiz_num_times_consume")[[2]]
pG2.WFlour    <- Poisson_ME_Fun_grains("harina.de.trigo_num_times_consume")[[2]]

lG.rice       <- LPM_ME_Fun_grains("arroz")[[1]]
lG.beans     <- LPM_ME_Fun_grains("frijol")[[1]]
lG.biscuits  <- LPM_ME_Fun_grains("galletas")[[1]]
lG.Cflour    <- LPM_ME_Fun_grains("maiz.en.grano")[[1]]
lG.Wbread    <- LPM_ME_Fun_grains("pan.blanco")[[1]]
lG.Pastries  <- LPM_ME_Fun_grains("pan.de.dulce")[[1]]
lG.Tortillas <-  LPM_ME_Fun_grains("tortialls.de.maiz")[[1]]
lG.WFlour    <- LPM_ME_Fun_grains("harina.de.trigo")[[1]]

lG2.rice       <- LPM_ME_Fun_grains("arroz")[[2]]
lG2.beans     <- LPM_ME_Fun_grains("frijol")[[2]]
lG2.biscuits  <- LPM_ME_Fun_grains("galletas")[[2]]
lG2.Cflour    <- LPM_ME_Fun_grains("maiz.en.grano")[[2]]
lG2.Wbread    <- LPM_ME_Fun_grains("pan.blanco")[[2]]
lG2.Pastries  <- LPM_ME_Fun_grains("pan.de.dulce")[[2]]
lG2.Tortillas <-  LPM_ME_Fun_grains("tortialls.de.maiz")[[2]]
lG2.WFlour    <- LPM_ME_Fun_grains("harina.de.trigo")[[2]]

# OTHER 

pM.sugar   <- Poisson_ME_Fun_misc("azucar_num_times_consume")[[1]]
pM.coffee  <- Poisson_ME_Fun_misc("cafe_num_times_consume")[[1]]
pM.soda    <- Poisson_ME_Fun_misc("refrescos_num_times_consume")[[1]]
pM.CupN    <- Poisson_ME_Fun_misc("sopa.de.pasta_num_times_consume")[[1]]
pM.VOil    <- Poisson_ME_Fun_misc("aciete.vegetal_num_times_consume")[[1]]
pM.Alcohol <- Poisson_ME_Fun_misc("bebidas.alcoholicas_num_times_consume")[[1]]
pM.BCereal <- Poisson_ME_Fun_misc("cereales.de.caja_num_times_consume")[[1]]

pM2.sugar   <- Poisson_ME_Fun_misc("azucar_num_times_consume")[[2]]
pM2.coffee  <- Poisson_ME_Fun_misc("cafe_num_times_consume")[[2]]
pM2.soda    <- Poisson_ME_Fun_misc("refrescos_num_times_consume")[[2]]
pM2.CupN    <- Poisson_ME_Fun_misc("sopa.de.pasta_num_times_consume")[[2]]
pM2.VOil    <- Poisson_ME_Fun_misc("aciete.vegetal_num_times_consume")[[2]]
pM2.Alcohol <- Poisson_ME_Fun_misc("bebidas.alcoholicas_num_times_consume")[[2]]
pM2.BCereal <- Poisson_ME_Fun_misc("cereales.de.caja_num_times_consume")[[2]]

lM.sugar   <- LPM_ME_Fun_misc("azucar")[[1]]
lM.coffee  <- LPM_ME_Fun_misc("cafe")[[1]]
lM.soda    <- LPM_ME_Fun_misc("refrescos")[[1]]
lM.CupN    <- LPM_ME_Fun_misc("sopa.de.pasta")[[1]]
lM.VOil    <- LPM_ME_Fun_misc("aciete.vegetal")[[1]]
lM.Alcohol <- LPM_ME_Fun_misc("bebidas.alcoholicas")[[1]]
lM.BCereal <- LPM_ME_Fun_misc("cereales.de.caja")[[1]]

lM2.sugar   <- LPM_ME_Fun_misc("azucar")[[2]]
lM2.coffee  <- LPM_ME_Fun_misc("cafe")[[2]]
lM2.soda    <- LPM_ME_Fun_misc("refrescos")[[2]]
lM2.CupN    <- LPM_ME_Fun_misc("sopa.de.pasta")[[2]]
lM2.VOil    <- LPM_ME_Fun_misc("aciete.vegetal")[[2]]
lM2.Alcohol <- LPM_ME_Fun_misc("bebidas.alcoholicas")[[2]]
lM2.BCereal <- LPM_ME_Fun_misc("cereales.de.caja")[[2]]


results.df <- data.frame(
  cbind(
  c("Chicken", "Beef/Pork","Eggs" , "Milk" ,"Fish" , "Tuna" , "Lard" ,
    "Onion"  , "Limes"  , "Apples" , "Oranges" , "Potatoes" , "Bananas" , "Greens" , "Carrots" ,
    "Tomatoes" , "Rice"     , "Beans"    , "Biscuits" , "Corn Flour"   , "White Bread"   , "Pastries" , "Tortillas",
    "Wheat Flour",    "Sugar"  , "Coffee" , "Soda"  , "Cup Noodles"   , "Veg. Oil"   , "Alcohol", "B. Cereal"),  
  c(la.chick, la.BPork,la.eggs , la.milk ,la.fish , la.tuna ,la.lard ,
    lVF.onion  ,lVF.limes  ,lVF.apples ,lVF.orange , lVF.potato ,lVF.banana ,lVF.greens ,lVF.carrot ,
    lVF.tomato , lG.rice, lG.beans,   lG.biscuits, 
    lG.Cflour,   lG.Wbread,   lG.Pastries, lG.Tortillas, lG.WFlour,    lM.sugar  ,lM.coffee ,lM.soda  , 
    lM.CupN   ,lM.VOil   ,lM.Alcohol,lM.BCereal), 
  c(la2.chick, la2.BPork,la2.eggs , la2.milk ,la2.fish , la2.tuna ,la2.lard ,
    lVF2.onion  ,lVF2.limes  ,lVF2.apples ,lVF2.orange , lVF2.potato ,lVF2.banana ,lVF2.greens ,lVF2.carrot ,
    lVF2.tomato , lG2.rice, lG2.beans,   lG2.biscuits, 
    lG2.Cflour,   lG2.Wbread,   lG2.Pastries, lG2.Tortillas, lG2.WFlour,    lM2.sugar  ,lM2.coffee ,lM2.soda  , 
    lM2.CupN   ,lM2.VOil   ,lM2.Alcohol,lM2.BCereal), 
  c(pa.chick,     pa.BPork,     pa.eggs ,     pa.milk ,     pa.fish ,     pa.tuna ,     pa.lard ,     pVF.onion ,
    pVF.limes ,     pVF.apples ,     pVF.oranges,     pVF.potato ,     pVF.banana ,     pVF.greens ,     pVF.carrots, 
    pVF.tomato ,     pG.rice      ,     pG.beans     ,     pG.biscuits  ,     pG.Cflour    ,     pG.Wbread    ,   
    pG.Pastries  ,     pG.Tortillas ,     pG.WFlour   ,     pM.sugar  ,     pM.coffee ,     pM.soda   ,     pM.CupN   , 
    pM.VOil   ,     pM.Alcohol,     pM.BCereal), 
  c(pa2.chick,     pa2.BPork,     pa2.eggs ,     pa2.milk ,     pa2.fish ,     pa2.tuna ,     pa2.lard ,     pVF2.onion ,
    pVF2.limes ,     pVF2.apples ,     pVF2.oranges,     pVF2.potato ,     pVF2.banana ,     pVF2.greens ,     pVF2.carrots, 
    pVF2.tomato ,     pG2.rice      ,     pG2.beans     ,     pG2.biscuits  , pG2.Cflour,     pG2.Wbread    ,   
    pG2.Pastries  ,     pG2.Tortillas ,   pG2.WFlour ,     pM2.sugar  ,     pM2.coffee ,     pM2.soda   ,     pM2.CupN   , 
    pM2.VOil   ,     pM2.Alcohol,     pM2.BCereal))
  )
   

colnames(results.df) <- c("Names", "LPM", "Squared", "Poi", "Cross")

results.df$LPM <- as.numeric(as.character(results.df$LPM))
results.df$Squared <- as.numeric(as.character(results.df$Squared))
results.df$Poi <- as.numeric(as.character(results.df$Poi))
results.df$Cross <- as.numeric(as.character(results.df$Cross))

results.df <- as.tibble(results.df)
results.df

# Chapter 8: Constructing the Results Figures #### 
boots1 <- read.csv("C:/Users/mjklein2/Desktop/toot/Programming_Directory/Bootstrap_Results1000_03_07_19.csv")
boots2 <- read.csv("C:/Users/mjklein2/Desktop/toot/Programming_Directory/Bootstrap_Results_03_07_19.csv") 
boots <- bind_rows(boots1, boots2)


boots.lpm <- boots[,c("LPM.Marginal.Chicken", "LPM.Marginal.BeefPork", "LPM.Marginal.Eggs" ,  "LPM.Marginal.Milk" ,
                      "LPM.Marginal.Fish" ,  "LPM.Marginal.Tuna" , "LPM.Marginal.Lard"  , 
                      # VegFruits  
                      "LPM.Marginal.Onion", "LPM.Marginal.Lime", "LPM.Marginal.Apple" ,"LPM.Marginal.Orange"  ,
                      "LPM.Marginal.Potato" , "LPM.Marginal.Banana", "LPM.Marginal.Greens" , "LPM.Marginal.Carrots" ,
                      "LPM.Marginal.Tomato"  ,
                      # Grains
                      "LPM.Marginal.Rice"     , "LPM.Marginal.Beans"  , "LPM.Marginal.Biscuits", "LPM.Marginal.CFlour"   ,  
                      "LPM.Marginal.WBread"   ,"LPM.Marginal.Pastries", "LPM.Marginal.Tortillas","LPM.Marginal.WFlour"  ,
                      # Other
                      "LPM.Marginal.Sugar"  ,  "LPM.Marginal.Coffee"  , "LPM.Marginal.Soda",  "LPM.Marginal.CNoodles" ,
                      "LPM.Marginal.VOil"   ,  "LPM.Marginal.Alcohol" ,   "LPM.Marginal.BCereal"  )]

boots.poisson <- boots[,c("Poisson.Marginal.Chicken", "Poisson.Marginal.BeefPork", "Poisson.Marginal.Eggs" ,  "Poisson.Marginal.Milk" ,
                      "Poisson.Marginal.Fish" ,  "Poisson.Marginal.Tuna" , "Poisson.Marginal.Lard"  , 
                      # VegFruits  
                      "Poisson.Marginal.Onion", "Poisson.Marginal.Lime", "Poisson.Marginal.Apple" ,"Poisson.Marginal.Orange"  ,
                      "Poisson.Marginal.Potato" , "Poisson.Marginal.Banana", "Poisson.Marginal.Greens" , "Poisson.Marginal.Carrots" ,
                      "Poisson.Marginal.Tomato"  ,
                      # Grains
                      "Poisson.Marginal.Rice"     , "Poisson.Marginal.Beans"  , "Poisson.Marginal.Biscuits", "Poisson.Marginal.CFlour"   ,  
                      "Poisson.Marginal.WBread"   ,"Poisson.Marginal.Pastries", "Poisson.Marginal.Tortillas","Poisson.Marginal.WFlour"  ,
                      # Other
                      "Poisson.Marginal.Sugar"  ,  "Poisson.Marginal.Coffee"  , "Poisson.Marginal.Soda",  "Poisson.Marginal.CNoodles" ,
                      "Poisson.Marginal.VOil"   ,  "Poisson.Marginal.Alcohol" ,   "Poisson.Marginal.BCereal"  )]

boots.BP2 <- boots[,c("LPM.BP2.Chicken", "LPM.BP2.BeefPork", "LPM.BP2.Eggs" ,  "LPM.BP2.Milk" ,
                          "LPM.BP2.Fish" ,  "LPM.BP2.Tuna" , "LPM.BP2.Lard"  , 
                          # VegFruits  
                          "LPM.BP2.Onion", "LPM.BP2.Lime", "LPM.BP2.Apple" ,"LPM.BP2.Orange"  ,
                          "LPM.BP2.Potato" , "LPM.BP2.Banana", "LPM.BP2.Greens" , "LPM.BP2.Carrots" ,
                          "LPM.BP2.Tomato"  ,
                          # Grains
                          "LPM.BP2.Rice"     , "LPM.BP2.Beans"  , "LPM.BP2.Biscuits", "LPM.BP2.CFlour"   ,  
                          "LPM.BP2.WBread"   ,"LPM.BP2.Pastries", "LPM.BP2.Tortillas","LPM.BP2.WFlour"  ,
                          # Other
                          "LPM.BP2.Sugar"  ,  "LPM.BP2.Coffee"  , "LPM.BP2.Soda",  "LPM.BP2.CNoodles" ,
                          "LPM.BP2.VOil"   ,  "LPM.BP2.Alcohol" ,   "LPM.BP2.BCereal"  )]

boots.Cross <- boots[,c("Cross.Partial.Chicken", "Cross.Partial.BeefPork", "Cross.Partial.Eggs" ,  "Cross.Partial.Milk" ,
                          "Cross.Partial.Fish" ,  "Cross.Partial.Tuna" , "Cross.Partial.Lard"  , 
                          # VegFruits  
                          "Cross.Partial.Onion", "Cross.Partial.Lime", "Cross.Partial.Apple" ,"Cross.Partial.Orange"  ,
                          "Cross.Partial.Potato" , "Cross.Partial.Banana", "Cross.Partial.Greens" , "Cross.Partial.Carrots" ,
                          "Cross.Partial.Tomato"  ,
                          # Grains
                          "Cross.Partial.Rice"     , "Cross.Partial.Beans"  , "Cross.Partial.Biscuits", "Cross.Partial.CFlour"   ,  
                          "Cross.Partial.WBread"   ,"Cross.Partial.Pastries", "Cross.Partial.Tortillas","Cross.Partial.WFlour"  ,
                          # Other
                          "Cross.Partial.Sugar"  ,  "Cross.Partial.Coffee"  , "Cross.Partial.Soda",  "Cross.Partial.CNoodles" ,
                          "Cross.Partial.VOil"   ,  "Cross.Partial.Alcohol" ,   "Cross.Partial.BCereal"  )]
boots.RHS <- boots[,c("RHS.Chicken", "RHS.BeefPork", "RHS.Eggs" ,  "RHS.Milk" ,
                        "RHS.Fish" ,  "RHS.Tuna" , "RHS.Lard"  , 
                        # VegFruits  
                        "RHS.Onion", "RHS.Lime", "RHS.Apple" ,"RHS.Orange"  ,
                        "RHS.Potato" , "RHS.Banana", "RHS.Greens" , "RHS.Carrots" ,
                        "RHS.Tomato"  ,
                        # Grains
                        "RHS.Rice"     , "RHS.Beans"  , "RHS.Biscuits", "RHS.CFlour"   ,  
                        "RHS.WBread"   ,"RHS.Pastries", "RHS.Tortillas","RHS.WFlour"  ,
                        # Other
                        "RHS.Sugar"  ,  "RHS.Coffee"  , "RHS.Soda",  "RHS.CNoodles" ,
                        "RHS.VOil"   ,  "RHS.Alcohol" ,   "RHS.BCereal"  )]


results.df$lpm.low <- c(as.numeric(lapply(boots.lpm, FUN = function(x) quantile(x, 0.025))))
results.df$lpm.high <-c(as.numeric(lapply(boots.lpm, FUN = function(x) quantile(x, 0.975))))

results.df$poi.low <- c(as.numeric(map(.x = boots.poisson, .f = function(x) quantile(x, 0.025))))
results.df$poi.high <- c(as.numeric(map(.x = boots.poisson, .f = function(x) quantile(x, 0.975))))

results.df$sq.low <- c(as.numeric(map(.x = boots.BP2[501:1000,], .f = function(x) quantile(x, 0.025))))
results.df$sq.high <- c(as.numeric(map(.x = boots.BP2[501:1000,], .f = function(x) quantile(x, 0.975))))

results.df$c.low <- c(as.numeric(map(.x = boots.Cross, .f = function(x) quantile(x, 0.025))))
results.df$c.high <- c(as.numeric(map(.x = boots.Cross, .f = function(x) quantile(x, 0.975))))

results.df$rhs.low <- c(as.numeric(map(.x = boots.RHS, .f = function(x) quantile(x, 0.025))))
results.df$rhs.high <- c(as.numeric(map(.x = boots.RHS, .f = function(x) quantile(x, 0.975))))


results.df <- results.df %>% 
  mutate(rhs = c(la.chick * DiD, # Animal Products
                 la.BPork * DiD, la.eggs * DiD, la.milk * DiD, la.fish * DiD, la.tuna * DiD, la.lard * DiD,  
                 lVF.onion  * DiD, #Veg and Fruits
                 lVF.limes  * DiD, lVF.apples * DiD,
                 lVF.orange * DiD, lVF.potato * DiD,
                 lVF.banana * DiD, lVF.greens * DiD,
                 lVF.carrot * DiD, lVF.tomato * DiD,
                 lG.rice      * DiD, lG.beans     * DiD,
                 lG.biscuits  * DiD, lG.Cflour    * DiD,
                 lG.Wbread    * DiD, lG.Pastries  * DiD,
                 lG.Tortillas * DiD, lG.WFlour    * DiD,
                 lM.sugar  * DiD, lM.coffee * DiD,
                 lM.soda   * DiD, lM.CupN   * DiD,
                 lM.VOil   * DiD, lM.Alcohol* DiD,
                 lM.BCereal* DiD))


results.df
results.df$Names <- as.character(results.df$Names)


results.df[c(1:7),] <- results.df[c(1, 2, 4, 3, 7, 5, 6),] # reorder to make the graph look nice
results.df[c(8:16),] <- results.df[c(12, 13, 11, 10, 14, 9, 15, 16, 8),] # reorder to make the graph look nice
results.df[c(17:24),] <- results.df[c(20, 22, 21, 17, 24, 18, 23, 19),] # reorder to make the graph look nice
results.df[c(25:31),] <- results.df[c(27, 28, 26, 31, 30, 25, 29),] # reorder to make the graph look nice

results.df <- results.df[c(31:25, 24:17, 16:8, 7:1),]

library(cowplot)
library(grid)
library(gridExtra)

results.df$Significant <- ifelse(results.df$lpm.low > 0, 1, 0)
results.df$Significant <- factor(ifelse(results.df$lpm.high < 0, 1, 
                                        results.df$Significant))
levels(results.df$Significant) <- c("No", "Yes")

panel_1 <- ggplot(data = results.df) +
  geom_point(mapping = aes(x = Names, y = LPM)) + 
  geom_errorbar(mapping = aes(x = Names,
                              y = LPM,
                              ymin=lpm.low,
                              ymax=lpm.high, 
                              color = Significant)) + 
 scale_x_discrete(limits = results.df$Names,
                   labels = results.df$Names) +
  geom_hline(yintercept=0, linetype = "dashed") +
  geom_vline(xintercept=7.5, linetype = "dashed") +
  geom_vline(xintercept=15.5, linetype = "dashed") +
  geom_vline(xintercept=24.5, linetype = "dashed") +
  theme(axis.ticks = element_blank(), 
     # axis.text.y = element_blank(),
      axis.title.y = element_blank(), 
     axis.title.x = element_blank(),
      legend.position="none") +
  labs(title = "Linear Probability Model Results", y = " ")  + 
  coord_flip()

panel_1
  
results.df$Significant <- ifelse(results.df$poi.low > 0, 1, 0)
results.df$Significant <- factor(ifelse(results.df$poi.high < 0, 1, 
                                        results.df$Significant))
levels(results.df$Significant) <- c("No", "Yes")

panel_2 <- ggplot(data = results.df) + 
  geom_point(mapping = aes(x = Names, y = Poi)) + 
  geom_errorbar(mapping = aes(x = Names, y = Poi, ymin=poi.low, ymax=poi.high,
                              color = Significant)) + 
  scale_x_discrete(limits = results.df$Names,
                   labels = results.df$Names) +
    # scale_x_discrete(limits = results.df$Names,
  #                  labels = c("A1", "A2", "A3", 
  #                             "A4", "A5", "A6", 
  #                             "A7", "FV1", "FV2",
  #                             "FV3", "FV4", "FV5", "FV6", "FV7", 
  #                             "FV8", "FV9",
  #                             "G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", 
  #                             "O1", "O2", "O3", "O4", "O5", "O6", "O7")) +
  geom_hline(yintercept=0, linetype = "dashed") +
  geom_vline(xintercept=7.5, linetype = "dashed") +
  geom_vline(xintercept=15.5, linetype = "dashed") +
  geom_vline(xintercept=24.5, linetype = "dashed") +
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(), 
        legend.position = "none") + 
  labs(title = "Count Data Model Results", 
       y = "Point Estimate and CI")  + coord_flip()  

panel_2

results.df$Significant <- ifelse(results.df$sq.low > 0, 1, 0)
results.df$Significant <- factor(ifelse(results.df$sq.high < 0, 1, 
                                        results.df$Significant))
levels(results.df$Significant) <- c("No", "Yes")

panel_3 <- ggplot(data = results.df) + 
  geom_point(mapping = aes(x = Names, y = Squared)) + 
  geom_errorbar(mapping = aes(x = Names, y = Squared, ymin=sq.low, ymax=sq.high, 
                              color = Significant)) + 
 # scale_x_discrete(limits = results.df$Names,
#                   labels = results.df$Names) +
  # scale_x_discrete(limits = results.df$Names,
  #                  labels = c("A1", "A2", "A3", 
  #                             "A4", "A5", "A6", 
  #                             "A7", "FV1", "FV2",
  #                             "FV3", "FV4", "FV5", "FV6", "FV7", 
  #                             "FV8", "FV9",
  #                             "G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", 
  #                             "O1", "O2", "O3", "O4", "O5", "O6", "O7")) +
  geom_hline(yintercept=0, linetype = "dashed") +
  geom_vline(xintercept=7.5, linetype = "dashed") +
  geom_vline(xintercept=15.5, linetype = "dashed") +
  geom_vline(xintercept=24.5, linetype = "dashed") +
  theme(#axis.text.x = element_text(color="navy", size=12, angle=90), 
        axis.ticks = element_blank(), 
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position="none") + 
  labs(title = "Power Squared Results", y = " ")  + 
  coord_flip()

panel_3


results.df$Significant <- ifelse(results.df$c.low > 0, 1, 0)
results.df$Significant <- factor(ifelse(results.df$c.high < 0, 1, 
                                        results.df$Significant))
levels(results.df$Significant) <- c("No", "Yes")

panel_4 <- ggplot(data = results.df) + geom_point(mapping = aes(x = Names, y = Cross)) + 
  geom_errorbar(mapping = aes(x = Names, y = Cross, ymin=c.low, ymax=c.high, 
                              color = Significant)) + 
  scale_x_discrete(limits = results.df$Names,
                   labels = results.df$Names) +
  geom_hline(yintercept=0, linetype = "dashed") +
  geom_vline(xintercept=7.5, linetype = "dashed") +
  geom_vline(xintercept=15.5, linetype = "dashed") +
  geom_vline(xintercept=24.5, linetype = "dashed") +
  theme(axis.text.y = element_blank(), # element_text(color="navy", size=12, angle=90), 
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
         axis.ticks = element_blank()) + 
  labs(title = "Cross Partials", y = "Point Estimate and CI")  +
  coord_flip()

panel_4

#cowplot::plot_grid(panel_1, panel_2, panel_4 ,labels = c("A", "C", "D"))

p <- cowplot::plot_grid(panel_1, panel_2, panel_3, panel_4, 
                   align = "h", #     labels = c("H1", "H2", "H3", "H4"), 
                   nrow=1)

#create common x label
x.grob <- textGrob("Point Estimates and Confidence Intervals", 
                   gp=gpar(fontface="bold", col="black", fontsize=15))


grid.arrange(arrangeGrob(p, bottom = x.grob))

# Check that the right confidence intervals are paired with the right foods. 
colnames(select(boots, starts_with("LPM")))
colnames(select(boots, starts_with("Poisson")))


# put a tiger on it: 
library(magick)
library(cowplot)
ggdraw() +
  draw_image("http://jeroen.github.io/images/tiger.svg") +
  draw_plot(panel_1)

ggdraw() +
  draw_image("http://jeroen.github.io/images/tiger.svg") +
  draw_plot(p)

# Put my face on it:
library(magick)
library(cowplot)
ggdraw() +
  draw_image("https://pbs.twimg.com/profile_images/1000060649421004800/ZzEbv1o__400x400.jpg") +
  draw_plot(panel_1)


ggdraw() +
  draw_image("https://pbs.twimg.com/profile_images/1000060649421004800/ZzEbv1o__400x400.jpg") +
  draw_plot(panel_1)

# haha

# Chapter 9: Right hand side calculations for equation 5 + Table ####

sig.diffs <-
  tibble('Names' = food_var_names, 
         'Sig' = (abs(significant.difference) > 1.96))

sig.diffs <- sig.diffs[sig.diffs$Sig == TRUE,]

food_var_names <- sig.diffs$Names
  
means_99_t <- map_dbl(.x = final.df[,food_var_names][final.df$wavenumber == 2 & final.df$treatment_household == 1,],
                      .f = function(x){100*round(mean(x, na.rm=T), 3)})

means_99_c <- map_dbl(.x = final.df[,food_var_names][final.df$wavenumber == 2 & final.df$treatment_household == 0,],
                      .f = function(x){100*round(mean(x, na.rm=T), 3)})

diff <- means_99_t - means_99_c

results.explain.df <- tibble(
  'Names' = food_var_names,
  '1999 Treatment' = means_99_t,
  '1999 Control' = means_99_c, 
  'Difference' = diff,
  'test' = 
c(unname(t.test(# Chicken
            x = final.df[,food_var_names[1]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
            y = final.df[,food_var_names[1]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat),
unname(t.test( # Beef Pork
  x = final.df[,food_var_names[2]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[2]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat),
unname(t.test( # Eggs
  x = final.df[,food_var_names[3]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[3]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat),
unname(t.test( # Milk
  x = final.df[,food_var_names[4]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[4]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat),
unname(t.test( # Potatoes
  x = final.df[,food_var_names[5]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[5]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat),
unname(t.test( # Chicken
  x = final.df[,food_var_names[6]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[6]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat),
unname(t.test( # Chicken
  x = final.df[,food_var_names[7]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[7]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat),
unname(t.test( # Chicken
  x = final.df[,food_var_names[8]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[8]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat),
unname(t.test( # Chicken
  x = final.df[,food_var_names[9]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[9]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat),
unname(t.test( # Chicken
  x = final.df[,food_var_names[10]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[10]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat),
unname(t.test( # Chicken
  x = final.df[,food_var_names[11]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[11]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat),
unname(t.test( # Chicken
  x = final.df[,food_var_names[12]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[12]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat),
unname(t.test( # Chicken
  x = final.df[,food_var_names[13]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[13]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat),
unname(t.test( # Chicken
  x = final.df[,food_var_names[14]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[14]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat),
unname(t.test( # Chicken
  x = final.df[,food_var_names[15]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[15]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat),
unname(t.test( # Chicken
  x = final.df[,food_var_names[16]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[16]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat),
unname(t.test( # Chicken
  x = final.df[,food_var_names[17]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[17]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat),
unname(t.test( # Chicken
  x = final.df[,food_var_names[18]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[18]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat),
unname(t.test( # Chicken
  x = final.df[,food_var_names[19]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[19]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat),
unname(t.test( # Chicken
  x = final.df[,food_var_names[20]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[20]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat),
unname(t.test( # Chicken
  x = final.df[,food_var_names[21]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[21]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat),
unname(t.test( # Chicken
  x = final.df[,food_var_names[22]][final.df$wavenumber == 2 & final.df$treatment_household == 1], 
  y = final.df[,food_var_names[22]][final.df$wavenumber == 2 & final.df$treatment_household == 0])$stat)),
'RHS' = c(lVF.tomato*DiD*100, 
          0, # onion
          lVF.potato*DiD*100,
          lVF.orange*DiD*100,
          lVF.banana*DiD*100,
          lVF.apples*DiD*100,
          0, # Limes
          0, # tortilla, 
          0, #Corn Flour, 
          lG.Wbread*DiD*100,
          0, # Pan de Dulce,
          0, # Wheat Flour, 
          lM.CupN*DiD*100, 
          0, #RICE
          lG.biscuits*DiD*100, 
          la.chick*DiD*100, # Multiple by 100 so it's in Percent Terms
          la.BPork *DiD*100,
          0, # Beef or pork
          la.eggs*DiD*100,
          0, # Lard
          0, #soda
          0), # coffee
'% Explained By Power'  = (RHS / Difference)*100)

results.explain.df[,2:7] <- round(results.explain.df[,2:7], 3)
results.explain.df

stargazer(results.explain.df, summary = FALSE)

# Plots

results.df$Significant <- ifelse(results.df$rhs.low > 0, 1, 0)
results.df$Significant <- factor(ifelse(results.df$rhs.high < 0, 1, 
                                        results.df$Significant))
levels(results.df$Significant) <- c("No", "Yes")

panel_5 <- ggplot(data = results.df) +
  geom_point(mapping = aes(x = Names, y = rhs)) + 
  geom_errorbar(mapping = aes(x = Names,
                              y = rhs,
                              ymin=rhs.low,
                              ymax=rhs.high, 
                              color = Significant)) +
  scale_x_discrete(limits = results.df$Names,
                   labels = results.df$Names) +
  geom_hline(yintercept=0, linetype = "dashed") +
  geom_vline(xintercept=7.5, linetype = "dashed") +
  geom_vline(xintercept=15.5, linetype = "dashed") +
  geom_vline(xintercept=24.5, linetype = "dashed") +
  theme(axis.ticks = element_blank(), 
        # axis.text.y = element_blank(),
        axis.title.y = element_blank(), 
        axis.title.x = element_blank()) +
       # legend.position="none") +
  labs(title = "RHS Estimates", y = " ")  + 
  coord_flip()

panel_5

# Chapter 10: Market Earnings Breakdown Table ####

# Step 0: Verify that the primary employment and the other income source variables are the same 
#         across waves
# step 1: recitfy differences between the other income source variables. 
#         They added a few options in the later two waves
#         There are pretty big differences in the number system, gotta sort em
# Step 2: use the count function to get a count for each

sample.analog$corrected_other_income_source_1 <- sample.analog$other_income_source
sample.analog$corrected_other_income_source_1[sample.analog$wavenumber == 2  &
                                                sample.analog$other_income_source == 6] <- 15
sample.analog$corrected_other_income_source_1[sample.analog$wavenumber == 2 &
                                                sample.analog$other_income_source == 7] <- 16
sample.analog$corrected_other_income_source_1[sample.analog$wavenumber == 2  &
                                                sample.analog$other_income_source == 8] <- 6
sample.analog$corrected_other_income_source_1[sample.analog$wavenumber == 2 &
                                                sample.analog$other_income_source == 9] <- 7
sample.analog$corrected_other_income_source_1[sample.analog$wavenumber == 2 &
                                                sample.analog$other_income_source == 10] <- 8
sample.analog$corrected_other_income_source_1[sample.analog$wavenumber == 2 &
                                                sample.analog$other_income_source == 11] <- 9
sample.analog$corrected_other_income_source_1[sample.analog$wavenumber == 2 &
                                                sample.analog$other_income_source == 12] <- 10
sample.analog$corrected_other_income_source_1[sample.analog$wavenumber == 2 &
                                                sample.analog$other_income_source == 13] <- 11

sample.analog$corrected_other_income_source_1[sample.analog$wavenumber == 3 & 
                                                sample.analog$other_income_source == 1] <- 17
sample.analog$corrected_other_income_source_1[sample.analog$wavenumber == 3 & 
                                                sample.analog$other_income_source == 2] <- 1
sample.analog$corrected_other_income_source_1[sample.analog$wavenumber == 3 & 
                                                sample.analog$other_income_source == 3] <- 2
sample.analog$corrected_other_income_source_1[sample.analog$wavenumber == 3 & 
                                                sample.analog$other_income_source == 4] <- 3
sample.analog$corrected_other_income_source_1[sample.analog$wavenumber == 3 & 
                                                sample.analog$other_income_source == 5] <- 4
sample.analog$corrected_other_income_source_1[sample.analog$wavenumber == 3 & 
                                                sample.analog$other_income_source == 6] <- 5
sample.analog$corrected_other_income_source_1[sample.analog$wavenumber == 3 & 
                                                sample.analog$other_income_source == 7] <- 15
sample.analog$corrected_other_income_source_1[sample.analog$wavenumber == 3 & 
                                                sample.analog$other_income_source == 8] <- 16
sample.analog$corrected_other_income_source_1[sample.analog$wavenumber == 3 & 
                                                sample.analog$other_income_source == 9] <- 6
sample.analog$corrected_other_income_source_1[sample.analog$wavenumber == 3 & 
                                                sample.analog$other_income_source == 10] <- 7
sample.analog$corrected_other_income_source_1[sample.analog$wavenumber == 3 & 
                                                sample.analog$other_income_source == 11] <- 8
sample.analog$corrected_other_income_source_1[sample.analog$wavenumber == 3 & 
                                                sample.analog$other_income_source == 12] <- 9
sample.analog$corrected_other_income_source_1[sample.analog$wavenumber == 3 & 
                                                sample.analog$other_income_source == 13] <- 10
sample.analog$corrected_other_income_source_1[sample.analog$wavenumber == 3 & 
                                                sample.analog$other_income_source == 14] <- 11

#Visualize
table(sample.analog$corrected_other_income_source_1, sample.analog$sex, sample.analog$wavenumber)

# Repeat for the tertiary income source: 
sample.analog$corrected_other_income_source_2 <- sample.analog$other_income_source2
sample.analog$corrected_other_income_source_2[sample.analog$wavenumber == 2  &
                                                sample.analog$other_income_source2 == 6] <- 15
sample.analog$corrected_other_income_source_2[sample.analog$wavenumber == 2 &
                                                sample.analog$other_income_source2 == 7] <- 16
sample.analog$corrected_other_income_source_2[sample.analog$wavenumber == 2  &
                                                sample.analog$other_income_source2 == 8] <- 6
sample.analog$corrected_other_income_source_2[sample.analog$wavenumber == 2 &
                                                sample.analog$other_income_source2 == 9] <- 7
sample.analog$corrected_other_income_source_2[sample.analog$wavenumber == 2 &
                                                sample.analog$other_income_source2 == 10] <- 8
sample.analog$corrected_other_income_source_2[sample.analog$wavenumber == 2 &
                                                sample.analog$other_income_source2 == 11] <- 9
sample.analog$corrected_other_income_source_2[sample.analog$wavenumber == 2 &
                                                sample.analog$other_income_source2 == 12] <- 10
sample.analog$corrected_other_income_source_2[sample.analog$wavenumber == 2 &
                                                sample.analog$other_income_source2 == 13] <- 11

sample.analog$corrected_other_income_source_2[sample.analog$wavenumber == 3 & 
                                                sample.analog$other_income_source2 == 1] <- 17
sample.analog$corrected_other_income_source_2[sample.analog$wavenumber == 3 & 
                                                sample.analog$other_income_source2 == 2] <- 1
sample.analog$corrected_other_income_source_2[sample.analog$wavenumber == 3 & 
                                                sample.analog$other_income_source2 == 3] <- 2
sample.analog$corrected_other_income_source_2[sample.analog$wavenumber == 3 & 
                                                sample.analog$other_income_source2 == 4] <- 3
sample.analog$corrected_other_income_source_2[sample.analog$wavenumber == 3 & 
                                                sample.analog$other_income_source2 == 5] <- 4
sample.analog$corrected_other_income_source_2[sample.analog$wavenumber == 3 & 
                                                sample.analog$other_income_source2 == 6] <- 5
sample.analog$corrected_other_income_source_2[sample.analog$wavenumber == 3 & 
                                                sample.analog$other_income_source2 == 7] <- 15
sample.analog$corrected_other_income_source_2[sample.analog$wavenumber == 3 & 
                                                sample.analog$other_income_source2 == 8] <- 16
sample.analog$corrected_other_income_source_2[sample.analog$wavenumber == 3 & 
                                                sample.analog$other_income_source2 == 9] <- 6
sample.analog$corrected_other_income_source_2[sample.analog$wavenumber == 3 & 
                                                sample.analog$other_income_source2 == 10] <- 7
sample.analog$corrected_other_income_source_2[sample.analog$wavenumber == 3 & 
                                                sample.analog$other_income_source2 == 11] <- 8
sample.analog$corrected_other_income_source_2[sample.analog$wavenumber == 3 & 
                                                sample.analog$other_income_source2 == 12] <- 9
sample.analog$corrected_other_income_source_2[sample.analog$wavenumber == 3 & 
                                                sample.analog$other_income_source2 == 13] <- 10
sample.analog$corrected_other_income_source_2[sample.analog$wavenumber == 3 & 
                                                sample.analog$other_income_source2 == 14] <- 11


employment_and_sex_97.df <-  sample.analog %>%
  filter(age %in% c(16:70), wavenumber == 1) %>%
  group_by(sex) %>% 
  count(primary_employment) %>%
  spread(key = sex, value = n)
colnames(employment_and_sex_97.df) <- c("Type", "Men", "Women")
employment_and_sex_97.df$wavenumber <- 1

# Make it a percent
employment_and_sex_97.df$Men <- employment_and_sex_97.df$Men / sum(employment_and_sex_97.df$Men)*100
employment_and_sex_97.df$Women <- employment_and_sex_97.df$Women / sum(employment_and_sex_97.df$Women)*100

# Repeat for Wave 2
employment_and_sex_99.df <-  sample.analog %>%
  filter(age %in% c(16:70), wavenumber == 2) %>%
  group_by(sex) %>% 
  count(primary_employment) %>%
  spread(key = sex, value = n)
colnames(employment_and_sex_99.df) <- c("Type", "Men", "Women")
employment_and_sex_99.df$wavenumber <- 2
employment_and_sex_99.df$Type[is.na(employment_and_sex_99.df$Type)] <- "Unemployed"

# Make it a percent
employment_and_sex_99.df$Men <- employment_and_sex_99.df$Men / sum(employment_and_sex_99.df$Men)*100
employment_and_sex_99.df$Women <- employment_and_sex_99.df$Women / sum(employment_and_sex_99.df$Women, na.rm=T)*100

# Repeat for Wave 3
employment_and_sex_00.df <-  sample.analog %>%
  filter(age %in% c(16:70), wavenumber == 3) %>%
  group_by(sex) %>% 
  count(primary_employment) %>% 
  spread(key = sex, value = n)
colnames(employment_and_sex_00.df) <- c("Type", "Men", "Women")
employment_and_sex_00.df$wavenumber <- 3

# Make it a percent
employment_and_sex_00.df$Men <- employment_and_sex_00.df$Men / sum(employment_and_sex_00.df$Men)*100
employment_and_sex_00.df$Women <- employment_and_sex_00.df$Women / sum(employment_and_sex_00.df$Women, na.rm=T)*100


employment_and_sex_97.df$Type <- c("Ejiditario", "Ag. Laborer", "Cooperative Member", "NR3", "Non-Ag. Laborer", "Other", 
                                "Manager", "Family Work (No Pay)", "Entreprenuer", "Work (No Pay)", "Unemployed" )

employment_and_sex_99.df$Type <- c("Ejiditario", "Ag. Laborer", "Cooperative Member", "NR3", "Non-Ag. Laborer", "Other", 
                                   "Manager", "Family Work (No Pay)", "Entreprenuer", "Work (No Pay)", "Unemployed" )


employment_and_sex_00.df$Type <- c("Ejiditario", "Ag. Laborer", "Cooperative Member", "NR3", "Non-Ag. Laborer", "Other", 
                                   "Manager", "Family Work (No Pay)", "Entreprenuer", "Work (No Pay)", "Unemployed" )

employment_and_sex.df <- bind_rows(employment_and_sex_97.df, employment_and_sex_99.df, employment_and_sex_00.df)
employment_and_sex.df$Men <- round(employment_and_sex.df$Men, digits = 1)
employment_and_sex.df$Women <- round(employment_and_sex.df$Women, digits = 1)

stargazer::stargazer(employment_and_sex.df, summary=F)

# Count the number of men and women ages 16-70 in each wave
filter(hh.df, age %in% c(16:70)) %>% count(sex, wavenumber)

#Table_2_Panel_A_Means <- filter(hh.df, age %in% c(16:70)) %>% summarize(ag_wages_men_1997 = mean(income_sources)
#                                                                        )

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# -- Other Employment 
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

other_employment_1_and_sex.df <-  sample.analog %>% 
  filter(age %in% c(16:70)) %>%
  group_by(sex) %>% 
  count(corrected_other_income_source_1, wavenumber) %>% 
  spread(key = sex, value = n) 

colnames(other_employment_1_and_sex.df) <- c("Type", "wavenumber", "Men", "Women")

# Numbers: 22634, 22807, 22404, 22094, 21126, 21584

# Convert to Percentages
other_employment_1_and_sex.df$Men[other_employment_1_and_sex.df$wavenumber == 1] <- 100*
  round(other_employment_1_and_sex.df$Men[other_employment_1_and_sex.df$wavenumber == 1] / 
  22634, digits = 3)
other_employment_1_and_sex.df$Men[other_employment_1_and_sex.df$wavenumber == 2] <- 100*
  round(other_employment_1_and_sex.df$Men[other_employment_1_and_sex.df$wavenumber == 2] / 
          22404, digits = 3)
other_employment_1_and_sex.df$Men[other_employment_1_and_sex.df$wavenumber == 3] <- 100*
  round(other_employment_1_and_sex.df$Men[other_employment_1_and_sex.df$wavenumber == 3] / 
          21126, digits = 3)

other_employment_1_and_sex.df$Women[other_employment_1_and_sex.df$wavenumber == 1] <- 100*
  round(other_employment_1_and_sex.df$Women[other_employment_1_and_sex.df$wavenumber == 1] / 
          22807, digits = 3)
other_employment_1_and_sex.df$Women[other_employment_1_and_sex.df$wavenumber == 2] <- 100*
  round(other_employment_1_and_sex.df$Women[other_employment_1_and_sex.df$wavenumber == 2] / 
          22094, digits = 3)
other_employment_1_and_sex.df$Women[other_employment_1_and_sex.df$wavenumber == 3] <- 100*
  round(other_employment_1_and_sex.df$Women[other_employment_1_and_sex.df$wavenumber == 3] / 
          21584, digits = 3)

View(other_employment_1_and_sex.df)

other_employment_2_and_sex.df <-  sample.analog %>% 
  filter(age %in% c(16:70)) %>%
  group_by(sex) %>% 
  count(corrected_other_income_source_2, wavenumber) %>% 
  spread(key = sex, value = n) 

colnames(other_employment_2_and_sex.df) <- c("Type", "wavenumber", "Men", "Women")

# Convert to Percentages
other_employment_2_and_sex.df$Men[other_employment_2_and_sex.df$wavenumber == 1] <- 100*
  round(other_employment_2_and_sex.df$Men[other_employment_2_and_sex.df$wavenumber == 1] / 
          22634, digits = 3)
other_employment_2_and_sex.df$Men[other_employment_2_and_sex.df$wavenumber == 2] <- 100*
  round(other_employment_2_and_sex.df$Men[other_employment_2_and_sex.df$wavenumber == 2] / 
          22404, digits = 3)
other_employment_2_and_sex.df$Men[other_employment_2_and_sex.df$wavenumber == 3] <- 100*
  round(other_employment_2_and_sex.df$Men[other_employment_2_and_sex.df$wavenumber == 3] / 
          21126, digits = 3)

other_employment_2_and_sex.df$Women[other_employment_2_and_sex.df$wavenumber == 1] <- 100*
  round(other_employment_2_and_sex.df$Women[other_employment_2_and_sex.df$wavenumber == 1] / 
          22807, digits = 3)
other_employment_2_and_sex.df$Women[other_employment_2_and_sex.df$wavenumber == 2] <- 100*
  round(other_employment_2_and_sex.df$Women[other_employment_2_and_sex.df$wavenumber == 2] / 
          22094, digits = 3)
other_employment_2_and_sex.df$Women[other_employment_2_and_sex.df$wavenumber == 3] <- 100*
  round(other_employment_2_and_sex.df$Women[other_employment_2_and_sex.df$wavenumber == 3] / 
          21584, digits = 3)

View(other_employment_2_and_sex.df)

other_income_total <- merge(other_employment_1_and_sex.df, other_employment_2_and_sex.df, by = c("Type", "wavenumber") )


other_income_total$Type[other_income_total$Type == 0] <- "NR"
other_income_total$Type[other_income_total$Type == 1] <- "Additional Job"
other_income_total$Type[other_income_total$Type == 2] <- "Pension"
other_income_total$Type[other_income_total$Type == 3] <- "Disability Payment"
other_income_total$Type[other_income_total$Type == 4] <- "Money From Neighbors"
other_income_total$Type[other_income_total$Type == 5] <- "Property Rents"
other_income_total$Type[other_income_total$Type == 6] <- "Procampo"
other_income_total$Type[other_income_total$Type == 7] <- "Scholarship"
other_income_total$Type[other_income_total$Type == 8] <- "Bank Interest"
other_income_total$Type[other_income_total$Type == 9] <- "Sold Products"
other_income_total$Type[other_income_total$Type == 10] <- "Other Transfers"
other_income_total$Type[other_income_total$Type == 11] <- "None"
other_income_total$Type[other_income_total$Type == 15] <- "Gov Credit Program"
other_income_total$Type[other_income_total$Type == 16] <- "Second Entreprenuial Venture"
other_income_total$Type[other_income_total$Type == 99] <- "None"

other_income_total[is.na(other_income_total)] <- 0

other_income_total$Men <- other_income_total$Men.x + other_income_total$Men.y
other_income_total$Women <- other_income_total$Women.x + other_income_total$Women.y

other_income_total <- other_income_total[,!(colnames(other_income_total) %in% c("Men.x", "Men.y", "Women.x", "Women.y"))]

stargazer::stargazer(other_income_total, summary = F, digits = 1)

income_sources <- bind_rows(employment_and_sex.df, other_income_total)

income_sources <- filter(income_sources, !(Type %in% c("NR3", "NA", "NR", "None", "No Response2", "Unemployment")))

stargazer::stargazer(income_sources, summary = F, digits = 2)


# And Progresa:
filter(hh.df, age %in% c(16:70), sex == 1, head_dummy == 1, wavenumber ==2) %>% count(treatment_household)
filter(hh.df, age %in% c(16:70), sex == 1, head_dummy == 1, wavenumber ==3) %>% count(treatment_household)
filter(hh.df, age %in% c(16:70), wavenumber ==2) %>% count(sex)
filter(hh.df, age %in% c(16:70), wavenumber ==3) %>% count(sex)
filter(hh.df, head_dummy ==1,  wavenumber ==2) %>% count(sex)
filter(hh.df, head_dummy ==1,  wavenumber ==3) %>% count(sex)



mean(sample.analog$otherincomeval1[sample.analog$corrected_other_income_source_1 == 6  &
                                     sample.analog$sex == 0 ], na.rm=T)

mean(sample.analog$otherincomeval1[sample.analog$corrected_other_income_source_1 == 6  &
                                     sample.analog$sex == 1 ], na.rm=T)


inflation_1998 <- 18.61/100
inflation_1999 <- 12.32/100
inflation_2000 <- 8.96/100

# Adding the means to Table 2

Table_2_market_fun <- function(wavenum, type){ # A function to generate summary stats for Table 2 in MKBB. 
                           # This just spits out the means for the table to go in parentheses
  a <- round(mean(hh.df$wages[hh.df$primary_employment == type & 
                                hh.df$sex == 0 & hh.df$wavenumber == wavenum],
                  na.rm=T), digits = 2)
  
  b <- round(mean(hh.df$wages[hh.df$primary_employment == type & 
                                hh.df$sex == 1 & hh.df$wavenumber == wavenum],
                  na.rm=T), digits = 2)
  
  if(wavenum == 1){
  print(paste0("MALE ", type, " ", a))
  print(paste0("FEMALE ", type, " ",b ))
  }
  
  if(wavenum == 2){
    print(paste0("MALE ", type, " ", round(a*(1-inflation_1998)*(1-inflation_1999), digits = 2)))
    print(paste0("FEMALE ", type, " ",round(b*(1-inflation_1998)*(1-inflation_1999), digits = 2) ))
  }
  
  if(wavenum == 3) {
    print(paste0("MALE ", type, " ", round(a*(1-inflation_1998)*(1-inflation_1999)*
                                             (1-inflation_2000), digits  = 2)))
    print(paste0("FEMALE ", type, " ",round(b*(1-inflation_1998)*(1-inflation_1999)*
                                              (1-inflation_2000), digits = 2 )))
  }
}


# Use purr to pass all the elements of the list of primary employment options to Table_2_fun 
walk(unique(hh.df$primary_employment), Table_2_market_fun, wavenum=1)
walk(unique(hh.df$primary_employment), Table_2_market_fun, wavenum=2)
walk(unique(hh.df$primary_employment), Table_2_market_fun, wavenum=3)

Table_2_non_market_fun <- function(wavenum, type){ # A function to generate summary stats for Table 2 in MKBB. 
  # This just spits out the means for the table to go in parentheses
  
  if(type ==0) print("NR")
  if(type ==1) print("Additional Job")
  if(type ==2) print("Pension")
  if(type ==3) print("Disability Payment")
  if(type ==4) print("Money from Neighbors")
  if(type ==5) print("Property Rent")
  if(type ==6) print("Procampo")
  if(type ==7) print("Scholarship")
  if(type ==8) print("Bank Interest")
  if(type ==9) print("Sold Products")
  if(type ==10) print("Other Transfer")
  if(type ==11) print("None")
  if(type ==14) print("None")
  if(type == 15) print ("Gov Credit Program")
  if(type == 16) print ("Second Entre Venture")
  
  
  a <- round(mean(sample.analog$otherincomeval[sample.analog$corrected_other_income_source_1 == type &
                                                 sample.analog$otherincomeval > 0 &
                                                 sample.analog$sex == 0 & 
                                                 sample.analog$wavenumber == wavenum], na.rm=T), digits = 2)
  
  b <- round(mean(sample.analog$otherincomeval[sample.analog$corrected_other_income_source_1 == type & 
                                                 sample.analog$otherincomeval > 0 &
                                                 sample.analog$sex == 1 &
                                                 sample.analog$wavenumber == wavenum], na.rm=T), digits = 2)
  
  if(wavenum == 1){
    print(paste0("MALE ", type, " ", a))
    print(paste0("FEMALE ", type, " ",b ))
  }
  
  if(wavenum == 2){
    print(paste0("MALE ", type, " ", round(a*(1-inflation_1998)*(1-inflation_1999), digits = 2)))
    print(paste0("FEMALE ", type, " ",round(b*(1-inflation_1998)*(1-inflation_1999), digits = 2) ))
  }
  
  if(wavenum == 3) {
    print(paste0("MALE ", type, " ", round(a*(1-inflation_1998)*(1-inflation_1999)*
                                             (1-inflation_2000), digits  = 2)))
    print(paste0("FEMALE ", type, " ",round(b*(1-inflation_1998)*(1-inflation_1999)*
                                              (1-inflation_2000), digits = 2 )))
  }
}


# Use purr to pass all the elements of the list of primary employment options to Table_2_fun 
walk(c(1:17), Table_2_non_market_fun, wavenum=1)
walk(c(1:17), Table_2_non_market_fun, wavenum=2)
walk(c(1:17), Table_2_non_market_fun, wavenum=3)



# Chapter 11: Constructing Table 8 - the Point Estimates, Bonferroni Updates, and CIs #####



results.df

results.df$LPM.CI <- paste0("[", round(results.df$lpm.low,3), ", ", round(results.df$lpm.high, 3), "]")
results.df$Poisson.CI <- paste0("[", round(results.df$poi.low,3), ", ", round(results.df$poi.high, 3), "]")

results.df$Significant.LPM <- ifelse(results.df$lpm.low > 0, 1, 0)
results.df$Significant.LPM <- factor(ifelse(results.df$lpm.high < 0, 1, 
                                        results.df$Significant.LPM))
levels(results.df$Significant.LPM) <- c("No", "Yes")

results.df$Significant.Poi <- ifelse(results.df$poi.low > 0, 1, 0)
results.df$Significant.Poi <- factor(ifelse(results.df$poi.high < 0, 1, 
                                        results.df$Significant.Poi))
levels(results.df$Significant.Poi) <- c("No", "Yes")

results.df$LPM <- round(results.df$LPM, 3)
results.df$Poi <- round(results.df$Poi, 3)

results.df <- results.df[c(31:25, 24:17, 16:8, 7:1),]

stargazer::stargazer(select(results.df, Names, LPM, LPM.CI, Poi, Poisson.CI), 
                     summary = FALSE, 
                     title = "Hypothesis 2 Results, The Marginal Effects of Power on Diet")







