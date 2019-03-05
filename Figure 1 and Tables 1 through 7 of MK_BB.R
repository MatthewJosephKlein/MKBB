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
               milk.price_hybrid + egg.price_hybrid + bean.price_hybrid + rice.price_hybrid | folio + wavenumber | 0 | loc_id,
             data = final.df)
  
  return(list(p1$coefficients[1] + 2*p1$coefficients[2]*mean(final.df$BP, na.rm = T),
              2*p1$coefficients[2], 
              p1$coefficients[3], 
              p1$coefficients[4]))
  
}


# (D) Poisson Model
Poisson_ME_Fun_animal <- function(food_name){
  i <- which(colnames(final.df.subset) == food_name)
  p1 <- glmmboot(final.df.subset[,i] ~ BP + BP2 + hh_log_wages +  hh_kids + hh_young_kids + wavenumber +
                   chicken.price_hybrid +
                   beef.price_hybrid + pork.price_hybrid +   
                   lard.price_hybrid + sardines.price_hybrid + tuna.price_hybrid +   
                   milk.price_hybrid + egg.price_hybrid + bean.price_hybrid + rice.price_hybrid, 
                 cluster = folio,
                 data = final.df.subset, family = poisson)
  
  temp <- as.data.frame(cbind(unique(final.df.subset$folio), p1$frail), nrow = 2) 
  colnames(temp) <- c("folio", "frail")
  model.mat <- merge(final.df.subset[,c("folio", "BP", "BP2", "hh_log_wages", "hh_kids", 
                                        "hh_young_kids", "wavenumber",
                                        "chicken.price_hybrid" ,
                                        "beef.price_hybrid" , "pork.price_hybrid" ,    
                                        "lard.price_hybrid" , "sardines.price_hybrid" , "tuna.price_hybrid" ,   
                                        "milk.price_hybrid" , "egg.price_hybrid" , "bean.price_hybrid" , "rice.price_hybrid")], temp, by = c("folio"))
  
  ME <- mean((p1$coefficients[1] + 
                2*p1$coefficients[2]*model.mat$BP) * 
               exp(model.mat$frail + as.matrix(model.mat[,c("BP", "BP2", "hh_log_wages",  "hh_kids", "hh_young_kids", "wavenumber",   
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
  p1 <- glmmboot(final.df.subset[,i] ~ BP + BP2 + hh_log_wages + hh_kids + hh_young_kids + wavenumber +
                   onion.price_hybrid + lime.price_hybrid + apple.price_hybrid + orange.price_hybrid +
                   potato.price_hybrid + banana.price_hybrid + leafy.green.price_hybrid +
                   tomato.price_hybrid +  
                   rice.price_hybrid + milk.price_hybrid + bean.price_hybrid + egg.price_hybrid, 
                 cluster = folio, # The fixed effect
                 data = final.df.subset, family = poisson)
  
  temp <- as.data.frame(cbind(unique(final.df.subset$folio), p1$frail), nrow = 2) 
  colnames(temp) <- c("folio", "frail")
  model.mat <- merge(final.df.subset[,c("folio", "BP", "BP2", "hh_log_wages", "hh_kids", 
                                        "hh_young_kids", "wavenumber",
                                        "onion.price_hybrid" , "lime.price_hybrid" , "apple.price_hybrid" , "orange.price_hybrid" ,
                                        "potato.price_hybrid" , "banana.price_hybrid" , "leafy.green.price_hybrid" ,
                                        "tomato.price_hybrid" ,  
                                        "rice.price_hybrid" , "milk.price_hybrid" , "bean.price_hybrid" , "egg.price_hybrid")], temp, by = c("folio"))
  
  ME <- mean((p1$coefficients[1] + 2*p1$coefficients[2]*model.mat$BP) * exp(model.mat$frail + 
                                                                              as.matrix(model.mat[,
                                                                                                  c("BP", "BP2", "hh_log_wages",  "hh_kids", "hh_young_kids", "wavenumber",   
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
  p1 <- glmmboot(final.df.subset[,i] ~ BP + BP2 + hh_log_wages + hh_kids + hh_young_kids + wavenumber +
                   digestive.biscuit.price_hybrid +      
                   pan.blanco.price_hybrid + 
                   tortilla.price_hybrid + wheat.flour.price_hybrid + 
                   rice.price_hybrid + milk.price_hybrid + bean.price_hybrid + egg.price_hybrid, 
                 cluster = folio,
                 data = final.df.subset, family = poisson)
  
  temp <- as.data.frame(cbind(unique(final.df.subset$folio), p1$frail), nrow = 2) 
  colnames(temp) <- c("folio", "frail")
  model.mat <- merge(final.df.subset[,c("folio", "BP", "BP2", "hh_log_wages", "hh_kids", 
                                        "hh_young_kids", "wavenumber",
                                        "digestive.biscuit.price_hybrid" ,      
                                        "pan.blanco.price_hybrid" , 
                                        "tortilla.price_hybrid" , "wheat.flour.price_hybrid" , 
                                        "rice.price_hybrid" , "milk.price_hybrid" , "bean.price_hybrid" , "egg.price_hybrid")], temp, by = c("folio"))
  
  ME <- mean((p1$coefficients[1] + 2*p1$coefficients[2]*model.mat$BP) * exp(model.mat$frail + 
                                                                              as.matrix(model.mat[,
                                                                                                  c("BP", "BP2", "hh_log_wages",  "hh_kids", "hh_young_kids", "wavenumber",   
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
               veg.oil.price_hybrid + sopa.de.pasta.price_hybrid + breakfast.cereal.price_hybrid + 
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
  p1 <- glmmboot(final.df.subset[,i] ~ BP + BP2 + hh_log_wages + hh_kids + hh_young_kids + wavenumber +
                   sugar.price_hybrid + coffee.price_hybrid + soda.price_hybrid + 
                   veg.oil.price_hybrid + sopa.de.pasta.price_hybrid + breakfast.cereal.price_hybrid + 
                   rice.price_hybrid + milk.price_hybrid + bean.price_hybrid + egg.price_hybrid, 
                 cluster = folio,
                 data = final.df.subset, family = poisson)
  
  temp <- as.data.frame(cbind(unique(final.df.subset$folio), p1$frail), nrow = 2) 
  colnames(temp) <- c("folio", "frail")
  model.mat <- merge(final.df.subset[,c("folio", "BP", "BP2", "hh_log_wages", "hh_kids", 
                                        "hh_young_kids", "wavenumber",
                                        "sugar.price_hybrid" , "coffee.price_hybrid" , "soda.price_hybrid" , 
                                        "veg.oil.price_hybrid" , "sopa.de.pasta.price_hybrid" , "breakfast.cereal.price_hybrid" , 
                                        "rice.price_hybrid" , "milk.price_hybrid" , "bean.price_hybrid" , "egg.price_hybrid")], temp, by = c("folio"))
  
  ME <- mean((p1$coefficients[1] + 2*p1$coefficients[2]*model.mat$BP) * exp(model.mat$frail + 
                                                                              as.matrix(model.mat[,
                                                                                                  c("BP", "BP2", "hh_log_wages",  "hh_kids", "hh_young_kids", "wavenumber",   
                                                                                                    "sugar.price_hybrid" , "coffee.price_hybrid" , "soda.price_hybrid" , 
                                                                                                    "veg.oil.price_hybrid" , "sopa.de.pasta.price_hybrid" , "breakfast.cereal.price_hybrid" , 
                                                                                                    "rice.price_hybrid" , "milk.price_hybrid" , "bean.price_hybrid" , "egg.price_hybrid" )]) %*% 
                                                                              as.numeric(p1$coefficients)), na.rm = T)
  
  Cross_Partial <- ME*as.numeric(p1$coefficients[3]) 
  
  return(list(ME, Cross_Partial)) }

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
sqrt(DiD_reg_summary$cov.unscaled[4,4])

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

food_var_names <- c('tomate.rojo', 'cebolla', 'papa', 'zanahorias', 'verdudas.de.hoja', 'narajas', 'platanos', 
                    'manzanas', 'limones',
                    'tortialls.de.maiz', 'maiz.en.grano', 'pan.blanco', 
                    'pan.de.dulce', 'pan.de.caja', 
                    'harina.de.trigo', 'sopa.de.pasta', 'arroz', 'galletas', 'frijol', 
                    'cereales.de.caja',
                    'pollo', 'carne.de.res.o.puerco', 'carne.de.cabra.u.oveja', 'pescados.y.mariscos', 
                    'sardinas.o.atun.en.lata', 
                    'huevos', 'leche', 'manteca.de.cerdo', 'pastelillos.en.bolsa', 'refrescos', 
                    'bebidas.alcoholicas', 'cafe', 'azucar', 'aciete.vegetal')

food_var_names_num <- paste0(food_var_names, "_num_times_consume", sep="")


stargazer(hh.df[,food_var_names][hh.df$wavenumber == 1,],
          nobs = FALSE, min.max = FALSE)

stargazer(hh.df[,food_var_names][hh.df$treatment_household == 1 & hh.df$wavenumber == 2,],
          hh.df[,food_var_names][hh.df$treatment_household == 0 & hh.df$wavenumber == 2,],
          nobs = FALSE, min.max = FALSE)

stargazer(hh.df[,food_var_names][hh.df$wavenumber == 3,],
          nobs = FALSE, min.max = FALSE)

stargazer(hh.df[,food_var_names_num][hh.df$wavenumber == 1,],
          hh.df[,food_var_names_num][hh.df$wavenumber == 2 & hh.df$treatment_household == 0,],
          hh.df[,food_var_names_num][hh.df$wavenumber == 2 & hh.df$treatment_household == 1,],
          hh.df[,food_var_names_num][hh.df$wavenumber == 3,],
          nobs = FALSE, min.max = FALSE)

# Put it together by hand. 

# Chapter 4: Generating Tables 4 and 5, the predicted earnings tables ####

data.df <- subset(sample.analog,  sample.analog$age > 15 & sample.analog$sex == 0 &  sample.analog$age <= 70)

selection_formula <- LFP ~ age + I(age^2) +  otherincomeval + hh_kids +  hh_young_kids + edu_yrs + literate + gov_transfer +
  indigenous_language + spanish_language + head_dummy + num_f_adults + num_m_adults + pobextre +  mpcalif  +
  number_female_kids + number_male_kids  + prop_mex_migrant + prop_usa_migrant + I(num_m_adults*prop_mex_migrant) +
  I(num_m_adults*prop_usa_migrant) + I(num_f_adults*prop_mex_migrant) + I(num_f_adults*prop_usa_migrant) +  
  as.factor(year_wave_FE) + treatment_dummy  +  # FE and Exclusion Restrictions
  ER + ER*number_female_kids +  ER*number_male_kids + ER*num_f_adults + ER*num_m_adults + proportion_need_permission + proportion_need_accompany  

outcome_formula <-  log_wages ~ age + I(age^2) +  otherincomeval + hh_kids +  hh_young_kids + edu_yrs + literate + gov_transfer +
  indigenous_language + spanish_language + head_dummy + num_f_adults + num_m_adults + pobextre +  mpcalif  +
  number_female_kids + number_male_kids  + prop_mex_migrant + prop_usa_migrant + I(num_m_adults*prop_mex_migrant) +
  I(num_m_adults*prop_usa_migrant) + I(num_f_adults*prop_mex_migrant) + I(num_f_adults*prop_usa_migrant) +
  as.factor(year_wave_FE) + treatment_dummy    

reg_men <- selection(selection_formula, outcome_formula, data = data.df, method = "ml")
summary(reg_men)

data.df <- subset(hh.df,  hh.df$age > 15 & hh.df$sex == 1 &  hh.df$age <= 70)

selection_formula <- LFP ~age + I(age^2) +  otherincomeval + hh_kids +  hh_young_kids + edu_yrs + literate + gov_transfer +
  indigenous_language + spanish_language + head_dummy + num_f_adults + num_m_adults + pobextre +  mpcalif  +
  number_female_kids + number_male_kids  + prop_mex_migrant + prop_usa_migrant + I(num_m_adults*prop_mex_migrant) +
  I(num_m_adults*prop_usa_migrant) + I(num_f_adults*prop_mex_migrant) + I(num_f_adults*prop_usa_migrant) +  
  as.factor(year_wave_FE) + treatment_dummy + proportion_need_accompany + proportion_need_permission + # FE and Exclusion Restrictions
  ER + ER*number_female_kids +  ER*number_male_kids + ER*num_f_adults + ER*num_m_adults  + progresa_income_mom   # Only difference between men and women is the addition of Progresa income for female HH heads that got the transfer

outcome_formula <-  log_wages ~age + I(age^2) +  otherincomeval + hh_kids +  hh_young_kids + edu_yrs + literate + gov_transfer +
  indigenous_language + spanish_language + head_dummy + num_f_adults + num_m_adults + pobextre +  mpcalif  +
  number_female_kids + number_male_kids  + prop_mex_migrant + prop_usa_migrant + I(num_m_adults*prop_mex_migrant) +
  I(num_m_adults*prop_usa_migrant) + I(num_f_adults*prop_mex_migrant) + I(num_f_adults*prop_usa_migrant) +  
  as.factor(year_wave_FE) + treatment_dummy + 
  progresa_income_mom # Only difference between men and women is the addition of Progresa income for female HH heads that got the transfer

reg_women <- selection(selection_formula, outcome_formula, data = data.df, method = "ml")
summary(reg_women)

stargazer::stargazer(reg_women, reg_men, omit = c("year_wave_FE"), single.row = T)

a <- summary(reg_women)
b <- summary(reg_men)

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
     breaks = 100, xlim = c(0.15,0.5), freq=FALSE, ylab = "Percent of Households")
par(new = T)
hist(final.df$BP[final.df$wavenumber == 1 & final.df$treatment_household == 1], col = rgb(0,0,0,0.25, 0.25),  
     breaks = 100, xlim = c(0.15,0.5), freq=FALSE,  axes = F, xlab =NA, ylab=NA, main = NA)

legend("topright", legend = c("control (white)", "treatment (grey)"), col = c("black", "black"), pch = c(0,15))

hist(final.df$BP[final.df$wavenumber == 2 & final.df$progresa_income_total == 0], col = rgb(1,1,1,0.25, 0.25), main = "1999", xlab = "Bargaining Power", 
     breaks = 100, xlim = c(0.15,0.5), freq=FALSE, ylab = "Percent of Households")
par(new = T)
hist(final.df$BP[final.df$wavenumber == 2 & final.df$progresa_income_total > 0], col = rgb(0,0,0,0.25, 0.25), 
     breaks = 100, xlim = c(0.15,0.5), freq=FALSE,  axes = F, xlab =NA, ylab=NA, main = NA)

hist(final.df$BP[final.df$wavenumber == 3 & final.df$progresa_income_total == 0], col = rgb(1,1,1, 0.25, 0.25), main = "2000", xlab = "Bargaining Power", 
     breaks = 100, xlim = c(0.15,0.5), freq=FALSE, ylab = "Percent of Households")
par(new = T)
hist(final.df$BP[final.df$wavenumber == 3 & final.df$progresa_income_total > 0], col = rgb(0,0,0, 0.25, 0.25), 
     breaks = 100, xlim = c(0.15,0.5), freq=FALSE,  axes = F, xlab =NA, ylab=NA, main = NA)


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


# Chapter 6: Generating the Chicken Table ####

summary(chick_LPM <- felm(pollo ~ BP + I(BP^2) + hh_log_wages + hh_kids + hh_young_kids + 
                            chicken.price_hybrid +
                            beef.price_hybrid + pork.price_hybrid +   beef.price_hybrid + pork.price_hybrid + 
                            lard.price_hybrid + sardines.price_hybrid + tuna.price_hybrid +  
                           # orange.price_hybrid + apple.price_hybrid + lime.price_hybrid +
                            milk.price_hybrid + egg.price_hybrid + bean.price_hybrid + rice.price_hybrid 
                          | folio + wavenumber | 0 | loc_id,
           data = final.df))

summary(milk_LPM <- felm(leche ~ BP + I(BP^2) + hh_log_wages + hh_kids + hh_young_kids + 
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

summary(chick_Poisson <- glmmboot(final.df.subset[,"pollo"] ~ BP + I(BP^2) + hh_log_wages + hh_kids + hh_young_kids + wavenumber +
                                    chicken.price_hybrid +
                                    lard.price_hybrid + sardines.price_hybrid + tuna.price_hybrid +   
                                    beef.price_hybrid + pork.price_hybrid +   
                                    milk.price_hybrid + egg.price_hybrid + bean.price_hybrid + rice.price_hybrid, 
               cluster = folio,
               data = final.df.subset, family = poisson))


summary(milk_Poisson <- glmmboot(final.df.subset[,"leche"] ~ BP + I(BP^2) + hh_log_wages + hh_kids + hh_young_kids + wavenumber +
                                    chicken.price_hybrid +
                                    lard.price_hybrid + sardines.price_hybrid + tuna.price_hybrid +   
                                    beef.price_hybrid + pork.price_hybrid +   
                                    milk.price_hybrid + egg.price_hybrid + bean.price_hybrid + rice.price_hybrid, 
                                  cluster = folio,
                                  data = final.df.subset, family = poisson))


#summary(milk_Poisson <- glmmboot(final.df[,"leche"] ~ BP + I(BP^2) + hh_log_wages + hh_kids + hh_young_kids + wavenumber +
#                                    chicken.price_hybrid +
#                                   lard.price_hybrid + sardines.price_hybrid + tuna.price_hybrid +   
#                                    beef.price_hybrid + pork.price_hybrid +   
#                                    milk.price_hybrid + egg.price_hybrid + bean.price_hybrid + rice.price_hybrid, 
#                                  cluster = folio,
#                                  data = final.df, family = poisson))

stargazer::stargazer(chick_LPM, milk_LPM, chick_LPM, milk_LPM, single.row=T)

# Chapter 7: generating 62 Point Estimate Results ###### 


keep.index <- with(final.df, { is.na(hh_log_wages) == FALSE & is.na(BP) == FALSE})
final.df.subset <- final.df[keep.index, ]
final.df.subset$BP2 <- final.df.subset$BP^2

Poisson_ME_Fun_animal("pollo_num_times_consume")
#Poisson_ME_Fun_animal("carne.de.cabra.u.oveja_num_times_consume")
Poisson_ME_Fun_animal("carne.de.res.o.puerco_num_times_consume")
Poisson_ME_Fun_animal("huevos_num_times_consume")
Poisson_ME_Fun_animal("leche_num_times_consume")
Poisson_ME_Fun_animal("pescados.y.mariscos_num_times_consume")       
Poisson_ME_Fun_animal("sardinas.o.atun.en.lata_num_times_consume")
Poisson_ME_Fun_animal("manteca.de.cerdo_num_times_consume")

LPM_ME_Fun_animal("pollo")
#LPM_ME_Fun_animal("carne.de.cabra.u.oveja")
LPM_ME_Fun_animal("carne.de.res.o.puerco")
LPM_ME_Fun_animal("huevos")
LPM_ME_Fun_animal("leche")
LPM_ME_Fun_animal("pescados.y.mariscos")       
LPM_ME_Fun_animal("sardinas.o.atun.en.lata")
LPM_ME_Fun_animal("manteca.de.cerdo")


#FRUITS AND VEGETABLES

Poisson_ME_Fun_VF("cebolla_num_times_consume")
Poisson_ME_Fun_VF("limones_num_times_consume")
Poisson_ME_Fun_VF("manzanas_num_times_consume")
Poisson_ME_Fun_VF("narajas_num_times_consume")
Poisson_ME_Fun_VF("papa_num_times_consume")
Poisson_ME_Fun_VF("platanos_num_times_consume")
Poisson_ME_Fun_VF("verdudas.de.hoja_num_times_consume")
Poisson_ME_Fun_VF("zanahorias_num_times_consume")
Poisson_ME_Fun_VF("tomate.rojo_num_times_consume")

LPM_ME_Fun_VF("cebolla")
LPM_ME_Fun_VF("limones")
LPM_ME_Fun_VF("manzanas")
LPM_ME_Fun_VF("narajas")
LPM_ME_Fun_VF("papa")
LPM_ME_Fun_VF("platanos")
LPM_ME_Fun_VF("verdudas.de.hoja")
LPM_ME_Fun_VF("zanahorias")
LPM_ME_Fun_VF("tomate.rojo")


#PULSES AND GRAINS

Poisson_ME_Fun_grains("arroz_num_times_consume")
Poisson_ME_Fun_grains("frijol_num_times_consume")
Poisson_ME_Fun_grains("galletas_num_times_consume")
Poisson_ME_Fun_grains("maiz.en.grano_num_times_consume")
Poisson_ME_Fun_grains("pan.blanco_num_times_consume")
#Poisson_ME_Fun_grains("pan.de.caja_num_times_consume")
Poisson_ME_Fun_grains("pan.de.dulce_num_times_consume")
#Poisson_ME_Fun_grains("pastelillos.en.bolsa_num_times_consume")
Poisson_ME_Fun_grains("tortialls.de.maiz_num_times_consume")
Poisson_ME_Fun_grains("harina.de.trigo_num_times_consume")

LPM_ME_Fun_grains("arroz")
LPM_ME_Fun_grains("frijol")
LPM_ME_Fun_grains("galletas")
LPM_ME_Fun_grains("maiz.en.grano")
LPM_ME_Fun_grains("harina.de.trigo")
LPM_ME_Fun_grains("pan.blanco")
#LPM_ME_Fun_grains("pan.de.caja")
LPM_ME_Fun_grains("pan.de.dulce")
#LPM_ME_Fun_grains("pastelillos.en.bolsa")
LPM_ME_Fun_grains("tortialls.de.maiz")


# OTHER 

Poisson_ME_Fun_misc("azucar_num_times_consume")
Poisson_ME_Fun_misc("cafe_num_times_consume")
Poisson_ME_Fun_misc("refrescos_num_times_consume")
Poisson_ME_Fun_misc("sopa.de.pasta_num_times_consume")
Poisson_ME_Fun_misc("aciete.vegetal_num_times_consume")
Poisson_ME_Fun_misc("bebidas.alcoholicas_num_times_consume")
Poisson_ME_Fun_misc("cereales.de.caja_num_times_consume")

LPM_ME_Fun_misc("azucar")
LPM_ME_Fun_misc("cafe")
LPM_ME_Fun_misc("refrescos")
LPM_ME_Fun_misc("sopa.de.pasta")
LPM_ME_Fun_misc("aciete.vegetal")
LPM_ME_Fun_misc("bebidas.alcoholicas")
LPM_ME_Fun_misc("cereales.de.caja")







# Chapter 9: Market Earnings Breakdown Table ####

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


