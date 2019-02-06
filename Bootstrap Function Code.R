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

temp.df <- aggregate(hh.df$progresa_income_total[hh.df$wavenumber==2], 
                     by =list(hh.df$folio[hh.df$wavenumber==2]), FUN=mean, na.rm=T)
colnames(temp.df) <- c("folio", "progresa_income_total_in_period_2")
temp.df$treatment_household <- rep(0)
temp.df$treatment_household[temp.df$progresa_income_total_in_period_2>0] <- 1
summary(temp.df)

hh.df <- merge(hh.df, temp.df, by = "folio")
table(hh.df$treatment_household, hh.df$wavenumber) #  Need to have a var that delineates the T hh's in waves 1 and 2 for the DiD calc
hh.df$treatment_household_0 <- ifelse(hh.df$wavenumber > 1, hh.df$treatment_household, 0)
      
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
                      sample.analog$age <= 65)
  
  if(gender_number == 0){   
  
  reg <- selection(selection = LFP ~ age + I(age^2) +  asinh(otherincomeval) + hh_kids +  hh_young_kids + edu_yrs + literate + gov_transfer +
                     indigenous_language + spanish_language + head_dummy + num_f_adults + num_m_adults + #pobextre +  mpcalif  +
                     number_female_kids + number_male_kids  + prop_mex_migrant + prop_usa_migrant + I(num_m_adults*prop_mex_migrant) +
                     I(num_m_adults*prop_usa_migrant) + I(num_f_adults*prop_mex_migrant) + I(num_f_adults*prop_usa_migrant) +  
                     as.factor(year_wave_FE) + treatment_household_0  +  # FE and Exclusion Restrictions
                     ER + ER*number_female_kids +  ER*number_male_kids + ER*num_f_adults + ER*num_m_adults +
                     proportion_need_permission + proportion_need_accompany,  
                   outcome = log_wages ~ age + I(age^2) +  asinh(otherincomeval) + hh_kids +  hh_young_kids + edu_yrs + literate + gov_transfer +
                     indigenous_language + spanish_language + head_dummy + num_f_adults + num_m_adults +# pobextre +  mpcalif  +
                     number_female_kids + number_male_kids  + prop_mex_migrant + prop_usa_migrant + I(num_m_adults*prop_mex_migrant) +
                     I(num_m_adults*prop_usa_migrant) + I(num_f_adults*prop_mex_migrant) + I(num_f_adults*prop_usa_migrant) +  
                     as.factor(year_wave_FE) + treatment_household_0,
                   data = data.df,
                   method = "ml")
  
  }
  
  if(gender_number == 1){  
  # Only difference between men and women is the addition of Progresa income for female HH heads that got the transfer
  
    reg <- selection(selection = LFP ~ age + I(age^2) +  asinh(otherincomeval) + hh_kids +  hh_young_kids + edu_yrs + literate + gov_transfer +
                       indigenous_language + spanish_language + head_dummy + num_f_adults + num_m_adults + #pobextre +  mpcalif  +
                       number_female_kids + number_male_kids  + prop_mex_migrant + prop_usa_migrant + I(num_m_adults*prop_mex_migrant) +
                       I(num_m_adults*prop_usa_migrant) + I(num_f_adults*prop_mex_migrant) + I(num_f_adults*prop_usa_migrant) +  
                       as.factor(year_wave_FE) + treatment_household_0  +  # FE and Exclusion Restrictions
                       ER + ER*number_female_kids +  ER*number_male_kids + ER*num_f_adults + ER*num_m_adults +
                       proportion_need_permission + proportion_need_accompany + progresa_income_mom, 
                     outcome = log_wages ~ age + I(age^2) +  asinh(otherincomeval) + hh_kids +  hh_young_kids + edu_yrs + literate + gov_transfer +
                       indigenous_language + spanish_language + head_dummy + num_f_adults + num_m_adults + #pobextre +  mpcalif  +
                       number_female_kids + number_male_kids  + prop_mex_migrant + prop_usa_migrant + I(num_m_adults*prop_mex_migrant) +
                       I(num_m_adults*prop_usa_migrant) + I(num_f_adults*prop_mex_migrant) +
                       I(num_f_adults*prop_usa_migrant) +  as.factor(year_wave_FE) + treatment_household_0  +  
                       progresa_income_mom,
                     data = data.df,
                     method = "ml")  
    
    }
  
  # Add the predicted values to data.df, conditional on LFP
  data.df <- cbind(data.df,
                   exp(predict(reg, newdata = data.df, type = "conditional")))
  
 
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
  print(i)
  p1 <- felm(final.df[,i] ~ BP + I(BP^2) + hh_log_wages + treatment_household_0 + hh_kids + hh_young_kids + 
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
  p1 <- glmmboot(final.df.subset[,i] ~ BP + BP2 + treatment_household_0 + hh_log_wages +  hh_kids + hh_young_kids + wavenumber +
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
  
  ME <- mean((p1$coefficients[1] + 2*p1$coefficients[2]*model.mat$BP) * exp(model.mat$frail + 
                                                                              as.matrix(model.mat[,
                                                                                                  c("BP", "BP2", "hh_log_wages",  "hh_kids", "hh_young_kids", "wavenumber",   
                                                                                                    "chicken.price_hybrid" ,
                                                                                                    "beef.price_hybrid" , "pork.price_hybrid" ,    
                                                                                                    "lard.price_hybrid" , "sardines.price_hybrid" , "tuna.price_hybrid" ,   
                                                                                                    "milk.price_hybrid" , "egg.price_hybrid" , "bean.price_hybrid" , "rice.price_hybrid")]) %*% as.numeric(p1$coefficients)), na.rm = T)
  
  Cross_Partial <- ME*as.numeric(p1$coefficients[3]) 
  
  return(list(ME, Cross_Partial)) }


#FRUITS AND VEGETABLES

LPM_ME_Fun_VF <- function(food_name){
  i <- which(colnames(final.df) == food_name)
  print(i)
  p1 <- felm(final.df[,i] ~ BP + I(BP^2) + hh_log_wages + hh_kids + hh_young_kids + 
               onion.price_hybrid + lime.price_hybrid + apple.price_hybrid + orange.price_hybrid +
               potato.price_hybrid + banana.price_hybrid + leafy.green.price_hybrid +
               tomato.price_hybrid +  
               rice.price_hybrid + milk.price_hybrid + bean.price_hybrid + egg.price_hybrid | folio + wavenumber | 0 | loc_id,
             data = final.df)
  
  return(list(p1$coefficients[1] + 2*p1$coefficients[2]*mean(final.df$BP, na.rm = T), 
              2*p1$coefficients[2],
              p1$coefficients[3], 
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
                 cluster = folio,
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
  print(i)
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
  print(i)
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



# (E) Analog
sample.analog.Fun <- function(){
  hh_nums <- sample(unique(hh.df$folio), N, replace=TRUE) # generates 19000+ cluster IDs
  reps <- plyr::count(hh_nums)                            # Counts the number of times each hh was pulled   
  names(reps) <- c('folio', 'folio_freq')                
  df <- merge(hh.df, reps)                                # Automatically drops households not pulled at all since all.x = F is the default
  sample.analog <- df[rep(seq_len(nrow(df)), df$folio_freq), ]     # repeats each cluster the number of times it was pulled
  return(sample.analog)
}



# Chapter 3: Bootstrap ####
B <- 500
N <- as.integer(length(table(unique(hh.df$folio))))

#update these when you call each function. 
boot_t <- DiD <- LPM.Marginal <- Poisson.Marginal <- LPM.BP.sqr.coef.est <- Cross_Partial <- RHS <- c(rep(NA, B))

# LPM.Marginal = RHS Derivative 1 & DiD = RHS derivative # 2, #LHS = DID for program participation on food consumption overall  

j = 1

while(j <= B) { #generating the bootstrap
  time <- proc.time()
  sample.analog <- sample.analog.Fun()  
  
  BP.Fun.Results <- BP.Fun()
  sample.analog <- BP.Fun.Results[[1]]
  
  # making a matrix of just the HH level variables: 
  final.df <- aggregate(sample.analog$BP, by = list(sample.analog$folio, sample.analog$wavenumber), FUN=mean, na.rm=T)
  colnames(final.df) <- c("folio", "wavenumber", "BP")
  
  final.df <- unique(left_join(sample.analog[,c("folio", "wavenumber", "loc_id", "hh_log_wages" , "hh_kids" , "hh_young_kids" , "seven_states",
                                            "progresa_income_total","wave2", "wave3", "treatment_household", 
                                            "rice.price_hybrid", "bean.price_hybrid", "egg.price_hybrid", "milk.price_hybrid", # Staples
                                            # ANIMAL PRODUCTS
                                            "pollo", "huevos", "leche", "pollo_num_times_consume", "leche_num_times_consume", "huevos_num_times_consume", "carne.de.res.o.puerco",
                                            "pescados.y.mariscos", "pescados.y.mariscos_num_times_consume", "sardinas.o.atun.en.lata", "sardinas.o.atun.en.lata_num_times_consume",
                                            "carne.de.res.o.puerco_num_times_consume", "manteca.de.cerdo", "manteca.de.cerdo_num_times_consume",
                                            "chicken.price_hybrid", "lard.price_hybrid" , "sardines.price_hybrid", "tuna.price_hybrid" ,
                                            "beef.price_hybrid",  "pork.price_hybrid",  "lard.price_hybrid" ,
                                            # FRUITS AND VEGETABLES
                                            #  "tomate.rojo" , "zanahorias", "narajas", "verdudas.de.hoja",
                                            #   "narajas_num_times_consume", "tomate.rojo_num_times_consume",
                                            #  "cebolla_num_times_consume", "cebolla",
                                            #  "verdudas.de.hoja", "verdudas.de.hoja_num_times_consume", "platanos", 
                                            #  "platanos_num_times_consume", "zanahorias_num_times_consume", "limones", "limones_num_times_consume",
                                            #  "papa_num_times_consume", "papa", "manzanas", "manzanas_num_times_consume",
                                            #  "onion.price_hybrid" , "lime.price_hybrid" , "apple.price_hybrid" , "orange.price_hybrid" ,
                                            #  "potato.price_hybrid" , "banana.price_hybrid" , "leafy.green.price_hybrid",
                                            #  "tomato.price_hybrid" ,
                                            # PULSES AND GRAINS
                                            #  "digestive.biscuit.price_hybrid" ,  "frijol", "frijol_num_times_consume",
                                            #  "pan.blanco.price_hybrid" , 
                                            #  "tortilla.price_hybrid" , "tortialls.de.maiz_num_times_consume", "tortialls.de.maiz",
                                            #  "cereales.de.caja", "cereales.de.caja_num_times_consume", "pan.blanco", "pan.de.dulce",
                                            #  "pan.de.dulce_num_times_consume", "galletas", "galletas_num_times_consume",
                                            #  "pan.blanco_num_times_consume", 
                                            #  "pastelillos.en.bolsa_num_times_consume", "pastelillos.en.bolsa",
                                            #  "maiz.en.grano_num_times_consume", "maiz.en.grano",
                                            #  "harina.de.trigo_num_times_consume", "harina.de.trigo",
                                            #  # Miscellaneous
                                            #  "bebidas.alcoholicas_num_times_consume", "bebidas.alcoholicas",
                                            #  "cafe", "cafe_num_times_consume", "arroz", "arroz_num_times_consume",
                                            #  "sugar.price_hybrid" , "coffee.price_hybrid" , "soda.price_hybrid" ,
                                            #  "azucar_num_times_consume", "azucar",
                                            #  "refrescos", "refrescos_num_times_consume",
                                            #  "sopa.de.pasta", "sopa.de.pasta_num_times_consume",
                                            #  "wheat.flour.price_hybrid" , "veg.oil.price_hybrid" , 
                                            #  "aciete.vegetal", "aciete.vegetal_num_times_consume",
                                            #  "sopa.de.pasta.price_hybrid", "breakfast.cereal.price_hybrid",
                                            "treatment_household", "treatment_household_0")], final.df, by =   c("folio", "wavenumber")))
  
  DiD[j] <- (mean(final.df$BP[final.df$wavenumber == 2 & final.df$treatment_household == 1], na.rm=T) - 
               mean(final.df$BP[final.df$wavenumber ==  1 & final.df$treatment_household == 1], na.rm=T)) -
    (mean(final.df$BP[final.df$wavenumber == 2 & final.df$treatment_household == 0], na.rm=T) -
       mean(final.df$BP[final.df$wavenumber == 1 & final.df$treatment_household == 0], na.rm=T))
  
  DiD_reg_summary <- summary(lm(BP ~ treatment_household + wavenumber + I(treatment_household*wavenumber), data = subset(final.df, final.df$wavenumber < 3)))
  
  boot_t[j] = (DiD[j] - 0.1705953)/sqrt(DiD_reg_summary$cov.unscaled[4,4]) 
  
  keep.index <- with(final.df, { is.na(hh_log_wages) == FALSE & 
                                 is.na(BP) == FALSE & 
                                 is.na(breakfast.cereal.price_hybrid) == FALSE })
  final.df.subset <- final.df[keep.index, ]
  final.df.subset$BP2 <- final.df.subset$BP^2
  
  # BS Animal Products Storage ####
  
  # 1 - Chicken
  temp  <- LPM_ME_Fun_animal("pollo")
  temp2 <- Poisson_ME_Fun_animal("pollo_num_times_consume")
  
  
  
  #Poisson_ME_Fun_animal("carne.de.cabra.u.oveja_num_times_consume")
  Poisson_ME_Fun_animal("carne.de.res.o.puerco_num_times_consume")
  Poisson_ME_Fun_animal("huevos_num_times_consume")
  Poisson_ME_Fun_animal("leche_num_times_consume")
  Poisson_ME_Fun_animal("pescados.y.mariscos_num_times_consume")       
  Poisson_ME_Fun_animal("sardinas.o.atun.en.lata_num_times_consume")
  Poisson_ME_Fun_animal("manteca.de.cerdo_num_times_consume")
  
  
  #LPM_ME_Fun_animal("carne.de.cabra.u.oveja")
  LPM_ME_Fun_animal("carne.de.res.o.puerco")
  LPM_ME_Fun_animal("huevos")
  LPM_ME_Fun_animal("leche")
  LPM_ME_Fun_animal("pescados.y.mariscos")       
  LPM_ME_Fun_animal("sardinas.o.atun.en.lata")
  LPM_ME_Fun_animal("manteca.de.cerdo")
  
  # Vegetables and Fruits
  
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
  
  # Grains
  
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
  
  # Miscellaneous
  
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
  
  
  
  LPM.Marginal[j] <- LPM_ME_Fun.Results[[1]]
  LPM.BP.sqr.coef.est[j] <- LPM_ME_Fun.Results[[2]]
  
  RHS[j] <- DiD[j]*LPM.Marginal[j]
  
  Poisson_ME_Fun.Results <- Poisson_ME_Fun(food_name = "sardinas.o.atun.en.lata_num_times_consume")
  Poisson.Marginal[j] <- Poisson_ME_Fun.Results[[1]]
  Cross_Partial[j] <- Poisson_ME_Fun.Results[[2]]
  
  j <- j+1
  print(j)
  print(proc.time() - time)
}

summary(LPM.Marginal)
summary(Poisson.Marginal)

p <- as.data.frame(cbind(LPM.Marginal, Poisson.Marginal, LPM.BP.sqr.coef.est, Cross_Partial, RHS, DiD, boot_t))

write.csv(p, file = "Tuna and Sardines 01.30.2017 Bootstrap Results.csv")

# Generating the Efron Intervals

p.sorted <- apply(p,2,sort,decreasing=F)

quantile(p.sorted[,"LPM.Marginal"],probs=c(.025,.975))
quantile(p.sorted[,"Poisson.Marginal"],probs=c(.025,.975))

quantile(p.sorted[,"LPM.BP.sqr.coef.est"],probs=c(.025,.975))
quantile(p.sorted[,"Cross_Partial"],probs=c(.025,.975))

quantile(p.sorted[,"RHS"],probs=c(.025,.975))
quantile(p.sorted[,"DiD"],probs=c(.025,.975))
quantile(p.sorted[,"boot_t"],probs=c(.025,.975))

critical.pivot <- 0.088 - (0.02499164 * quantile(boot_t,probs=c(.975,.025)))
critical.pivot



# Appendix of unused code #### 
# coefs_o <- reg$estimate[53:(length(reg$estimate)-2)] # Note: could index programatically using the param info stored in the selection object
# coefs_s <- reg$estimate[1:52]
# sigma <- reg$estimate[98]
# rho <- reg$estimate[99]
# length(coefs)
# 
# if(gender_number==1){
#   coefs_s <- reg$estimate[1:53]
#   coefs_o <- reg$estimate[54:(length(reg$estimate)-2)]
#   sigma <- reg$estimate[100]
#   rho <- reg$estimate[101]
# }
# 
# Xo <- model.matrix(outcome_formula, 
#                    model.frame(outcome_formula,
#                                data = data.df,
#                                na.action = na.pass)) # Thanks, Travis, for the idea
# 
# Xs <- model.matrix(selection_formula, # To make the inverse mills ratio 
#                    model.frame(selection_formula,
#                                data = data.df,
#                                na.action = na.pass)) # Thanks, Travis, for the idea
# # dim(X)
# 
# lin_pred <- Xs%*%coefs_s
# 
# summary(y_hat <- exp(Xo%*%coefs_o + rho*sigma*(dnorm(lin_pred))/pnorm(lin_pred)))

# 
# 
# 
# 
# # (B) Linear Probability Model on Whole Sample
# LPM_ME_Fun <- function(food_name){
#   i <- which(colnames(final.df) == food_name)
#   #print(i)
#   p1 <- felm(final.df[,i] ~ BP + I(BP^2) + hh_log_wages + hh_kids + hh_young_kids + 
#                chicken.price_hybrid +
#                beef.price_hybrid + pork.price_hybrid +   
#                lard.price_hybrid + sardines.price_hybrid + tuna.price_hybrid +   
#                milk.price_hybrid + egg.price_hybrid + bean.price_hybrid + rice.price_hybrid   | folio + wavenumber | 0 | loc_id,
#              data = final.df)
#   
#   return(list(p1$coefficients[1] + 2*p1$coefficients[2]*mean(final.df$BP, na.rm = T), 2*p1$coefficients[2]))
#   
# }
# 
# 
# # (D) Poisson Model
# Poisson_ME_Fun <- function(food_name){
#   i <- which(colnames(final.df.subset) == food_name)
#   p1 <- glmmboot(final.df.subset[,i] ~ BP + I(BP^2) + hh_log_wages + hh_kids + hh_young_kids + wavenumber +
#                    chicken.price_hybrid +
#                    beef.price_hybrid + pork.price_hybrid + 
#                    lard.price_hybrid + sardines.price_hybrid + tuna.price_hybrid +   
#                    milk.price_hybrid + egg.price_hybrid + bean.price_hybrid + rice.price_hybrid, 
#                  cluster = folio,
#                  data = final.df.subset, family = poisson)
#   
#   temp <- as.data.frame(cbind(unique(final.df.subset$folio), p1$frail), nrow = 2) 
#   colnames(temp) <- c("folio", "frail")
#   model.mat <- merge(final.df.subset[,c("folio", "BP", "BP2", "hh_log_wages",  "hh_kids", "hh_young_kids", "wavenumber",   
#                                         "chicken.price_hybrid" ,
#                                         "beef.price_hybrid" , "pork.price_hybrid" ,  
#                                         "lard.price_hybrid" , "sardines.price_hybrid" , "tuna.price_hybrid" ,   
#                                         "milk.price_hybrid" , "egg.price_hybrid" , "bean.price_hybrid" , 
#                                         "rice.price_hybrid")], temp, by = "folio")
#   
#   ME <- mean((p1$coefficients[1] + 2*p1$coefficients[2]*model.mat$BP) * exp(model.mat$frail + 
#                                                                               as.matrix(model.mat[,
#                                                                                                   c("BP", "BP2", "hh_log_wages",  "hh_kids", "hh_young_kids", "wavenumber",   
#                                                                                                     "chicken.price_hybrid" ,
#                                                                                                     "beef.price_hybrid" , "pork.price_hybrid" ,  
#                                                                                                     "lard.price_hybrid" , "sardines.price_hybrid" , "tuna.price_hybrid" ,   
#                                                                                                     "milk.price_hybrid" , "egg.price_hybrid" , "bean.price_hybrid" , 
#                                                                                                     "rice.price_hybrid")]) %*% as.numeric(p1$coefficients)), 
#              na.rm = T)
#   
#   Cross_Partial <- ME*as.numeric(p1$coefficients[3]) 
#   
#   return(list(ME, Cross_Partial)) }

