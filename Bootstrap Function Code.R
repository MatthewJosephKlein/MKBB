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
                    # sample.analog$prop_mex_migrant <= quantile(sample.analog$prop_mex_migrant, 0.95) 
                    # sample.analog$prop_usa_migrant <= quantile(sample.analog$prop_usa_migrant, 0.95))
                    #  sample.analog$num_f_adults <= quantile(sample.analog$num_f_adults, 0.95) &
                    #  sample.analog$num_m_adults <= quantile(sample.analog$num_m_adults, 0.95) )
                    # sample.analog$hh_kids <= quantile(sample.analog$hh_kids, 0.95) &
                    #  &
                    # sample.analog$number_female_kids <= quantile(sample.analog$number_female_kids, 0.95) &
                    # sample.analog$number_male_kids <= quantile(sample.analog$number_male_kids, 0.95) &
                    # sample.analog$edu_yrs <= quantile(sample.analog$edu_yrs, 0.95) &

  
  
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
                   rice.price_hybrid + milk.price_hybrid + bean.price_hybrid + egg.price_hybrid 
                 cluster = factor(folio),
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

#update these when you call each function: Animal Products
LPM.Marginal.Lard  <- RHS.Lard  <- Poisson.Marginal.Lard <- Cross.Partial.Lard <-
LPM.Marginal.Tuna  <- RHS.Tuna  <- Poisson.Marginal.Tuna <- Cross.Partial.Tuna <- 
LPM.Marginal.Fish  <- RHS.Fish  <- Poisson.Marginal.Fish <- Cross.Partial.Fish <- 
LPM.Marginal.Milk  <- RHS.Milk  <- Poisson.Marginal.Milk <- Cross.Partial.Milk <- 
LPM.Marginal.Eggs  <- RHS.Eggs  <- Poisson.Marginal.Eggs <- Cross.Partial.Eggs <- 
LPM.Marginal.Chicken  <- RHS.Chicken <- Poisson.Marginal.Chicken <- Cross.Partial.Chicken <- 
LPM.Marginal.BeefPork <- RHS.BeefPork  <- Poisson.Marginal.BeefPork <- Cross.Partial.BeefPork <- 
  LPM.BP2.Lard    <- LPM.BP2.Tuna  <- LPM.BP2.Fish  <- LPM.BP2.Milk  <- LPM.BP2.Eggs  <- LPM.BP2.Chicken <- LPM.BP2.BeefPork <-
c(rep(NA, B))

#update these when you call each function: Fruits and Vegetabels 
LPM.Marginal.Tomato <- RHS.Tomato  <- Poisson.Marginal.Tomato <- Cross.Partial.Tomato <- 
LPM.Marginal.Carrots <- RHS.Carrots <- Poisson.Marginal.Carrots <- Cross.Partial.Carrots <- 
LPM.Marginal.Greens  <- RHS.Greens  <- Poisson.Marginal.Greens <- Cross.Partial.Greens <- 
LPM.Marginal.Banana  <- RHS.Banana  <-  Poisson.Marginal.Banana <- Cross.Partial.Banana <- 
LPM.Marginal.Potato  <-  RHS.Potato  <- Poisson.Marginal.Potato <- Cross.Partial.Potato <- 
LPM.Marginal.Orange  <- RHS.Orange  <- Poisson.Marginal.Orange <- Cross.Partial.Orange <- 
LPM.Marginal.Apple  <- RHS.Apple  <-  Poisson.Marginal.Apple <- Cross.Partial.Apple <- 
LPM.Marginal.Onion  <- RHS.Onion  <-  Poisson.Marginal.Onion <-  Cross.Partial.Onion <- 
LPM.Marginal.Lime  <- RHS.Lime  <- Poisson.Marginal.Lime <- Cross.Partial.Lime <-
LPM.BP2.Tomato <- LPM.BP2.Carrots<- LPM.BP2.Greens <- LPM.BP2.Banana <- LPM.BP2.Potato <- 
  LPM.BP2.Orange <- LPM.BP2.Apple  <- LPM.BP2.Onion  <- LPM.BP2.Lime  <-
c(rep(NA, B))

#update these when you call each function: Grains
LPM.Marginal.WFlour    <-  RHS.WFlour  <- Poisson.Marginal.WFlour <- Cross.Partial.WFlour <- 
LPM.Marginal.CFlour    <- RHS.CFlour  <- Poisson.Marginal.CFlour <- Cross.Partial.CFlour <- 
LPM.Marginal.Rice      <- RHS.Rice  <- Poisson.Marginal.Rice <- Cross.Partial.Rice <- 
LPM.Marginal.Beans     <- RHS.Beans  <- Poisson.Marginal.Beans <- Cross.Partial.Beans <- 
LPM.Marginal.Biscuits  <- RHS.Biscuits  <- Poisson.Marginal.Biscuits <- Cross.Partial.Biscuits <- 
LPM.Marginal.WBread    <- RHS.WBread <- Poisson.Marginal.WBread <- Cross.Partial.WBread <-
LPM.Marginal.Tortillas <- RHS.Tortillas <- Poisson.Marginal.Tortillas <- Cross.Partial.Tortillas <-
LPM.BP2.WFlour   <- LPM.BP2.CFlour   <- LPM.BP2.Rice     <- LPM.BP2.Beans    <- LPM.BP2.Biscuits <- 
  LPM.BP2.WBread   <- LPM.BP2.Tortillas<-
  c(rep(NA, B))

#update these when you call each function: Misc
LPM.Marginal.Sugar   <- RHS.Sugar  <- Poisson.Marginal.Sugar <- Cross.Partial.Sugar <-
LPM.Marginal.Coffee  <- RHS.Coffee  <- Poisson.Marginal.Coffee <- Cross.Partial.Coffee <-
LPM.Marginal.Soda    <- RHS.Soda  <- Poisson.Marginal.Soda <- Cross.Partial.Soda <-
LPM.Marginal.CNoodles<- RHS.CNoodles  <- Poisson.Marginal.CNoodles <- Cross.Partial.CNoodles <- 
LPM.Marginal.VOil    <- RHS.VOil  <-  Poisson.Marginal.VOil <- Cross.Partial.VOil <- 
LPM.Marginal.Alcohol <-  RHS.Alcohol  <- Poisson.Marginal.Alcohol <- Cross.Partial.Alcohol <- 
LPM.Marginal.BCereal <- RHS.BCereal  <- Poisson.Marginal.BCereal <- Cross.Partial.BCereal <-
LPM.Marginal.Pastries <- RHS.Pastries <-  Poisson.Marginal.Pastries <- Cross.Partial.Pastries <-  
LPM.BP2.Sugar   <- LPM.BP2.Coffee  <- LPM.BP2.Soda    <- LPM.BP2.CNoodles<- LPM.BP2.VOil    <- LPM.BP2.Alcohol <-
  LPM.BP2.BCereal <-  LPM.BP2.Pastries <-
  c(rep(NA, B))

# For the BP values
mean_BP_97 <- mean_BP_99 <- mean_BP_00  <- boot_t <- DiD  <- c(rep(NA, B))

# LPM.Marginal = RHS Derivative 1 & DiD = RHS derivative # 2, #LHS = DID for program participation on food consumption overall  

j = 1
 
while(j <= 500) { #generating the bootstrap
  time <- proc.time()
  sample.analog <- sample.analog.Fun()  
  
  BP.Fun.Results <- BP.Fun()
  sample.analog <- BP.Fun.Results[[1]] 
  
  # making a matrix of just the HH level variables: 
  final.df <- aggregate(sample.analog$BP, by = list(sample.analog$folio, sample.analog$wavenumber), FUN=mean, na.rm=T)
  colnames(final.df) <- c("folio", "wavenumber", "BP")
  
  mean_BP_97[j] <-  mean(final.df$BP[final.df$wavenumber == 1], na.rm = T)
  mean_BP_99[j] <- mean(final.df$BP[final.df$wavenumber == 2], na.rm = T)
  mean_BP_00[j] <- mean(final.df$BP[final.df$wavenumber == 3], na.rm = T)

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
  
  DiD[j] <- (mean(final.df$BP[final.df$wavenumber == 2 & final.df$treatment_household == 1], na.rm=T) - 
               mean(final.df$BP[final.df$wavenumber ==  1 & final.df$treatment_household == 1], na.rm=T)) -
    (mean(final.df$BP[final.df$wavenumber == 2 & final.df$treatment_household == 0], na.rm=T) -
       mean(final.df$BP[final.df$wavenumber == 1 & final.df$treatment_household == 0], na.rm=T))
  
  DiD_reg_summary <- summary(lm(BP ~ treatment_household + wavenumber + I(treatment_household*wavenumber), data = subset(final.df, final.df$wavenumber < 3)))
  
  boot_t[j] = (DiD[j] - 0.240468)/DiD_reg_summary$coefficients[4,2] 
  
  keep.index <- with(final.df, { is.na(hh_log_wages) == FALSE & 
                                 is.na(BP) == FALSE })# & 
                               #  is.na(breakfast.cereal.price_hybrid) == FALSE })
  final.df.subset <- final.df[keep.index, ]
  final.df.subset$BP2 <- final.df.subset$BP^2
  
  # BS 1-7 Animal Products Storage ####
  
  # 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111
  # 1 - Chicken
  temp  <- LPM_ME_Fun_animal("pollo")
  temp2 <- Poisson_ME_Fun_animal("pollo_num_times_consume")
  
  LPM.Marginal.Chicken[j] <- temp[[1]] # 1
  LPM.BP2.Chicken[j] <- temp[[2]] # 2
  RHS.Chicken[j] <- DiD[j]*temp[[1]]   # 3
#  Income.ME.Chicken[j] <- temp[[3]]       # 6
#  Info.ME.Chicken[j] <- temp[[4]]         # 7
  
  Poisson.Marginal.Chicken[j] <- temp2[[1]] # 4
  Cross.Partial.Chicken[j] <- temp2[[2]]    # 5
  
  # 2222222222222222222222222222222222222222222222222222222222222222222222222222222222222
  # 2 - Beef and pork
  temp  <- LPM_ME_Fun_animal("carne.de.res.o.puerco")
  temp2 <- Poisson_ME_Fun_animal("carne.de.res.o.puerco_num_times_consume")
  
  LPM.Marginal.BeefPork[j] <- temp[[1]] # 1
  LPM.BP2.BeefPork[j] <- temp[[2]] 
  RHS.BeefPork[j] <- DiD[j]*temp[[1]]   # 3
#  Income.ME.BeefPork[j] <- temp[[3]]       # 6
#  Info.ME.BeefPork[j] <- temp[[4]]         # 7
  
  Poisson.Marginal.BeefPork[j] <- temp2[[1]] # 4
  Cross.Partial.BeefPork[j] <- temp2[[2]]    # 5
  
  # 333333333333333333333333333333333333333333333333333333333333333333333333333333333333
  # 3 - Eggs
  temp  <- LPM_ME_Fun_animal("huevos")
  temp2 <- Poisson_ME_Fun_animal("huevos_num_times_consume")
  
  LPM.Marginal.Eggs[j] <- temp[[1]] # 1
  LPM.BP2.Eggs[j] <- temp[[2]] 
  RHS.Eggs[j] <- DiD[j]*temp[[1]]   # 3
  #  Income.ME.Eggs[j] <- temp[[3]]       # 6
  #Info.ME.Eggs[j] <- temp[[4]]         # 7
  
  Poisson.Marginal.Eggs[j] <- temp2[[1]] # 4
  Cross.Partial.Eggs[j] <- temp2[[2]]    # 5
  
  # 44444444444444444444444444444444444444444444444444444444444444444444444444444
  # 4 - Milk
  temp  <- LPM_ME_Fun_animal("leche")
  temp2 <- Poisson_ME_Fun_animal("leche_num_times_consume")
  
  LPM.Marginal.Milk[j] <- temp[[1]] # 1
  LPM.BP2.Milk[j] <- temp[[2]] 
  RHS.Milk[j] <- DiD[j]*temp[[1]]   # 3
  # Income.ME.Milk[j] <- temp[[3]]       # 6
  # Info.ME.Milk[j] <- temp[[4]]         # 7
  
  Poisson.Marginal.Milk[j] <- temp2[[1]] # 4
  Cross.Partial.Milk[j] <- temp2[[2]]    # 5
  
  # 55555555555555555555555555555555555555555555555555555555555555555555555555555
  # 5 - Fish
  temp  <- LPM_ME_Fun_animal("pescados.y.mariscos")
  temp2 <- Poisson_ME_Fun_animal("pescados.y.mariscos_num_times_consume")
  
  LPM.Marginal.Fish[j] <- temp[[1]] # 1
  LPM.BP2.Fish[j] <- temp[[2]] 
  RHS.Fish[j] <- DiD[j]*temp[[1]]   # 3
  # Income.ME.Fish[j] <- temp[[3]]       # 6
  # Info.ME.Fish[j] <- temp[[4]]         # 7
  
  Poisson.Marginal.Fish[j] <- temp2[[1]] # 4
  Cross.Partial.Fish[j] <- temp2[[2]]    # 5
  
  # 666666666666666666666666666666666666666666666666666666666666666666666666666666
  # 6 - Canned Sardines and Tuna
  temp  <- LPM_ME_Fun_animal("sardinas.o.atun.en.lata")
  temp2 <- Poisson_ME_Fun_animal("sardinas.o.atun.en.lata_num_times_consume")
  
  LPM.Marginal.Tuna[j] <- temp[[1]] # 1
  LPM.BP2.Tuna[j] <- temp[[2]] 
  RHS.Tuna[j] <- DiD[j]*temp[[1]]   # 3
  # Income.ME.Tuna[j] <- temp[[3]]       # 6
  # Info.ME.Tuna[j] <- temp[[4]]         # 7
  
  Poisson.Marginal.Tuna[j] <- temp2[[1]] # 4
  Cross.Partial.Tuna[j] <- temp2[[2]]    # 5
  
  # 7777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
  # 7 - Lard
  temp  <- LPM_ME_Fun_animal("manteca.de.cerdo")
  temp2 <- Poisson_ME_Fun_animal("manteca.de.cerdo_num_times_consume")
  
  LPM.Marginal.Lard[j] <- temp[[1]] # 1
  LPM.BP2.Lard[j] <- temp[[2]] 
  RHS.Lard[j] <- DiD[j]*temp[[1]]   # 3
  # Income.ME.Lard[j] <- temp[[3]]       # 6
  #  Info.ME.Lard[j] <- temp[[4]]         # 7
    
  Poisson.Marginal.Lard[j] <- temp2[[1]] # 4
  Cross.Partial.Lard[j] <- temp2[[2]]    # 5
  
  # 8 - 16 BS Vegetables and Fruits Storage ####
  
  
  # 88888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888
  # 8 - Onion
  temp  <- LPM_ME_Fun_VF("cebolla")
  temp2 <- Poisson_ME_Fun_VF("cebolla_num_times_consume")
  
  LPM.Marginal.Onion[j] <- temp[[1]] # 1
  LPM.BP2.Onion[j] <- temp[[2]] 
  RHS.Onion[j] <- DiD[j]*temp[[1]]   # 3
  # Income.ME.Onion[j] <- temp[[3]]       # 6
  #  Info.ME.Onion[j] <- temp[[4]]         # 7
  
  Poisson.Marginal.Onion[j] <- temp2[[1]] # 4
  Cross.Partial.Onion[j] <- temp2[[2]]    # 5
  
  # 99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999
  # 9 - Lime
  temp  <- LPM_ME_Fun_VF("limones")
  temp2 <- Poisson_ME_Fun_VF("limones_num_times_consume")
  
  LPM.Marginal.Lime[j] <- temp[[1]] # 1
  LPM.BP2.Lime[j] <- temp[[2]] 
  RHS.Lime[j] <- DiD[j]*temp[[1]]   # 3
  #Income.ME.Lime[j] <- temp[[3]]       # 6
  #Info.ME.Lime[j] <- temp[[4]]         # 7
  
  Poisson.Marginal.Lime[j] <- temp2[[1]] # 4
  Cross.Partial.Lime[j] <- temp2[[2]]    # 5
  
  # 10101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010
  # 10 - Apples
  temp  <- LPM_ME_Fun_VF("manzanas")
  temp2 <- Poisson_ME_Fun_VF("manzanas_num_times_consume")
  
  LPM.Marginal.Apple[j] <- temp[[1]] # 1
  LPM.BP2.Apple[j] <- temp[[2]] 
  RHS.Apple[j] <- DiD[j]*temp[[1]]   # 3
  # Income.ME.Apple[j] <- temp[[3]]       # 6
  # Info.ME.Apple[j] <- temp[[4]]         # 7
  
  Poisson.Marginal.Apple[j] <- temp2[[1]] # 4
  Cross.Partial.Apple[j] <- temp2[[2]]    # 5
  
  # 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
  # 11 - Oranges
  temp  <- LPM_ME_Fun_VF("narajas")
  temp2 <- Poisson_ME_Fun_VF("narajas_num_times_consume")
  
  LPM.Marginal.Orange[j] <- temp[[1]] # 1
  LPM.BP2.Orange[j] <- temp[[2]] 
  RHS.Orange[j] <- DiD[j]*temp[[1]]   # 3
  #Income.ME.Orange[j] <- temp[[3]]       # 6
  # Info.ME.Orange[j] <- temp[[4]]         # 7
  
  Poisson.Marginal.Orange[j] <- temp2[[1]] # 4
  Cross.Partial.Orange[j] <- temp2[[2]]    # 5
  
  # 12121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212
  # 12 - Potatos
  temp  <- LPM_ME_Fun_VF("papa")
  temp2 <- Poisson_ME_Fun_VF("papa_num_times_consume")
  
  LPM.Marginal.Potato[j] <- temp[[1]] # 1
  LPM.BP2.Potato[j] <- temp[[2]] 
  RHS.Potato[j] <- DiD[j]*temp[[1]]   # 3
  # Income.ME.Potato[j] <- temp[[3]]       # 6
  # Info.ME.Potato[j] <- temp[[4]]         # 7
  
  Poisson.Marginal.Potato[j] <- temp2[[1]] # 4
  Cross.Partial.Potato[j] <- temp2[[2]]    # 5
  
  # 13131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313
  # 13 - Bananas
  temp  <- LPM_ME_Fun_VF("platanos")
  temp2 <- Poisson_ME_Fun_VF("platanos_num_times_consume")
  
  LPM.Marginal.Banana[j] <- temp[[1]] # 1
  LPM.BP2.Banana[j] <- temp[[2]] 
  RHS.Banana[j] <- DiD[j]*temp[[1]]   # 3
  #Income.ME.Banana[j] <- temp[[3]]       # 6
  # Info.ME.Banana[j] <- temp[[4]]         # 7
  
  Poisson.Marginal.Banana[j] <- temp2[[1]] # 4
  Cross.Partial.Banana[j] <- temp2[[2]]    # 5
  
  # 14141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414
  # 14 - Leafy Greens
  temp  <- LPM_ME_Fun_VF("verdudas.de.hoja")
  temp2 <- Poisson_ME_Fun_VF("verdudas.de.hoja_num_times_consume")
  
  LPM.Marginal.Greens[j] <- temp[[1]] # 1
  LPM.BP2.Greens[j] <- temp[[2]] 
  RHS.Greens[j] <- DiD[j]*temp[[1]]   # 3
  # Income.ME.Greens[j] <- temp[[3]]       # 6
  #  Info.ME.Greens[j] <- temp[[4]]         # 7
  
  Poisson.Marginal.Greens[j] <- temp2[[1]] # 4
  Cross.Partial.Greens[j] <- temp2[[2]]    # 5
  
  # 1515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515
  # 15 - Carrots
  temp  <- LPM_ME_Fun_VF("zanahorias")
  temp2 <- Poisson_ME_Fun_VF("zanahorias_num_times_consume")
  
  LPM.Marginal.Carrots[j] <- temp[[1]] # 1
  LPM.BP2.Carrots[j] <- temp[[2]] 
  RHS.Carrots[j] <- DiD[j]*temp[[1]]   # 3
  # Income.ME.Carrots[j] <- temp[[3]]       # 6
  # Info.ME.Carrots[j] <- temp[[4]]         # 7
  
  Poisson.Marginal.Carrots[j] <- temp2[[1]] # 4
  Cross.Partial.Carrots[j] <- temp2[[2]]    # 5
  
  # 1616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616
  # 16 - Tomato
  temp  <- LPM_ME_Fun_VF("tomate.rojo")
  temp2 <- Poisson_ME_Fun_VF("tomate.rojo_num_times_consume")
  
  LPM.Marginal.Tomato[j] <- temp[[1]] # 1
  LPM.BP2.Tomato[j] <- temp[[2]] 
  RHS.Tomato[j] <- DiD[j]*temp[[1]]   # 3
  # Income.ME.Tomato[j] <- temp[[3]]       # 6
  # Info.ME.Tomato[j] <- temp[[4]]         # 7
  
  Poisson.Marginal.Tomato[j] <- temp2[[1]] # 4
  Cross.Partial.Tomato[j] <- temp2[[2]]    # 5
  
  # BS 17-24 Grains Storage ####
  
  # 171717171717171717171717171717171717171717171717171717171717171717171717171717171717171717
  # 17 - Rice
  temp  <- LPM_ME_Fun_VF("arroz")
  temp2 <- Poisson_ME_Fun_VF("arroz_num_times_consume")
  
  LPM.Marginal.Rice[j] <- temp[[1]] # 1
  LPM.BP2.Rice[j] <- temp[[2]] 
  RHS.Rice[j] <- DiD[j]*temp[[1]]   # 3
  # Income.ME.Rice[j] <- temp[[3]]       # 6
  # Info.ME.Rice[j] <- temp[[4]]         # 7
  
  Poisson.Marginal.Rice[j] <- temp2[[1]] # 4
  Cross.Partial.Rice[j] <- temp2[[2]]    # 5
  
  # 181818181818181818181818181818181818181818181818181818181818181818181818181818181818181818
  # 18 - Beans
  temp  <- LPM_ME_Fun_VF("frijol")
  temp2 <- Poisson_ME_Fun_VF("frijol_num_times_consume")
  
  LPM.Marginal.Beans[j] <- temp[[1]] # 1
  LPM.BP2.Beans[j] <- temp[[2]] 
  RHS.Beans[j] <- DiD[j]*temp[[1]]   # 3
  # Income.ME.Beans[j] <- temp[[3]]       # 6
  # Info.ME.Beans[j] <- temp[[4]]         # 7
  
  Poisson.Marginal.Beans[j] <- temp2[[1]] # 4
  Cross.Partial.Beans[j] <- temp2[[2]]    # 5
  
  # 19191919191919191919191919191919191919191919191919191919191919191919191919191919191919191919
  # 19 - Digestive Biscuits
  temp  <- LPM_ME_Fun_VF("galletas")
  temp2 <- Poisson_ME_Fun_VF("galletas_num_times_consume")
  
  LPM.Marginal.Biscuits[j] <- temp[[1]] # 1
  LPM.BP2.Biscuits[j] <- temp[[2]] 
  RHS.Biscuits[j] <- DiD[j]*temp[[1]]   # 3
  # Income.ME.Biscuits[j] <- temp[[3]]       # 6
  # Info.ME.Biscuits[j] <- temp[[4]]         # 7
  
  Poisson.Marginal.Biscuits[j] <- temp2[[1]] # 4
  Cross.Partial.Biscuits[j] <- temp2[[2]]    # 5
  
  # 20202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020
  # 20 - Corn Flour
  temp  <- LPM_ME_Fun_VF("maiz.en.grano")
  temp2 <- Poisson_ME_Fun_VF("maiz.en.grano_num_times_consume")
  
  LPM.Marginal.CFlour[j] <- temp[[1]] # 1
  LPM.BP2.CFlour[j] <- temp[[2]] 
  RHS.CFlour[j] <- DiD[j]*temp[[1]]   # 3
  #  Income.ME.CFlour[j] <- temp[[3]]       # 6
  #  Info.ME.CFlour[j] <- temp[[4]]         # 7
  
  Poisson.Marginal.CFlour[j] <- temp2[[1]] # 4
  Cross.Partial.CFlour[j] <- temp2[[2]]    # 5
  
  # 21212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121
  # 21 - White Bread
  temp  <- LPM_ME_Fun_VF("pan.blanco")
  temp2 <- Poisson_ME_Fun_VF("pan.blanco_num_times_consume")
  
  LPM.Marginal.WBread[j] <- temp[[1]] # 1
  LPM.BP2.WBread[j] <- temp[[2]] 
  RHS.WBread[j] <- DiD[j]*temp[[1]]   # 3
  # Income.ME.WBread[j] <- temp[[3]]       # 6
  # Info.ME.WBread[j] <- temp[[4]]         # 7
  
  Poisson.Marginal.WBread[j] <- temp2[[1]] # 4
  Cross.Partial.WBread[j] <- temp2[[2]]    # 5
  
  # 222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222
  # 22 - Pastries
  temp  <- LPM_ME_Fun_VF("pan.de.dulce")
  temp2 <- Poisson_ME_Fun_VF("pan.de.dulce_num_times_consume")
  
  LPM.Marginal.Pastries[j] <- temp[[1]] # 1
  LPM.BP2.Pastries[j] <- temp[[2]] 
  RHS.Pastries[j] <- DiD[j]*temp[[1]]   # 3
  # Income.ME.Pastries[j] <- temp[[3]]       # 6
  # Info.ME.Pastries[j] <- temp[[4]]         # 7
  
  Poisson.Marginal.Pastries[j] <- temp2[[1]] # 4
  Cross.Partial.Pastries[j] <- temp2[[2]]    # 5
  
  # 232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323
  # 23 - Tortillas
  temp  <- LPM_ME_Fun_VF("tortialls.de.maiz")
  temp2 <- Poisson_ME_Fun_VF("tortialls.de.maiz_num_times_consume")
  
  LPM.Marginal.Tortillas[j] <- temp[[1]] # 1
  LPM.BP2.Tortillas[j] <- temp[[2]] 
  RHS.Tortillas[j] <- DiD[j]*temp[[1]]   # 3
  # Income.ME.Tortillas[j] <- temp[[3]]       # 6
  # Info.ME.Tortillas[j] <- temp[[4]]         # 7
  
  Poisson.Marginal.Tortillas[j] <- temp2[[1]] # 4
  Cross.Partial.Tortillas[j] <- temp2[[2]]    # 5
  
  # 2424242424242424242424242424242424242424242424242424242424242424242424242424242424242424242424
  # 24 - Wheat Flour
  temp  <- LPM_ME_Fun_VF("harina.de.trigo")
  temp2 <- Poisson_ME_Fun_VF("harina.de.trigo_num_times_consume")
  
  LPM.Marginal.WFlour[j] <- temp[[1]] # 1
  LPM.BP2.WFlour[j] <- temp[[2]] 
  RHS.WFlour[j] <- DiD[j]*temp[[1]]   # 3
  # Income.ME.Tortillas[j] <- temp[[3]]       # 6
  # Info.ME.Tortillas[j] <- temp[[4]]         # 7
  
  Poisson.Marginal.WFlour[j] <- temp2[[1]] # 4
  Cross.Partial.WFlour[j] <- temp2[[2]]    # 5
  
  # BS 25- 31 Miscellaneous Storage ####
  
  # 252525252525252525252525252525252525252525252525252525252525252525252525252525252525252525
  # 25 - Sugar
  temp  <- LPM_ME_Fun_VF("azucar")
  temp2 <- Poisson_ME_Fun_VF("azucar_num_times_consume")
  
  LPM.Marginal.Sugar[j] <- temp[[1]] # 1
  LPM.BP2.Sugar[j] <- temp[[2]] 
  RHS.Sugar[j] <- DiD[j]*temp[[1]]   # 3
  Poisson.Marginal.Sugar[j] <- temp2[[1]] # 4
  Cross.Partial.Sugar[j] <- temp2[[2]]    # 5
  
  # 2626262626262626262626262626262626262626262626262626262626262626262626262626262626262626262626
  # 26 - Coffee
  temp  <- LPM_ME_Fun_VF("cafe")
  temp2 <- Poisson_ME_Fun_VF("cafe_num_times_consume")
  
  LPM.Marginal.Coffee[j] <- temp[[1]] # 1
  LPM.BP2.Coffee[j] <- temp[[2]] 
  RHS.Coffee[j] <- DiD[j]*temp[[1]]   # 3
  Poisson.Marginal.Coffee[j] <- temp2[[1]] # 4
  Cross.Partial.Coffee[j] <- temp2[[2]]    # 5
  
  # 2727272727272727272727272727272727272727272727272727272727272727272727272727272727272727272727
  # 27 - Soda
  temp  <- LPM_ME_Fun_VF("refrescos")
  temp2 <- Poisson_ME_Fun_VF("refrescos_num_times_consume")
  
  LPM.Marginal.Soda[j] <- temp[[1]] # 1
  LPM.BP2.Soda[j] <- temp[[2]] 
  RHS.Soda[j] <- DiD[j]*temp[[1]]   # 3
  Poisson.Marginal.Soda[j] <- temp2[[1]] # 4
  Cross.Partial.Soda[j] <- temp2[[2]]    # 5
  
  
  # 282828282828282828282828282828282828282828282828282828282828282828282828282828282828282828282828
  # 28 - Cup Noodles
  temp  <- LPM_ME_Fun_VF("sopa.de.pasta")
  temp2 <- Poisson_ME_Fun_VF("sopa.de.pasta_num_times_consume")
  
  LPM.Marginal.CNoodles[j] <- temp[[1]] # 1
  LPM.BP2.CNoodles[j] <- temp[[2]] 
  RHS.CNoodles[j] <- DiD[j]*temp[[1]]   # 3
  Poisson.Marginal.CNoodles[j] <- temp2[[1]] # 4
  Cross.Partial.CNoodles[j] <- temp2[[2]]    # 5
  
  # 292929292929292929292929292929292929292929292929292929292929292929292929292929292929292929292929
  # 29 - Vegetable Oil
  temp  <- LPM_ME_Fun_VF("aciete.vegetal")
  temp2 <- Poisson_ME_Fun_VF("aciete.vegetal_num_times_consume")
  
  LPM.Marginal.VOil[j] <- temp[[1]] # 1
  LPM.BP2.VOil[j] <- temp[[2]] 
  RHS.VOil[j] <- DiD[j]*temp[[1]]   # 3
  Poisson.Marginal.VOil[j] <- temp2[[1]] # 4
  Cross.Partial.VOil[j] <- temp2[[2]]    # 5
  
  # 3030303030303030303030303030303030303030303030303030303030303030303030303030303030303030303030
  # 30 - Alcohol
  temp  <- LPM_ME_Fun_VF("bebidas.alcoholicas")
  temp2 <- Poisson_ME_Fun_VF("bebidas.alcoholicas_num_times_consume")
  
  LPM.Marginal.Alcohol[j] <- temp[[1]] # 1
  LPM.BP2.Alcohol[j] <- temp[[2]] 
  RHS.Alcohol[j] <- DiD[j]*temp[[1]]   # 3
  Poisson.Marginal.Alcohol[j] <- temp2[[1]] # 4
  Cross.Partial.Alcohol[j] <- temp2[[2]]    # 5
  
  
  # 3131313131313131313131313131313131313131313131313131313131313131313131313131313131313131313131
  # 31 - Breakfast Cereal
  temp  <- LPM_ME_Fun_VF("cereales.de.caja")
  temp2 <- Poisson_ME_Fun_VF("cereales.de.caja_num_times_consume")
  
  LPM.Marginal.BCereal[j] <- temp[[1]] # 1
  LPM.BP2.BCereal[j] <- temp[[2]] 
  RHS.BCereal[j] <- DiD[j]*temp[[1]]   # 3
  Poisson.Marginal.BCereal[j] <- temp2[[1]] # 4
  Cross.Partial.BCereal[j] <- temp2[[2]]    # 5
  

  j <- j+1
  print(j)
  print(proc.time() - time)
}

# Saving all the Bootstrap Estimates into a single matrix, and writing that matrix to a csv file #### 

boot <- as.data.frame(cbind(mean_BP_97, mean_BP_99, mean_BP_00, boot_t,  DiD, # BP Values
                            # saving: Animal Products
                            LPM.Marginal.Lard  , RHS.Lard  ,          Poisson.Marginal.Lard , Cross.Partial.Lard ,
                              LPM.Marginal.Tuna  , RHS.Tuna  ,        Poisson.Marginal.Tuna , Cross.Partial.Tuna , 
                              LPM.Marginal.Fish  , RHS.Fish  ,        Poisson.Marginal.Fish , Cross.Partial.Fish , 
                              LPM.Marginal.Milk  , RHS.Milk  ,        Poisson.Marginal.Milk , Cross.Partial.Milk , 
                              LPM.Marginal.Eggs  , RHS.Eggs  ,        Poisson.Marginal.Eggs , Cross.Partial.Eggs , 
                              LPM.Marginal.Chicken  , RHS.Chicken ,   Poisson.Marginal.Chicken , Cross.Partial.Chicken , 
                              LPM.Marginal.BeefPork , RHS.BeefPork  , Poisson.Marginal.BeefPork , Cross.Partial.BeefPork ,  
                            # saving: Fruits and Vegetabels 
                            LPM.Marginal.Tomato , RHS.Tomato  ,      Poisson.Marginal.Tomato , Cross.Partial.Tomato , 
                              LPM.Marginal.Carrots , RHS.Carrots ,   Poisson.Marginal.Carrots , Cross.Partial.Carrots , 
                              LPM.Marginal.Greens  , RHS.Greens  ,   Poisson.Marginal.Greens , Cross.Partial.Greens , 
                              LPM.Marginal.Banana  , RHS.Banana  ,   Poisson.Marginal.Banana , Cross.Partial.Banana , 
                              LPM.Marginal.Potato  ,  RHS.Potato  ,  Poisson.Marginal.Potato , Cross.Partial.Potato , 
                              LPM.Marginal.Orange  , RHS.Orange  ,   Poisson.Marginal.Orange , Cross.Partial.Orange , 
                              LPM.Marginal.Apple  , RHS.Apple  ,     Poisson.Marginal.Apple , Cross.Partial.Apple , 
                              LPM.Marginal.Onion  , RHS.Onion  ,     Poisson.Marginal.Onion ,  Cross.Partial.Onion , 
                              LPM.Marginal.Lime  , RHS.Lime  ,       Poisson.Marginal.Lime , Cross.Partial.Lime ,
                            # saving: Grains
                            LPM.Marginal.WFlour    ,  RHS.WFlour  ,    Poisson.Marginal.WFlour , Cross.Partial.WFlour , 
                              LPM.Marginal.CFlour    , RHS.CFlour  ,   Poisson.Marginal.CFlour , Cross.Partial.CFlour , 
                              LPM.Marginal.Rice      , RHS.Rice  ,     Poisson.Marginal.Rice , Cross.Partial.Rice , 
                              LPM.Marginal.Beans     , RHS.Beans  ,    Poisson.Marginal.Beans , Cross.Partial.Beans , 
                              LPM.Marginal.Biscuits  , RHS.Biscuits  , Poisson.Marginal.Biscuits , Cross.Partial.Biscuits , 
                              LPM.Marginal.WBread , RHS.WBread ,       Poisson.Marginal.WBread , Cross.Partial.WBread, 
                              LPM.Marginal.Tortillas , RHS.Tortillas , Poisson.Marginal.Tortillas , Cross.Partial.Tortillas ,
                            # saving: Misc
                            LPM.Marginal.Sugar   , RHS.Sugar  ,      Poisson.Marginal.Sugar , Cross.Partial.Sugar ,
                              LPM.Marginal.Coffee  , RHS.Coffee  ,   Poisson.Marginal.Coffee , Cross.Partial.Coffee ,
                              LPM.Marginal.Soda    , RHS.Soda  ,     Poisson.Marginal.Soda , Cross.Partial.Soda ,
                              LPM.Marginal.CNoodles, RHS.CNoodles  , Poisson.Marginal.CNoodles , Cross.Partial.CNoodles , 
                              LPM.Marginal.VOil    , RHS.VOil  ,     Poisson.Marginal.VOil , Cross.Partial.VOil , 
                              LPM.Marginal.Alcohol ,  RHS.Alcohol  , Poisson.Marginal.Alcohol , Cross.Partial.Alcohol , 
                              LPM.Marginal.Pastries , RHS.Pastries , Poisson.Marginal.Pastries , Cross.Partial.Pastries, 
                              LPM.Marginal.BCereal , RHS.BCereal  ,  Poisson.Marginal.BCereal , Cross.Partial.BCereal, 
                            # Saving the BP2 Results. Added 03/05/2019
                              LPM.BP2.Lard    , LPM.BP2.Tuna  , LPM.BP2.Fish  , LPM.BP2.Milk  ,
                              LPM.BP2.Eggs  , LPM.BP2.Chicken , LPM.BP2.BeefPork ,
                              LPM.BP2.Tomato , LPM.BP2.Carrots, LPM.BP2.Greens , LPM.BP2.Banana , 
                              LPM.BP2.Potato , 
                              LPM.BP2.Orange , LPM.BP2.Apple  , LPM.BP2.Onion  , LPM.BP2.Lime  , 
                              LPM.BP2.WFlour   , LPM.BP2.CFlour   , LPM.BP2.Rice     , LPM.BP2.Beans, 
                              LPM.BP2.Biscuits , 
                              LPM.BP2.WBread   , LPM.BP2.Tortillas,
                              LPM.BP2.Sugar   , LPM.BP2.Coffee  , LPM.BP2.Soda    , LPM.BP2.CNoodles, 
                              LPM.BP2.VOil    , LPM.BP2.Alcohol ,
                              LPM.BP2.BCereal ,  LPM.BP2.Pastries))

write.csv(boot, file = "Bootstrap_Results_03_05_19.csv")

# Generating the Efron Intervals ####

# Did Progresa increase women's bargaining power? 
critical.pivot <- 0.240468 - ( 0.003846 * quantile(boot_t,probs=c(.975,.025)))
critical.pivot # Yes, we reject at the 95% level the null that it did not. 

# Did Progresa increase the associated 
quantile(boot[,"DiD"],probs=c(.05,.95))


# LPM Animals:
map(.x = list(LPM.Marginal.Lard  ,  LPM.Marginal.Tuna  , LPM.Marginal.Fish  , LPM.Marginal.Milk  , 
        LPM.Marginal.Eggs  , LPM.Marginal.Chicken, LPM.Marginal.BeefPork), 
    .f = function(x) { 
      quantile(x ,probs=c(.025,.975))})

# LPM Veg: 
map(.x = list(LPM.Marginal.Tomato , 
              LPM.Marginal.Carrots ,
              LPM.Marginal.Greens  ,
              LPM.Marginal.Banana  ,
              LPM.Marginal.Potato  ,
              LPM.Marginal.Orange  ,
              LPM.Marginal.Apple  ,
              LPM.Marginal.Onion  ,
              LPM.Marginal.Lime), 
    .f = function(x) { 
      quantile(x ,probs=c(.025,.975))})



map(.x = list(LPM.Marginal.WFlour   , 
              LPM.Marginal.CFlour    ,
              LPM.Marginal.Rice     ,
              LPM.Marginal.Beans    ,
              LPM.Marginal.Biscuits ,
              LPM.Marginal.WBread , 
              LPM.Marginal.Tortillas ),
    .f = function(x) { 
      quantile(x ,probs=c(.05,.95))})

        
map(.x = list( LPM.Marginal.Sugar   ,
               LPM.Marginal.Coffee  ,
               LPM.Marginal.Soda    ,
               LPM.Marginal.CNoodles,
               LPM.Marginal.VOil    ,
               LPM.Marginal.Alcohol ,
               LPM.Marginal.Pastries,
               LPM.Marginal.BCereal ), 
    .f = function(x) { 
      quantile(x ,probs=c(.05,.95))})

# Intensive Margin

# Animal Products
map(.x = list( Poisson.Marginal.Lard ,
               Poisson.Marginal.Tuna ,
               Poisson.Marginal.Fish ,
               Poisson.Marginal.Milk ,
               Poisson.Marginal.Eggs ,
               Poisson.Marginal.Chicken,
               Poisson.Marginal.BeefPork), 
    .f = function(x) { 
      quantile(x ,probs=c(.05,.95))})


# Fruit and veg
map(.x = list( Poisson.Marginal.Tomato , 
               Poisson.Marginal.Carrots ,
               Poisson.Marginal.Greens , 
               Poisson.Marginal.Banana , 
               Poisson.Marginal.Potato , 
               Poisson.Marginal.Orange , 
               Poisson.Marginal.Apple , 
               Poisson.Marginal.Onion ,  
               Poisson.Marginal.Lime), 
    .f = function(x) { 
      quantile(x ,probs=c(.05,.95))})


map(.x = list( Poisson.Marginal.WFlour , 
               Poisson.Marginal.CFlour , 
               Poisson.Marginal.Rice , 
               Poisson.Marginal.Beans , 
               Poisson.Marginal.Biscuits ,
               Poisson.Marginal.WBread , 
               Poisson.Marginal.Tortillas), 
    .f = function(x) { 
      quantile(x ,probs=c(.05,.95))})

map(.x = list( Poisson.Marginal.Sugar , 
               Poisson.Marginal.Coffee ,
               Poisson.Marginal.Soda , 
               Poisson.Marginal.CNoodles,
               Poisson.Marginal.VOil , 
               Poisson.Marginal.Alcohol ,
               Poisson.Marginal.Pastries, 
               Poisson.Marginal.BCereal ), 
    .f = function(x) { 
      quantile(x ,probs=c(.05,.95))})











          
        
        
        
        
        
    



quantile(p.sorted[,"LPM.Marginal"],probs=c(.025,.975))
quantile(p.sorted[,"Poisson.Marginal"],probs=c(.025,.975))

quantile(p.sorted[,"LPM.BP.sqr.coef.est"],probs=c(.025,.975))
quantile(p.sorted[,"Cross_Partial"],probs=c(.025,.975))

quantile(p.sorted[,"RHS"],probs=c(.025,.975))
quantile(p.sorted[,"DiD"],probs=c(.025,.975))
quantile(p.sorted[,"boot_t"],probs=c(.025,.975))




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

