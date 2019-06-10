# Mjklein 
# Madison, Wisc.

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
    (1/2) + (1/2)*((
      (sample.analog$Mom_SW_combined[sample.analog$wavenumber == 1] + 
         sample.analog$T_mom_total[sample.analog$wavenumber == 1]) - 
        (sample.analog$Dad_SW_combined[sample.analog$wavenumber == 1] + 
           sample.analog$T_dad_total[sample.analog$wavenumber == 1])) / 
        (sample.analog$hh_wages[sample.analog$wavenumber == 1]))
  
  sample.analog$BP[sample.analog$wavenumber == 2] <-
    (1/2) + (1/2)*((
      (sample.analog$Mom_SW_combined[sample.analog$wavenumber == 2] + 
         sample.analog$T_mom_total[sample.analog$wavenumber == 2]) - 
        (sample.analog$Dad_SW_combined[sample.analog$wavenumber == 2] + 
           sample.analog$T_dad_total[sample.analog$wavenumber == 2])) / 
        (sample.analog$hh_wages[sample.analog$wavenumber == 2]))
  
  
  sample.analog$BP[sample.analog$wavenumber == 3] <-
    (1/2) + (1/2)*((
      (sample.analog$Mom_SW_combined[sample.analog$wavenumber == 3] + 
         sample.analog$T_mom_total[sample.analog$wavenumber == 3]) - 
        (sample.analog$Dad_SW_combined[sample.analog$wavenumber == 3] + 
           sample.analog$T_dad_total[sample.analog$wavenumber == 3])) / 
        (sample.analog$hh_wages[sample.analog$wavenumber == 3]))
  
  # The Kuhn-tucker conditions don't allow for eta < 0 or eta > 1 (see [lambda_3] and [lambda_4] in Section 3)
  sample.analog$BP[sample.analog$BP <= 0] <- 0  
  sample.analog$BP[sample.analog$BP >= 1] <- 1
  
  return(list(sample.analog,  
              mean(sample.analog$BP[sample.analog$wave1 == 1], na.rm = T), 
              mean(sample.analog$BP[sample.analog$wave2 == 1], na.rm = T),
              mean(sample.analog$BP[sample.analog$wave3 == 1], na.rm = T)))  
} 




# Estimating BP for each HH in each period ####

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
                               #    "AD_equal_rights", "AD_women_in_home", "AD_obedience",
                              #     "AD_say_comm", "AD_women_opinions","AD_women_job",
                                   # "accompanied", 
                                   "need_permission",
                                   "receive_progresa", "unique_loc_id", "num_f_adults", "num_m_adults", 
                              "pobre", "pobextre")],
                   final.df, by =   c("folio", "wavenumber")))

summary(final.df$BP[final.df$wavenumber == 1])

# Chapter 2 - loading in the other decision making variables ####

library("foreign")

load("hh.df.Rda") # This data.frame is generated in "Final Cleaning for Master Panel Construction.R"
hh98.df <- read.spss('socioec_encel_98m.sav', use.value.labels = FALSE, to.data.frame = TRUE)
hh99m.df <- read.spss('socioec_encel_99m.sav', use.value.labels = FALSE, to.data.frame = TRUE)
#hh00.df <- read.spss('socioec_encel_2000n.sav', use.value.labels = FALSE, to.data.frame = TRUE)

# In this pipe, we select the columns we want from hh98.df, then we aggregate the information to the HH level 
# Rename the columns at the same time.
sub.98 <- hh98.df %>% 
  select(folio, p05003, p05004, p05005, p05006, p05007, p05008, p05009, 
         p05010, p127, p128, p129, p130, p131, p132, p133, p13401, p13402, 
         p13403, p13701, p13702, p13703, p13704, p13705, p13706)  %>%
  group_by(folio) %>%  # This command aggregates all of the following variables and renames them
  summarize(DM_who_tells_sick_kid = mean(p127, na.rm=T),
            DM_tells_kid_school = mean(p128, na.rm=T),
            DM_spend_women_income = mean(p129, na.rm=T),
            DM_HH_fix_expenditures = mean(p130, na.rm=T),
            DM_buy_kids_shoes = mean(p131, na.rm=T),
            DM_control_small_livestock = mean(p132, na.rm=T),
            DM_decides_garden = mean(p133, na.rm=T),
            DM_allowed_visit_parents = mean(p13401, na.rm=T), 
            DM_allowed_visit_friends = mean(p13401, na.rm=T),
            DM_not_allowed_visit = mean(p13401, na.rm=T), 
            PAG_girls_clothing = mean(p05003, na.rm=T),
            PAG_boys_clothing = mean(p05004, na.rm=T),
            PAG_womens_clothing = mean(p05005, na.rm=T),
            PAG_mens_clothing = mean(p05006, na.rm=T),
            PAG_girls_shoes = mean(p05007, na.rm=T),
            PAG_boys_shoes = mean(p05008, na.rm=T),
            PAG_womens_shoes = mean(p05009, na.rm=T),
            PAG_mens_shoes = mean(p05010, na.rm=T),
            AD_women_in_home = mean(p13701, na.rm=T), 
            AD_obedience = mean(p13702, na.rm=T), 
            AD_say_comm = mean(p13703, na.rm=T),
            AD_women_job = mean(p13704, na.rm=T), 
            AD_equal_rights= mean(p13705, na.rm=T), 
            AD_women_opinions = mean(p13706, na.rm=T))                                     

sub.98$wavenumber <- rep(1)

#sub.98.land <- hh98.df %>% select()

sub.99 <- hh99m.df %>% 
  select(folio, m10904, m10905, m11001, m11002, m11003, m11004, m11005, m11006, 
         m137, m138, m139, m140, m141, m142, m143, m144, m14801, m14802, m14803, 
         m14804, m14805, m14806)  %>%
  group_by(folio) %>%  # This command aggregates all of the following variables and renames them
  summarize(DM_who_tells_sick_kid = mean(m137, na.rm=T),
            DM_tells_kid_school = mean(m138, na.rm=T),
            DM_kids_leave_house = mean(m139, na.rm=T),
            DM_buy_kids_shoes = mean(m140, na.rm=T),
            DM_buy_food = mean(m141, na.rm=T),
            DM_HH_fix_expenditures = mean(m142, na.rm=T),
            DM_HH_durable_purchase = mean(m143, na.rm=T),
            DM_spend_women_income = mean(m144, na.rm=T), 
            PAG_girls_clothing = mean(m10904, na.rm=T),
            PAG_boys_clothing = mean(m10905, na.rm=T),
            PAG_womens_clothing = mean(m11001, na.rm=T),
            PAG_mens_clothing = mean(m11002, na.rm=T),
            PAG_girls_shoes = mean(m11003, na.rm=T),
            PAG_boys_shoes = mean(m11004, na.rm=T),
            PAG_womens_shoes = mean(m11005, na.rm=T),
            PAG_mens_shoes = mean(m11006, na.rm=T), 
            AD_women_in_home = mean(m14801, na.rm=T), 
            AD_obedience = mean(m14802, na.rm=T), 
            AD_say_comm = mean(m14803, na.rm=T),
            AD_women_job = mean(m14804, na.rm=T), 
            AD_equal_rights= mean(m14805, na.rm=T), 
            AD_women_opinions = mean(m14806, na.rm=T))    

sub.99$wavenumber <- rep(2)

sub <- bind_rows(sub.98, sub.99)

final.df <- left_join(final.df, sub, by = c("folio", "wavenumber"))

# Making a total HH size variable

hh.size <- hh.df %>% group_by(wavenumber) %>% count(folio) 
colnames(hh.size) <- c("wavenumber", "folio", "hh_size")

final.df <- left_join(final.df, hh.size, by = c("folio", "wavenumber"))

# Chapter 3 - Variable Construction and regressions ####

library(dummies)
final.df$AD_equal_rights[final.df$AD_equal_rights != 1] <- 0
final.df$AD_women_in_home[final.df$AD_women_in_home != 1] <- 0
final.df$AD_obedience[final.df$AD_obedience != 1] <- 0
final.df$AD_say_comm[final.df$AD_say_comm != 1] <- 0
final.df$AD_women_opinions[final.df$AD_women_opinions != 1] <- 0
final.df$AD_women_job[final.df$AD_women_job != 1] <- 0

final.df$need_permission_dummy <- 0
final.df$need_permission_dummy[final.df$need_permission == 1] <- 1

# Dummify and Generate Summary Stats Table 

# 1
final.df$DM_HH_fix_expenditures_M <- as.tibble(dummy(final.df$DM_HH_fix_expenditures))[[1]]  
final.df$DM_HH_fix_expenditures_W <- as.tibble(dummy(final.df$DM_HH_fix_expenditures))[[2]]
final.df$DM_HH_fix_expenditures_T <- as.tibble(dummy(final.df$DM_HH_fix_expenditures))[[3]]
# 2
final.df$DM_tells_kid_school_M <- as.tibble(dummy(final.df$DM_tells_kid_school))[[1]]  
final.df$DM_tells_kid_school_W <- as.tibble(dummy(final.df$DM_tells_kid_school))[[2]]
final.df$DM_tells_kid_school_T <- as.tibble(dummy(final.df$DM_tells_kid_school))[[3]]
# 3
final.df$DM_spend_women_income_M <- as.tibble(dummy(final.df$DM_spend_women_income))[[1]]  
final.df$DM_spend_women_income_W <- as.tibble(dummy(final.df$DM_spend_women_income))[[2]]
final.df$DM_spend_women_income_T <- as.tibble(dummy(final.df$DM_spend_women_income))[[3]]
# 4
final.df$DM_who_tells_sick_kid_M <- as.tibble(dummy(final.df$DM_who_tells_sick_kid))[[1]]  
final.df$DM_who_tells_sick_kid_W <- as.tibble(dummy(final.df$DM_who_tells_sick_kid))[[2]]
final.df$DM_who_tells_sick_kid_T <- as.tibble(dummy(final.df$DM_who_tells_sick_kid))[[3]]
# 5  
final.df$DM_buy_kids_shoes_M <- as.tibble(dummy(final.df$DM_buy_kids_shoes))[[1]]  
final.df$DM_buy_kids_shoes_W <- as.tibble(dummy(final.df$DM_buy_kids_shoes))[[2]]
final.df$DM_buy_kids_shoes_T <- as.tibble(dummy(final.df$DM_buy_kids_shoes))[[3]]
# 6
final.df$DM_control_small_livestock_M <- as.tibble(dummy(final.df$DM_control_small_livestock))[[1]]  
final.df$DM_control_small_livestock_W <- as.tibble(dummy(final.df$DM_control_small_livestock))[[2]]
final.df$DM_control_small_livestock_T <- as.tibble(dummy(final.df$DM_control_small_livestock))[[3]]
# 7
final.df$DM_decides_garden_M <- as.tibble(dummy(final.df$DM_decides_garden))[[1]]  
final.df$DM_decides_garden_W <- as.tibble(dummy(final.df$DM_decides_garden))[[2]]
final.df$DM_decides_garden_T <- as.tibble(dummy(final.df$DM_decides_garden))[[3]]

final.df$DM_count_W <- rowSums(cbind(final.df$DM_HH_fix_expenditures_W, final.df$DM_tells_kid_school_W, 
                                     final.df$DM_buy_kids_shoes_W, final.df$DM_control_small_livestock_W,      
                                     final.df$DM_decides_garden_W, final.df$DM_spend_women_income_W,  
                                     final.df$DM_who_tells_sick_kid_W), na.rm = T)

final.df$DM_count_T <- rowSums(cbind(final.df$DM_HH_fix_expenditures_T, final.df$DM_tells_kid_school_T, 
                                     final.df$DM_buy_kids_shoes_T, final.df$DM_control_small_livestock_T,      
                                     final.df$DM_decides_garden_T, final.df$DM_spend_women_income_T,  
                                     final.df$DM_who_tells_sick_kid_T), na.rm = T)

final.df$DM_count_M <- rowSums(cbind(final.df$DM_HH_fix_expenditures_M, final.df$DM_tells_kid_school_M, 
                                     final.df$DM_buy_kids_shoes_M, final.df$DM_control_small_livestock_M,      
                                     final.df$DM_decides_garden_M, final.df$DM_spend_women_income_M,  
                                     final.df$DM_who_tells_sick_kid_M), na.rm = T)

summary(reg1 <-  lfe::felm(DM_buy_kids_shoes_T ~ BP + I(BP^2) + hh_log_wages +  num_f_adults + num_m_adults + 
                             hh_size + hh_kids + hh_young_kids + factor(pobre) + factor(pobextre) | unique_loc_id + wavenumber | 0 | 
                       unique_loc_id, data = final.df))

summary(reg2 <-  lfe::felm(DM_tells_kid_school_T ~ BP + I(BP^2) + hh_log_wages +  num_f_adults + num_m_adults + 
                             hh_size + hh_kids + hh_young_kids + factor(pobre) + factor(pobextre) | unique_loc_id + wavenumber | 0 | 
                       unique_loc_id, data = final.df))

summary(reg3 <-  lfe::felm(DM_spend_women_income_T ~ BP + I(BP^2) + hh_log_wages +  num_f_adults + num_m_adults + 
                             hh_size + hh_kids + hh_young_kids + factor(pobre) + factor(pobextre)| unique_loc_id + wavenumber | 0 | 
                             unique_loc_id, data = final.df))

summary(reg4 <-  lfe::felm(DM_who_tells_sick_kid_T ~BP + I(BP^2) + hh_log_wages +  num_f_adults + num_m_adults + 
                             hh_size + hh_kids + hh_young_kids + factor(pobre) + factor(pobextre) | unique_loc_id + wavenumber | 0 | 
                             unique_loc_id, data = final.df))

summary(reg5 <-  lfe::felm(need_permission_dummy ~BP + I(BP^2) + hh_log_wages +  num_f_adults + num_m_adults + 
                             hh_size + hh_kids + hh_young_kids + factor(pobre) + factor(pobextre)  | unique_loc_id + wavenumber | 0 | 
                             unique_loc_id, data = final.df))

summary(reg6 <-  lfe::felm(DM_decides_garden_T ~ BP + I(BP^2) + hh_log_wages +  num_f_adults + num_m_adults + 
                             hh_size + hh_kids + hh_young_kids + factor(pobre) + factor(pobextre) | unique_loc_id + wavenumber | 0 | 
                             unique_loc_id, data = final.df))


summary(reg7 <-  lfe::felm(DM_control_small_livestock_T ~ BP + I(BP^2) + hh_log_wages +  num_f_adults + num_m_adults + 
                             hh_size + hh_kids + hh_young_kids + factor(pobre) + factor(pobextre)  | unique_loc_id + wavenumber | 0 | 
                             unique_loc_id, data = final.df))

summary(reg8 <-  lfe::felm(DM_HH_fix_expenditures_T ~ BP + I(BP^2) + hh_log_wages +  num_f_adults + num_m_adults + 
                             hh_size + hh_kids + hh_young_kids + factor(pobre) + factor(pobextre) | unique_loc_id + wavenumber | 0 | 
                             unique_loc_id, data = final.df))

stargazer::stargazer(reg1, reg2, reg3, reg4, covariate.labels = c("$hat{mu}$", "$hat{mu}^2$", "HH Log Earnings", 
                                                                  "# Women", "# Men", "HH Size", "# Kids", "# Young Kids",
                                                                  "Gov. Poverty Dummy", "NA", "Extreme Pov 1", "Extreme Pov 2"), 
                     title = "Decision Making Patterns and Bargaining Power")

stargazer::stargazer(reg5, reg6, reg7, reg8,  covariate.labels = c("$hat{mu}$", "$hat{mu}^2$", "HH Log Earnings", 
                                                                   "# Women", "# Men", "HH Size", "# Kids", "# Young Kids",
                                                                   "Gov. Poverty Dummy", "NA", "Extreme Pov 1", "Extreme Pov 2"), 
                     title = "Decision Making Patterns and Bargaining Power, Continued")



summary(reg9 <-  lfe::felm(AD_women_in_home ~ BP + I(BP^2) + hh_log_wages +  num_f_adults + num_m_adults +
                             hh_size + hh_kids + hh_young_kids  + factor(pobre) + factor(pobextre)  | factor(unique_loc_id) +  factor(wavenumber) | 0 | 
                             folio, data = final.df))

summary(reg10 <-  lfe::felm(AD_obedience ~ BP + I(BP^2) + hh_log_wages +   num_f_adults + num_m_adults +
                              hh_size + hh_kids + hh_young_kids + factor(pobre) + factor(pobextre) | factor(unique_loc_id) +  factor(wavenumber) | 0 | 
                              folio, data = final.df))

summary(reg11 <-  lfe::felm(AD_say_comm ~ BP + I(BP^2) + hh_log_wages +   num_f_adults + num_m_adults +
                              hh_size + hh_kids + hh_young_kids + factor(pobre) + factor(pobextre)  | factor(unique_loc_id) +  factor(wavenumber) | 0 | 
                              folio, data = final.df))

summary(reg12 <-  lfe::felm(AD_women_job ~ BP + I(BP^2) + hh_log_wages +   num_f_adults + num_m_adults +
                              hh_size + hh_kids + hh_young_kids + factor(pobre) + factor(pobextre) | 
                              factor(unique_loc_id) +  factor(wavenumber) | 0 | 
                              folio, data = final.df))

summary(reg13 <-  lfe::felm(AD_equal_rights ~ BP + I(BP^2) + hh_log_wages +   num_f_adults + num_m_adults +
                              hh_size + hh_kids + hh_young_kids + factor(pobre) + factor(pobextre) | 
                              factor(unique_loc_id) +  factor(wavenumber) | 0 | 
                              folio, data = final.df))

summary(reg14 <-  lfe::felm(AD_women_opinions ~ BP + I(BP^2) + hh_log_wages +  num_f_adults + num_m_adults + 
                              hh_size + hh_kids + hh_young_kids + factor(pobre) + factor(pobextre) |
                              factor(unique_loc_id) +  factor(wavenumber) | 0 | 
                              folio, data = final.df))

library(stargazer)
stargazer(reg9, reg10, reg11, reg12, reg13, reg14, 
          covariate.labels = c("$hat{mu}$", "$hat{mu}^2$", "HH Log Earnings", "# Women", "# Men", 
                               "HH Size", "# Kids", "# Young Kids", "Government Poverty Dummy", 
                               "Extreme Poverty Dummy", "Extreme Poverty Dummy 1", "Extreme Poverty Dummy 2"), 
          title = "Power and the Family's Views on Women's Status")

final.df$PAG_boys <- rowSums(cbind(final.df$PAG_boys_clothing, final.df$PAG_boys_shoes), na.rm = T) 
final.df$PAG_girls <- rowSums(cbind(final.df$PAG_girls_clothing, final.df$PAG_girls_shoes), na.rm = T)                          
final.df$PAG_womens <- rowSums(cbind(final.df$PAG_womens_clothing, final.df$PAG_womens_shoes), na.rm = T) 
final.df$PAG_mens <- rowSums(cbind(final.df$PAG_mens_clothing, final.df$PAG_mens_shoes), na.rm = T) 

final.df$PAG <- rowSums(cbind(final.df$PAG_boys, final.df$PAG_girls, final.df$PAG_womens, final.df$PAG_mens),
                        na.rm=T)
summary(final.df$PAG)


summary(reg15 <-  lfe::felm(asinh(PAG_boys) ~ BP + I(BP^2) + hh_log_wages +  num_f_adults + num_m_adults + 
                              hh_size + hh_kids + hh_young_kids + factor(pobre) + factor(pobextre) | #factor(pobre) + factor(pobextre) |
                              factor(unique_loc_id) +  factor(wavenumber) | 0 | 
                              folio, data = final.df))

summary(reg16 <-  lfe::felm(asinh(PAG_girls) ~ BP + I(BP^2) + hh_log_wages +  num_f_adults + num_m_adults + 
                              hh_size + hh_kids + hh_young_kids + factor(pobre) + factor(pobextre) | #factor(pobre) + factor(pobextre) |
                              factor(unique_loc_id) +  factor(wavenumber) | 0 | 
                              folio, data = final.df))

summary(reg17 <-  lfe::felm(asinh(PAG_womens) ~ BP + I(BP^2) + hh_log_wages +  num_f_adults + num_m_adults + 
                              hh_size + hh_kids + hh_young_kids + factor(pobre) + factor(pobextre)| # |
                              factor(unique_loc_id) +  factor(wavenumber) | 0 | 
                              folio, data = final.df))

summary(reg18 <-  lfe::felm(asinh(PAG_mens) ~ BP + I(BP^2) + hh_log_wages +  num_f_adults + num_m_adults + 
                              hh_size + hh_kids + hh_young_kids + factor(pobre) + factor(pobextre) | #factor(pobre) + factor(pobextre) |
                              factor(unique_loc_id) +  factor(wavenumber) | 0 | 
                              folio, data = final.df))


summary(reg19 <-  lfe::felm(asinh(PAG) ~ BP + I(BP^2) + hh_log_wages +  num_f_adults + num_m_adults + 
                              hh_size + hh_kids + hh_young_kids + factor(pobre) + factor(pobextre) | #factor(pobre) + factor(pobextre) |
                              factor(unique_loc_id) +  factor(wavenumber) | 0 | 
                              folio, data = final.df))

stargazer(reg15, reg16, reg17, reg18, reg19, covariate.labels = c("$hat{mu}$", "$hat{mu}^2$", "HH Log Earnings", "# Women", "# Men", 
                                                                  "HH Size", "# Kids", "# Young Kids", "Government Poverty Dummy", 
                                                                  "Extreme Poverty Dummy", "Extreme Poverty Dummy 1", "Extreme Poverty Dummy 2"), 
          title = "Power and Private Assignable Goods Expenditures")



my_tobit1 <- censReg(formula = asinh(PAG_girls) ~ BP + I(BP^2) + hh_log_wages +  num_f_adults + num_m_adults + 
                               hh_size + hh_kids + hh_young_kids + #factor(pobre) + factor(pobextre) |
                               factor(seven_states) +  factor(wavenumber) , left = 0, data = final.df, method = "BFGS")

summary(my_tobit1)

summary(my_tobit2 <- censReg(formula = asinh(PAG_boys) ~ BP + I(BP^2) + hh_log_wages +  num_f_adults + num_m_adults + 
                       hh_size + hh_kids + hh_young_kids + #factor(pobre) + factor(pobextre) |
                       factor(seven_states) +  factor(wavenumber) , left = 0, data = final.df, method = "BFGS"))

summary(my_tobit3 <- censReg(formula = asinh(PAG_womens) ~ BP + I(BP^2) + hh_log_wages +  num_f_adults + num_m_adults + 
                               hh_size + hh_kids + hh_young_kids + #factor(pobre) + factor(pobextre) |
                               factor(seven_states) +  factor(wavenumber) , left = 0, data = final.df, method = "BFGS"))

summary(my_tobit4 <- censReg(formula = asinh(PAG_mens) ~ BP + I(BP^2) + hh_log_wages +  num_f_adults + num_m_adults + 
                               hh_size + hh_kids + hh_young_kids + #factor(pobre) + factor(pobextre) |
                               factor(seven_states) +  factor(wavenumber) , left = 0, data = final.df, method = "BFGS"))


# p <- plot(x = final.df$BP, y = predict(reg1, data = final.df))
# 
# 
# ggplot(data = final.df) + 
#   geom_point(mapping = aes(x = BP, y = DM_HH_fix_expenditures_T)) + 
#   geom_smooth(mapping = aes(x = BP, y = DM_HH_fix_expenditures_T))
# 
# 
# 
# summary(reg_count_1 <- glmmML::glmmboot(DM_count_W ~  BP + I(BP^2) + hh_log_wages +  num_f_adults + num_m_adults + 
#                                           hh_size + hh_kids + hh_young_kids + #factor(pobre) + factor(pobextre) +
#                                           factor(wavenumber),
#                                         cluster = folio, data = final.df, family="poisson"))
# 
# summary(reg_count_2 <- glmmML::glmmboot(DM_count_M ~ BP + I(BP^2) + hh_log_wages +  num_f_adults + num_m_adults + 
#                                           hh_size + hh_kids + hh_young_kids  + 
#                                           factor(wavenumber),
#                                         cluster = folio, data = final.df, family="poisson"))
# 
# summary(reg_count_3 <- glmmML::glmmboot(DM_count_T ~ BP + I(BP^2) + hh_log_wages + hh_kids + hh_young_kids +
#                                           factor(wavenumber), 
#                                         cluster = folio, data = final.df, family="poisson"))
# 
# 
# coefs.df <- tibble(Names = c("$hat{mu}$", "$hat{mu}^{2}$", "HH Log Earnings",
#                              "Num Kids", "Num Young Kids"),
#                    "Women Only" = as.numeric(unname(reg_count_1$coefficients)[1:5]),
#                    "Men Only" = as.numeric(unname(reg_count_2$coefficients)[1:5]),
#                    "Together" = as.numeric(reg_count_3$coefficients[1:5]))
# 
# se.df <- tibble(Names = paste0( c("$hat{mu}$", "$hat{mu}^{2}$", "HH Log Earnings",
#                                   "Num Kids", "Num Young Kids", "Chicken Price", 
#                                   "Beef Price", "Pork Price", "Lard Price", "Sardine Price", "Tuna Price", 
#                                   "Milk Price", "Egg Price", "Bean Price", "Rice Price"), "_se"), 
#                 Chicken.LPM = unname(p1$coefficients[1:15,2]),
#                 Milk.LPM = unname(p2$coefficients[1:15,2]),
#                 Chicken.Poi = unname(chick_Poisson$sd[1:15]),
#                 Milk.Poi = unname(milk_Poisson$sd[1:15]))
# 
# table_6 <- bind_rows(coefs.df, se.df)
# table_6[,2:5] <- round(table_6[,2:5], 3) 


# Multinomial Logit
# library(mlogit)
# final.mdata <- mlogit.data(final.df[, c("DM_spend_women_income", "BP", "hh_log_wages")]
#                            , shape = "wide", choice = "DM_spend_women_income")
# 
# filter(final.mdata, hh_log_wages > 0)
# 
# summary(mlogit(DM_spend_women_income ~ BP + I(BP^2) + hh_log_wages | 0 | 0, 
#                data = final.mdata))




