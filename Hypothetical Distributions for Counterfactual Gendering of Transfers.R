# HYPOTHETICAL DISTRIBUTIONS


rm(list=ls())
library("sampleSelection")
library("lfe")
library("glmmML")
library("tidyverse")
getwd()

# Chapter 1: Functions ####

load("hh.df.Rda") # This data.frame is generated in "Final Cleaning for Master Panel Construction.R"

temp.df <- aggregate(hh.df$progresa_income_total[hh.df$wavenumber==2], 
                     by =list(hh.df$folio[hh.df$wavenumber==2]), FUN=mean, na.rm=T)
colnames(temp.df) <- c("folio", "progresa_income_total_in_period_2")
temp.df$treatment_household <- rep(0)
temp.df$treatment_household[temp.df$progresa_income_total_in_period_2>0] <- 1
summary(temp.df)

hh.df <- merge(hh.df, temp.df, by = c("folio"))

# Editing the Transfer variables to reflect different counterfacutals: 
# One where men received the transfer and 
# One where the transfer was split 50/50

hh.df$Hypothetical_T_Mom_1 <- hh.df$T_mom_total - hh.df$progresa_income_total
hh.df$Hypothetical_T_Dad_1 <- hh.df$T_dad_total + hh.df$progresa_income_total

hh.df$Hypothetical_T_Mom_2 <- hh.df$T_mom_total - (hh.df$progresa_income_total/2)
hh.df$Hypothetical_T_Dad_2 <- hh.df$T_dad_total + (hh.df$progresa_income_total/2)


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
  
  sample.analog$Hyp_BP[sample.analog$wave1 == 1] <- (sample.analog$Mom_SW_combined[sample.analog$wavenumber == 1] + 
                                                   sample.analog$T_mom_total[sample.analog$wavenumber == 1] )  / 
    (sample.analog$Mom_SW_combined[sample.analog$wavenumber == 1] + 
       sample.analog$T_mom_total[sample.analog$wavenumber == 1] +
       sample.analog$Dad_SW_combined[sample.analog$wavenumber == 1] + 
       sample.analog$T_dad_total[sample.analog$wavenumber == 1])
  
  sample.analog$Hyp_BP[sample.analog$wavenumber == 2] <- 
    (sample.analog$Mom_SW_combined[sample.analog$wavenumber == 2] + 
       sample.analog$Hypothetical_T_Mom_2[sample.analog$wavenumber == 2])  /
    (sample.analog$Mom_SW_combined[sample.analog$wavenumber == 2] +
       sample.analog$Hypothetical_T_Mom_2[sample.analog$wavenumber == 2] + 
       sample.analog$Dad_SW_combined[sample.analog$wavenumber == 2] + 
       sample.analog$Hypothetical_T_Dad_2[sample.analog$wavenumber == 2])
  
  sample.analog$Hyp_BP[sample.analog$wavenumber == 3] <- 
    (sample.analog$Mom_SW_combined[sample.analog$wavenumber == 3] + 
       sample.analog$Hypothetical_T_Mom_2[sample.analog$wavenumber == 3])  /
    (sample.analog$Mom_SW_combined[sample.analog$wavenumber == 3] +
       sample.analog$Hypothetical_T_Mom_2[sample.analog$wavenumber == 3] + 
       sample.analog$Dad_SW_combined[sample.analog$wavenumber == 3] + 
       sample.analog$Hypothetical_T_Dad_2[sample.analog$wavenumber == 3])
  
  
  return(list(sample.analog, 
              mean(sample.analog$BP[sample.analog$wave1 == 1], na.rm = T), 
              mean(sample.analog$BP[sample.analog$wave2 == 1], na.rm = T),
              mean(sample.analog$BP[sample.analog$wave3 == 1], na.rm = T)))  
} 


# Chapter 1.b: Generating the BP values and Final.df #####

sample.analog <- hh.df 

BP.Fun.Results <- BP.Fun()
sample.analog <- BP.Fun.Results[[1]]

# making a matrix of just the HH level variables: 
final.df <- aggregate(sample.analog$Hyp_BP, by = list(sample.analog$folio, sample.analog$wavenumber), FUN=mean, na.rm=T)
colnames(final.df) <- c("folio", "wavenumber", "Hyp_BP")

final.df <- unique(merge(sample.analog[,c("folio", "wavenumber", "loc_id", "hh_log_wages" , "hh_kids" , "hh_young_kids" , 
                                          "progresa_income_total","wave2", "wave3", "treatment_dummy_num", "seven_states", "mpio", 
                                          "rice.price_hybrid", "bean.price_hybrid", "egg.price_hybrid", "milk.price_hybrid", # Staples
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
                                          #  # Miscellaneous
                                          "bebidas.alcoholicas_num_times_consume", "bebidas.alcoholicas",
                                          "cafe", "cafe_num_times_consume", "arroz", "arroz_num_times_consume",
                                          "sugar.price_hybrid" , "coffee.price_hybrid" , "soda.price_hybrid" ,
                                          "azucar_num_times_consume", "azucar",
                                          "refrescos", "refrescos_num_times_consume",
                                          "sopa.de.pasta", "sopa.de.pasta_num_times_consume",
                                          "wheat.flour.price_hybrid" , "veg.oil.price_hybrid" , 
                                          "aciete.vegetal", "aciete.vegetal_num_times_consume",
                                          "sopa.de.pasta.price_hybrid", "breakfast.cereal.price_hybrid",
                                          "treatment_dummy", "treatment_household")], final.df, by =   c("folio", "wavenumber")))




# Chapter 5: Generating Figure 1: a graph of BP over time for the treatment and control groups #####

par(mfrow=c(3,1))
#hist(final.df$BP[final.df$wavenumber == 1], col = rgb(1,1,1), main = "1997", xlab = "Bargaining Power", 
#     breaks = 100, xlim = c(0.15,0.5), freq=FALSE, ylab = "Percent of Households")

hist(final.df$Hyp_BP[final.df$wavenumber == 1 & final.df$treatment_household == 0], main = NA, sub = "1997", col = rgb(1,1,1,0.25, 0.25), xlab = "Bargaining Power", 
     breaks = 100, xlim = c(0.15,0.5), freq=FALSE, ylab = "Percent of Households")
par(new = T)
hist(final.df$Hyp_BP[final.df$wavenumber == 1 & final.df$treatment_household == 1], col = rgb(0,0,0,0.25, 0.25),  
     breaks = 100, xlim = c(0.15,0.5), freq=FALSE,  axes = F, xlab =NA, ylab=NA, main = NA)

title(main = "Hypothetical Distributions when Transfers are Split")
legend("topright", legend = c("control (white)", "treatment (grey)"), col = c("black", "black"), pch = c(0,15))

hist(final.df$Hyp_BP[final.df$wavenumber == 2 & final.df$progresa_income_total == 0], col = rgb(1,1,1,0.25, 0.25), main = "1999", xlab = "Bargaining Power", 
     breaks = 100, xlim = c(0.15,0.5), freq=FALSE, ylab = "Percent of Households")
par(new = T)
hist(final.df$Hyp_BP[final.df$wavenumber == 2 & final.df$progresa_income_total > 0], col = rgb(0,0,0,0.25, 0.25), 
     breaks = 100, xlim = c(0.15,0.5), freq=FALSE,  axes = F, xlab =NA, ylab=NA, main = NA)

hist(final.df$Hyp_BP[final.df$wavenumber == 3 & final.df$progresa_income_total == 0], col = rgb(1,1,1, 0.25, 0.25), main = "2000", xlab = "Bargaining Power", 
     breaks = 100, xlim = c(0.15,0.5), freq=FALSE, ylab = "Percent of Households")
par(new = T)
hist(final.df$Hyp_BP[final.df$wavenumber == 3 & final.df$progresa_income_total > 0], col = rgb(0,0,0, 0.25, 0.25), 
     breaks = 100, xlim = c(0.15,0.5), freq=FALSE,  axes = F, xlab =NA, ylab=NA, main = NA)
