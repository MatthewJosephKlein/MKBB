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
         sample.analog$Hypothetical_T_Mom_1[sample.analog$wavenumber == 1]) - 
        (sample.analog$Dad_SW_combined[sample.analog$wavenumber == 1] + 
           sample.analog$Hypothetical_T_Dad_1[sample.analog$wavenumber == 1])) / 
        (sample.analog$hh_wages[sample.analog$wavenumber == 1]))
  
  sample.analog$BP[sample.analog$wavenumber == 2] <-
    (1/2) + (1/2)*((
      (sample.analog$Mom_SW_combined[sample.analog$wavenumber == 2] + 
         sample.analog$Hypothetical_T_Mom_1[sample.analog$wavenumber == 2]) - 
        (sample.analog$Dad_SW_combined[sample.analog$wavenumber == 2] + 
           sample.analog$Hypothetical_T_Dad_1[sample.analog$wavenumber == 2])) / 
        (sample.analog$hh_wages[sample.analog$wavenumber == 2]))
  
  
  sample.analog$BP[sample.analog$wavenumber == 3] <-
    (1/2) + (1/2)*((
      (sample.analog$Mom_SW_combined[sample.analog$wavenumber == 3] + 
         sample.analog$Hypothetical_T_Mom_1[sample.analog$wavenumber == 3]) - 
        (sample.analog$Dad_SW_combined[sample.analog$wavenumber == 3] + 
           sample.analog$Hypothetical_T_Dad_1[sample.analog$wavenumber == 3])) / 
        (sample.analog$hh_wages[sample.analog$wavenumber == 3]))
  
  # The Kuhn-tucker conditions don't allow for eta < 0 or eta > 1 (see [lambda_3] and [lambda_4] in Section 3)
  sample.analog$BP[sample.analog$BP <= 0] <- 0  
  sample.analog$BP[sample.analog$BP >= 1] <- 1
  
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
final.df <- aggregate(sample.analog$BP, by = list(sample.analog$folio, sample.analog$wavenumber), FUN=mean, na.rm=T)
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

# hist(final.df$Hyp_BP[final.df$wavenumber == 1 & final.df$treatment_household == 0],
#      main = NA, sub = "1997", col = rgb(1,1,1,0.25, 0.25), xlab = "Bargaining Power",
#      breaks = 1000, xlim = c(0,0.1), freq=FALSE, ylab = "Percent of Households")
# par(new = T)
# hist(final.df$Hyp_BP[final.df$wavenumber == 1 & final.df$treatment_household == 1],
#      col = rgb(0,0,0,0.25, 0.25),
#      breaks = 100, xlim = c(0.15,0.65), freq=FALSE,  axes = F, xlab =NA, ylab=NA, main = NA)
# 
# title(main = "Hypothetical Distributions when Transfers are to Men")
# legend("topright", legend = c("control (white)", "treatment (grey)"), col = c("black", "black"), pch = c(0,15))
# 
# hist(final.df$Hyp_BP[final.df$wavenumber == 2 & final.df$progresa_income_total == 0], col = rgb(1,1,1,0.25, 0.25), main = "1999", xlab = "Bargaining Power",
#      breaks = 100, xlim = c(0.15,0.65), freq=FALSE, ylab = "Percent of Households")
# par(new = T)
# hist(final.df$Hyp_BP[final.df$wavenumber == 2 & final.df$progresa_income_total > 0], col = rgb(0,0,0,0.25, 0.25),
#      breaks = 100, xlim = c(0.15,0.65), freq=FALSE,  axes = F, xlab =NA, ylab=NA, main = NA)
# 
# hist(final.df$Hyp_BP[final.df$wavenumber == 3 & final.df$progresa_income_total == 0], col = rgb(1,1,1, 0.25, 0.25), main = "2000", xlab = "Bargaining Power",
#      breaks = 100, xlim = c(0.15,0.65), freq=FALSE, ylab = "Percent of Households")
# par(new = T)
# hist(final.df$Hyp_BP[final.df$wavenumber == 3 & final.df$progresa_income_total > 0], col = rgb(0,0,0, 0.25, 0.25),
#      breaks = 100, xlim = c(0.15,0.65), freq=FALSE,  axes = F, xlab =NA, ylab=NA, main = NA)
# 


final.df$Positive_Progresa <- 0
final.df$Positive_Progresa[final.df$progresa_income_total > 0] <- 1


library(cowplot)
library(grid)
library(gridExtra)
library(scales)


final.df$Treatment <- as.factor(final.df$treatment_household)
levels(final.df$Treatment) <- c("Not Treated", "Treated")


panel_1A <- ggplot(filter(final.df, wavenumber == 1)) + 
  geom_histogram(mapping = aes(x = Hyp_BP, y = ..density..,
                               fill = Treatment, 
                               group = Treatment), 
                 bins = 25, position="identity", alpha = 0.5) + 
  #ylim(c(0,4)) +
  geom_density(mapping = aes(x = Hyp_BP, y = ..density.., 
                             #color = Treatment,
                             group = Treatment), size = 0.75, adjust = 5) + 
  labs(title = "1997", y = "% of Households", x = "Bargaining Power") + 
  theme(axis.ticks = element_blank(),
        # axis.text.y = element_blank(),
        #axis.title.y = element_blank(), 
        # axis.title.x = element_blank(), 
        legend.position = c(0.7, 0.8))
panel_1A                  

final.df$Positive_Progresa <- 0
final.df$Positive_Progresa[final.df$progresa_income_total > 0] <- 1

final.df$Positive_Progresa <- as.factor(final.df$Positive_Progresa)

panel_2A <- ggplot(filter(final.df, wavenumber == 2)) + 
  geom_histogram(mapping = aes(x = Hyp_BP, y = ..density.., 
                               fill = Positive_Progresa), 
                 bins = 25, position="identity",
                 alpha = 0.5) + 
  # ylim(c(0,4)) +
  geom_density(mapping = aes(x = Hyp_BP, y = ..density..,
                            # color = Positive_Progresa,
                             group = Positive_Progresa),
               position="identity", adjust = 5, size = 0.75) + 
  labs(title = "1999", y = "% of Households", x = "Bargaining Power") + 
  theme(axis.ticks = element_blank(), 
        # axis.text.y = element_blank(),
        #axis.title.y = element_blank(), 
        # axis.title.x = element_blank(), 
        legend.position = "none") 
panel_2A                  

panel_3A <- ggplot(filter(final.df, wavenumber == 3)) + 
  geom_histogram(mapping = aes(x = Hyp_BP, y = ..density..,
                               fill = Positive_Progresa), 
                 bins = 25,  position="identity", alpha = 0.5) + 
 # ylim(c(0,4)) +
  geom_density(mapping = aes(x = Hyp_BP, y = ..density.., 
                            # color = Positive_Progresa,
                             group = Positive_Progresa), adjust = 5, size = 0.75) + 
  labs(title = "2000", y = "% of Households", x = "Bargaining Power") + 
  theme(axis.ticks = element_blank(), 
        # title.position = c(1, 0.5),
        # axis.text.y = element_blank(),
        #axis.title.y = element_blank(), 
        # axis.title.x = element_blank(), 
        legend.position = "none")  
panel_3A                  


p_A <- cowplot::plot_grid(panel_1A, panel_2A, panel_3A, 
                          align = "v", #     labels = c("H1", "H2", "H3", "H4"), 
                          nrow=3)

p_A


#create common x label
 x.grob <- textGrob("Hypothetical Distributions of Power Under Alternative Gender-Targetting", 
                    gp=gpar(fontface="bold", col="black", fontsize=15))
 title.grob <- textGrob("Counterfactual Distributions", 
                        gp=gpar(fontface="bold", col="black", fontsize=24))
# 
# 
 grid.arrange(arrangeGrob(p_A, bottom = x.grob, top = title.grob))

 summary(final.df$Hyp_BP[final.df$wavenumber == 2 & final.df$Positive_Progresa == 0])
 summary(final.df$Hyp_BP[final.df$wavenumber == 2 & final.df$Positive_Progresa == 1])

 
  