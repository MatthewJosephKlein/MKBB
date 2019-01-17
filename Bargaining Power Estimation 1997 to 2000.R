####################



# Reading on 1/16/2019: 

BP Estimation Code not up to date. I guess the proper file to use is just the Bootstrap Function Code.R file


# Matthew Klein. This code takes as an input "final.panel.97.99.00.Rda" and gives as an output the same dataset with additional bargaining power measuers
# Bargaining power is calculated by specifying the wave and gender.   
# 
# This code works as follows:
#          Intro: Final data cleaning exercises
#          Chapter 1: BP function
#          Chapter 2: call the function for each wave and gender, put together the BP measure, and save the final.panel data file again, with the BP measures
# 01/09/2017 - Began
# 01/10/2017 - fiddled with the specification 
# 01/12/2017 - fiddled with specification, dropped observations that reported their job as "coop," (43 observations) and consolodated "family job no pay" and "job no pay" 
#              IMPORTANT COMMENT: found errors in data. sometimes the wages jump by 3 orders of magnitude in 1999 and then back down in 2000. Looks like an error from the gov data recorder. The wages will be something like 320, 320360, 280. 
# 01/22/2017 - updated the praimary_employment variable so it' more useful. Before, the ordering and the "Unemployed" option were causing an error: all with wages have a 0 for unemployed, causing a colinearity error.

# 03/24/2017 - ran the regressions specified with Brad and Esteban

# 11/07/2017 - included the gender variables from March 1998, October 1998, and March 1999 to use as exclusion restrictions

# Introduction & preliminary data work ####################

#memory.limit(size = 14215)

rm(list=ls())
#setwd("C:/Users/Matthew/Desktop/-/ProgrammingDirectory")
load("Master_with_loc.Rda")

library("sampleSelection")
library("stargazer")
library("dummies")
library("stats")
library("xtable")


# creating dummies for primary employment. as.factor() is causing an error in the regression:
# consilodate dummies for options with low numbers of obs.
# Rearrange the order so that unemployed comes first, and is thus dropped from the regressions below. 

hh.df$employment <- c(rep(0, length(hh.df$folio)))
hh.df$employment[hh.df$primary_employment ==  "Miembro de una cooperativa" |                       #AG
                   hh.df$primary_employment ==  "Jornalero rural o peón de campo" |  
                   hh.df$primary_employment == "Ejidatario o comunero" ] <- 1
hh.df$employment[hh.df$primary_employment ==  "Otros" | hh.df$primary_employment ==  "NR" |        #Other/NR/Manager
                   hh.df$primary_employment ==  "Patrón, o empleador de un negocio"] <- 2    
hh.df$employment[hh.df$primary_employment ==  "Obrero o empleado NO agropecuario" | 
                   hh.df$primary_employment == "Trabajador por cuenta propia"] <- 3    # NON-AG 
table(hh.df$employment, hh.df$sex)

# Summing other income because of the low number of females who recorded a third type (causes "matrix is exactly singular" error)
hh.df$otherincomeval1[is.na(hh.df$otherincomeval1)] <- 0
hh.df$otherincomeval2[is.na(hh.df$otherincomeval2)] <- 0
hh.df$otherincomeval <- hh.df$otherincomeval1 + hh.df$otherincomeval2


# Making a dummy for whether individuals got any government transfer (other than progresa)
hh.df$gov_transfer <- c(rep(0, length(hh.df$folio)))
for(i in 1:length(hh.df$folio)){
  if(sum(hh.df$apoyo_del_INI_dummy_IND[i], hh.df$apoyo_del_programa_Ninos_de_Solidaridad_dummy_IND[i], hh.df$Conasupo_dummy_IND[i], 
         hh.df$DIF_kitchen_dummy_IND[i], hh.df$PROBECAT_o_CIMO_dummy_IND[i],   hh.df$Tortilla_solidaridad_dummy_IND[i], 
         hh.df$temp_employment_program_dummy_IND[i], na.rm = T) > 0) hh.df$gov_transfer[i] <- 1
}

# consolidating the landed ownership variables
hh.df$land <- c(rep(0, length(hh.df$folio)))
hh.df$land[hh.df$land_ownership1 > 0] <- 1
hh.df$land[hh.df$land_ownership2 > 0] <- 1
hh.df$land[hh.df$land_ownership3 > 0] <- 1
hh.df$land[hh.df$land_ownership4 > 0] <- 1
hh.df$land[hh.df$land_ownership5 > 0] <- 1
hh.df$land[hh.df$number_land_parcels > 0] <- 1

hh_land.df <- aggregate(hh.df$land, by = list(Category=hh.df$folio), FUN=sum)
names(hh_land.df) <- c("folio", "hh_land")
hh.df <- merge(hh.df, hh_land.df, by = "folio")
hh.df$hh_land[hh.df$hh_land > 0] <- 1 
summary(hh.df$hh_land)


# Editing the agestartedwork variable to make the 98 and 99 values = NA
hh.df$agestartedwork[hh.df$agestartedwork == 98 | hh.df$agestartedwork ==99] <- NA

# Some observations have typos in their wages. They jump up by 3 orders of magnitude in 1999 then back down in 2000. Dropping them here. (Could write better code)
# For instance, In wave 2, household 6009963, individual 1, there is an entry error for wages, making SW estimates go bonkers. Dropped as an error/outlier
# Instead of dropping, could give them the average of the 1997 and the 2000 wages in the future. 

# Other demographics - male and female and total adult numbers per household

hh.df$adult <- hh.df$male_adult <- hh.df$female_adult <-
  hh.df$num_adults <- hh.df$num_f_adults <- hh.df$num_m_adults <- c(rep(0, length(hh.df$folio)))
hh.df$adult[hh.df$age > 16] <- 1
hh.df$male_adult[hh.df$age > 16 & hh.df$sex == 0] <- 1
hh.df$female_adult[hh.df$age > 16 & hh.df$sex == 1] <- 1

adults_97 <- aggregate(hh.df[hh.df$wave1 == 1,]$adult, by = list(Category=hh.df[hh.df$wave1 == 1,]$folio), FUN=sum) 
names(adults_97) <- c("folio", "num_adults_97")

adults_99 <- aggregate(hh.df[hh.df$wave2 == 1,]$adult, by = list(Category=hh.df[hh.df$wave2 == 1,]$folio), FUN=sum) 
names(adults_99) <- c("folio", "num_adults_99")

adults_00 <- aggregate(hh.df[hh.df$wave3 == 1,]$adult, by = list(Category=hh.df[hh.df$wave3 == 1,]$folio), FUN=sum) 
names(adults_00) <- c("folio", "num_adults_00")

f_adults_97 <- aggregate(hh.df[hh.df$wave1 == 1,]$female_adult, by = list(Category=hh.df[hh.df$wave1 == 1,]$folio), FUN=sum) 
names(f_adults_97) <- c("folio", "num_female_adults_97")

f_adults_99 <- aggregate(hh.df[hh.df$wave2 == 1,]$female_adult, by = list(Category=hh.df[hh.df$wave2 == 1,]$folio), FUN=sum) 
names(f_adults_99) <- c("folio", "num_female_adults_99")

f_adults_00 <- aggregate(hh.df[hh.df$wave3 == 1,]$female_adult, by = list(Category=hh.df[hh.df$wave3 == 1,]$folio), FUN=sum) 
names(f_adults_00) <- c("folio", "num_female_adults_00")

m_adults_97 <- aggregate(hh.df[hh.df$wave1 == 1,]$male_adult, by = list(Category=hh.df[hh.df$wave1 == 1,]$folio), FUN=sum) 
names(m_adults_97) <- c("folio", "num_male_adults_97")

m_adults_99 <- aggregate(hh.df[hh.df$wave2 == 1,]$male_adult, by = list(Category=hh.df[hh.df$wave2 == 1,]$folio), FUN=sum) 
names(m_adults_99) <- c("folio", "num_male_adults_99")

m_adults_00 <- aggregate(hh.df[hh.df$wave3 == 1,]$male_adult, by = list(Category=hh.df[hh.df$wave3 == 1,]$folio), FUN=sum) 
names(m_adults_00) <- c("folio", "num_male_adults_00")

hh.df <- merge(hh.df, adults_97, by = "folio", all.x=T)
hh.df <- merge(hh.df, adults_99, by = "folio", all.x=T)
hh.df <- merge(hh.df, adults_00, by = "folio", all.x=T)

hh.df <- merge(hh.df, f_adults_97, by = "folio", all.x=T)
hh.df <- merge(hh.df, f_adults_99, by = "folio", all.x=T)
hh.df <- merge(hh.df, f_adults_00, by = "folio", all.x=T)

hh.df <- merge(hh.df, m_adults_97, by = "folio", all.x=T)
hh.df <- merge(hh.df, m_adults_99, by = "folio", all.x=T)
hh.df <- merge(hh.df, m_adults_00, by = "folio", all.x=T)

hh.df$num_adults[hh.df$wave1 == 1] <- hh.df$num_adults_97[hh.df$wave1 == 1]
hh.df$num_adults[hh.df$wave2 == 1] <- hh.df$num_adults_99[hh.df$wave2 == 1]
hh.df$num_adults[hh.df$wave3 == 1] <- hh.df$num_adults_00[hh.df$wave3 == 1]

hh.df$num_f_adults[hh.df$wave1 == 1] <- hh.df$num_female_adults_97[hh.df$wave1 == 1]
hh.df$num_f_adults[hh.df$wave2 == 1] <- hh.df$num_female_adults_99[hh.df$wave2 == 1]
hh.df$num_f_adults[hh.df$wave3 == 1] <- hh.df$num_female_adults_00[hh.df$wave3 == 1]

hh.df$num_m_adults[hh.df$wave1 == 1] <- hh.df$num_male_adults_97[hh.df$wave1 == 1]
hh.df$num_m_adults[hh.df$wave2 == 1] <- hh.df$num_male_adults_99[hh.df$wave2 == 1]
hh.df$num_m_adults[hh.df$wave3 == 1] <- hh.df$num_male_adults_00[hh.df$wave3 == 1]


hh.df$log_wages <- log(hh.df$wages)
hh.df$log_wages[hh.df$log_wages == -Inf] <- NA

hh.df$LFP[is.na(hh.df$LFP)] <- 0

save(hh.df, file = "hh.df.Rda")

#hh.df <- subset(hh.df, sex != 8 & is.na(sex) == FALSE &
#                    number_heads == 2 &
#                    inf != 99 &
#                    wages_top_coded_dummy == 0 &
#                    wages_not_known == 0 &
#                    wages_not_reported == 0 &
#                    payment_period_not_known == 0 &
#                    payment_period_not_reported == 0 & 
#                    otherincomeper1_not_reported == 0 & 
#                    otherincomeval1_not_reported == 0 & 
#                    otherincomeval1_not_known == 0 & 
#                    otherincomeper1_not_known == 0 & 
#                    otherincomeper2_not_reported == 0 & 
#                    otherincomeval2_not_reported == 0 & 
#                    otherincomeval2_not_known == 0 &
#                    otherincomeper2_not_known == 0 &
#                    numdaysworked_no_response == 0)

keep.index <- with(hh.df, { sex != 8 & is.na(sex) == FALSE &
    number_heads == 2 &
    inf != 99 &
    wages_top_coded_dummy == 0 &
    wages_not_known == 0 &
    wages_not_reported == 0 &
    payment_period_not_known == 0 &
    payment_period_not_reported == 0 & 
    otherincomeper1_not_reported == 0 & 
    otherincomeval1_not_reported == 0 & 
    otherincomeval1_not_known == 0 & 
    otherincomeper1_not_known == 0 & 
    otherincomeper2_not_reported == 0 & 
    otherincomeval2_not_reported == 0 & 
    otherincomeval2_not_known == 0 &
    otherincomeper2_not_known == 0 &
    numdaysworked_no_response == 0  
})

hh.df <- hh.df[keep.index, ]

which.max(hh.df$wages)
#View(hh.df[ 176921 ,])
hh.df <- subset(hh.df, hh.df$folio != 478481)
hh.df <- subset(hh.df, hh.df$folio != 609963)
hh.df <- subset(hh.df, hh.df$folio != 694682)
hh.df <- subset(hh.df, hh.df$folio != 515870)

hh.df$year_wave_FE <- paste0(hh.df$wavenumber, hh.df$seven_states, sep = "")

hh.df$progresa_income_mom <- hh.df$progresa_income_total*hh.df$head_dummy*hh.df$sex

save(hh.df, file = "hh.df.Rda")

# Incorporating Exclusion Restriction Variables ####

hh.98 <- foreign::read.spss('socioec_encel_98m.sav', use.value.labels = FALSE, to.data.frame = TRUE)
hh.98o <- foreign::read.spss('socioec_encel_98o.sav', use.value.labels = FALSE, to.data.frame = TRUE)
hh.99 <- foreign::read.spss('socioec_encel_99m.sav', use.value.labels = FALSE, to.data.frame = TRUE)


var.names <- c("folio", "ind_ID",
               "accompanied", "need_permission", "AD_women_in_home", "AD_obedience", 
               "AD_say_comm", "AD_women_job", "AD_equal_rights", "AD_women_opinions", "wavenumber")

sub.1 <- hh.98[,c("folio", "renglon", 
                  "p135", "p136", "p13701", "p13702", "p13703", "p13704", "p13705", "p13706")]  
sub.1$wavenumber <- c(rep(1, length(sub.1$folio)))
colnames(sub.1) <- var.names

sub.2 <- hh.98o[,c("folio", "renglon", 
                   "r188", "r187")]  
sub.2$wavenumber <- c(rep(2, length(sub.2$folio)))
colnames(sub.2) <- c("folio", "ind_ID", "accompanied", "need_permission", "wavenumber")

sub.2$AD_women_in_home <- sub.2$AD_obedience <- sub.2$AD_say_comm <- sub.2$AD_women_job <- sub.2$AD_equal_rights <-
  sub.2$AD_women_opinions <- rep(NA)

sub.3 <- hh.99[,c("folio", "renglon",  "m146", "m145", "m14801",
                  "m14802", "m14803", "m14804", "m14805", "m14806")]
sub.3$wavenumber <- c(rep(3, length(sub.3$folio)))
colnames(sub.3) <- var.names

sub <- rbind(sub.1, sub.2)
sub <- rbind(sub, sub.3)

hh.df <- merge(hh.df, sub, by = c("folio", "ind_ID", "wavenumber"), all.x=T)

save(hh.df, file = "hh.df.Rda")

# Chapter 1: Function for calculating BP DO NOT USE NOT UP TO DATE AS OF 1/11/2018#### 


BP.fun <- function(gender_number){ #gender_number == 1 -> women. wavenumber == wave1 -> 1997 
  
  data.df <- subset(hh.df,  hh.df$age > 15 & hh.df$sex == gender_number &  hh.df$age <= 70)
  
#  selection_formula <- LFP ~ age + I(age^2) +  otherincomeval + hh_kids +  hh_young_kids + edu_yrs + literate + gov_transfer +
    indigenous_language + spanish_language + head_dummy + num_f_adults + num_m_adults + pobextre +  mpcalif  +
    number_female_kids + number_male_kids  + prop_mex_migrant + prop_usa_migrant + I(num_m_adults*prop_mex_migrant) +
    I(num_m_adults*prop_usa_migrant) + I(num_f_adults*prop_mex_migrant) + I(num_f_adults*prop_usa_migrant) +  as.factor(year_wave_FE) + treatment_dummy
  
  outcome_formula <-  log_wages ~ age + I(age^2) +  otherincomeval + hh_kids +  hh_young_kids + edu_yrs + literate + gov_transfer +
    indigenous_language + spanish_language + head_dummy +  num_f_adults + num_m_adults + pobextre + 
    number_female_kids + number_male_kids + as.factor(year_wave_FE) + treatment_dummy    
  
  if(gender_number == 1){  
    
    selection_formula <- LFP ~ age + I(age^2) +  otherincomeval + hh_kids +  hh_young_kids + edu_yrs + literate + gov_transfer +
      indigenous_language + spanish_language + head_dummy  + num_f_adults + num_m_adults + pobextre  + mpcalif +
      number_female_kids + number_male_kids  + prop_mex_migrant + prop_usa_migrant +I(num_m_adults*prop_mex_migrant) +
      I(num_m_adults*prop_usa_migrant) + I(num_f_adults*prop_mex_migrant) + I(num_f_adults*prop_usa_migrant)  + 
      as.factor(year_wave_FE) + treatment_dummy + progresa_income_mom
    
    outcome_formula <-  log_wages ~ age + I(age^2) +  otherincomeval + hh_kids +  hh_young_kids + edu_yrs + literate + gov_transfer +
      indigenous_language + spanish_language + head_dummy + num_f_adults + num_m_adults + pobextre   +
      number_female_kids + number_male_kids + as.factor(year_wave_FE) + treatment_dummy + progresa_income_mom
    
  }
  
  
  reg <- selection(selection_formula, outcome_formula, data = data.df, method = "2step", print.level = 2)
  summary(reg)
  
  # Have to generate the fitted values by hand since the canned packages aren't calculating the values for NA's.
  
  coefs <- matrix(as.numeric(reg$lm$coefficients), ncol = 1)
  
  X <- cbind(model.matrix(outcome_formula, model.frame(outcome_formula, data.df, na.action = na.pass)), data.df$LFP) # Thanks, Travis, for the idea
  
  y_hat <- X%*%coefs
  y_hat <- exp(y_hat)
  hist(y_hat, breaks = 100)
  
  y_hat.df <- as.data.frame(cbind(data.df$folio, data.df$ind_ID, data.df$wavenumber, y_hat))
  return(y_hat.df)
  
}

# Chapter 2: Calculating Bargaining Power #### 

# steps in calculating BP: (1) call the function for each wave and gender, storing the results. (2) Merge the results into the main dataframe. 
#                          (3) make variables that are zero for everyone but mom's and dad's shadow wages in each period
#                          (4) aggregate these variables so that each household has a single variable per wave for mom's shadow wages and dad's shadow wages
#                          (5) calculate the BP for each wave for the parents. Save the results for each wave.

#step 1 ####
y_hat_men_combined <- BP.fun(gender_number = 0)
y_hat_women_combined <- BP.fun(gender_number = 1)
names(y_hat_men_combined) <- c("folio", "ind_ID", "wavenumber", "y_hat_men_combined")
names(y_hat_women_combined) <- c("folio", "ind_ID", "wavenumber", "y_hat_women_combined")

#names(y_hat_men_1997) <- c("folio", "ind_ID", "wavenumber", "y_hat_men_1997")
#y_hat_women_1997 <- BP.fun(gender_number = 1, wave_number = 1)
#names(y_hat_women_1997) <- c("folio", "ind_ID", "wavenumber", "y_hat_women_1997")


summary(y_hat_women_combined$y_hat_women_combined)
summary(y_hat_men_combined$y_hat_men_combined)

#step 2. DO NOT DELETE THE LINES THAT CONVERT NA's TO 0's.
hh.df_inter <- merge(hh.df, y_hat_women_combined, by = c("folio", "ind_ID", "wavenumber"), all.x = TRUE)
hh.df_inter$y_hat_women_combined[is.na(hh.df_inter$y_hat_women_combined)] <- 0
hh.df_inter <- merge(hh.df_inter, y_hat_men_combined, by = c("folio", "ind_ID", "wavenumber"), all.x = TRUE)
hh.df_inter$y_hat_men_combined[is.na(hh.df_inter$y_hat_men_combined)] <- 0

summary(hh.df_inter$y_hat_men_combined)
summary(hh.df_inter$y_hat_men_combined)
#save(hh.df_inter, file = "hh.df_inter.Rda") # clear out the gunk to free up memory 

#rm(list=ls())
#setwd("C:/Users/Matthew/Desktop/-/ProgrammingDirectory")
#load("hh.df_inter.Rda")

#steps 3 and 4

hh.df_inter$Mom_SW_combined_a <- hh.df_inter$y_hat_women_combined * hh.df_inter$head_dummy
Mom_SW_combined.df <- aggregate(hh.df_inter$Mom_SW_combined_a, by = list(Category=hh.df_inter$folio), FUN=sum)
names(Mom_SW_combined.df) <- c("folio", "Mom_SW_combined")
hh.df_inter <- merge(hh.df_inter, Mom_SW_combined.df, by = "folio")

hh.df_inter$Dad_SW_combined_a <- hh.df_inter$y_hat_men_combined * hh.df_inter$head_dummy
Dad_SW_combined.df <- aggregate(hh.df_inter$Dad_SW_combined_a, by = list(Category=hh.df_inter$folio), FUN=sum)
names(Dad_SW_combined.df) <- c("folio", "Dad_SW_combined")
hh.df_inter <- merge(hh.df_inter, Dad_SW_combined.df, by = "folio")

summary(hh.df_inter$Mom_SW_combined)
summary(hh.df_inter$Dad_SW_combined)

hh.df_inter$RSW <- hh.df_inter$BP <- c(rep(0, length(hh.df_inter$folio))) 
hh.df_inter$RSW[hh.df_inter$wave1 == 1] <- hh.df_inter$Mom_SW_combined[hh.df_inter$wave1 == 1]  / (hh.df_inter$Mom_SW_combined[hh.df_inter$wave1 == 1] + 
                                                                                                     hh.df_inter$Dad_SW_combined[hh.df_inter$wave1 == 1])

hh.df_inter$RSW[hh.df_inter$wave2 == 1] <- hh.df_inter$Mom_SW_combined[hh.df_inter$wave2 == 1]  / (hh.df_inter$Mom_SW_combined[hh.df_inter$wave2 == 1] + 
                                                                                                     hh.df_inter$Dad_SW_combined[hh.df_inter$wave2 == 1])

hh.df_inter$RSW[hh.df_inter$wave3 == 1] <- hh.df_inter$Mom_SW_combined[hh.df_inter$wave3 == 1]  / (hh.df_inter$Mom_SW_combined[hh.df_inter$wave3 == 1] + 
                                                                                                     hh.df_inter$Dad_SW_combined[hh.df_inter$wave3 == 1])


hh.df_inter$BP[hh.df_inter$wave1 == 1] <- hh.df_inter$Mom_SW_combined[hh.df_inter$wave1 == 1]  / (hh.df_inter$Mom_SW_combined[hh.df_inter$wave1 == 1] + 
                                                                                                    hh.df_inter$Dad_SW_combined[hh.df_inter$wave1 == 1])

hh.df_inter$BP[hh.df_inter$wave2 == 1] <- (hh.df_inter$Mom_SW_combined[hh.df_inter$wave2 == 1] + hh.df_inter$progresa_income_total[hh.df_inter$wave2 == 1])  /
  (hh.df_inter$Mom_SW_combined[hh.df_inter$wave2 == 1] + hh.df_inter$Dad_SW_combined[hh.df_inter$wave2 == 1] + 
     hh.df_inter$progresa_income_total[hh.df_inter$wave2 == 1])

hh.df_inter$BP[hh.df_inter$wave3 == 1] <- (hh.df_inter$Mom_SW_combined[hh.df_inter$wave3 == 1] + hh.df_inter$progresa_income_total[hh.df_inter$wave3 == 1]) /
  (hh.df_inter$Mom_SW_combined[hh.df_inter$wave3 == 1] + hh.df_inter$Dad_SW_combined[hh.df_inter$wave3 == 1] + 
     + hh.df_inter$progresa_income_total[hh.df_inter$wave3 == 1])


summary(hh.df_inter$RSW[hh.df_inter$wave1 == 1])
summary(hh.df_inter$RSW[hh.df_inter$wave2 == 1])
summary(hh.df_inter$RSW[hh.df_inter$wave3 == 1])

summary(hh.df_inter$BP[hh.df_inter$wave1 == 1])
summary(hh.df_inter$BP[hh.df_inter$wave2 == 1])
summary(hh.df_inter$BP[hh.df_inter$wave3 == 1])


#basic means test of BP
Diff_A <- mean(hh.df_inter$BP[hh.df_inter$wave3 == 1 & hh.df_inter$treatment_dummy == "Basal"], na.rm = T) - 
  mean(hh.df_inter$BP[hh.df_inter$wave1 == 1 & hh.df_inter$treatment_dummy == "Basal"], na.rm = T)
Diff_B <- mean(hh.df_inter$BP[hh.df_inter$wave3 == 1 & hh.df_inter$treatment_dummy == "Control"], na.rm = T) - 
  mean(hh.df_inter$BP[hh.df_inter$wave1 == 1 & hh.df_inter$treatment_dummy == "Control"], na.rm = T)

#basic means test of Relative ShadoW Earnings
Diff_C <- mean(hh.df_inter$RSW[hh.df_inter$wave3 == 1 & hh.df_inter$treatment_dummy == "Basal"], na.rm = T) - 
  mean(hh.df_inter$RSW[hh.df_inter$wave1 == 1 & hh.df_inter$treatment_dummy == "Basal"], na.rm = T)
Diff_D <- mean(hh.df_inter$RSW[hh.df_inter$wave3 == 1 & hh.df_inter$treatment_dummy == "Control"], na.rm = T) - 
  mean(hh.df_inter$RSW[hh.df_inter$wave1 == 1 & hh.df_inter$treatment_dummy == "Control"], na.rm = T)

# Two t-tests, one for control and one for treatment, that there was a change over time in BP

t.test(hh.df_inter$BP[hh.df_inter$wave2 == 1 & hh.df_inter$treatment_dummy == "Basal"], hh.df_inter$BP[hh.df_inter$wave1 == 1 & hh.df_inter$treatment_dummy == "Basal"])
t.test(hh.df_inter$BP[hh.df_inter$wave2 == 1 & hh.df_inter$treatment_dummy == "Control"], hh.df_inter$BP[hh.df_inter$wave1 == 1 & hh.df_inter$treatment_dummy == "Control"])

hh.df <- hh.df_inter

save(hh.df, file = "hh.df.with.BP.Rda")

# More summary Stats and Graphs

#par(mfrow=c(1,1))
#hist(hh.df_inter$BP_1997, col=rgb(0.5,0,0,1,0.5), main = "Bargaining power in 1997 and 1999", xlab = "Bargaining Power", ylim = c(0,95000)) 
#hist(hh.df_inter$BP_1999, add = TRUE, col=rgb(0,1,0,.4)) 
#legend("topleft", c("1999"), fill=c("green"))
#box()

#hist(hh.df_inter$BP_1999, col=rgb(0.5,0,0,.5), main = "Bargaining power in 1999 and 2000", xlab = "Bargaining Power", ylim = c(0,95000)) 
#hist(hh.df_inter$BP_2000, add = TRUE, col=rgb(0,0.5,0,.5)) 
#legend("topleft", c("Bargaining Power in 1999"), fill=c("maroon"))
#box()

par(mfrow=c(3,1))
hist(hh.df_inter$BP[hh.df_inter$wave1 == 1], freq=F, col = rgb(1,0,0), main = "1997", xlab = "Bargaining Power", breaks = 150, xlim = c(0, .4))
hist(hh.df_inter$BP[hh.df_inter$wave2 == 1], freq=F, col = rgb(1,0,1), main = "1999", xlab = "Bargaining Power", breaks = 150, xlim = c(0, .4))
hist(hh.df_inter$BP[hh.df_inter$wave3 == 1], freq=F, col = rgb(0,1,1), main = "2000", xlab = "Bargaining Power", breaks = 150, xlim = c(0, .4))



