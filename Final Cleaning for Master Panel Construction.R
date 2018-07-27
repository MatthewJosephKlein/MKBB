# Creating the loaclity variables
# EDIT HISTORY: 9/8/17: Changed value.labels = TRUE to FALSE
#               01/22/2018: moved remaining data cleaning from "Bargainng Power estimation 1997 to 2000.R" and 
#                           "Bootstrap Function Code.R" to the bottom of this code and organized this into three chapters
# Chapter 1: location level data
# Chapter 2: the remaining data cleaning previously stored in "Bargaining Power Estimation 1997 to 2000.R"
# Chapter 3: The remaining data cleaning previously stored in "Bootstrap Function Code.R"

# Chapter 1 ####
rm(list=ls())
#setwd("C:/Users/Matthew/Desktop/-/ProgrammingDirectory")
load("Master_Panel.Rda")
hh.df <- final.panel.97.99.00.df

library("stargazer")
library("dummies")
library("xtable")

# Making unique Location ID's to merge the datasets 

#editing state to be numbers instead of atomics
hh.df$state <- formatC(as.numeric(hh.df$state), flag = "0", width = 2) 
hh.df$municipio <- formatC(hh.df$municipio, flag = "0", width = 3) 
hh.df$localidad <- formatC(hh.df$localidad, flag = "0", width = 4) 
hh.df$unique_loc_id <- paste0(hh.df$state, hh.df$municipio, hh.df$localidad) 

# Chapter 1: six variables constructed from the Master_Panel ####
# number of people that migrate from a location / number of people in each location, for each year, for MEX and USA

# number of people in each location in each year

total_97 <- as.data.frame(table(subset(hh.df, hh.df$wave1 == 1)$unique_loc_id))
names(total_97) <- c("unique_loc_id", "number_hh_97")

total_99 <- as.data.frame(table(subset(hh.df, hh.df$wave2 == 1)$unique_loc_id))
names(total_99) <- c("unique_loc_id", "number_hh_99")

total_00 <- as.data.frame(table(subset(hh.df, hh.df$wave3 == 1)$unique_loc_id))
names(total_00) <- c("unique_loc_id", "number_hh_00")

hh.df$hh_mex_mig_97_head_dummy <- hh.df$hh_mex_mig_99_head_dummy <- hh.df$hh_mex_mig_00_head_dummy <-
  hh.df$hh_usa_mig_97_head_dummy <- hh.df$hh_usa_mig_99_head_dummy <- hh.df$hh_usa_mig_00_head_dummy <- c(rep(0, length(hh.df$folio)))

hh.df$hh_mex_mig_97_head_dummy[hh.df$MEX_Migrant_97 > 0 & hh.df$ind_ID == 1] <- 1
hh.df$hh_mex_mig_99_head_dummy[hh.df$MEX_Migrant_99 > 0 & hh.df$ind_ID == 1] <- 1
hh.df$hh_mex_mig_00_head_dummy[hh.df$MEX_Migrant_00 > 0 & hh.df$ind_ID == 1] <- 1
hh.df$hh_usa_mig_97_head_dummy[hh.df$USA_Migrant_97 > 0 & hh.df$ind_ID == 1] <- 1
hh.df$hh_usa_mig_99_head_dummy[hh.df$USA_Migrant_99 > 0 & hh.df$ind_ID == 1] <- 1
hh.df$hh_usa_mig_00_head_dummy[hh.df$USA_Migrant_00 > 0 & hh.df$ind_ID == 1] <- 1

m7 <- aggregate(hh.df$hh_mex_mig_97_head_dummy, by = list(Category = hh.df$unique_loc_id), FUN=sum)
colnames(m7) <- c("unique_loc_id", "number_hh_with_mex_migrant_97")

m9 <- aggregate(hh.df$hh_mex_mig_99_head_dummy, by = list(Category = hh.df$unique_loc_id), FUN=sum)
colnames(m9) <- c("unique_loc_id", "number_hh_with_mex_migrant_99")

m0 <- aggregate(hh.df$hh_mex_mig_00_head_dummy, by = list(Category = hh.df$unique_loc_id), FUN=sum)
colnames(m0) <- c("unique_loc_id", "number_hh_with_mex_migrant_00")


u7 <- aggregate(hh.df$hh_usa_mig_97_head_dummy, by = list(Category = hh.df$unique_loc_id), FUN=sum)
colnames(u7) <- c("unique_loc_id", "number_hh_with_usa_migrant_97")

u9 <- aggregate(hh.df$hh_usa_mig_99_head_dummy, by = list(Category = hh.df$unique_loc_id), FUN=sum)
colnames(u9) <- c("unique_loc_id", "number_hh_with_usa_migrant_99")

u0 <- aggregate(hh.df$hh_usa_mig_00_head_dummy, by = list(Category = hh.df$unique_loc_id), FUN=sum)
colnames(u0) <- c("unique_loc_id", "number_hh_with_usa_migrant_00")

hh.df <- merge(hh.df, m7, by = "unique_loc_id", all.x = TRUE)
hh.df <- merge(hh.df, m9, by = "unique_loc_id", all.x = TRUE)
hh.df <- merge(hh.df, m0, by = "unique_loc_id", all.x = TRUE)
hh.df <- merge(hh.df, u7, by = "unique_loc_id", all.x = TRUE)
hh.df <- merge(hh.df, u9, by = "unique_loc_id", all.x = TRUE)
hh.df <- merge(hh.df, u0, by = "unique_loc_id", all.x = TRUE)
hh.df <- merge(hh.df, total_97, by = "unique_loc_id", all.x = TRUE)
hh.df <- merge(hh.df, total_99, by = "unique_loc_id", all.x = TRUE)
hh.df <- merge(hh.df, total_00, by = "unique_loc_id", all.x = TRUE)

hh.df$prop_mex_migrant_97 <- hh.df$number_hh_with_mex_migrant_97 / hh.df$number_hh_97
hh.df$prop_mex_migrant_99 <- hh.df$number_hh_with_mex_migrant_99 / hh.df$number_hh_99
hh.df$prop_mex_migrant_00 <- hh.df$number_hh_with_mex_migrant_00 / hh.df$number_hh_00

hh.df$prop_usa_migrant_97 <- hh.df$number_hh_with_usa_migrant_97 / hh.df$number_hh_97
hh.df$prop_usa_migrant_99 <- hh.df$number_hh_with_usa_migrant_99 / hh.df$number_hh_99
hh.df$prop_usa_migrant_00 <- hh.df$number_hh_with_usa_migrant_00 / hh.df$number_hh_00

hh.df$prop_mex_migrant <- hh.df$prop_usa_migrant <- c(rep(0, length(hh.df$folio)))

hh.df$prop_mex_migrant[hh.df$wave1 == 1] <- hh.df$prop_mex_migrant_97[hh.df$wave1 == 1]
hh.df$prop_mex_migrant[hh.df$wave2 == 1] <- hh.df$prop_mex_migrant_99[hh.df$wave2 == 1]
hh.df$prop_mex_migrant[hh.df$wave3 == 1] <- hh.df$prop_mex_migrant_00[hh.df$wave3 == 1]

hh.df$prop_usa_migrant[hh.df$wave1 == 1] <- hh.df$prop_usa_migrant_97[hh.df$wave1 == 1]
hh.df$prop_usa_migrant[hh.df$wave2 == 1] <- hh.df$prop_usa_migrant_99[hh.df$wave2 == 1]
hh.df$prop_usa_migrant[hh.df$wave3 == 1] <- hh.df$prop_usa_migrant_00[hh.df$wave3 == 1]

summary(hh.df$prop_mex_migrant_97)
summary(hh.df$prop_mex_migrant_99)
summary(hh.df$prop_mex_migrant_00)

summary(hh.df$prop_usa_migrant_97)
summary(hh.df$prop_usa_migrant_99)
summary(hh.df$prop_usa_migrant_00)

# dummies for whether the household has a migrant to the USA
# dummy(hh.df$hh_migrant_loc)


# Chapter 2: variables taken from the locality level data #### 
# average wages for men and women in ag/non-ag/employee positions
# infrastructure from 2000

loc.99 <- foreign::read.spss('bd_rur_1999_n_localidad.sav', use.value.labels = FALSE, to.data.frame = TRUE)
colnames(loc.99)
summary(loc.99)

# Making unique Location ID's to merge the datasets  
loc.99$cveest <- formatC(loc.99$cveest, flag = "0", width = 2) 
loc.99$cvemun <- formatC(loc.99$cvemun, flag = "0", width = 3) 
loc.99$cveloc <- formatC(loc.99$cveloc, flag = "0", width = 4) 
loc.99$unique_loc_id <- paste0(loc.99$cveest, loc.99$cvemun, loc.99$cveloc) 

loc_vars_99.df <- loc.99[, c("unique_loc_id", "n043", "n044", "n045", "n047", "n048", "n049","n050", "n051", "n052", "n053", "n054")]
colnames(loc_vars_99.df) <- c("unique_loc_id", "min_wage_99", "male_ag_wages_99", "female_ag_wages_99", "male_lab_wages_99", "male_lab_description_99",
                              "female_lab_wages_99", "female_lab_description_99", "male_employee_wages_99", "male_employee_description_99", 
                              "female_employee_wages_99", "female_employee_description_99")
loc_vars_99.df$wave2 <- c(rep(1, length(loc_vars_99.df$unique_loc_id)))

hh.df <- merge(hh.df, loc_vars_99.df, by = c("unique_loc_id", "wave2"), all.x = TRUE)

loc.00 <- foreign::read.spss('bd_rur_2000_n_localidad.sav', use.value.labels = FALSE, to.data.frame = TRUE)
colnames(loc.00)
summary(loc.00)

# Making unique Location ID's to merge the datasets 
loc.00$cveest <- formatC(loc.00$cveest, flag = "0", width = 2) 
loc.00$cvemun <- formatC(loc.00$cvemun, flag = "0", width = 3) 
loc.00$cveloc <- formatC(loc.00$cveloc, flag = "0", width = 4) 
loc.00$unique_loc_id <- paste0(loc.00$cveest, loc.00$cvemun, loc.00$cveloc) 

loc_vars_00.df <- loc.00[, c("unique_loc_id", "wl041", "wl042", "wl043", "wl045", "wl046", "wl047","wl048", "wl049", "wl050", "wl051", "wl052")]
colnames(loc_vars_00.df) <- c("unique_loc_id", "min_wage_00", "male_ag_wages_00", "female_ag_wages_00", "male_lab_wages_00", "male_lab_description_00",
                              "female_lab_wages_00", "female_lab_description_00", "male_employee_wages_00", "male_employee_description_00",
                              "female_employee_wages_00", "female_employee_description_00")
loc_vars_00.df$wave3 <- c(rep(1, length(loc_vars_00.df$unique_loc_id)))

hh.df <- merge(hh.df, loc_vars_00.df, by = c("unique_loc_id", "wave3"), all.x = TRUE)

save(hh.df, file = "Master_with_loc.Rda")

# Chapter 2 ####

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

hh.df$year_wave_FE <- paste0(hh.df$wavenumber, hh.df$seven_states, sep = "")

hh.df$progresa_income_mom <- hh.df$progresa_income_total*hh.df$head_dummy*hh.df$sex

save(hh.df, file = "hh.df.Rda")

# Incorporating Exclusion Restriction Variables #

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

# Chapter 3 ####
rm(list=ls())
library("sampleSelection")
library("lfe")
library("glmmML")
library("plyr")

# Chapter 1: Final Data Cleaning and Functions (same code as in Bootstrap Function.R) ####

load("hh.df.Rda") # This data.frame is generated in Chapter 1 of the file "Bargaining Power Estimation 1997 to 2000.Rda", Approximately lines 1-240. 
# The input into that .Rda file is "Master_with_loc.Rda", which is generated by the code called "location level variable construction"
# Which, in turn, takes as an input "Master_Panel.Rda" as input, which is generated by "Master Panel Construction.R"

load("master_food_hh.Rda") # Generated in "Master HH Level Food Panel Construction.R"

# Merge the food data.frames in 

drops <- c("age")
master_food_hh.df <- master_food_hh.df[ , !(names(master_food_hh.df) %in% drops)]

hh.df <- merge(hh.df, master_food_hh.df, by = c("folio", "ind_ID", "wavenumber"))

hh.df$unique_ID <- as.integer(paste0(hh.df$folio, hh.df$ind_ID, hh.df$wavenumber, sep = ""))

hh.df$loc_id <- as.numeric(paste0(hh.df[,"state"], hh.df[,"municipio"], hh.df[,"localidad"]))

# Generating the hh_log_wages variable
# 1997 wages aggregated
hh_wages_97.df <- aggregate((hh.df$wages[hh.df$wave1 == 1] + hh.df$otherincomeval1[hh.df$wavenumber == 1] + hh.df$otherincomeval2[hh.df$wavenumber == 1]),
                            by = list(Category=hh.df$folio[hh.df$wave1 == 1]), FUN=sum, na.rm = T)
colnames(hh_wages_97.df) <- c("folio", "hh_wages_97")
hh_wages_97.df$wavenumber <- c(rep(1, length(hh_wages_97.df$folio)))

hh.df <- merge(hh.df, hh_wages_97.df, by = c("folio", "wavenumber"), all.x = T)

# 1999 wages aggregated plus progresa
hh.df$wages_plus_progresa <- hh.df$wages + hh.df$progresa_income_total*hh.df$head_dummy*hh.df$sex
hh_wages_99.df <- aggregate((hh.df$wages_plus_progresa[hh.df$wavenumber == 2] + hh.df$otherincomeval1[hh.df$wavenumber == 2] +
                               hh.df$otherincomeval2[hh.df$wavenumber == 2]),
                            by = list(Category=hh.df$folio[hh.df$wave2 == 1]), FUN=sum, na.rm = T)
colnames(hh_wages_99.df) <- c("folio", "hh_wages_99")
hh_wages_99.df$wavenumber <- c(rep(2, length(hh_wages_99.df$folio)))

hh.df <- merge(hh.df, hh_wages_99.df, by = c("folio", "wavenumber"), all.x = T)

# 2000 wages aggregated plus progresa
hh_wages_00.df <- aggregate((hh.df$wages_plus_progresa[hh.df$wavenumber == 3] + hh.df$otherincomeval1[hh.df$wavenumber == 3] +
                               hh.df$otherincomeval2[hh.df$wavenumber == 3]), 
                            by = list(Category=hh.df$folio[hh.df$wavenumber == 3]), FUN=sum, na.rm = T)
colnames(hh_wages_00.df) <- c("folio", "hh_wages_00")
hh_wages_00.df$wavenumber <- c(rep(3, length(hh_wages_00.df$folio)))

hh.df <- merge(hh.df, hh_wages_00.df, by = c("folio", "wavenumber"), all.x = T)

hh.df$hh_wages_97[is.na(hh.df$hh_wages_97)] <- 0
hh.df$hh_wages_99[is.na(hh.df$hh_wages_99)] <- 0 
hh.df$hh_wages_00[is.na(hh.df$hh_wages_00)] <- 0

hh.df$hh_wages <- hh.df$hh_wages_97 + hh.df$hh_wages_99 + hh.df$hh_wages_00
hh.df$hh_log_wages <- log(hh.df$hh_wages)
hh.df$hh_log_wages[hh.df$hh_log_wages == -Inf] <- NA

summary(hh.df$hh_log_wages)
summary(hh.df$hh_wages)

#Merging in the Prices Variables
load("prices.panel.Rda")

hh.df$unique_ID <- as.integer(paste0(hh.df$folio, hh.df$ind_ID, hh.df$wavenumber, sep = ""))

hh.df$loc_id <- as.numeric(paste0(hh.df[,"state"], hh.df[,"municipio"], hh.df[,"localidad"]))


#editing state to be numbers instead of atomics

prices.panel$entidad <- formatC(as.numeric(prices.panel$entidad),
                                flag = "0", width = 2) 
prices.panel$mpio <- formatC(prices.panel$mpio, flag = "0", width = 3) 
prices.panel$local <- formatC(prices.panel$local, flag = "0", width = 4) 
prices.panel$loc_id <- paste0(prices.panel$entidad, prices.panel$mpio,
                              prices.panel$local) 

prices.panel$loc_id <- as.numeric(prices.panel$loc_id)

hh.df <- merge(hh.df, prices.panel, by = c("loc_id", "wavenumber"))

# Generating the Exclusion Restriction Proportion Variables

hh.df$need_permission_dummy <- hh.df$need_permission
hh.df$need_permission_dummy[hh.df$need_permission_dummy != 1] <- 0

hh.df$accompanied_dummy <- hh.df$accompanied
hh.df$accompanied_dummy[hh.df$accompanied_dummy == 1] <- 0
hh.df$accompanied_dummy[hh.df$accompanied_dummy > 1] <- 1

hh.df$AD_women_job_baseline_dummy <- hh.df$AD_women_job
hh.df$AD_women_job_baseline_dummy[hh.df$AD_women_job_baseline_dummy > 1] <- 0
hh.df$AD_women_job_baseline_dummy[hh.df$wavenumber == 3 | hh.df$wavenumber == 2] <- NA
table(hh.df$AD_women_job_baseline_dummy)

hh.df$AD_women_job_2000_dummy <- hh.df$AD_women_job
hh.df$AD_women_job_2000_dummy[hh.df$AD_women_job_2000_dummy > 1] <- 0
hh.df$AD_women_job_2000_dummy[hh.df$wavenumber == 1 | hh.df$wavenumber == 2] <- NA
table(hh.df$AD_women_job_2000_dummy)

er.sub <- aggregate(hh.df$need_permission_dummy, by = list(hh.df$loc_id, hh.df$wavenumber), FUN=mean, na.rm=T)
er.sub2 <- aggregate(hh.df$accompanied_dummy, by = list(hh.df$loc_id, hh.df$wavenumber), FUN=mean, na.rm=T)
er.sub3 <- aggregate(hh.df$AD_women_job_baseline_dummy, by = list(hh.df$loc_id), FUN=mean, na.rm=T)
er.sub4 <- aggregate(hh.df$AD_women_job_2000_dummy, by = list(hh.df$loc_id), FUN=mean, na.rm=T)

colnames(er.sub) <- c("loc_id", "wavenumber", "proportion_need_permission")
colnames(er.sub2) <- c("loc_id", "wavenumber", "proportion_need_accompany")
colnames(er.sub3) <- c("loc_id", "proportion_women_job_opinion_baseline")
colnames(er.sub4) <- c("loc_id", "proportion_women_job_opinion_1999")

hh.df <- merge(hh.df, er.sub, by = c("loc_id", "wavenumber"), all.x=T)
hh.df <- merge(hh.df, er.sub2, by = c("loc_id", "wavenumber"), all.x=T)
hh.df <- merge(hh.df, er.sub3, by = c("loc_id"), all.x=T)
hh.df <- merge(hh.df, er.sub4, by = c("loc_id"), all.x=T)

hh.df$not_base_line <- rep(0)
hh.df$not_base_line[hh.df$wavenumber==2] <- 1
hh.df$not_base_line[hh.df$wavenumber==3] <- 1

hh.df$ER <- rep(NA)
hh.df$ER[hh.df$wavenumber==1] <- hh.df$proportion_women_job_opinion_baseline[hh.df$wavenumber==1]
hh.df$ER[hh.df$wavenumber==2] <- hh.df$proportion_women_job_opinion_1999[hh.df$wavenumber==2]
hh.df$ER[hh.df$wavenumber==3] <- hh.df$proportion_women_job_opinion_1999[hh.df$wavenumber==3]

#drop households with earnings outliers 
hh.df$outlier_dummy <- rep(0)
hh.df$outlier_dummy[hh.df$wages >= quantile(hh.df$wages[hh.df$wages>0], c(0.99))] <- 1
temp.df <- aggregate(hh.df$outlier_dummy, by = list(hh.df$folio), FUN=mean, na.rm=T)
colnames(temp.df) <- c("folio", "hh_outlier_dummy")
hh.df <- merge(hh.df, temp.df, by = "folio")
hh.df <- subset(hh.df, hh.df$hh_outlier_dummy == 0)

hh.df$outlier_dummy <- rep(0)
hh.df$outlier_dummy[hh.df$otherincomeval >= quantile(hh.df$otherincomeval[hh.df$otherincomeval>0], c(.99))] <- 1
temp.df <- aggregate(hh.df$outlier_dummy, by = list(hh.df$folio), FUN=mean, na.rm=T)
colnames(temp.df) <- c("folio", "hh_outlier_dummy_2")
hh.df <- merge(hh.df, temp.df, by = "folio")
hh.df <- subset(hh.df, hh.df$hh_outlier_dummy_2 == 0)

hh.df$outlier_dummy <- rep(0)
hh.df$outlier_dummy[hh.df$hh_wages >= quantile(hh.df$hh_wages[hh.df$hh_wages>0], c(.99)) ] <- 1
temp.df <- aggregate(hh.df$outlier_dummy, by = list(hh.df$folio), FUN=mean, na.rm=T)
colnames(temp.df) <- c("folio", "hh_outlier_dummy_3")
hh.df <- merge(hh.df, temp.df, by = "folio")
hh.df <- subset(hh.df, hh.df$hh_outlier_dummy_3 == 0)

table(hh.df$treatment_dummy_num, hh.df$treatment_dummy)
hh.df$treatment_dummy_num[hh.df$treatment_dummy_num == 2] <- 0 

hh.df$T <- hh.df$otherincomeval + hh.df$progresa_income_mom

# One variable per HH per period that describes Mom's T, one per period per HH that describes Dad's T. 
# Use variable demarcator "_total" to denote that the variable is the same for each member of the HH (different across periods)

hh.df$T_mom <- hh.df$T_dad <- rep(0)
hh.df$T_mom[hh.df$head_dummy==1 & hh.df$sex == 1] <- hh.df$T[hh.df$head_dummy==1 & hh.df$sex == 1]
hh.df$T_dad[hh.df$head_dummy==1 & hh.df$sex == 0] <- hh.df$T[hh.df$head_dummy==1 & hh.df$sex == 0]
summary(hh.df$T_dad)
summary(hh.df$T_mom)

temp.df <- aggregate(hh.df$T_mom, by = list(hh.df$folio, hh.df$wavenumber), FUN = sum, na.rm=T)
colnames(temp.df) <- c("folio", "wavenumber", "T_mom_total")

hh.df <- merge(hh.df, temp.df, by = c("folio", "wavenumber"))
summary(hh.df$T_mom_total)

temp.df <- aggregate(hh.df$T_dad, by = list(hh.df$folio, hh.df$wavenumber), FUN = sum, na.rm=T)
colnames(temp.df) <- c("folio", "wavenumber", "T_dad_total")

hh.df <- merge(hh.df, temp.df, by = c("folio", "wavenumber"))
summary(hh.df$T_dad_total)

# table(hh.df$T_mom_total >= hh.df$progresa_income_total) # One check that coding is correct.
# View(hh.df[hh.df$T_mom_total < hh.df$progresa_income_total,]) # For whom does this fail? 
# Well looks like some households have a HH head that leaves the home to migrate or some such for one or more year. Let's drop them.
hh.df <- hh.df[hh.df$T_mom_total >= hh.df$progresa_income_total,]

save(hh.df, file = "hh.df.Rda")
