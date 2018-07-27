# Creating the loaclity variables
# EDIT HISTORY: 9/8/17: Changed value.labels = TRUE to FALSE
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

