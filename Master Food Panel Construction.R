####################
# Matthew Klein. generating the food data - a panel of dummy variables equal to 1 when individual i in time t consumed a food in the last week 
# This code is split into ___ chapters:
#       Chapter 1: taking a subset of the March 98 data and saving it in a smaller data.frame, called cleaned_base_food.Rda
#       Chapter 2: taking the age variables from the 97 data and merging that into the cleaned_base_food.Rda file
#       Chapter 3: generating a matrix of dummies from cleaned_base_food.Rda (using aggregate) called consumedummy_base.Rda
#       Chapter 4: taking a subset of the november 2000 data and saving it as a smaller subset, called cleaned_nov_2000_food.Rda
#       Chapter 5: using cleaned_nov_2000.Rda to generate a matrix of dummy variables called consumedummy_2000.Rda
#       Chapter 6: preparing the data sets and r binding together (with wave numbers 1 and 3) and saving as Master_Food_Panel.Rda

#       Chapter 7: (added May 30th) generating count data by taking the consumedummy_base.Rda, consumedummy_2000.Rda, cleaned_base_food.Rda, and cleaned_
#                  nov_2000_food.Rda, generating individual level count data for each wave with the third and fourth dataframes, and then multiplying them 
#                  against the consume_dummy matricies to generate individual level count data

# written 05/18/2017
# editted 05/30/2017

memory.limit(size = 14215)

# Chapter 1: generating cleaned_base_food.Rda ####

rm(list=ls())
setwd("C:/Users/Matthew/Desktop/-/ProgrammingDirectory")
f_98 <- foreign::read.spss('socioec_encel_98m.sav', use.value.labels=TRUE, to.data.frame=TRUE)

cleaned_base_food <- as.data.frame(f_98[,c("folio", "renglon", "p034a01", "p034a02", "p034a03", "p034a04", "p034a05", "p034a06", # Q34 asks how frequently the fam ate this food type 
                                           "p034a07", "p034a08", "p034a09", "p034a10", "p034b01", "p034b02", "p034b03", "p034b04",
                                           "p034b05", "p034b06", "p034b07", "p034b08", "p034b09", "p034b10", "p034b11", "p034b12",
                                           "p034c01", "p034c02", "p034c03", "p034c04", "p034c05", "p034c06", "p034c07", "p034c08",
                                           "p034d01", "p034d02", "p034d03", "p034d04", "p034d05", "p034d06",
                                           "p036a01", "p036a02", "p036a03", "p036a04", "p036a05", "p036a06", # Q36 asks how frequently the fam ate this food type 
                                           "p036a07", "p036a08", "p036a09", "p036a10", "p036b01", "p036b02", "p036b03", "p036b04",
                                           "p036b05", "p036b06", "p036b07", "p036b08", "p036b09", "p036b10", "p036b11", "p036b12",
                                           "p036c01", "p036c02", "p036c03", "p036c04", "p036c05", "p036c06", "p036c07", "p036c08",
                                           "p036d01", "p036d02", "p036d03", "p036d04", "p036d05", "p036d06")])

food_var_names <- c('jitomate o tomate rojo', 
                    'cebolla', 'papa', 'zanahorias', 'verdudas de hoja', 'narajas',
                    'platanos', 'manzanas', 
                    'limones', 'otras verduras y frutas',
                    'tortialls de maiz', 'masa de nixtamal', 'maiz en grano',
                    'pan blanco', 
                    'pan de dulce', 'pan de caja (Bimbo)', 'harina de trigo', 'sopa de pasta', 'arroz', 'galletas', 
                    'frijol', 'cereales de caja',
                    'pollo', 'carne de res o puerco', 'carne de cabra u oveja',
                    'pescados y mariscos', 
                    'sardinas o atun en lata','huevos', 'leche', 'manteca de cerdo',
                    'pastelillos en bolsa', 
                    'refrescos', 'bebidas alcoholicas', 
                    'cafe o te', 'azucar', 'aciete vegetal')

colnames(cleaned_base_food) <- c(list("folio", "ind_ID"), paste(food_var_names[1:36], "num_times_consume", sep = "_"),  
                                     paste(food_var_names[1:36], "who_ate_what", sep = "_"))

# Chapter 2: merging in age vars from ENCASEH 97_CALIF ORIG Y 2003.sav ####

hh97.df <- foreign::read.spss('ENCASEH 97_CALIF ORIG Y 2003.sav', use.value.labels=TRUE,to.data.frame=TRUE)
  
subset97.df <- hh97.df[,c("folio", "renglon", "p08")]
colnames(subset97.df) <- c("folio", "ind_ID", "age")

cleaned_base_food <- merge(cleaned_base_food, subset97.df, by = c("folio", "ind_ID"), all.x = TRUE)

save(cleaned_base_food, file = "cleaned_base_food.Rda")

# Chapter 3: generating the dummy variable matrix ####

# generating age bin dummy variables
cleaned_base_food$adult <- as.numeric(cleaned_base_food$age >= 16 & cleaned_base_food$age < 98)
cleaned_base_food$adult[is.na(cleaned_base_food$adult)] <- 0
cleaned_base_food$age_0_5 <- as.numeric(cleaned_base_food$age < 5)
cleaned_base_food$age_0_5[is.na(cleaned_base_food$age_0_5)] <- 0
cleaned_base_food$age_5_15 <- as.numeric(cleaned_base_food$age >= 5 & cleaned_base_food$age < 16)
cleaned_base_food$age_5_15[is.na(cleaned_base_food$age_5_15)] <- 0

#converting the who_ate_what information into a number, 1 - 5, that corresponds to the answer and assigning it to all members of the household 

for(i in 39:74){
  food_name <- colnames(cleaned_base_food)[i]
  cleaned_base_food$fn_cat_var <- c(rep(0, NROW(cleaned_base_food$folio)))
  
  cleaned_base_food[,food_name] <- as.character(cleaned_base_food[,food_name])
  cleaned_base_food$fn_cat_var[cleaned_base_food[,food_name] == "todos en el hogar?"] <- 1 
  cleaned_base_food$fn_cat_var[cleaned_base_food[,food_name] == "sólo los adultos?"] <- 2
  cleaned_base_food$fn_cat_var[cleaned_base_food[,food_name] == "sólo los niños menores de 5 años?"] <- 3
  cleaned_base_food$fn_cat_var[cleaned_base_food[,food_name] == "sólo los niños mayores de 5 años?"] <- 4
  cleaned_base_food$fn_cat_var[cleaned_base_food[,food_name] == "no sabe/no recuerda"] <- 5
  
  wai.df <- aggregate(cleaned_base_food$fn_cat_var, na.rm = T, by = list(Category=cleaned_base_food$folio), FUN=sum)
  colnames(wai.df) <- c("folio", paste(food_name, "_num", sep=""))
  cleaned_base_food <- merge(cleaned_base_food, wai.df, by = "folio", all.x = TRUE)
  print(i)
  }
summary(cleaned_base_food)


consumedummy_base <- as.data.frame(matrix(0, nrow = NROW(cleaned_base_food), ncol = 36))
colnames(consumedummy_base) <- food_var_names
consumedummy_base$folio <- cleaned_base_food$folio                                    
consumedummy_base$ind_ID <- cleaned_base_food$ind_ID
consumedummy_base$wavenumber = c(rep(1, NROW(consumedummy_base)))

#for each of the 36 "_num" variables loop over all and fill in the appropriate observations with 1 if they ate it. 

for(i in 1:36){
for(j in 1:NROW(cleaned_base_food)){
  if(cleaned_base_food[j,79+i] == 1) consumedummy_base[j,i] <- 1
  if(cleaned_base_food[j,79+i] == 2 & cleaned_base_food$adult[j] == 1) consumedummy_base[j,i] <- 1
  if(cleaned_base_food[j,79+i] == 3 & cleaned_base_food$age_0_5[j] == 1) consumedummy_base[j,i] <- 1
  if(cleaned_base_food[j,79+i] == 4 & cleaned_base_food$age_5_15[j] == 1) consumedummy_base[j,i] <- 1
}
  print(i)
}

summary(consumedummy_base)  

save(consumedummy_base, file = "consumedummy_base.Rda")

# Chapter 4: generating a subset of the november 2000 data ####  

rm(list=ls())
setwd("C:/Users/Matthew/Desktop/-/ProgrammingDirectory")
f_00 <- foreign::read.spss('socioec_encel_2000n.sav', use.value.labels=TRUE, to.data.frame=TRUE)

cleaned_00_food <- as.data.frame(f_00[,c("folio", "renglon", "w002cor", "w11701", "w11702", "w11703", "w11704", "w11705", # Q117 asks how frequently the fam ate this food type 
                                         "w11706", "w11707", "w11708", "w11709", "w11711", # skipping 111710 since that is Nopales, which doesn't show up in the baseline survey
                                         "w11712", "w11713", "w11714", "w11715", "w11716", "w11717", "w11718", "w11719", "w11720",
                                         "w11721", "w11722", # skipping 23, which asks other grains?
                                         "w11724", "w11725",  "w11726", "w11727", "w11728", "w11729", "w11730", "w11731", "w11732",
                                         "w11734",  "w11735", "w11736", "w11737", "w11738", "w11739",
                                         "w12201", "w12202", "w12203", "w12204", "w12205", # Q122 asks who ate what 
                                         "w12206", "w12207", "w12208", "w12209", "w12211", 
                                         "w12212", "w12213", "w12214", "w12215", "w12216", "w12217", "w12218", "w12219", "w12220",
                                         "w12221", "w12222", 
                                         "w12224", "w12225",  "w12226", "w12227", "w12228", "w12229", "w12230", "w12231", "w12232",
                                         "w12234",  "w12235", "w12236", "w12237", "w12238", "w12239")])
                 
food_var_names <- c('tomate rojo', 'cebolla', 'papa', 'zanahorias', 'verdudas de hoja', 'narajas', 'platanos', 
                    'manzanas', 'limones',
                    'otras verduras y frutas', 'tortialls de maiz', 'maiz en grano', 'pan blanco', 
                    'pan de dulce', 'pan de caja', 
                    'harina de trigo', 'sopa de pasta', 'arroz', 'galletas', 'frijol', 
                    'cereales de caja',
                    'pollo', 'carne de res o puerco', 'carne de cabra u oveja', 'pescados y mariscos', 
                    'sardinas o atun en lata', 
                    'huevos', 'leche', 'queso', 'manteca de cerdo', 'pastelillos en bolsa', 'refrescos', 
                    'bebidas alcoholicas', 'cafe', 'azucar', 'aciete vegetal')



colnames(cleaned_00_food) <- c(list("folio", "ind_ID", "age"), paste(food_var_names[1:36], "num_times_consume", sep = "_"),  
                                 paste(food_var_names[1:36], "who_ate_what", sep = "_"))


save(cleaned_00_food, file = "cleaned_00_food.Rda")

# Chapter 5: generating the consume dummy matrix for nov 2000 ####

cleaned_00_food$adult <- as.numeric(cleaned_00_food$age >= 16 & cleaned_00_food$age < 98)
cleaned_00_food$adult[is.na(cleaned_00_food$adult)] <- 0
cleaned_00_food$age_0_5 <- as.numeric(cleaned_00_food$age < 5)
cleaned_00_food$age_0_5[is.na(cleaned_00_food$age_0_5)] <- 0
cleaned_00_food$age_5_15 <- as.numeric(cleaned_00_food$age >= 5 & cleaned_00_food$age < 16)
cleaned_00_food$age_5_15[is.na(cleaned_00_food$age_5_15)] <- 0

#converting the who_ate_what information into a number, 1 - 5, that corresponds to the answer and assigning it to all members of the household 

for(i in 40:75){
  food_name <- colnames(cleaned_00_food)[i]
  cleaned_00_food$fn_cat_var <- c(rep(0, NROW(cleaned_00_food$folio)))
  
  cleaned_00_food[,food_name] <- as.character(cleaned_00_food[,food_name])
  cleaned_00_food$fn_cat_var[cleaned_00_food[,food_name] == "todos en el hogar?"] <- 1 
  cleaned_00_food$fn_cat_var[cleaned_00_food[,food_name] == "sólo los adultos?"] <- 2
  cleaned_00_food$fn_cat_var[cleaned_00_food[,food_name] == "sólo los niños menores de 5 años?"] <- 3
  cleaned_00_food$fn_cat_var[cleaned_00_food[,food_name] == "sólo los niños mayores de 5 años?"] <- 4
  cleaned_00_food$fn_cat_var[cleaned_00_food[,food_name] == "no sabe/no recuerda"] <- 5
  
  wai.df <- aggregate(cleaned_00_food$fn_cat_var, na.rm = T, by = list(Category=cleaned_00_food$folio), FUN=sum)
  colnames(wai.df) <- c("folio", paste(food_name, "_num", sep=""))
  cleaned_00_food <- merge(cleaned_00_food, wai.df, by = "folio", all.x = TRUE)
  print(i)
}
summary(cleaned_00_food)


consumedummy_00 <- as.data.frame(matrix(0, nrow = NROW(cleaned_00_food), ncol = 36))
colnames(consumedummy_00) <- food_var_names
consumedummy_00$folio <- cleaned_00_food$folio                                    
consumedummy_00$ind_ID <- cleaned_00_food$ind_ID
consumedummy_00$wavenumber = c(rep(3, NROW(consumedummy_00)))

#for each of the 36 "_num" variables

for(i in 1:36){
  for(j in 1:NROW(cleaned_00_food)){
    if(cleaned_00_food[j,79+i] == 1) consumedummy_00[j,i] <- 1
    if(cleaned_00_food[j,79+i] == 2 & cleaned_00_food$adult[j] == 1) consumedummy_00[j,i] <- 1
    if(cleaned_00_food[j,79+i] == 3 & cleaned_00_food$age_0_5[j] == 1) consumedummy_00[j,i] <- 1
    if(cleaned_00_food[j,79+i] == 4 & cleaned_00_food$age_5_15[j] == 1) consumedummy_00[j,i] <- 1
  }
  print(i)
}

summary(consumedummy_00)  

save(consumedummy_00, file = "consumedummy_00.Rda")

# Chapter 6: R binding the two data.frames into a single panel ####

rm(list=ls())
setwd("C:/Users/Matthew/Desktop/-/ProgrammingDirectory")
load("consumedummy_base.Rda")
load("consumedummy_00.Rda")

colnames(consumedummy_base)[1] <- "tomate rojo" # previously jitomate o ...
colnames(consumedummy_base)[34] <- "cafe" # previously cafe o te
colnames(consumedummy_base)[16] <- "pan de caja" # previously... (Bimbo) - a brand name in parenthases

consumedummy_00 <- consumedummy_00[,-29] # dropping queso (not in baseline)
consumedummy_base <- consumedummy_base[,-12] # dropping masa de nixtomal (not in 2000)

consumedummy_base_ao <- consumedummy_base[, order(colnames(consumedummy_base))]
consumedummy_00_ao <- consumedummy_00[, order(colnames(consumedummy_00))]

colnames(consumedummy_base_ao) == colnames(consumedummy_00_ao)

master_food.df <- data.frame(rbind(consumedummy_base_ao, consumedummy_00_ao))
save(master_food.df, file = "Master_Food_Panel.Rda")

xtable::xtable(table(master_food.df$huevos, master_food.df$wavenumber))

stargazer::stargazer(master_food.df[master_food.df$wavenumber == 1,], master_food.df[master_food.df$wavenumber == 3,], 
                     omit = c("ind_ID", "folio", "wavenumber"), nobs = FALSE, min.max = FALSE)

# merge in contba so that I can assess the hh level trends in food by year and by treatment 
f_98 <- foreign::read.spss('socioec_encel_98m.sav', use.value.labels=TRUE, to.data.frame=TRUE)

f_sub <- f_98[, c("folio", "renglon", "contba")]
names(f_sub) <- c("folio", "ind_ID", "contba")

sum_Stats <- merge(f_sub, master_food.df, by = c("folio", "ind_ID"))

stargazer::stargazer(sum_Stats[sum_Stats$contba == "Basal" & sum_Stats$wavenumber == 3,], min.max = FALSE, nobs = FALSE)
stargazer::stargazer(sum_Stats[sum_Stats$contba == "Basal" & sum_Stats$wavenumber == 1,], min.max = FALSE, nobs = FALSE)
stargazer::stargazer(sum_Stats[sum_Stats$contba == "Control" & sum_Stats$wavenumber == 3,], min.max = FALSE, nobs = FALSE)
stargazer::stargazer(sum_Stats[sum_Stats$contba == "Control" & sum_Stats$wavenumber == 1,], min.max = FALSE, nobs = FALSE)

stargazer::stargazer(sum_Stats[sum_Stats$contba == "Control",], min.max = FALSE, nobs = FALSE)
stargazer::stargazer(sum_Stats[sum_Stats$contba == "Basal",], min.max = FALSE, nobs = FALSE)
# Chapter 7: generating Count Data ####

rm(list=ls())
setwd("C:/Users/Matthew/Desktop/-/ProgrammingDirectory")
load("consumedummy_base.Rda")
load("consumedummy_00.Rda")
load("cleaned_base_food.Rda")
load("cleaned_00_food.Rda")

# Generating the count data matrix for the baseline 
count_data_base.df <- as.data.frame(matrix(0, nrow = NROW(cleaned_base_food), ncol = 2)) 
count_data_base.df[,1] <- cleaned_base_food$folio 
count_data_base.df[,2] <- cleaned_base_food$ind_ID 
colnames(count_data_base.df) <- c("folio", "ind_ID")

for(i in 3:38){ # make everyone in the household have the same information for num_times_consume information 
  count.df <- aggregate(cleaned_base_food[,i], na.rm = T, by = list(Category=cleaned_base_food$folio), FUN=sum) 
  colnames(count.df) <- c("folio", colnames(cleaned_base_food)[i])
  count_data_base.df <- merge(count_data_base.df, count.df, by ="folio") 
  
  count_data_base.df[,i] <- count_data_base.df[,i]*consumedummy_base[,i-2] # multiply the count data against the consume_dummy. offset by two. see colnames of both
}

count_data_base.df$wavenumber <- c(rep(1, NROW(count_data_base.df)))

# Generating the count data matrix for 2000
count_data_00.df <- as.data.frame(matrix(0, nrow = NROW(cleaned_00_food), ncol = 3)) 
count_data_00.df[,1] <- cleaned_00_food$folio 
count_data_00.df[,2] <- cleaned_00_food$ind_ID 
count_data_00.df[,3] <- c(rep(3, NROW(count_data_00.df)))
colnames(count_data_00.df) <- c("folio", "ind_ID", "wavenumber")

for(i in 4:39){ # make everyone in the household have the same information for num_times_consume information 
  count.df <- aggregate(cleaned_00_food[,i], na.rm = T, by = list(Category=cleaned_00_food$folio), FUN=sum) 
  colnames(count.df) <- c("folio", colnames(cleaned_00_food)[i])
  count_data_00.df <- merge(count_data_00.df, count.df, by ="folio") 
  
  count_data_00.df[,i] <- count_data_00.df[,i]*consumedummy_00[,i-3] # multiply the count data against the consume_dummy. offset - see colnames for both
}

# Merging them into a single data.frame

colnames(count_data_base.df)[3] <- "tomate rojo_num_times_consume" # previously jitomate o ...
colnames(count_data_base.df)[36] <- "cafe_num_times_consume" # previously cafe o te
colnames(count_data_base.df)[18] <- "pan de caja_num_times_consume" # previously... (Bimbo) - a brand name in parenthases

count_data_00.df <- count_data_00.df[,-32] # dropping queso (not in baseline)
count_data_base.df <- count_data_base.df[,-14] # dropping masa de nixtomal (not in 2000)

count_data_base_ao <- count_data_base.df[, order(colnames(count_data_base.df))]
count_data_00_ao <- count_data_00.df[, order(colnames(count_data_00.df))]

table(colnames(count_data_00_ao) == colnames(count_data_base_ao))

count_food.df <- data.frame(rbind(count_data_base_ao, count_data_00_ao))
save(count_food.df, file = "count_food.Rda")



#summary stats
#stargazer::stargazer(count_data_00.df, count_data_base.df, min.max = FALSE, nobs = FALSE)

#load("count_food.Rda")
#f_98 <- foreign::read.spss('socioec_encel_98m.sav', use.value.labels=TRUE, to.data.frame=TRUE)
#f <- f_98[,c("folio", "contba")]
#count_food.df <- merge(count_food.df, f, by = "folio", all.y = FALSE)

#summary(count_food.df$pollo_num_times_consume[count_food.df$wavenumber == 3 & count_food.df$contba == "Basal"])
#summary(count_food.df$pollo_num_times_consume[count_food.df$wavenumber == 3 & count_food.df$contba == "Control"])

#sub_treat <- subset(count_food.df, count_food.df$contba == "Basal")
#sub_control <- subset(count_food.df,count_food.df$contba == "Control")


#stargazer::stargazer(sub_treat[sub_treat$wavenumber == 3,], sub_control[sub_control$wavenumber == 3,])










