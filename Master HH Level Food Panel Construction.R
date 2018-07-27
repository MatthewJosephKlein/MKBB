# Matt K
# 06/26/2017
# Generating a panel of the baseline, NOV 1999, and NOV 2000 household-level binary and count data for each food type
# Chapter 1: generating a dataframe of folio, ind_ID, wavenumber, dummy data at the hh level, and count data at the hh level for the baseline
# Chapter 2: generating a dataframe of folio, ind_ID, wavenumber, dummy data at the hh level, and count data at the hh level for 1999
# Chapter 3: generating a dataframe of folio, ind_ID, wavenumber, dummy data at the hh level, and count data at the hh level for 2000
# Chapter 4: R binding the three prior data.frames into a single panel

# EDIT HISTORY: 9/8/17 - changed use.value.labels=TRUE to use.value.labels=FALSE

#memory.limit(size = 14215)

rm(list=ls())
#setwd("C:/Users/Matthew/Desktop/-/ProgrammingDirectory")


# Chapter 1: Baseline food data ####

f_98 <- foreign::read.spss('socioec_encel_98m.sav', use.value.labels=FALSE, to.data.frame=TRUE)

cleaned_base_food <- as.data.frame(f_98[,c("folio", "renglon", "p034a01", "p034a02", "p034a03", "p034a04", "p034a05", "p034a06", # Q34 asks how frequently the fam ate this food type 
                                           "p034a07", "p034a08", "p034a09", "p034a10", "p034b01", "p034b02", "p034b03", "p034b04",
                                           "p034b05", "p034b06", "p034b07", "p034b08", "p034b09", "p034b10", "p034b11", "p034b12",
                                           "p034c01", "p034c02", "p034c03", "p034c04", "p034c05", "p034c06", "p034c07", "p034c08",
                                           "p034d01", "p034d02", "p034d03", "p034d04", "p034d05", "p034d06")])

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

colnames(cleaned_base_food) <- c(list("folio", "ind_ID"), paste(food_var_names[1:36], "num_times_consume", sep = "_"))

# Aggregating the information to the household level
for(i in 3:NCOL(cleaned_base_food)){
  food_name <- colnames(cleaned_base_food)[i]
  wai.df <- aggregate(cleaned_base_food[,i], na.rm = T, by = list(Category=cleaned_base_food$folio), FUN=sum)
  colnames(wai.df) <- c("folio", paste(food_name, sep=""))
  cleaned_base_food <- merge(cleaned_base_food, wai.df, by = "folio", all.x = TRUE)
  print(i)
}
summary(cleaned_base_food)

colnames(cleaned_base_food)[39:74] <- c(paste(food_var_names, "num_times_consume", sep = "_"))

cleaned_base_food_hh <- as.data.frame(cbind(cleaned_base_food[,1:2], cleaned_base_food[,39:74], cleaned_base_food[,39:74]))

# Generating dummy variables for each of the food items

colnames(cleaned_base_food_hh)[39:NCOL(cleaned_base_food_hh)] <- food_var_names

cleaned_base_food_hh[,39:NCOL(cleaned_base_food_hh)][cleaned_base_food_hh[,39:NCOL(cleaned_base_food_hh)] > 1] <- 1 

# Checking that things went smoothely
a <- cleaned_base_food_hh[,3]
a[a>1] <- 1
summary(a)

hh97.df <- foreign::read.spss('ENCASEH 97_CALIF ORIG Y 2003.sav', use.value.labels=FALSE,to.data.frame=TRUE)

subset97.df <- hh97.df[,c("folio", "renglon", "p08")]
colnames(subset97.df) <- c("folio", "ind_ID", "age")

cleaned_base_food_hh <- merge(cleaned_base_food_hh, subset97.df, by = c("folio", "ind_ID"), all.x = TRUE)

# Looks good. tried beans and tomatoes and both are fine. 

save(cleaned_base_food_hh, file = "cleaned_base_food_hh.Rda")

# Chapter 2: 1999 food data ####

rm(list=ls())

hh99.df_large <- foreign::read.spss('socioec_encel_99n.sav', use.value.labels = FALSE, to.data.frame = TRUE)

hh99.df <- hh99.df_large[,c("folio", "renglon", "n002", "n06901", "n06902", "n06903", "n06904", "n06905", "n06906", "n06907", "n06908", "n06909", # fruits and veg 
                            "n06912", "n06913", "n06914", "n06915", "n06916", "n06917", "n06918", "n06919", "n06920", "n06921", "n06922", # grains 
                            "n06924", "n06925", "n06926", "n06927", "n06928", "n06929", "n06930", "n06931", "n06932", # meats
                            "n06934", "n06935", "n06936", "n06937", "n06938", "n06939")] # other 


names.list <- c('folio', 'ind_ID', 'age', 'tomate rojo', 'cebolla', 'papa', 'zanahorias',
                'verdudas de hoja', 'narajas', 'platanos', 'manzanas',
                'limones', 'tortialls de maiz', 'maiz en grano', 'pan blanco',
                'pan de dulce', 'pan de caja', 'harina de trigo', 'sopa de pasta', 
                'arroz', 'galletas', 'frijol', 'cereales de caja',
                'pollo', 'carne de res o puerco', 'carne de cabra u oveja', 
                'pescados y mariscos', 'sardinas o atun en lata', 'huevos', 'leche',
                'queso', 'manteca de cerdo', 'pastelillos en bolsa', 'refrescos',
                'bebidas alcoholicas', 'cafe', 'azucar', 'aciete vegetal')

colnames(hh99.df) <- names.list

# Aggregating the information to the household level
for(i in 4:NCOL(hh99.df)){
  food_name <- colnames(hh99.df)[i]
  wai.df <- aggregate(hh99.df[,i], na.rm = T, by = list(Category=hh99.df$folio), FUN=sum)
  colnames(wai.df) <- c("folio", paste(food_name, "_num_times_consume", sep=""))
  hh99.df <- merge(hh99.df, wai.df, by = "folio", all.x = TRUE)
  print(i)
}
summary(hh99.df)

cleaned_99_food_hh <- as.data.frame(cbind(hh99.df[,1:3], hh99.df[,39:73], hh99.df[,39:73]))

# Generating dummy variables for each of the food items

colnames(cleaned_99_food_hh)[39:NCOL(cleaned_99_food_hh)] <- names.list[4:length(names.list)]

cleaned_99_food_hh[,39:NCOL(cleaned_99_food_hh)][cleaned_99_food_hh[,39:NCOL(cleaned_99_food_hh)] > 1] <- 1 

# Checking that things went smoothely
a <- cleaned_99_food_hh[,"pollo"]
a[a>1] <- 1
summary(a)

# Looks good, checked for tomatoes and chicken

colnames(cleaned_99_food_hh)
save(cleaned_99_food_hh, file = "cleaned_99_food_hh.Rda")




# Chapter 3: 2000 food data ####

rm(list=ls())
f_00 <- foreign::read.spss('socioec_encel_2000n.sav', use.value.labels=FALSE, to.data.frame=TRUE)

cleaned_00_food <- as.data.frame(f_00[,c("folio", "renglon", "w002cor", "w11701", "w11702", "w11703", "w11704", "w11705", # Q117 asks how frequently the fam ate this food type 
                                         "w11706", "w11707", "w11708", "w11709", "w11711", # skipping 111710 since that is Nopales, which doesn't show up in the baseline survey
                                         "w11712", "w11713", "w11714", "w11715", "w11716", "w11717", "w11718", "w11719", "w11720",
                                         "w11721", "w11722", # skipping 23, which asks other grains?
                                         "w11724", "w11725",  "w11726", "w11727", "w11728", "w11729", "w11730", "w11731", "w11732",
                                         "w11734",  "w11735", "w11736", "w11737", "w11738", "w11739")])

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



colnames(cleaned_00_food) <- c(list("folio", "ind_ID", "age"), paste(food_var_names[1:36]))


# Aggregating the information to the household level
for(i in 4:NCOL(cleaned_00_food)){
  food_name <- colnames(cleaned_00_food)[i]
  wai.df <- aggregate(cleaned_00_food[,i], na.rm = T, by = list(Category=cleaned_00_food$folio), FUN=sum)
  colnames(wai.df) <- c("folio", paste(food_name, "_num_times_consume", sep=""))
  cleaned_00_food <- merge(cleaned_00_food, wai.df, by = "folio", all.x = TRUE)
  print(i)
}
summary(cleaned_00_food)

cleaned_00_food_hh <- as.data.frame(cbind(cleaned_00_food[,1:3], cleaned_00_food[,40:75], cleaned_00_food[,40:75]))

# Generating dummy variables for each of the food items

colnames(cleaned_00_food_hh)[40:NCOL(cleaned_00_food_hh)] <- food_var_names

cleaned_00_food_hh[,40:NCOL(cleaned_00_food_hh)][cleaned_00_food_hh[,40:NCOL(cleaned_00_food_hh)] > 1] <- 1 

# Checking that things went smoothely
a <- cleaned_00_food_hh[,"pollo"]
a[a>1] <- 1
summary(a)

# Looks good, checked for chicken

save(cleaned_00_food_hh, file = "cleaned_00_food_hh.Rda")


# Chapter 4: Rbinding the three dataframes into a panel #### 

rm(list=ls())

load("cleaned_base_food_hh.Rda")
load("cleaned_99_food_hh.Rda")
load("cleaned_00_food_hh.Rda")

# creating a wavenumber var for each panel
cleaned_base_food_hh$wavenumber <- c(rep(1, NROW(cleaned_base_food_hh)))
cleaned_99_food_hh$wavenumber <- c(rep(2, NROW(cleaned_99_food_hh)))
cleaned_00_food_hh$wavenumber <- c(rep(3, NROW(cleaned_00_food_hh)))


# Step 4: renaming items that are different over time

colnames(cleaned_base_food_hh)[39] <- "tomate rojo"
colnames(cleaned_base_food_hh)[3] <- "tomate rojo_num_times_consume"

colnames(cleaned_base_food_hh)[36] <- "cafe_num_times_consume"
colnames(cleaned_base_food_hh)[72] <- "cafe"

colnames(cleaned_base_food_hh)[54] <- "pan de caja"
colnames(cleaned_base_food_hh)[18] <- "pan de caja_num_times_consume"

# Step 2:  Reordering the baseline so that the columns are in the same order as they are in the later waves
ao_base <- cleaned_base_food_hh[, order(names(cleaned_base_food_hh))] #ao == "alphabetic ordering"
ao_99 <- cleaned_99_food_hh[, order(names(cleaned_99_food_hh))]
ao_00 <- cleaned_00_food_hh[, order(names(cleaned_00_food_hh))]

# step 3: dropping variables not required for analysis (or that don't show up in all of the dataframes.) 
drops_base <- c("otras verduras y frutas", "otras verduras y frutas_num_times_consume", "masa de nixtamal",
                "masa de nixtamal_num_times_consume")
ao_base <- ao_base[, !(names(ao_base) %in% drops_base)]


drops_99 <- c("queso","queso_num_times_consume")
ao_99 <- ao_99[, !(names(ao_99) %in% drops_99)]

drops_00 <-c("otras verduras y frutas", "otras verduras y frutas_num_times_consume", "queso",
             "queso_num_times_consume")
ao_00 <- ao_00[, !(names(ao_00) %in% drops_00)]

colnames(ao_00) == colnames(ao_base)
colnames(ao_00) == colnames(ao_99)
colnames(ao_99) == colnames(ao_base)

# step 4: rbind the three dataframes into a single panel: reference: unified_cd <- as.data.frame(rbind(subset1, subset2), header = T)

master_food_hh.df <- data.frame(rbind(ao_base, ao_99)) 
colnames(master_food_hh.df) <- colnames(ao_base)
master_food_hh.df <- data.frame(rbind(master_food_hh.df, ao_00)) 
save(master_food_hh.df, file = "master_food_hh.Rda")




# Summary stats

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



stargazer::stargazer(master_food_hh.df[master_food_hh.df$wavenumber == 1,c(food_var_names)], 
                     master_food_hh.df[master_food_hh.df$wavenumber == 2,c(food_var_names)], 
                     master_food_hh.df[master_food_hh.df$wavenumber == 3,c(food_var_names)], nobs = FALSE, min.max = FALSE)
 
#          hh.df[,110:144][hh.df$treatment_dummy == "Basal" & hh.df$wavenumber == 1,],
#          nobs = FALSE, min.max = FALSE)

#stargazer(hh.df[,225:259][hh.df$treatment_dummy == "Control" & hh.df$wavenumber == 3,],
#          hh.df[,225:259][hh.df$treatment_dummy == "Basal" & hh.df$wavenumber == 3,],
#          nobs = FALSE, min.max = FALSE)


