#################### Intro
# Matthew Klein.
# Master Panel Construction
# There are five chapters dedicated to panel construction, one for each survey and one for panel construction, and a chapter for Progresa transfer amount calc.
# For each new dataset, all memory is cleared, data is cleaned, the new dataset is saved as a .Rda file.
# After all dataests are prepared, they are all loaded and formatted into a panel with three waves - called Master_Panel.Rda

# This code works as follows:
#                            Chapter 1 takes as an input raw data - ENCASEH 97_CALIF ORIG Y 2003.sav (1997),
#                                      and outputs cleaned.97.Master.Rda
#                            Chapter 2 takes as an input raw data - socioec_encel_98m.sav (1998) (other portion of baseline)
#                                      and also cleaned.97.Master.Rda
#                                      and outputs Cleaned.Baseline.Master.Rda
#                            Chapter 3 takes as an input raw data - socioec_encel_99n.sav (1999) (wave 2)
#                                      and outputs cleaned.99.Master.Rda
#                            Chapter 4 takes as an input raw data - socioec_encel_2000n.sav (2000) (wave 3) 
#                                      and outputs cleaned.00.Master.Rda
#                            Chapter 5 takes as inputs Cleaned.Baseline.Rda, ____, and _____
#                                      and outputs Master_panel.Rda
#                            Chapter 6 takes Master_panel.Rda as input, calculates the amount of prgresa transfer each hh got, and writes back over the .RDa file.
#                                       

# datasets downloadable at https://evaluacion.prospera.gob.mx/es/eval_cuant/p_bases_cuanti.php
#
# Edit History: 
#
# 01/12/2017 - Copy/pasted the "data cleaning 1997 to 2000.R" file and then edited it so that, instead of dropping potentially problematic variables, I simply 
#              flag them with a dummy
# 03/22/2017 Fixed the young_kid_dummy variable, included the poor measures from the baseline, included migration destination info, included education, made hh level migration dummies. 
# 03/23/2017 fixed the sex variable to accurately reflect the sex of people who were previously interviewed 
# 05/14/2017 Computed 
# 09/7/2017 - Changed the use.value.labels = TRUE to FALSE in the read.spss lines. THIS MAKES A BIG DIFFERENCE.
#             Now lot's has to change: ex. head_dummy

# Next Step - update the 98, 99, and 2000 waves to include information over new individuals. For instance, new people have sex recorded in their respective first waves,
#   have to incldue that info, edit the old variables so that the merger goes cleanly
# Update on next step: maybe I don't need to do that?

# NEED TO CHECK WHETHER THE "aggregate" COMMANDS ARE CORRECT. THEY MAY BE INTRODUCING na'S. I CAN USE "na.rm = T" TO GET AROUND THIS

# memory.limit(size = 14215)

# Chapter 1: Baseline ####################

rm(list=ls())
setwd("C:/Users/mjklein2/Desktop/toot/Programming_Directory")
library(foreign)
hh97.df <- read.spss('ENCASEH 97_CALIF ORIG Y 2003.sav', use.value.labels = FALSE, to.data.frame = TRUE)
# 'Warning!! Check that the variables are not factors!'
str(hh97.df) #visualize

# Individual Identification Information and HH info 
folio <- hh97.df[,"folio"]
ind_ID <- hh97.df[,"renglon"]
treatment_dummy <- hh97.df[,"contba"]
municipio <- hh97.df[,'mpio']
localidad <- hh97.df[,'local']
state <- hh97.df[,'entidad']
sex <- hh97.df[,"p11"]
age <- hh97.df[,"p08"]
relation_to_jefe <- hh97.df[,"p12"]
dad_live_in_hh <- hh97.df[,"p13"]
mom_live_in_hh <- hh97.df[,"p14"]
inf <- hh97.df[,"inf"] # Ind_ID of person answering questions
table(inf) # see page 2 of the 1997 code book for more information on "inf"

pobre <- hh97.df[,"pobre"]
pobextre <- hh97.df[,"pobextre"]
mpcalif <- hh97.df[,"mpcalif"]
mppob <- hh97.df[,"mppob"]

# assigning hh_head status to male head and female spouce, or female head and male spouce
head_dummy <- c(rep(0,length(relation_to_jefe)))
for(i in 1:length(head_dummy)){
  if(relation_to_jefe[i] == 1 | relation_to_jefe[i] == 2 )  head_dummy[i] <- 1 
}
number_heads.df <- aggregate(head_dummy, by = list(Category=folio), FUN=sum)
names(number_heads.df) <- c("folio", "number_heads")
hh97.df <- merge(hh97.df, number_heads.df, by = "folio") 
number_heads <- hh97.df[,"number_heads"]

# creating dummies for the number of children ages 0-5 and 6-16 in the hh  
young_kid_dummy <- kid_dummy <- matrix(0, length(hh97.df[,"folio"]), 1)   # kids age chosen based on survey age of kids (there are sections that say only for kids under 16)
for(i in 1:length(hh97.df[,"folio"])){
  if(age[i] < 5) young_kid_dummy[i] <- 1 
  if(age[i] >= 5 && age[i] < 17) kid_dummy[i] <- 1  }

hh_young_kids.df <- aggregate(young_kid_dummy, by = list(Category=folio), FUN=sum)
hh_kids.df <- aggregate(kid_dummy, by=list(Category=folio), FUN=sum)
names(hh_kids.df) <- c("folio", "hh_kids")
names(hh_young_kids.df) <- c("folio", "hh_young_kids")
hh97.df <- merge(hh97.df, hh_kids.df, by = "folio")
hh97.df <- merge(hh97.df, hh_young_kids.df, by = "folio")

# creating dummy variables for whether the mom's/dad's parent's live in the HH (34 lines of code)
# steps: (1) determine gender of question-answerer using inf and sex vars, (2) create a dummy equal to one if jefe's parents live in hh, 
# diff dummy if spouce's parents live in household (first create ind dummies, then aggregate)  (3) combine the two pieces of information to 
# determine whether mom or dad's parents live in hh. 
inf_dummy <- dad_parents_live_in_hh <- mom_parents_live_in_hh <-  matrix(0, length(inf), 1) #step 1
for(i in 1:length(inf)){
  if(ind_ID[i] == inf[i]) inf_dummy[i] <- 1 
}
QA_gender <- sex*inf_dummy

jefe_parents_live_in_hh_dummy_IND <- jefe_spouce_parents_live_in_hh_dummy_IND <- jefe_parents_live_in_hh_dummy_hh <- #step 2
  jefe_spouce_parents_live_in_hh_dummy_hh  <- matrix(0, nrow=length(inf), ncol=1) 
for(i in 1:length(inf)){
  if(relation_to_jefe[i] == 4) jefe_parents_live_in_hh_dummy_IND[i] <- 1
  if(relation_to_jefe[i] == 12) jefe_spouce_parents_live_in_hh_dummy_IND[i] <- 1 
}

jefe_parents_live_in_hh_dummy_hh.df <- aggregate(jefe_parents_live_in_hh_dummy_IND, by = list(Category=folio), FUN=sum)
names(jefe_parents_live_in_hh_dummy_hh.df) <- c("folio", "jefe_parents_live_in_hh_dummy_hh")
hh97.df <- merge(hh97.df, jefe_parents_live_in_hh_dummy_hh.df, by = "folio")

jefe_spouce_parents_live_in_hh_dummy_hh.df <- aggregate(jefe_spouce_parents_live_in_hh_dummy_IND, by = list(Category=folio), FUN=sum)
names(jefe_spouce_parents_live_in_hh_dummy_hh.df) <- c("folio", "jefe_spouce_parents_live_in_hh_dummy_hh")
hh97.df <- merge(hh97.df, jefe_spouce_parents_live_in_hh_dummy_hh.df, by = "folio")

for(i in 1:length(inf)){  # step 3
  if(QA_gender[i] == 1 && hh97.df[i,"jefe_parents_live_in_hh_dummy_hh"] == 1) dad_parents_live_in_hh[i] <- 1
  if(QA_gender[i] == 1 && hh97.df[i,"jefe_spouce_parents_live_in_hh_dummy_hh"] == 1) mom_parents_live_in_hh[i] <- 1
}

dad_parents_live_in_hh_dummy_hh.df <- aggregate(dad_parents_live_in_hh, by = list(Category=folio), FUN=sum)
names(dad_parents_live_in_hh_dummy_hh.df) <- c("folio", "dad_parents_live_in_hh_dummy_hh")
hh97.df <- merge(hh97.df, dad_parents_live_in_hh_dummy_hh.df, by = "folio")
dad_parents_live_in_hh_dummy_hh <- hh97.df[,"dad_parents_live_in_hh_dummy_hh"]

mom_parents_live_in_hh_dummy_hh.df <- aggregate(mom_parents_live_in_hh, by = list(Category=folio), FUN=sum)
names(mom_parents_live_in_hh_dummy_hh.df) <- c("folio", "mom_parents_live_in_hh_dummy_hh")
hh97.df <- merge(hh97.df, mom_parents_live_in_hh_dummy_hh.df, by = "folio")
mom_parents_live_in_hh_dummy_hh <- hh97.df[,"mom_parents_live_in_hh_dummy_hh"]

# human capital 
indigenous_language <- hh97.df[,"p16"] 
spanish_language <- hh97.df[,"p17"] # If speak local language, also speak indigenous language dummy

literate <- hh97.df[,"p18"]
evergonetoschool <- hh97.df[,"p19"]
highest_grade_completed <- hh97.df[,"p20"]
currently_attending_school<- hh97.df[,"p21"]

#labor force participation and wages
LFP <- worked_last_7_days <- hh97.df[,"p22"] # make a dummy for labor force participation (LFP) 
hh_work <- hh97.df[,"p23"]
LFP[LFP == 2] <- 1
LFP[LFP == 3 | LFP == 4 | LFP ==9] <- 0
LFP[hh_work == 1 | hh_work == 2 | hh_work == 3 | hh_work == 4 | hh_work == 5] <- 1

whynotwork <- hh97.df[,"p24"]
primary_employment <- as.character(hh97.df[,"p25"]) 
numdaysworked <- hh97.df[,"p261"]
numdaysworked_no_response <- c(rep(0, length(folio)))
numdaysworked_no_response[numdaysworked == 9] <- 1
numhrsperdayworked <- hh97.df[,"p262"]
employment_benefits <- hh97.df[,"p27"]
allyear_or_seasonal_employment <- hh97.df[,"p281"]
whynotworkallyear <- hh97.df[,"p282"]

wages <- hh97.df[,"p291m"] # Wages are top-coded at 98000 pesos (timeframe variable) - I create a dummy for top coded 
wages_top_coded_dummy <-  wages_not_reported <- wages_not_known <- c(rep(0, length(wages)))
wages_top_coded_dummy[wages == 98000] <- 1 # only two individuals have top coded income, flagged, can drop if want. 
wages_not_known[wages == 98888] <- 1    # 415 individuals reported not knowning 
wages_not_reported[wages == 99999] <- 1 # 699 chose not to report

payment_period <- hh97.df[,"p291p"] # Some individuals didn't know their payment period, some didn't want to respond, dummies created for them as well.
payment_period_not_known <- payment_period_not_reported <- c(rep(0, length(wages)))
payment_period_not_known[payment_period == 8] <- 1
payment_period_not_reported[payment_period == 9] <- 1

other_income_source <- hh97.df[,"p301"]
other_income_source2 <- hh97.df[,"p302"]

## 31. what is the value of your additional income?
otherincomeper1 <- hh97.df[,"p31a1"]  # additional income source 1 amount 
otherincomeval1 <- hh97.df[,"p31a2"]  # additional income source 1 payment period
otherincomeper2 <- hh97.df[,"p31b1"]
otherincomeval2 <- hh97.df[,"p31b2"]

# Dummies for whether the second income is not reported or known
otherincomeper1_not_reported <- otherincomeval1_not_reported <- otherincomeval1_not_known <- otherincomeper1_not_known <-
  otherincomeper2_not_reported <- otherincomeval2_not_reported <- otherincomeval2_not_known <- otherincomeper2_not_known <-  c(rep(0, length(wages)))

otherincomeper1_not_reported[otherincomeper1 == 9] <- 1
otherincomeval1_not_reported[otherincomeval1 == 99999] <- 1
otherincomeper1_not_known[otherincomeper1 == 8] <- 1
otherincomeval1_not_known[otherincomeval1 == 98888]  <- 1

otherincomeper2_not_reported[otherincomeper2 == 9] <- 1
otherincomeval2_not_reported[otherincomeval2 == 99999] <- 1
otherincomeper2_not_known[otherincomeper2 == 8] <- 1
otherincomeval2_not_known[otherincomeval2 == 98888]  <- 1

dummy_income_sources <- hh97.df[,"p38"] # FACTOR give info on the second and third income source from q31: was the cash from a transfer? "Si" , "No", "NR"
nature_of_transfer <- hh97.df[,"p39"] # FACTOR give info on the second and third income source from q31: what transfer? "Pension", "Interest", ...

# converting wages earned to pesos/week from all three income sources
# Currently weighting the daily income FOR ALL THREE TYPES OF INCOME by number of days worked, as opposed to by 7. 

for(i in 1:length(wages)){
  if(payment_period[i] == 1) wages[i] <- wages[i]*numdaysworked[i] # primary income
  if(payment_period[i] == 3) wages[i] <- wages[i]/2
  if(payment_period[i] == 4) wages[i] <- wages[i]/4
  if(payment_period[i] == 5) wages[i] <- wages[i]/52
  
  if(otherincomeper1[i] == 1) otherincomeval1[i] <- otherincomeval1[i]*numdaysworked[i] # Secondary income
  if(otherincomeper1[i] == 3) otherincomeval1[i] <- otherincomeval1[i]/2
  if(otherincomeper1[i] == 4) otherincomeval1[i] <- otherincomeval1[i]/4
  if(otherincomeper1[i] == 5) otherincomeval1[i] <- otherincomeval1[i]/52                     # Coding error here requires special care (dif symbols) 
  
  if(otherincomeper2[i] == 1) otherincomeval2[i] <- otherincomeval2[i]*numdaysworked[i] # Tertiary income
  if(otherincomeper2[i] == 3) otherincomeval2[i] <- otherincomeval2[i]/2
  if(otherincomeper2[i] == 4) otherincomeval2[i] <- otherincomeval2[i]/4
  if(otherincomeper2[i] == 5) otherincomeval2[i] <- otherincomeval2[i]/52
  
} 

# The first "other income" source is "a second or third job" so this shifts those earnings to the earnings category. 
wages[other_income_source == 1] <- wages[other_income_source == 1] + otherincomeval1[other_income_source == 1] 
wages[other_income_source2 == 1] <- wages[other_income_source2 == 1] + otherincomeval1[other_income_source2 == 1]

otherincomeval1[other_income_source == 1] <- 0
otherincomeval2[other_income_source2 == 1] <- 0

agestartedwork <- hh97.df[,"p32"]


## module on remittances 
traveled_for_work <- hh97.df[,"p331"]
traveled_where <- hh97.df[,"p332"]
how_long_traveled_for_work <- hh97.df[,"p34"]
sent_help <- hh97.df[,"p35"]
sent_remittances <- c(rep(0, length(sent_help)))
sent_remittances[sent_help == 1] <- 1

relationship_status <- hh97.df[,"p36"]
livewithpartner <- hh97.df[,"p37"]


## 24 dummies, only the "dummy_IND" vars are for final use. 
## The code is structured such that the same answer is stored for each of the following variables for every member of the hh
## So if the Jefe's partner got a transfer, the dummy would be "Si" and every member of the household would have "2" has their response for ID_1
## If someone else also got the transfer, the ID_2 var would have their ID for all. 

DIF_kitchen_dummy_hh <- hh97.df[,"p40a1"]  # Did anyone in the household recieve the DIF transfer?
DIF_kitchen_dummy_IND <- c(rep(0,length(hh97.df[,"p40a1"])))  # Did you recieve the DIF transfer? 1 <- yes, 0 <- No
DIF_kitchen_ID_1 <- hh97.df[,"p40a2"]  # What was their identification number? 
DIF_kitchen_ID_2 <- hh97.df[,"p40a3"]  # If a second person also got the transfer, what was their identification number?

apoyo_del_programa_Ninos_de_Solidaridad_dummy_hh <- hh97.df[,"p40b1"] 
apoyo_del_programa_Ninos_de_Solidaridad_dummy_IND <- c(rep(0,length(hh97.df[,"p40b1"])))  # Did you recieve the NdeS transfer? 1 <- yes, 0 <- No
apoyo_del_programa_Ninos_de_Solidaridad_ID_1 <- hh97.df[,"p40b2"]
apoyo_del_programa_Ninos_de_Solidaridad_ID_2 <- hh97.df[,"p40b3"]

apoyo_del_INI_dummy_hh <- hh97.df[,"p40c1"]
apoyo_del_INI_dummy_IND <- c(rep(0,length(hh97.df[,"p40c1"])))  # Did you recieve the INI transfer? 1 <- yes, 0 <- No
apoyo_del_INI_ID_1 <- hh97.df[,"p40c2"]
apoyo_del_INI_ID_2 <- hh97.df[,"p40c3"]

PROBECAT_o_CIMO_dummy_hh <- hh97.df[,"p40d1"]
PROBECAT_o_CIMO_dummy_IND <- c(rep(0,length(hh97.df[,"p40d1"])))  # Did you recieve the CIMO transfer? 1 <- yes, 0 <- No
PROBECAT_o_CIMO_ID_1 <- hh97.df[,"p40d2"]
# no hh has more than one person with this transfer

temp_employment_program_dummy_hh <- hh97.df[,"p40e1"]
temp_employment_program_dummy_IND <- c(rep(0,length(hh97.df[,"p40e1"])))  # Did you recieve the temp.emp. transfer? 1 <- yes, 0 <- No
temp_employment_program_ID_1 <- hh97.df[,"p40e2"]
temp_employment_program_ID_2 <- hh97.df[,"p40e3"]

school_breakfast_dummy_hh <- hh97.df[,"p40f1"]
school_breakfast_dummy_IND <- c(rep(0,length(hh97.df[,"p40f1"])))  # Did you recieve the breakfast transfer? 1 <- yes, 0 <- No
school_breakfast_ID_1 <- hh97.df[,"p40f2"]
school_breakfast_ID_2 <- hh97.df[,"p40f3"]

# Conasupo and tortilla help
Tortilla_solidaridad_dummy_hh <- hh97.df[,"p41a1"]
Tortilla_solidaridad_card_holder <- hh97.df[,"p41a2"]
Conasupo_dummy <- hh97.df[,"p41b1"]
Conasupo_card_holder <- hh97.df[,"p41b2"]
Tortilla_solidaridad_dummy_IND <- Conasupo_dummy_IND <- c(rep(0,length(hh97.df[,"p40f1"])))


for(i in 1:length(hh97.df[,"p40e1"])){ #loop over individuals and update the dummy_IND variables to 1 if the hh reports that individual's ID number in the ID_1 or ID_2 vars
  if(ind_ID[i] == DIF_kitchen_ID_1[i]) DIF_kitchen_dummy_IND[i] <- 1
  if(apoyo_del_programa_Ninos_de_Solidaridad_ID_1[i] == ind_ID[i]) apoyo_del_programa_Ninos_de_Solidaridad_dummy_IND[i] <- 1
  if(ind_ID[i] == apoyo_del_INI_ID_1[i]) apoyo_del_INI_dummy_IND[i] <- 1
  if(ind_ID[i] == PROBECAT_o_CIMO_ID_1[i]) PROBECAT_o_CIMO_dummy_IND[i] <- 1
  if(ind_ID[i] == temp_employment_program_ID_1[i]) temp_employment_program_dummy_IND[i] <- 1
  if(ind_ID[i] == school_breakfast_ID_1[i]) school_breakfast_dummy_IND[i] <- 1
  
  if(ind_ID[i] == DIF_kitchen_ID_2[i]) DIF_kitchen_dummy_IND[i] <- 1
  if(ind_ID[i] == apoyo_del_programa_Ninos_de_Solidaridad_ID_2[i]) apoyo_del_programa_Ninos_de_Solidaridad_dummy_IND[i] <- 1
  if(ind_ID[i] == apoyo_del_INI_ID_2[i]) apoyo_del_INI_dummy_IND[i] <- 1
  # Probecat o Cimo all NA for the second person in the hh,  (and so causes code error if left in)
  if(ind_ID[i] == temp_employment_program_ID_2[i]) temp_employment_program_dummy_IND[i] <- 1
  if(ind_ID[i] == school_breakfast_ID_2[i]) school_breakfast_dummy_IND[i] <- 1
  
  if(ind_ID[i] == Tortilla_solidaridad_card_holder[i]) Tortilla_solidaridad_dummy_IND[i] <- 1    # Conasupo and Tortilla help
  if(ind_ID[i] == Conasupo_card_holder[i]) Conasupo_dummy_IND[i] <- 1
}

# result = dummy vars for individuals about whether they recieve these types of gov assistance


# 68 land ownership
land_ownership1 <- hh97.df[,"p681"] # hh level var
land_ownership2 <- hh97.df[,"p682"]
land_ownership3 <- hh97.df[,"p683"]
land_ownership4 <- hh97.df[,"p684"]
land_ownership5 <- hh97.df[,"p685"]

land_temp_or_irregated1 <- hh97.df[,"p691"]
land_temp_or_irregated2 <- hh97.df[,"p692"]
land_temp_or_irregated3 <- hh97.df[,"p693"]
land_temp_or_irregated4 <- hh97.df[,"p694"]
land_temp_or_irregated5 <- hh97.df[,"p695"]

land_was_used_for1 <- hh97.df[,"p701"]
land_was_used_for2 <- hh97.df[,"p702"]
land_was_used_for3 <- hh97.df[,"p703"]
land_was_used_for4 <- hh97.df[,"p704"]
land_was_used_for5 <- hh97.df[,"p705"]

## 71-73 how many of each type of animal do you own? Commented out b/c data not available in Nov. 2000 wave, which I use for my 731 project.

# horse <- hh97.df[,"p72a"] 
# donkey_and_mule <- hh97.df[,"p72b"]
# Ox <- hh97.df[,"p72c"]
# goat_and_sheep <- hh97.df[,"p73a"]
# cattle <- hh97.df[,"p73b"]
# hen <- hh97.df[,"p73c"]
# pig <- hh97.df[,"p73d"]
# rabbit <- hh97.df[,"p73e"]

# saving the dataframe to a .Rda file so that the last datacleaning section can load it in, 
# while the remanining dataset construction process can occur without a cluttered environment

cleaned.97.df <- as.data.frame(list(folio, ind_ID, treatment_dummy, municipio, localidad, 
                                    state, sex, age, inf, relation_to_jefe, dad_live_in_hh, mom_live_in_hh, head_dummy, number_heads, 
                                    dad_parents_live_in_hh_dummy_hh, mom_parents_live_in_hh_dummy_hh, 
                                    indigenous_language, spanish_language, literate, 
                                    evergonetoschool, highest_grade_completed, 
                                    currently_attending_school, LFP, worked_last_7_days, hh_work, 
                                    whynotwork, primary_employment, numdaysworked, numdaysworked_no_response, numhrsperdayworked,
                                    employment_benefits, allyear_or_seasonal_employment, whynotworkallyear, 
                                    wages, wages_top_coded_dummy, 
                                    payment_period, other_income_source, other_income_source2,
                                    otherincomeval1, otherincomeper1, otherincomeval2, otherincomeper2, dummy_income_sources,
                                    nature_of_transfer, agestartedwork, traveled_for_work, traveled_where,
                                    how_long_traveled_for_work, sent_remittances, relationship_status, livewithpartner,
                                    DIF_kitchen_dummy_IND, apoyo_del_programa_Ninos_de_Solidaridad_dummy_IND,
                                    apoyo_del_INI_dummy_IND, PROBECAT_o_CIMO_dummy_IND, temp_employment_program_dummy_IND, school_breakfast_dummy_IND, 
                                    Tortilla_solidaridad_dummy_IND, Conasupo_dummy_IND,
                                    land_ownership1, land_ownership2, land_ownership3, land_ownership4, land_ownership5, land_temp_or_irregated1,
                                    land_temp_or_irregated2, land_temp_or_irregated3, land_temp_or_irregated4, land_temp_or_irregated5, 
                                    land_was_used_for1, land_was_used_for2, land_was_used_for3, land_was_used_for4, land_was_used_for5, 
                                    wages_not_known, wages_not_reported, payment_period_not_known, payment_period_not_reported, otherincomeper1_not_reported, 
                                    otherincomeval1_not_reported, otherincomeval1_not_known, otherincomeper1_not_known, otherincomeper2_not_reported,
                                    otherincomeval2_not_reported, otherincomeval2_not_known, otherincomeper2_not_known, 
                                    pobre, pobextre, mpcalif, mppob, hh97.df$hh_kids, 
                                    hh97.df$hh_young_kids), nrow = length(folio), stringsAsFactors = FALSE)

colnames(cleaned.97.df) <- c("folio", "ind_ID", "treatment_dummy", "municipio", "localidad", 
                             "state", "sex", "age", "inf", "relation_to_jefe", "dad_live_in_hh", "mom_live_in_hh", "head_dummy", "number_heads", 
                             "dad_parents_live_in_hh_dummy_hh", "mom_parents_live_in_hh_dummy_hh", 
                             "indigenous_language", "spanish_language", "literate", 
                             "evergonetoschool", "highest_grade_completed", 
                             "currently_attending_school", "LFP", "worked_last_7_days", "hh_work", 
                             "whynotwork", "primary_employment", "numdaysworked", "numdaysworked_no_response", "numhrsperdayworked",
                             "employment_benefits", "allyear_or_seasonal_employment", "whynotworkallyear", 
                             "wages", "wages_top_coded_dummy", 
                             "payment_period", "other_income_source", "other_income_source2",
                             "otherincomeval1", "otherincomeper1", "otherincomeval2", "otherincomeper2", "dummy_income_sources",
                             "nature_of_transfer", "agestartedwork", "traveled_for_work", "traveled_where",
                             "how_long_traveled_for_work", "sent_remittances", "relationship_status", "livewithpartner",
                             "DIF_kitchen_dummy_IND", "apoyo_del_programa_Ninos_de_Solidaridad_dummy_IND",
                             "apoyo_del_INI_dummy_IND", "PROBECAT_o_CIMO_dummy_IND", "temp_employment_program_dummy_IND", "school_breakfast_dummy_IND", 
                             "Tortilla_solidaridad_dummy_IND", "Conasupo_dummy_IND",
                             "land_ownership1", "land_ownership2", "land_ownership3", "land_ownership4", "land_ownership5", "land_temp_or_irregated1",
                             "land_temp_or_irregated2", "land_temp_or_irregated3", "land_temp_or_irregated4", "land_temp_or_irregated5", 
                             "land_was_used_for1", "land_was_used_for2", "land_was_used_for3", "land_was_used_for4", "land_was_used_for5", 
                             "wages_not_known", "wages_not_reported", "payment_period_not_known", "payment_period_not_reported", "otherincomeper1_not_reported", 
                             "otherincomeval1_not_reported", "otherincomeval1_not_known", "otherincomeper1_not_known", "otherincomeper2_not_reported",
                             "otherincomeval2_not_reported", "otherincomeval2_not_known", "otherincomeper2_not_known",
                             "pobre", "pobextre", "mpcalif", "mppob", "hh_kids", "hh_young_kids")

summary(cleaned.97.df)

save(cleaned.97.df, file = "cleaned.97.Master.Rda")

# Chapter 2: March 1998 ####

rm(list=ls())
setwd("C:/Users/Matthew/Desktop/-/ProgrammingDirectory")
library(foreign)
load("cleaned.97.Master.Rda")
hh98.df <- read.spss('socioec_encel_98m.sav', use.value.labels = FALSE, to.data.frame = TRUE)

folio <- hh98.df[,"folio"]
ind_ID <- hh98.df[,"renglon"]
currently_in_school <- hh98.df[,"p006"]
why_not_in_school <- hh98.df[,"p007"]

cleaned.98.df <- as.data.frame(list(folio, ind_ID, currently_in_school, why_not_in_school), nrow = length(folio), stringsAsFactors = FALSE)
names(cleaned.98.df) <- c("folio", "ind_ID", "currently_in_school", "why_not_in_school")

Cleaned.Baseline.Master.df <- merge(cleaned.97.df, cleaned.98.df, by = c("folio","ind_ID"), all.y = TRUE) 
summary(Cleaned.Baseline.Master.df)                             
save(Cleaned.Baseline.Master.df, file = "Cleaned.Baseline.Master.Rda")                             

# Chapter 3: November 1999 #### 

rm(list=ls())
#setwd("C:/Users/Matthew/Desktop/-/ProgrammingDirectory")
hh99.df <- foreign::read.spss('socioec_encel_99n.sav', use.value.labels = FALSE, to.data.frame = TRUE)
str(hh99.df) #visualize

# Individual Identification Information and HH info 
folio <- hh99.df[,"folio"]
ind_ID <- hh99.df[,"renglon"]
ind_ID[is.na(ind_ID)] <- 99 # Setting NA's in renglon to 99
treatment_dummy <- hh99.df[,"contba"]
municipio <- hh99.df[,'cvemun']
localidad <- hh99.df[,'cveloc']
state <- hh99.df[,'cveest']
sex <- hh99.df[,"n008"]
table(sex)

age <- hh99.df[,"n002"]
# Setting AGE NA's to 99, the coded value for "No Reponse" 
age[is.na(hh99.df[,"n002"]) == TRUE] <- 99

# creating dummies for the number of children ages 0-5 and 6-16 in the hh  
young_kid_dummy <- kid_dummy <- matrix(0, length(folio), 1)   # kids age chosen based on survey age of kids (there are sections that say only for kids under 16)
for(i in 1:length(folio)){
  if(age[i] < 5) young_kid_dummy[i] <- 1 
  if(age[i] >= 5 && age[i] < 17) kid_dummy[i] <- 1  }

hh_young_kids.df <- aggregate(young_kid_dummy, by = list(Category=folio), FUN=sum)
hh_kids.df <- aggregate(kid_dummy, by=list(Category=folio), FUN=sum)
names(hh_kids.df) <- c("folio", "hh_kids")
names(hh_young_kids.df) <- c("folio", "hh_young_kids")  
hh99.df <- merge(hh99.df, hh_kids.df, by = "folio")
hh99.df <- merge(hh99.df, hh_young_kids.df, by = "folio")

hh_young_kids <- hh99.df[,"hh_young_kids"]
hh_kids <- hh99.df[,"hh_kids"]

# kid's education (same as the questions in M.1998)
currently_in_school <- hh99.df[,"n039"]
why_not_in_school <- hh99.df[,"n040"]

#labor force participation and wages
worked_last_7_days <- hh99.df[,"n050"]
hh_work <- hh99.df[,"n051"]
LFP <- c(rep(0, length(folio)))
LFP[worked_last_7_days == 1] <- 1
LFP[worked_last_7_days == 2] <- 1
LFP[hh_work == 1 | hh_work == 2 |
      hh_work == 3 | hh_work == 4 |
      hh_work == 5] <- 1

primary_employment <- hh99.df[,"n052"]
numdaysworked <- hh99.df[,"n056"]
numdaysworked_no_response <- c(rep(0, length(folio)))
numdaysworked_no_response[numdaysworked == 9] <- 1

wages <- hh99.df[,"n058a"] # Wages are top-coded at 98000 pesos (timeframe variable) - I create a dummy for top coded 
wages_top_coded_dummy <- wages_not_reported <- wages_not_known <- c(rep(0, length(wages)))
wages_not_known[wages == 98888] <- 1    # 415 individuals reported not knowning 
wages_not_reported[wages == 99999] <- 1 # 699 chose not to report

payment_period <- hh99.df[,"n058b"] # Some individuals didn't know their payment period, some didn't want to respond, dummies created for them as well.
payment_period_not_known <- payment_period_not_reported <- c(rep(0, length(wages)))
payment_period_not_known[payment_period == 8] <- 1
payment_period_not_reported[payment_period == 9] <- 1

other_income_source <- hh99.df[,"n059a"]
other_income_source[is.na(other_income_source)] <- 0
other_income_source2 <- hh99.df[,"n059b"] 
other_income_source2[is.na(other_income_source2)] <- 0

## 31. what is the value of your additional income?
otherincomeval1 <- hh99.df[,"n060a1"]  # additional income source 1 amount 
otherincomeper1 <- hh99.df[,"n060a2"] # additional income source 1 payment period

otherincomeval2 <- hh99.df[,"n060b1"]
otherincomeper2 <- hh99.df[,"n060b2"] 

# Dummies for whether the second income is not reported or known
otherincomeper1_not_reported <- otherincomeval1_not_reported <- otherincomeval1_not_known <- otherincomeper1_not_known <-
  otherincomeper2_not_reported <- otherincomeval2_not_reported <- otherincomeval2_not_known <- otherincomeper2_not_known <-  c(rep(0, length(wages)))

otherincomeper1_not_reported[otherincomeper1 == 9] <- 1
otherincomeval1_not_reported[otherincomeval1 == 99999] <- 1
otherincomeper1_not_known[otherincomeper1 == 8] <- 1
otherincomeval1_not_known[otherincomeval1 == 98888]  <- 1

otherincomeper2_not_reported[otherincomeper2 == 9] <- 1
otherincomeval2_not_reported[otherincomeval2 == 99999] <- 1
otherincomeper2_not_known[otherincomeper2 == 8] <- 1
otherincomeval2_not_known[otherincomeval2 == 98888]  <- 1

# converting wages earned to pesos/week from all three income sources
# Currently weighting the daily income FOR ALL THREE TYPES OF INCOME by number of days worked, as opposed to by 7. 

payment_period[is.na(payment_period) == TRUE] <- 0 # NA's generate error's in loop
otherincomeper1[is.na(otherincomeper1) == TRUE] <- 0
otherincomeper2[is.na(otherincomeper2) == TRUE] <- 0
numdaysworked[is.na(numdaysworked)] <- 0
otherincomeval1[is.na(otherincomeval1)] <- 0

for(i in 1:length(wages)){
  if(payment_period[i] == 1) wages[i] <- wages[i]*numdaysworked[i] # primary income
  if(payment_period[i] == 3) wages[i] <- wages[i]/2
  if(payment_period[i] == 4) wages[i] <- wages[i]/4
  if(payment_period[i] == 5) wages[i] <- wages[i]/52
  
  if(otherincomeper1[i] == 1) otherincomeval1[i] <- otherincomeval1[i]*numdaysworked[i] # Secondary income
  if(otherincomeper1[i] == 3) otherincomeval1[i] <- otherincomeval1[i]/2
  if(otherincomeper1[i] == 4) otherincomeval1[i] <- otherincomeval1[i]/4
  if(otherincomeper1[i] == 5) otherincomeval1[i] <- otherincomeval1[i]/52                     # Coding error here requires special care (dif symbols) 
  
  if(otherincomeper2[i] == 1) otherincomeval2[i] <- otherincomeval2[i]*numdaysworked[i] # Tertiary income
  if(otherincomeper2[i] == 3) otherincomeval2[i] <- otherincomeval2[i]/2
  if(otherincomeper2[i] == 4) otherincomeval2[i] <- otherincomeval2[i]/4
  if(otherincomeper2[i] == 5) otherincomeval2[i] <- otherincomeval2[i]/52
} 

# The first "other income" source is "a second or third job" so this shifts those earnings to the earnings category. 
wages[other_income_source == 1] <- wages[other_income_source == 1] + otherincomeval1[other_income_source == 1] 
wages[other_income_source2 == 1] <- wages[other_income_source2 == 1] + otherincomeval1[other_income_source2 == 1]

otherincomeval1[other_income_source == 1] <- 0
otherincomeval2[other_income_source2 == 1] <- 0


## 40 dummies, only the "dummy_IND" vars are for final use. 
## the code is stored differently than it is in the 1997 wave. Here, only the head of the household has a response.
## So, aggregation is required. Then it will be the same as the other format.
## There's gonna be a lot of additional data frames if I just go ahead like normal, so what I'll do to keep it reasonable is 
## make a single dataframe that I keep merging stuff into

DIF_kitchen_dummy_hh <- hh99.df[,"n06801a"] # FACTOR  # Did anyone in the household recieve the DIF transfer? 
DIF_kitchen_dummy_IND <- c(rep(0,length(DIF_kitchen_dummy_hh)))  # Did you recieve the DIF transfer? 1 <- yes, 0 <- No
DIF_kitchen_ID_1 <- hh99.df[,"n06801b1"]  # What was their identification number? 
DIF_kitchen_ID_2 <- hh99.df[,"n06801b2"]  # If a second person also got the transfer, what was their identification number?
DIF_kitchen_ID_1[is.na(DIF_kitchen_ID_1) == TRUE] <- 0
DIF_kitchen_ID_2[is.na(DIF_kitchen_ID_2) == TRUE] <- 0

master.df <- aggregate(DIF_kitchen_ID_1, by = list(Category=folio), FUN=sum)
names(master.df) <- c("folio", "DIF_kitchen_ID_1")

DIF_kitchen_ID_2.df <- aggregate(DIF_kitchen_ID_2, by = list(Category=folio), FUN=sum)
names(DIF_kitchen_ID_2.df) <- c("folio", "DIF_kitchen_ID_2")
master.df <- merge(master.df, DIF_kitchen_ID_2.df)

apoyo_del_programa_Ninos_de_Solidaridad_dummy_hh <- hh99.df[,"n06701a"] 
apoyo_del_programa_Ninos_de_Solidaridad_dummy_IND <- c(rep(0,length(hh99.df[,"n06701a"])))  # Did you recieve the NdeS transfer? 1 <- yes, 0 <- No
apoyo_del_programa_Ninos_de_Solidaridad_ID_1 <- hh99.df[,"n06701b1"]
apoyo_del_programa_Ninos_de_Solidaridad_ID_2 <- hh99.df[,"n06701b2"]
apoyo_del_programa_Ninos_de_Solidaridad_ID_1[is.na(apoyo_del_programa_Ninos_de_Solidaridad_ID_1) == TRUE] <- 0
apoyo_del_programa_Ninos_de_Solidaridad_ID_2[is.na(apoyo_del_programa_Ninos_de_Solidaridad_ID_2) == TRUE] <- 0

apoyo_del_programa_Ninos_de_Solidaridad_ID_1.df <- aggregate(apoyo_del_programa_Ninos_de_Solidaridad_ID_1, by = list(Category=folio), FUN=sum)
names(apoyo_del_programa_Ninos_de_Solidaridad_ID_1.df) <- c("folio", "apoyo_del_programa_Ninos_de_Solidaridad_ID_1")
master.df <- merge(master.df, apoyo_del_programa_Ninos_de_Solidaridad_ID_1.df, by = "folio")

apoyo_del_programa_Ninos_de_Solidaridad_ID_2.df <- aggregate(apoyo_del_programa_Ninos_de_Solidaridad_ID_2, by = list(Category=folio), FUN=sum)
names(apoyo_del_programa_Ninos_de_Solidaridad_ID_2.df) <- c("folio", "apoyo_del_programa_Ninos_de_Solidaridad_ID_2")
master.df <- merge(master.df, apoyo_del_programa_Ninos_de_Solidaridad_ID_2.df, by = "folio")

apoyo_del_INI_dummy_hh <- hh99.df[,"n06702a"]
apoyo_del_INI_dummy_IND <- c(rep(0,length(hh99.df[,"n06702a"])))  # Did you recieve the INI transfer? 1 <- yes, 0 <- No
apoyo_del_INI_ID_1 <- hh99.df[,"n06702b1"]
apoyo_del_INI_ID_1[is.na(apoyo_del_INI_ID_1) == TRUE] <- 0
#only one card holder in any household here
apoyo_del_INI_ID_1.df  <- aggregate(apoyo_del_INI_ID_1, by = list(Category=folio), FUN=sum)
names(apoyo_del_INI_ID_1.df) <- c("folio", "apoyo_del_INI_ID_1")
master.df <- merge(master.df, apoyo_del_INI_ID_1.df, by = "folio")

PROBECAT_o_CIMO_dummy_hh <- hh99.df[,"n06703a"]
PROBECAT_o_CIMO_dummy_IND <- c(rep(0,length(hh99.df[,"n06703a"])))  # Did you recieve the CIMO transfer? 1 <- yes, 0 <- No
PROBECAT_o_CIMO_ID_1 <- hh99.df[,"n06703b1"]
PROBECAT_o_CIMO_ID_2 <- hh99.df[,"n06703b2"]
PROBECAT_o_CIMO_ID_1[is.na(PROBECAT_o_CIMO_ID_1) == TRUE] <- 0
PROBECAT_o_CIMO_ID_2[is.na(PROBECAT_o_CIMO_ID_2) == TRUE] <- 0

PROBECAT_o_CIMO_ID_1.df  <- aggregate(PROBECAT_o_CIMO_ID_1, by = list(Category=folio), FUN=sum)
names(PROBECAT_o_CIMO_ID_1.df) <- c("folio", "PROBECAT_o_CIMO_ID_1")
master.df <- merge(master.df, PROBECAT_o_CIMO_ID_1.df, by = "folio")

PROBECAT_o_CIMO_ID_2.df  <- aggregate(PROBECAT_o_CIMO_ID_2, by = list(Category=folio), FUN=sum)
names(PROBECAT_o_CIMO_ID_2.df) <- c("folio", "PROBECAT_o_CIMO_ID_2")
master.df <- merge(master.df, PROBECAT_o_CIMO_ID_2.df, by = "folio")

temp_employment_program_dummy_hh <- hh99.df[,"n06704a"]
temp_employment_program_dummy_IND <- c(rep(0,length(hh99.df[,"n06704a"])))  # Did you recieve the temp.emp. transfer? 1 <- yes, 0 <- No
temp_employment_program_ID_1 <- hh99.df[,"n06704b1"]
temp_employment_program_ID_2 <- hh99.df[,"n06704b2"]
temp_employment_program_ID_1[is.na(temp_employment_program_ID_1) == TRUE] <- 0
temp_employment_program_ID_2[is.na(temp_employment_program_ID_2) == TRUE] <- 0

temp_employment_program_ID_1.df  <- aggregate(temp_employment_program_ID_1, by = list(Category=folio), FUN=sum)
names(temp_employment_program_ID_1.df) <- c("folio", "temp_employment_program_ID_1")
master.df <- merge(master.df, temp_employment_program_ID_1.df, by = "folio")

temp_employment_program_ID_2.df  <- aggregate(temp_employment_program_ID_2, by = list(Category=folio), FUN=sum)
names(temp_employment_program_ID_2.df) <- c("folio", "temp_employment_program_ID_2")
master.df <- merge(master.df, temp_employment_program_ID_2.df, by = "folio")

progresa_cash_dummy_hh <- hh99.df[,"n06706a"]
progresa_cash_dummy_IND <- c(rep(0, length(hh99.df[,"n06706a"])))
progresa_cash_ID_1 <- hh99.df[,"n06706b1"]
progresa_cash_ID_2 <- hh99.df[,"n06706b2"]
progresa_cash_ID_3 <- hh99.df[,"n06706b3"]
progresa_cash_ID_4 <- hh99.df[,"n06706b4"]
progresa_cash_ID_1[is.na(progresa_cash_ID_1) == TRUE] <- 0
progresa_cash_ID_2[is.na(progresa_cash_ID_2) == TRUE] <- 0
progresa_cash_ID_3[is.na(progresa_cash_ID_3) == TRUE] <- 0
progresa_cash_ID_4[is.na(progresa_cash_ID_4) == TRUE] <- 0

progresa_cash_ID_1.df  <- aggregate(progresa_cash_ID_1, by = list(Category=folio), FUN=sum)
names(progresa_cash_ID_1.df) <- c("folio", "progresa_cash_ID_1")
master.df <- merge(master.df, progresa_cash_ID_1.df, by = "folio")

progresa_cash_ID_2.df  <- aggregate(progresa_cash_ID_2, by = list(Category=folio), FUN=sum)
names(progresa_cash_ID_2.df) <- c("folio", "progresa_cash_ID_2")
master.df <- merge(master.df, progresa_cash_ID_2.df, by = "folio")

progresa_cash_ID_3.df  <- aggregate(progresa_cash_ID_3, by = list(Category=folio), FUN=sum)
names(progresa_cash_ID_3.df) <- c("folio", "progresa_cash_ID_3")
master.df <- merge(master.df, progresa_cash_ID_3.df, by = "folio")

progresa_cash_ID_4.df  <- aggregate(progresa_cash_ID_4, by = list(Category=folio), FUN=sum)
names(progresa_cash_ID_4.df) <- c("folio", "progresa_cash_ID_4")
master.df <- merge(master.df, progresa_cash_ID_4.df, by = "folio")

progresa_suppliment_dummy_hh <- hh99.df[,"n06707a"]
progresa_suppliment_dummy_IND <- c(rep(0, length(hh99.df[,"n06707a"])))
progresa_suppliment_ID_1 <- hh99.df[,"n06707b1"]
progresa_suppliment_ID_2 <- hh99.df[,"n06707b2"]
progresa_suppliment_ID_3 <- hh99.df[,"n06707b3"]
progresa_suppliment_ID_4 <- hh99.df[,"n06707b4"]
progresa_suppliment_ID_1[is.na(progresa_suppliment_ID_1) == TRUE] <- 0
progresa_suppliment_ID_2[is.na(progresa_suppliment_ID_2) == TRUE] <- 0
progresa_suppliment_ID_3[is.na(progresa_suppliment_ID_3) == TRUE] <- 0
progresa_suppliment_ID_4[is.na(progresa_suppliment_ID_4) == TRUE] <- 0

progresa_suppliment_ID_1.df  <- aggregate(progresa_suppliment_ID_1, by = list(Category=folio), FUN=sum)
names(progresa_suppliment_ID_1.df) <- c("folio", "progresa_suppliment_ID_1")
master.df <- merge(master.df, progresa_suppliment_ID_1.df, by = "folio")

progresa_suppliment_ID_2.df  <- aggregate(progresa_suppliment_ID_2, by = list(Category=folio), FUN=sum)
names(progresa_suppliment_ID_2.df) <- c("folio", "progresa_suppliment_ID_2")
master.df <- merge(master.df, progresa_suppliment_ID_2.df, by = "folio")

progresa_suppliment_ID_3.df  <- aggregate(progresa_suppliment_ID_3, by = list(Category=folio), FUN=sum)
names(progresa_suppliment_ID_3.df) <- c("folio", "progresa_suppliment_ID_3")
master.df <- merge(master.df, progresa_suppliment_ID_3.df, by = "folio")

progresa_suppliment_ID_4.df  <- aggregate(progresa_suppliment_ID_4, by = list(Category=folio), FUN=sum)
names(progresa_suppliment_ID_4.df) <- c("folio", "progresa_suppliment_ID_4")
master.df <- merge(master.df, progresa_suppliment_ID_4.df, by = "folio")

#Conasupo, tortilla, and Progresa help
Tortilla_solidaridad_dummy_hh <- hh99.df[,"n06803a"]
Tortilla_solidaridad_card_holder <- hh99.df[,"n06803b1"]
Tortilla_solidaridad_card_holder[is.na(Tortilla_solidaridad_card_holder) == TRUE] <- 0

Tortilla_solidaridad_card_holder.df  <- aggregate(Tortilla_solidaridad_card_holder, by = list(Category=folio), FUN=sum)
names(Tortilla_solidaridad_card_holder.df) <- c("folio", "Tortilla_solidaridad_card_holder")
master.df <- merge(master.df, Tortilla_solidaridad_card_holder.df, by = "folio")

Conasupo_dummy <- hh99.df[,"n06801a"]
Conasupo_card_holder <- hh99.df[,"n06801b1"]
Conasupo_card_holder[is.na(Conasupo_card_holder) == TRUE] <- 0

Conasupo_card_holder.df  <- aggregate(Conasupo_card_holder, by = list(Category=folio), FUN=sum)
names(Conasupo_card_holder.df) <- c("folio", "Conasupo_card_holder")
master.df <- merge(master.df, Conasupo_card_holder.df, by = "folio")

apoyo_monetario_Progresa_dummy_hh <- hh99.df[,"n06804a"]
apoyo_monetario_Progresa_card_holder_1 <-   hh99.df[,"n06804b1"]
apoyo_monetario_Progresa_card_holder_2 <-   hh99.df[,"n06804b2"]
apoyo_monetario_Progresa_card_holder_1[is.na(apoyo_monetario_Progresa_card_holder_1) == TRUE] <- 0
apoyo_monetario_Progresa_card_holder_2[is.na(apoyo_monetario_Progresa_card_holder_2) == TRUE] <- 0

apoyo_monetario_Progresa_card_holder_1.df  <- aggregate(apoyo_monetario_Progresa_card_holder_1, by = list(Category=folio), FUN=sum)
names(apoyo_monetario_Progresa_card_holder_1.df) <- c("folio", "apoyo_monetario_Progresa_card_holder_1")
master.df <- merge(master.df, apoyo_monetario_Progresa_card_holder_1.df, by = "folio")

apoyo_monetario_Progresa_card_holder_2.df  <- aggregate(apoyo_monetario_Progresa_card_holder_2, by = list(Category=folio), FUN=sum)
names(apoyo_monetario_Progresa_card_holder_2.df) <- c("folio", "apoyo_monetario_Progresa_card_holder_2")
master.df <- merge(master.df, apoyo_monetario_Progresa_card_holder_2.df, by = "folio")

apoyo_monetario_Progresa_dummy_IND <- Tortilla_solidaridad_dummy_IND <- Conasupo_dummy_IND <- c(rep(0,length(wages)))

hh99.df <- merge(hh99.df, master.df, by = "folio")

for(i in 1:length(hh99.df[,"folio"])){ #loop over individuals and update the dummy_IND variables to 1 if the hh reports that individual's ID number in the ID_1 or ID_2 vars
  if(ind_ID[i] == hh99.df[i,"DIF_kitchen_ID_1"]) DIF_kitchen_dummy_IND[i] <- 1
  if(ind_ID[i] == hh99.df[i,"apoyo_del_programa_Ninos_de_Solidaridad_ID_1"]) apoyo_del_programa_Ninos_de_Solidaridad_dummy_IND[i] <- 1
  if(ind_ID[i] == hh99.df[i,"apoyo_del_INI_ID_1"]) apoyo_del_INI_dummy_IND[i] <- 1
  if(ind_ID[i] == hh99.df[i,"PROBECAT_o_CIMO_ID_1"]) PROBECAT_o_CIMO_dummy_IND[i] <- 1
  if(ind_ID[i] == hh99.df[i,"temp_employment_program_ID_1"]) temp_employment_program_dummy_IND[i] <- 1
  if(ind_ID[i] == hh99.df[i,"progresa_cash_ID_1"]) progresa_cash_dummy_IND[i] <- 1
  if(ind_ID[i] == hh99.df[i,"progresa_suppliment_ID_1"]) progresa_suppliment_dummy_IND[i] <- 1
  if(ind_ID[i] == hh99.df[i,"apoyo_monetario_Progresa_card_holder_1"]) apoyo_monetario_Progresa_dummy_IND[i] <- 1
  
  
  if(ind_ID[i] == hh99.df[i,"DIF_kitchen_ID_2"]) DIF_kitchen_dummy_IND[i] <- 1
  if(ind_ID[i] == hh99.df[i,"apoyo_del_programa_Ninos_de_Solidaridad_ID_2"]) apoyo_del_programa_Ninos_de_Solidaridad_dummy_IND[i] <- 1
  if(ind_ID[i] == hh99.df[i,"temp_employment_program_ID_2"]) temp_employment_program_dummy_IND[i] <- 1
  if(ind_ID[i] == hh99.df[i,"progresa_cash_ID_2"]) progresa_cash_dummy_IND[i] <- 1
  if(ind_ID[i] == hh99.df[i,"progresa_suppliment_ID_2"]) progresa_suppliment_dummy_IND[i] <- 1
  if(ind_ID[i] == hh99.df[i,"apoyo_monetario_Progresa_card_holder_2"]) apoyo_monetario_Progresa_dummy_IND[i] <- 1
  
  if(ind_ID[i] == hh99.df[i,"progresa_cash_ID_3"]) progresa_cash_dummy_IND[i] <- 1
  if(ind_ID[i] == hh99.df[i,"progresa_suppliment_ID_3"]) progresa_suppliment_dummy_IND[i] <- 1
  
  if(ind_ID[i] == hh99.df[i,"progresa_cash_ID_4"]) progresa_cash_dummy_IND[i] <- 1
  if(ind_ID[i] == hh99.df[i,"progresa_suppliment_ID_4"]) progresa_suppliment_dummy_IND[i] <- 1
  
  if(ind_ID[i] == hh99.df[i,"Tortilla_solidaridad_card_holder"]) Tortilla_solidaridad_dummy_IND[i] <- 1    # Conasupo and Tortilla help
  if(ind_ID[i] == hh99.df[i,"Conasupo_card_holder"]) Conasupo_dummy_IND[i] <- 1
  
}

# result = dummy vars for individuals about whether they recieve these types of gov assistance

# module on remittances 
traveled_where <- hh99.df[,"n136"]
sent_help <- hh99.df[,"n137"]
sent_remittances <- c(rep(0, length(sent_help)))
sent_remittances[sent_help == 1] <- 1

# 68 land ownership
land_ownership1 <- hh99.df[,"n10001"] # hh level var
land_ownership2 <- hh99.df[,"n10002"]
land_ownership3 <- hh99.df[,"n10003"]
land_ownership4 <- hh99.df[,"n10004"]
land_ownership5 <- hh99.df[,"n10005"]

number_land_parcels <- hh99.df[,"n099"]

# saving the dataframe to a .Rda file so that the last datacleaning section can load it in, 
# while the remanining dataset construction process can occur without a cluttered environment

cleaned.99.df <- as.data.frame(list(folio, ind_ID, sex, treatment_dummy, municipio, localidad, 
                                    state, age, 
                                    LFP, worked_last_7_days, hh_work, 
                                    primary_employment, numdaysworked, numdaysworked_no_response,
                                    wages, wages_top_coded_dummy,
                                    payment_period, other_income_source, other_income_source2,
                                    otherincomeval1, otherincomeper1, otherincomeval2, otherincomeper2,
                                    traveled_where, sent_remittances, 
                                    apoyo_monetario_Progresa_dummy_IND, progresa_suppliment_dummy_IND, progresa_cash_dummy_IND,
                                    DIF_kitchen_dummy_IND, apoyo_del_programa_Ninos_de_Solidaridad_dummy_IND,
                                    apoyo_del_INI_dummy_IND, PROBECAT_o_CIMO_dummy_IND, temp_employment_program_dummy_IND,  
                                    Tortilla_solidaridad_dummy_IND, Conasupo_dummy_IND,
                                    land_ownership1, land_ownership2, land_ownership3, land_ownership4, land_ownership5, number_land_parcels, 
                                    wages_not_known, wages_not_reported, payment_period_not_known, payment_period_not_reported, otherincomeper1_not_reported, 
                                    otherincomeval1_not_reported, otherincomeval1_not_known, otherincomeper1_not_known, otherincomeper2_not_reported,
                                    otherincomeval2_not_reported, otherincomeval2_not_known, otherincomeper2_not_known, 
                                    hh_young_kids, hh_kids, currently_in_school, why_not_in_school), nrow = length(folio), stringsAsFactors = FALSE)

colnames(cleaned.99.df) <- c("folio", "ind_ID", "sex", "treatment_dummy", "municipio", "localidad", 
                             "state", "age", 
                             "LFP", "worked_last_7_days", "hh_work", 
                             "primary_employment", "numdaysworked", "numdaysworked_no_response",
                             "wages", "wages_top_coded_dummy",
                             "payment_period", "other_income_source", "other_income_source2",
                             "otherincomeval1", "otherincomeper1", "otherincomeval2", "otherincomeper2",
                             "traveled_where",  "sent_remittances", 
                             "apoyo_monetario_Progresa_dummy_IND", "progresa_suppliment_dummy_IND", "progresa_cash_dummy_IND",
                             "DIF_kitchen_dummy_IND", "apoyo_del_programa_Ninos_de_Solidaridad_dummy_IND",
                             "apoyo_del_INI_dummy_IND", "PROBECAT_o_CIMO_dummy_IND", "temp_employment_program_dummy_IND",  
                             "Tortilla_solidaridad_dummy_IND", "Conasupo_dummy_IND",
                             "land_ownership1", "land_ownership2", "land_ownership3", "land_ownership4", "land_ownership5", "number_land_parcels", 
                             "wages_not_known", "wages_not_reported", "payment_period_not_known", "payment_period_not_reported", "otherincomeper1_not_reported", 
                             "otherincomeval1_not_reported", "otherincomeval1_not_known", "otherincomeper1_not_known", "otherincomeper2_not_reported",
                             "otherincomeval2_not_reported", "otherincomeval2_not_known", "otherincomeper2_not_known", 
                             "hh_young_kids", "hh_kids", "currently_in_school", "why_not_in_school")

# Dropping households that don't know or don't report their income, as well as households without a man and a woman as the hh heads, and didn't have an inf

summary(cleaned.99.df)

save(cleaned.99.df, file = "cleaned.99.Master.Rda")

# Chapter 4: November 2000 ####

rm(list=ls())
#setwd("C:/Users/Matthew/Desktop/-/ProgrammingDirectory")
hh00.df <- foreign::read.spss('socioec_encel_2000n.sav', use.value.labels = FALSE, to.data.frame = TRUE)

# Setting Age NA's to 99, the reported value for "No Response"
age <- hh00.df[,"w002cor"]
age[is.na(age)] <- 99 ## Needed for loops below

# Individual Identification Information and HH info 
folio <- hh00.df[,"folio"]
ind_ID <- hh00.df[,"renglon"]
ind_ID[is.na(ind_ID)] <- 99
treatment_dummy <- hh00.df[,"contba"]
municipio <- hh00.df[,'cvemun']
localidad <- hh00.df[,'cveloc']
state <- hh00.df[,'cveest']
#levels(state) <- c('Guerrero', 'Hidalgo', 'Michoacán', 'Puebla', 'Queretaro', 'San Luis Potosí', 'Veracruz')
sex <- hh00.df[,"w008"]
str(sex)
summary(sex) # assign base sex to people originally in the sample below 

# no relation to jefe, have to add that in from the baseline. see chapter five

# kid's education 
currently_in_school <- hh00.df[,"w039"]
why_not_in_school <- hh00.df[,"w040"]

# creating dummies for the number of children ages 0-5 and 6-16 in the hh  
young_kid_dummy <- kid_dummy <- matrix(0, length(folio), 1)   # kids age chosen based on survey age of kids (there are sections that say only for kids under 16)
for(i in 1:length(folio)){
  if(age[i] < 5) young_kid_dummy[i] <- 1 
  if(age[i] >= 5 && age[i] < 17) kid_dummy[i] <- 1  }

hh_young_kids.df <- aggregate(young_kid_dummy, by = list(Category=folio), FUN=sum)
hh_kids.df <- aggregate(kid_dummy, by=list(Category=folio), FUN=sum)
names(hh_kids.df) <- c("folio", "hh_kids")
names(hh_young_kids.df) <- c("folio", "hh_young_kids") 
hh00.df <- merge(hh00.df, hh_kids.df, by = "folio")
hh00.df <- merge(hh00.df, hh_young_kids.df, by = "folio")

hh_young_kids <- hh00.df[,"hh_young_kids"]
hh_kids <- hh00.df[,"hh_kids"]

#labor force participation and wages
worked_last_7_days <- hh00.df[,"w057"] 
hh_work <- hh00.df[,"w058"]
LFP <- c(rep(0, length(folio)))
LFP[worked_last_7_days == 1] <- 1
LFP[worked_last_7_days == 2] <- 1
LFP[hh_work == 1 | hh_work == 2 |
      hh_work == 3 | hh_work == 4 |
      hh_work == 5] <- 1

primary_employment <- hh00.df[,"w059"]
primary_employment[is.na(primary_employment) == TRUE] <- 0 
numdaysworked <- hh00.df[,"w061a"]
numdaysworked_no_response <- c(rep(0, length(folio)))
numdaysworked_no_response[numdaysworked == 9] <- 1
numdaysworked[numdaysworked == 9] <- 0 #just need a number here, this line is redundant since these observations are dropped anyway

wages <- hh00.df[,"w062a"] # Wages are top-coded at 98000 pesos (timeframe variable) - I create a dummy for top coded 
wages_top_coded_dummy <- wages_not_reported <- wages_not_known <- c(rep(0, length(wages)))
wages_top_coded_dummy[wages == 98000] <- 1    # 5 individuals 
wages_not_known[wages == 98888] <- 1    # 415 individuals reported not knowning 
wages_not_reported[wages == 99999] <- 1 # 455 chose not to report

payment_period <- hh00.df[,"w062b"] # Some individuals didn't know their payment period, some didn't want to respond, dummies created for them as well.
payment_period_not_known <- payment_period_not_reported <- c(rep(0, length(wages)))
payment_period_not_known[payment_period == 8] <- 1
payment_period_not_reported[payment_period == 9] <- 1

other_income_source <- hh00.df[,"w063a"] 
other_income_source[is.na(other_income_source)] <- 0
other_income_source2 <- hh00.df[,"w063b"]
other_income_source2[is.na(other_income_source2)] <- 0

## 31. what is the value of your additional income?
otherincomeval1 <- hh00.df[,"w064a1"]  # additional income source 1 amount 
otherincomeper1 <- hh00.df[,"w064b1"]  

otherincomeval2 <- hh00.df[,"w064a2"]
otherincomeper2 <- hh00.df[,"w064b2"] 

# Dummies for whether the second income is not reported or known
otherincomeper1_not_reported <- otherincomeval1_not_reported <- otherincomeval1_not_known <- otherincomeper1_not_known <-
  otherincomeper2_not_reported <- otherincomeval2_not_reported <- otherincomeval2_not_known <- otherincomeper2_not_known <-  c(rep(0, length(wages)))

otherincomeper1_not_reported[otherincomeper1 == 9] <- 1
otherincomeval1_not_reported[otherincomeval1 == 9999999 | otherincomeval1 == 99999999 | 
                               otherincomeval1 == 99999] <- 1
otherincomeper1_not_known[otherincomeper1 == 8] <- 1
otherincomeval1_not_known[otherincomeval1 == 88888888 | otherincomeval1 == 99999999 | 
                            otherincomeval1 ==  98888888]  <- 1

otherincomeper2_not_reported[otherincomeper2 == 9] <- 1
otherincomeval2_not_reported[otherincomeval2 == 99999 | otherincomeval2 == 99999999] <- 1
otherincomeper2_not_known[otherincomeper2 == 8] <- 1
otherincomeval2_not_known[otherincomeval2 == 98888 | otherincomeval2 == 98888888]  <- 1

# converting wages earned to pesos/week from all three income sources
# Currently weighting the daily income FOR ALL THREE TYPES OF INCOME by number of days worked, as opposed to by 7. 

# NA's cause error's in the coming for loop and nested if loops, so I recode them here to be zero's or, for character variables, "NA"       

numdaysworked[is.na(numdaysworked)] <- 
  otherincomeper1[is.na(otherincomeper1)] <- 
  otherincomeper2[is.na(otherincomeper2)] <- 
  otherincomeval1[is.na(otherincomeval1)] <- 
  otherincomeval2[is.na(otherincomeval2)] <- 
  wages[is.na(wages)] <- 
  payment_period[is.na(payment_period) == TRUE] <- 0


for(i in 1:length(wages)){
  if(payment_period[i] == 1) wages[i] <- wages[i]*numdaysworked[i] # primary income
  if(payment_period[i] == 3) wages[i] <- wages[i]/2
  if(payment_period[i] == 4) wages[i] <- wages[i]/4
  if(payment_period[i] == 5) wages[i] <- wages[i]/52
  
  if(otherincomeper1[i] == 1) otherincomeval1[i] <- otherincomeval1[i]*numdaysworked[i] # Secondary income
  if(otherincomeper1[i] == 3) otherincomeval1[i] <- otherincomeval1[i]/2
  if(otherincomeper1[i] == 4) otherincomeval1[i] <- otherincomeval1[i]/4
  if(otherincomeper1[i] == 5) otherincomeval1[i] <- otherincomeval1[i]/52                     # Coding error here requires special care (dif symbols) 
  
  if(otherincomeper2[i] == 1) otherincomeval2[i] <- otherincomeval2[i]*numdaysworked[i] # Tertiary income
  if(otherincomeper2[i] == 3) otherincomeval2[i] <- otherincomeval2[i]/2
  if(otherincomeper2[i] == 4) otherincomeval2[i] <- otherincomeval2[i]/4
  if(otherincomeper2[i] == 5) otherincomeval2[i] <- otherincomeval2[i]/52
} 

# The first "other income" source is "a second or third job" so this shifts those earnings to the earnings category. 
wages[other_income_source == 1] <- wages[other_income_source == 1] + otherincomeval1[other_income_source == 1] 
wages[other_income_source2 == 1] <- wages[other_income_source2 == 1] + otherincomeval1[other_income_source2 == 1]

otherincomeval1[other_income_source == 1] <- 0
otherincomeval2[other_income_source2 == 1] <- 0


## 40 dummies, only the "dummy_IND" vars are for final use. 
## the variables are stored in the same way as they are in the November 1999 wave (Here, only the head of the household has a response.)
## I use the master.df approach again 

DIF_kitchen_dummy_hh <- hh00.df[,"w08102a"] # Did anyone in the household recieve the DIF transfer? 
DIF_kitchen_dummy_IND <- c(rep(0,length(DIF_kitchen_dummy_hh)))  # Did you recieve the DIF transfer? 1 <- yes, 0 <- No
DIF_kitchen_ID_1 <- hh00.df[,"w08102b1"]  # What was their identification number? 
DIF_kitchen_ID_2 <- hh00.df[,"w08102b2"]  # If a second person also got the transfer, what was their identification number?
DIF_kitchen_ID_1[is.na(DIF_kitchen_ID_1)] <- 0
DIF_kitchen_ID_2[is.na(DIF_kitchen_ID_2) == TRUE] <- 0

master.df <- aggregate(DIF_kitchen_ID_1, by = list(Category=folio), FUN=sum)
names(master.df) <- c("folio", "DIF_kitchen_ID_1")

DIF_kitchen_ID_2.df <- aggregate(DIF_kitchen_ID_2, by = list(Category=folio), FUN=sum)
names(DIF_kitchen_ID_2.df) <- c("folio", "DIF_kitchen_ID_2")
master.df <- merge(master.df, DIF_kitchen_ID_2.df)

apoyo_del_programa_Ninos_de_Solidaridad_dummy_hh <- hh00.df[,"w08001a"] # FACTOR
apoyo_del_programa_Ninos_de_Solidaridad_dummy_IND <- c(rep(0,length(hh00.df[,"w08001a"])))  # Did you recieve the NdeS transfer? 1 <- yes, 0 <- No
apoyo_del_programa_Ninos_de_Solidaridad_ID_1 <- hh00.df[,"w08001b1"]
apoyo_del_programa_Ninos_de_Solidaridad_ID_2 <- hh00.df[,"w08001b2"]
apoyo_del_programa_Ninos_de_Solidaridad_ID_1[is.na(apoyo_del_programa_Ninos_de_Solidaridad_ID_1) == TRUE] <- 0
apoyo_del_programa_Ninos_de_Solidaridad_ID_2[is.na(apoyo_del_programa_Ninos_de_Solidaridad_ID_2) == TRUE] <- 0

apoyo_del_programa_Ninos_de_Solidaridad_ID_1.df <- aggregate(apoyo_del_programa_Ninos_de_Solidaridad_ID_1, by = list(Category=folio), FUN=sum)
names(apoyo_del_programa_Ninos_de_Solidaridad_ID_1.df) <- c("folio", "apoyo_del_programa_Ninos_de_Solidaridad_ID_1")
master.df <- merge(master.df, apoyo_del_programa_Ninos_de_Solidaridad_ID_1.df, by = "folio")

apoyo_del_programa_Ninos_de_Solidaridad_ID_2.df <- aggregate(apoyo_del_programa_Ninos_de_Solidaridad_ID_2, by = list(Category=folio), FUN=sum)
names(apoyo_del_programa_Ninos_de_Solidaridad_ID_2.df) <- c("folio", "apoyo_del_programa_Ninos_de_Solidaridad_ID_2")
master.df <- merge(master.df, apoyo_del_programa_Ninos_de_Solidaridad_ID_2.df, by = "folio")

apoyo_del_INI_dummy_hh <- hh00.df[,"w08002a"]
apoyo_del_INI_dummy_IND <- c(rep(0,length(hh00.df[,"w08002a"])))  # Did you recieve the INI transfer? 1 <- yes, 0 <- No
apoyo_del_INI_ID_1 <- hh00.df[,"w08002b1"]
apoyo_del_INI_ID_1[is.na(apoyo_del_INI_ID_1) == TRUE] <- 0
#only one card holder in any household here
apoyo_del_INI_ID_1.df  <- aggregate(apoyo_del_INI_ID_1, by = list(Category=folio), FUN=sum)
names(apoyo_del_INI_ID_1.df) <- c("folio", "apoyo_del_INI_ID_1")
master.df <- merge(master.df, apoyo_del_INI_ID_1.df, by = "folio")

PROBECAT_o_CIMO_dummy_hh <- hh00.df[,"w08003a"]
PROBECAT_o_CIMO_dummy_IND <- c(rep(0,length(hh00.df[,"w08003a"])))  # Did you recieve the CIMO transfer? 1 <- yes, 0 <- No
PROBECAT_o_CIMO_ID_1 <- hh00.df[,"w08003b1"]
PROBECAT_o_CIMO_ID_2 <- hh00.df[,"w08003b2"]
PROBECAT_o_CIMO_ID_1[is.na(PROBECAT_o_CIMO_ID_1) == TRUE] <- 0
PROBECAT_o_CIMO_ID_2[is.na(PROBECAT_o_CIMO_ID_2) == TRUE] <- 0

PROBECAT_o_CIMO_ID_1.df  <- aggregate(PROBECAT_o_CIMO_ID_1, by = list(Category=folio), FUN=sum)
names(PROBECAT_o_CIMO_ID_1.df) <- c("folio", "PROBECAT_o_CIMO_ID_1")
master.df <- merge(master.df, PROBECAT_o_CIMO_ID_1.df, by = "folio")

PROBECAT_o_CIMO_ID_2.df  <- aggregate(PROBECAT_o_CIMO_ID_2, by = list(Category=folio), FUN=sum)
names(PROBECAT_o_CIMO_ID_2.df) <- c("folio", "PROBECAT_o_CIMO_ID_2")
master.df <- merge(master.df, PROBECAT_o_CIMO_ID_2.df, by = "folio")

temp_employment_program_dummy_hh <- hh00.df[,"w08004a"]
temp_employment_program_dummy_IND <- c(rep(0,length(hh00.df[,"w08004a"])))  # Did you recieve the temp.emp. transfer? 1 <- yes, 0 <- No
temp_employment_program_ID_1 <- hh00.df[,"w08004b1"]
temp_employment_program_ID_2 <- hh00.df[,"w08004b2"]
temp_employment_program_ID_1[is.na(temp_employment_program_ID_1) == TRUE] <- 0
temp_employment_program_ID_2[is.na(temp_employment_program_ID_2) == TRUE] <- 0

temp_employment_program_ID_1.df  <- aggregate(temp_employment_program_ID_1, by = list(Category=folio), FUN=sum)
names(temp_employment_program_ID_1.df) <- c("folio", "temp_employment_program_ID_1")
master.df <- merge(master.df, temp_employment_program_ID_1.df, by = "folio")

temp_employment_program_ID_2.df  <- aggregate(temp_employment_program_ID_2, by = list(Category=folio), FUN=sum)
names(temp_employment_program_ID_2.df) <- c("folio", "temp_employment_program_ID_2")
master.df <- merge(master.df, temp_employment_program_ID_2.df, by = "folio")

progresa_cash_dummy_hh <- hh00.df[,"w08201a"]
progresa_cash_dummy_IND <- c(rep(0, length(hh00.df[,"w08201a"])))
progresa_cash_ID_1 <- hh00.df[,"w08201b1"]
progresa_cash_ID_2 <- hh00.df[,"w08201b2"]
progresa_cash_ID_3 <- hh00.df[,"w08201b3"]
progresa_cash_ID_4 <- hh00.df[,"w08201b4"]
progresa_cash_ID_1[is.na(progresa_cash_ID_1) == TRUE] <- 0
progresa_cash_ID_2[is.na(progresa_cash_ID_2) == TRUE] <- 0
progresa_cash_ID_3[is.na(progresa_cash_ID_3) == TRUE] <- 0
progresa_cash_ID_4[is.na(progresa_cash_ID_4) == TRUE] <- 0

progresa_cash_ID_1.df  <- aggregate(progresa_cash_ID_1, by = list(Category=folio), FUN=sum)
names(progresa_cash_ID_1.df) <- c("folio", "progresa_cash_ID_1")
master.df <- merge(master.df, progresa_cash_ID_1.df, by = "folio")

progresa_cash_ID_2.df  <- aggregate(progresa_cash_ID_2, by = list(Category=folio), FUN=sum)
names(progresa_cash_ID_2.df) <- c("folio", "progresa_cash_ID_2")
master.df <- merge(master.df, progresa_cash_ID_2.df, by = "folio")

progresa_cash_ID_3.df  <- aggregate(progresa_cash_ID_3, by = list(Category=folio), FUN=sum)
names(progresa_cash_ID_3.df) <- c("folio", "progresa_cash_ID_3")
master.df <- merge(master.df, progresa_cash_ID_3.df, by = "folio")

progresa_cash_ID_4.df  <- aggregate(progresa_cash_ID_4, by = list(Category=folio), FUN=sum)
names(progresa_cash_ID_4.df) <- c("folio", "progresa_cash_ID_4")
master.df <- merge(master.df, progresa_cash_ID_4.df, by = "folio")

progresa_suppliment_dummy_hh <- hh00.df[,"w08202a"]
progresa_suppliment_dummy_IND <- c(rep(0, length(hh00.df[,"w08202a"])))
progresa_suppliment_ID_1 <- hh00.df[,"w08202b1"]
progresa_suppliment_ID_2 <- hh00.df[,"w08202b2"]
progresa_suppliment_ID_3 <- hh00.df[,"w08202b3"]
progresa_suppliment_ID_4 <- hh00.df[,"w08202b4"]
progresa_suppliment_ID_1[is.na(progresa_suppliment_ID_1) == TRUE] <- 0
progresa_suppliment_ID_2[is.na(progresa_suppliment_ID_2) == TRUE] <- 0
progresa_suppliment_ID_3[is.na(progresa_suppliment_ID_3) == TRUE] <- 0
progresa_suppliment_ID_4[is.na(progresa_suppliment_ID_4) == TRUE] <- 0

progresa_suppliment_ID_1.df  <- aggregate(progresa_suppliment_ID_1, by = list(Category=folio), FUN=sum)
names(progresa_suppliment_ID_1.df) <- c("folio", "progresa_suppliment_ID_1")
master.df <- merge(master.df, progresa_suppliment_ID_1.df, by = "folio")

progresa_suppliment_ID_2.df  <- aggregate(progresa_suppliment_ID_2, by = list(Category=folio), FUN=sum)
names(progresa_suppliment_ID_2.df) <- c("folio", "progresa_suppliment_ID_2")
master.df <- merge(master.df, progresa_suppliment_ID_2.df, by = "folio")

progresa_suppliment_ID_3.df  <- aggregate(progresa_suppliment_ID_3, by = list(Category=folio), FUN=sum)
names(progresa_suppliment_ID_3.df) <- c("folio", "progresa_suppliment_ID_3")
master.df <- merge(master.df, progresa_suppliment_ID_3.df, by = "folio")

progresa_suppliment_ID_4.df  <- aggregate(progresa_suppliment_ID_4, by = list(Category=folio), FUN=sum)
names(progresa_suppliment_ID_4.df) <- c("folio", "progresa_suppliment_ID_4")
master.df <- merge(master.df, progresa_suppliment_ID_4.df, by = "folio")

#Conasupo, tortilla, and Progresa help
Tortilla_solidaridad_dummy_hh <- hh00.df[,"w08104a"]
Tortilla_solidaridad_card_holder <- hh00.df[,"w08104b1"]
Tortilla_solidaridad_card_holder[is.na(Tortilla_solidaridad_card_holder) == TRUE] <- 0

Tortilla_solidaridad_card_holder.df  <- aggregate(Tortilla_solidaridad_card_holder, by = list(Category=folio), FUN=sum)
names(Tortilla_solidaridad_card_holder.df) <- c("folio", "Tortilla_solidaridad_card_holder")
master.df <- merge(master.df, Tortilla_solidaridad_card_holder.df, by = "folio")

Conasupo_dummy <- hh00.df[,"w08101a"]
Conasupo_card_holder <- hh00.df[,"w08101b1"]
Conasupo_card_holder[is.na(Conasupo_card_holder) == TRUE] <- 0

Conasupo_card_holder.df  <- aggregate(Conasupo_card_holder, by = list(Category=folio), FUN=sum)
names(Conasupo_card_holder.df) <- c("folio", "Conasupo_card_holder")
master.df <- merge(master.df, Conasupo_card_holder.df, by = "folio")

apoyo_monetario_Progresa_dummy_hh <- hh00.df[,"w08105a"]
apoyo_monetario_Progresa_card_holder_1 <- hh00.df[,"w08105b1"]
apoyo_monetario_Progresa_card_holder_2 <- hh00.df[,"w08105b2"]
apoyo_monetario_Progresa_card_holder_1[is.na(apoyo_monetario_Progresa_card_holder_1) == TRUE] <- 0
apoyo_monetario_Progresa_card_holder_2[is.na(apoyo_monetario_Progresa_card_holder_2) == TRUE] <- 0

apoyo_monetario_Progresa_card_holder_1.df  <- aggregate(apoyo_monetario_Progresa_card_holder_1, by = list(Category=folio), FUN=sum)
names(apoyo_monetario_Progresa_card_holder_1.df) <- c("folio", "apoyo_monetario_Progresa_card_holder_1")
master.df <- merge(master.df, apoyo_monetario_Progresa_card_holder_1.df, by = "folio")

apoyo_monetario_Progresa_card_holder_2.df  <- aggregate(apoyo_monetario_Progresa_card_holder_2, by = list(Category=folio), FUN=sum)
names(apoyo_monetario_Progresa_card_holder_2.df) <- c("folio", "apoyo_monetario_Progresa_card_holder_2")
master.df <- merge(master.df, apoyo_monetario_Progresa_card_holder_2.df, by = "folio")

apoyo_monetario_Progresa_dummy_IND <- Tortilla_solidaridad_dummy_IND <- Conasupo_dummy_IND <- c(rep(0,length(wages)))

hh00.df <- merge(hh00.df, master.df, by = "folio")

for(i in 1:length(hh00.df[,"folio"])){ #loop over individuals and update the dummy_IND variables to 1 if the hh reports that individual's ID number in the ID_1 or ID_2 vars
  if(ind_ID[i] == hh00.df[i,"DIF_kitchen_ID_1"]) DIF_kitchen_dummy_IND[i] <- 1
  if(ind_ID[i] == hh00.df[i,"apoyo_del_programa_Ninos_de_Solidaridad_ID_1"]) apoyo_del_programa_Ninos_de_Solidaridad_dummy_IND[i] <- 1
  if(ind_ID[i] == hh00.df[i,"apoyo_del_INI_ID_1"]) apoyo_del_INI_dummy_IND[i] <- 1
  if(ind_ID[i] == hh00.df[i,"PROBECAT_o_CIMO_ID_1"]) PROBECAT_o_CIMO_dummy_IND[i] <- 1
  if(ind_ID[i] == hh00.df[i,"temp_employment_program_ID_1"]) temp_employment_program_dummy_IND[i] <- 1
  if(ind_ID[i] == hh00.df[i,"progresa_cash_ID_1"]) progresa_cash_dummy_IND[i] <- 1
  if(ind_ID[i] == hh00.df[i,"progresa_suppliment_ID_1"]) progresa_suppliment_dummy_IND[i] <- 1
  if(ind_ID[i] == hh00.df[i,"apoyo_monetario_Progresa_card_holder_1"]) apoyo_monetario_Progresa_dummy_IND[i] <- 1
  
  if(ind_ID[i] == hh00.df[i,"DIF_kitchen_ID_2"]) DIF_kitchen_dummy_IND[i] <- 1
  if(ind_ID[i] == hh00.df[i,"apoyo_del_programa_Ninos_de_Solidaridad_ID_2"]) apoyo_del_programa_Ninos_de_Solidaridad_dummy_IND[i] <- 1
  if(ind_ID[i] == hh00.df[i,"temp_employment_program_ID_2"]) temp_employment_program_dummy_IND[i] <- 1
  if(ind_ID[i] == hh00.df[i,"progresa_cash_ID_2"]) progresa_cash_dummy_IND[i] <- 1
  if(ind_ID[i] == hh00.df[i,"progresa_suppliment_ID_2"]) progresa_suppliment_dummy_IND[i] <- 1
  if(ind_ID[i] == hh00.df[i,"apoyo_monetario_Progresa_card_holder_2"]) apoyo_monetario_Progresa_dummy_IND[i] <- 1
  
  if(ind_ID[i] == hh00.df[i,"progresa_cash_ID_3"]) progresa_cash_dummy_IND[i] <- 1
  if(ind_ID[i] == hh00.df[i,"progresa_suppliment_ID_3"]) progresa_suppliment_dummy_IND[i] <- 1
  
  if(ind_ID[i] == hh00.df[i,"progresa_cash_ID_4"]) progresa_cash_dummy_IND[i] <- 1
  if(ind_ID[i] == hh00.df[i,"progresa_suppliment_ID_4"]) progresa_suppliment_dummy_IND[i] <- 1
  
  if(ind_ID[i] == hh00.df[i,"Tortilla_solidaridad_card_holder"]) Tortilla_solidaridad_dummy_IND[i] <- 1    # Conasupo and Tortilla help
  if(ind_ID[i] == hh00.df[i,"Conasupo_card_holder"]) Conasupo_dummy_IND[i] <- 1
  
}

# module on remittances 
traveled_where <- hh00.df[,"w114"]
sent_help <- hh00.df[,"w115"]
sent_remittances <- c(rep(0, length(sent_help)))
sent_remittances[sent_help == 1] <- 1 # BE CAREFUL, the coding includes an extra space before "Si, en dinero" !!!

# 68 land ownership
land_ownership1 <- hh00.df[,"w14001a"] # hh level var
land_ownership2 <- hh00.df[,"w14002a"]
land_ownership3 <- hh00.df[,"w14003a"]
land_ownership4 <- hh00.df[,"w14004a"]
land_ownership5 <- hh00.df[,"w14005a"]

number_land_parcels <- hh00.df[,"w139"]

# saving the dataframe to a .Rda file so that the last datacleaning section can load it in, 
# while the remanining dataset construction process can occur without a cluttered environment

cleaned.00.df <- as.data.frame(list(folio, ind_ID, sex, treatment_dummy, municipio, localidad, 
                                    state, age, 
                                    LFP, worked_last_7_days, hh_work, 
                                    primary_employment, numdaysworked, numdaysworked_no_response,
                                    wages, wages_top_coded_dummy,
                                    payment_period, other_income_source, other_income_source2,
                                    otherincomeval1, otherincomeper1, otherincomeval2, otherincomeper2,
                                    traveled_where, sent_remittances, 
                                    apoyo_monetario_Progresa_dummy_IND, progresa_suppliment_dummy_IND, progresa_cash_dummy_IND,
                                    DIF_kitchen_dummy_IND, apoyo_del_programa_Ninos_de_Solidaridad_dummy_IND,
                                    apoyo_del_INI_dummy_IND, PROBECAT_o_CIMO_dummy_IND, temp_employment_program_dummy_IND,  
                                    Tortilla_solidaridad_dummy_IND, Conasupo_dummy_IND,
                                    land_ownership1, land_ownership2, land_ownership3, land_ownership4, land_ownership5, number_land_parcels, 
                                    wages_not_known, wages_not_reported, payment_period_not_known, payment_period_not_reported, otherincomeper1_not_reported, 
                                    otherincomeval1_not_reported, otherincomeval1_not_known, otherincomeper1_not_known, otherincomeper2_not_reported,
                                    otherincomeval2_not_reported, otherincomeval2_not_known, otherincomeper2_not_known, 
                                    hh_young_kids, hh_kids, currently_in_school, why_not_in_school), nrow = length(folio), stringsAsFactors = FALSE)

colnames(cleaned.00.df) <- c("folio", "ind_ID", "sex", "treatment_dummy", "municipio", "localidad", 
                             "state", "age", 
                             "LFP", "worked_last_7_days", "hh_work", 
                             "primary_employment", "numdaysworked", "numdaysworked_no_response",
                             "wages", "wages_top_coded_dummy",
                             "payment_period", "other_income_source", "other_income_source2",
                             "otherincomeval1", "otherincomeper1", "otherincomeval2", "otherincomeper2",
                             "traveled_where",  "sent_remittances", 
                             "apoyo_monetario_Progresa_dummy_IND", "progresa_suppliment_dummy_IND", "progresa_cash_dummy_IND",
                             "DIF_kitchen_dummy_IND", "apoyo_del_programa_Ninos_de_Solidaridad_dummy_IND",
                             "apoyo_del_INI_dummy_IND", "PROBECAT_o_CIMO_dummy_IND", "temp_employment_program_dummy_IND",  
                             "Tortilla_solidaridad_dummy_IND", "Conasupo_dummy_IND",
                             "land_ownership1", "land_ownership2", "land_ownership3", "land_ownership4", "land_ownership5", "number_land_parcels", 
                             "wages_not_known", "wages_not_reported", "payment_period_not_known", "payment_period_not_reported", "otherincomeper1_not_reported", 
                             "otherincomeval1_not_reported", "otherincomeval1_not_known", "otherincomeper1_not_known", "otherincomeper2_not_reported",
                             "otherincomeval2_not_reported", "otherincomeval2_not_known", "otherincomeper2_not_known", 
                             "hh_young_kids", "hh_kids", "currently_in_school", "why_not_in_school")

# Dropping households that don't know or don't report their income, as well as households without a man and a woman as the hh heads, and didn't have an inf

summary(cleaned.00.df)

save(cleaned.00.df, file = "cleaned.00.Master.Rda")

# Chapter 5: Panel Construction ####
# NOTE: To construct the panel, each of the three waves have to have the same column names. 
#       To achieve this, add some columns to 99 and 00, subtract some from 97
rm(list=ls())
#setwd("C:/Users/Matthew/Desktop/-/ProgrammingDirectory")
load("Cleaned.Baseline.Master.Rda")
hh97.df <- Cleaned.Baseline.Master.df
load("cleaned.99.Master.Rda")
hh99.df <- cleaned.99.df
load("cleaned.00.Master.Rda")
hh00.df <- cleaned.00.df

hh97.df$baseSex <- hh97.df$sex # The third wave saved sex info only for new people. So I'll just make all the 2000 NA's for sex into the baseSex vals. 

# step 1: adding variables to the later two waves: 

missing_1.df <- hh97.df[, c("folio", "ind_ID", "relation_to_jefe", "inf", "baseSex",
                            "dad_live_in_hh", "mom_live_in_hh", "head_dummy", "number_heads",
                            "dad_parents_live_in_hh_dummy_hh", "mom_parents_live_in_hh_dummy_hh",
                            "indigenous_language", 
                            "spanish_language", "literate", "evergonetoschool", "highest_grade_completed",
                            "employment_benefits", "agestartedwork", "pobre", "pobextre", "mpcalif", "mppob")]


hh99.df <- merge(hh99.df, missing_1.df, by = c("folio", "ind_ID"), all.x = TRUE)

hh00.df <- merge(hh00.df, missing_1.df, by = c("folio", "ind_ID"), all.x = TRUE) 

# Step 2: adding a dummy that describes which wave you're in. 

hh97.df$wave2 <- 
  hh97.df$wave3 <- c(rep(0, length(hh97.df[,"folio"])))
hh99.df$wave1 <- 
  hh99.df$wave3 <- c(rep(0, length(hh99.df[,"folio"])))
hh00.df$wave1 <- 
  hh00.df$wave2 <- c(rep(0, length(hh00.df[,"folio"])))

hh97.df$wave1 <- c(rep(1, length(hh97.df[,"folio"])))
hh99.df$wave2 <- c(rep(1, length(hh99.df[,"folio"])))
hh00.df$wave3 <- c(rep(1, length(hh00.df[,"folio"])))

# step 3: adding 3 dummies to the baseline that hold the spot for progresa information 

hh97.df$apoyo_monetario_Progresa_dummy_IND <- 
  hh97.df$progresa_suppliment_dummy_IND <- 
  hh97.df$progresa_cash_dummy_IND <- 
  hh97.df$number_land_parcels <- c(rep(NA, length(hh97.df[,"folio"]))) 

# Step 4:  Reordering the baseline so that the columns are in the same order as they are in the later waves
hh97.df_ao <- hh97.df[, order(names(hh97.df))] #ao == "alphabetic ordering"
hh99.df_ao <- hh99.df[, order(names(hh99.df))]
hh00.df_ao <- hh00.df[, order(names(hh00.df))]


# step 5: dropping variables not required for analysis (or that don't show up in all of the dataframes.) 
hh97.df_ao_dropped <- subset(hh97.df, select=c(cbind(names(hh99.df_ao)))) # dropped == "extra vars dropped"

#check:
colnames(hh97.df_ao_dropped) == colnames(hh99.df_ao)

# step 6: rbind the three dataframes into a single panel: reference: unified_cd <- as.data.frame(rbind(subset1, subset2), header = T)

final.panel.97.99.00.df_a <- data.frame(rbind(hh97.df_ao_dropped, hh99.df_ao)) 
final.panel.97.99.00.df <- data.frame(rbind(final.panel.97.99.00.df_a, hh00.df_ao)) 
save(final.panel.97.99.00.df, file = "Master.Panel.97.99.00.Rda")

rm(list=ls()) #cleaning out the gunk to make some additional edits without stuff in the environment. 
#setwd("C:/Users/Matthew/Desktop/-/ProgrammingDirectory")
library(dummies)
load("Master.Panel.97.99.00.Rda")
hh.df <- final.panel.97.99.00.df 

str(hh.df$state)
table(hh.df$state)
hh.df$seven_states <- as.character(rep(NA, length(hh.df$folio)))
hh.df$seven_states[as.character(hh.df$state) == 12]  <- "Guerrero"
hh.df$seven_states[as.character(hh.df$state) == 13]  <- "Hidalgo"
hh.df$seven_states[as.character(hh.df$state) == 16]  <- "Michoacán"
hh.df$seven_states[as.character(hh.df$state) == 21]  <- "Puebla"
hh.df$seven_states[as.character(hh.df$state) == 22]  <- "Queretaro"
hh.df$seven_states[as.character(hh.df$state) == 24]  <- "San Luis Potosí"
hh.df$seven_states[as.character(hh.df$state) == 30]  <- "Veracruz"
table(hh.df$seven_states)

save(hh.df, file = "hh.intermediate.Rda")

# Cosmetic adjustments that make regression on subsets feasbile
# Adding one more variable and restructuring the data set so that the variables are in the format we want. I.e. age as a number, not a character.  
rm(list=ls())
#setwd("C:/Users/Matthew/Desktop/-/ProgrammingDirectory")
library(lattice)
load("hh.intermediate.Rda")

str(hh.df)

# number of female and male children in hh
hh.df$female_kid_1 <- hh.df$female_kid_2 <- hh.df$female_kid_3 <- hh.df$male_kid_1 <- hh.df$male_kid_2 <- hh.df$male_kid_3 <- c(rep(0, length(hh.df$folio))) 

hh.df$female_kid_1[hh.df$sex == 2 & as.numeric(hh.df$age) < 17 & hh.df$wave1 == 1] <- 1
hh.df$female_kid_2[hh.df$sex == 2 & as.numeric(hh.df$age) < 17 & hh.df$wave2 == 1] <- 1
hh.df$female_kid_3[hh.df$sex == 2 & as.numeric(hh.df$age) < 17 & hh.df$wave3 == 1] <- 1

hh_f_kids_1.df <- aggregate(hh.df$female_kid_1, by = list(Category=hh.df$folio), FUN=sum)
names(hh_f_kids_1.df) <- c("folio","hh_f_kids_1") 
hh_f_kids_2.df <- aggregate(hh.df$female_kid_2, by = list(Category=hh.df$folio), FUN=sum)
names(hh_f_kids_2.df) <- c("folio","hh_f_kids_2") 
hh_f_kids_3.df <- aggregate(hh.df$female_kid_3, by = list(Category=hh.df$folio), FUN=sum)
names(hh_f_kids_3.df) <- c("folio","hh_f_kids_3") 

hh.df$male_kid_1[hh.df$sex == 1 & as.numeric(hh.df$age) < 17 & hh.df$wave1 == 1] <- 1
hh.df$male_kid_2[hh.df$sex == 1 & as.numeric(hh.df$age) < 17 & hh.df$wave2 == 1] <- 1
hh.df$male_kid_3[hh.df$sex == 1 & as.numeric(hh.df$age) < 17 & hh.df$wave3 == 1] <- 1

hh_m_kids_1.df <- aggregate(hh.df$male_kid_1, by = list(Category=hh.df$folio), FUN=sum)
names(hh_m_kids_1.df) <- c("folio","hh_m_kids_1") 
hh_m_kids_2.df <- aggregate(hh.df$male_kid_2, by = list(Category=hh.df$folio), FUN=sum)
names(hh_m_kids_2.df) <- c("folio","hh_m_kids_2") 
hh_m_kids_3.df <- aggregate(hh.df$male_kid_3, by = list(Category=hh.df$folio), FUN=sum)
names(hh_m_kids_3.df) <- c("folio","hh_m_kids_3")

hh.df <- merge( hh.df, hh_f_kids_1.df, by = "folio")
hh.df <- merge( hh.df, hh_f_kids_2.df, by = "folio")
hh.df <- merge( hh.df, hh_f_kids_3.df, by = "folio")
hh.df <- merge( hh.df, hh_m_kids_1.df, by = "folio")
hh.df <- merge( hh.df, hh_m_kids_2.df, by = "folio")
hh.df <- merge( hh.df, hh_m_kids_3.df, by = "folio")

hh.df$number_female_kids <- hh.df$number_male_kids <- c(rep(0, length(hh.df$folio)))

hh.df$number_female_kids[hh.df$wave1 == 1] <- hh.df$hh_f_kids_1[hh.df$wave1 == 1]
hh.df$number_female_kids[hh.df$wave2 == 1] <- hh.df$hh_f_kids_1[hh.df$wave2 == 1]
hh.df$number_female_kids[hh.df$wave3 == 1] <- hh.df$hh_f_kids_1[hh.df$wave3 == 1]

hh.df$number_male_kids[hh.df$wave1 == 1] <- hh.df$hh_m_kids_1[hh.df$wave1 == 1]
hh.df$number_male_kids[hh.df$wave2 == 1] <- hh.df$hh_m_kids_1[hh.df$wave2 == 1]
hh.df$number_male_kids[hh.df$wave3 == 1] <- hh.df$hh_m_kids_1[hh.df$wave3 == 1]

# dummy for not hh_head
hh.df$not_head <- (hh.df$head_dummy - 1)*(-1)

#converting some variables to dummy variables (e.g. indigenous language has 1 = Si, 2 = No, 9 = NR, should have NR = No = 0)
hh.df$indigenous_language[hh.df$indigenous_language == 9 | hh.df$indigenous_language == 2] <- 0
hh.df$spanish_language[hh.df$spanish_language == 9 | hh.df$spanish_language == 2] <- 0
hh.df$literate[hh.df$literate == 9 | hh.df$literate == 2] <- 0

# Comments - add string attributes to a variable that give additional information for data analyist 
comment(hh.df$number_land_parcels) <- "this question not asked in baseline"

# Updating storage format for variables
# ex. people with no land have value 0 for landownership_1 in 1997, but NA for the same variable/question in 1999. 
hh.df$land_ownership1[is.na(hh.df$land_ownership1) == TRUE] <- 0
hh.df$land_ownership2[is.na(hh.df$land_ownership2) == TRUE] <- 0
hh.df$land_ownership3[is.na(hh.df$land_ownership3) == TRUE] <- 0
hh.df$land_ownership4[is.na(hh.df$land_ownership4) == TRUE] <- 0
hh.df$land_ownership5[is.na(hh.df$land_ownership5) == TRUE] <- 0

# rename primary employment variables.
hh.df$primary_employment <- as.character(hh.df$primary_employment)
hh.df$primary_employment[hh.df$primary_employment == 0] <- "Unemployed"
hh.df$primary_employment[hh.df$primary_employment == 1] <- "Jornalero rural o peón de campo"
hh.df$primary_employment[hh.df$primary_employment == "jornalero rural o peón de campo?"] <- "Jornalero rural o peón de campo"
hh.df$primary_employment[hh.df$primary_employment == 2] <- "Obrero o empleado NO agropecuario"
hh.df$primary_employment[hh.df$primary_employment == "obrero o empleado No agropecuario?"] <- "Obrero o empleado NO agropecuario"
hh.df$primary_employment[hh.df$primary_employment == 3] <- "Trabajador por cuenta propia"
hh.df$primary_employment[hh.df$primary_employment == "trabajador por cuenta propia?"] <- "Trabajador por cuenta propia"
hh.df$primary_employment[hh.df$primary_employment == 4] <- "Patrón, o empleador de un negocio" 
hh.df$primary_employment[hh.df$primary_employment == "patrón, o empleador de un negocio?"] <- "Patrón, o empleador de un negocio" 
hh.df$primary_employment[hh.df$primary_employment == "Patrón oempleador de un negocio"] <- "Patrón, o empleador de un negocio" 
hh.df$primary_employment[hh.df$primary_employment == 5] <- "Trabajador en negocio familiar sin retribución" 
hh.df$primary_employment[hh.df$primary_employment == "trabajador en negocio familiar sin retribución?"] <- "Trabajador en negocio familiar sin retribución" 
hh.df$primary_employment[hh.df$primary_employment == 6] <- "Trabajador sin retribución (NO familiar)"
hh.df$primary_employment[hh.df$primary_employment == "trabajador sin retribución?"] <- "Trabajador sin retribución (NO familiar)"
hh.df$primary_employment[hh.df$primary_employment == 7] <- "Miembro de una cooperativa"
hh.df$primary_employment[hh.df$primary_employment == "miembro de una cooperativa?"] <- "Miembro de una cooperativa"
hh.df$primary_employment[hh.df$primary_employment == 8] <- "Ejidatario o comunero"
hh.df$primary_employment[hh.df$primary_employment == "ejidatario o comunero?"] <- "Ejidatario o comunero"
hh.df$primary_employment[hh.df$primary_employment == 9] <- "Otros"
hh.df$primary_employment[hh.df$primary_employment == 99] <- "NR"

hh.df$wages[is.na(hh.df$wages) == TRUE] <- 0 

hh.df$otherincomeval1[is.na(hh.df$otherincomeval1) == TRUE] <- 0
hh.df$otherincomeval2[is.na(hh.df$otherincomeval2) == TRUE] <- 0

str(hh.df)

# updating the education variables

# creating households level migration variables
# traveled_where different in the baseline than in the later two waves. In the baseline,  the value coresponds to the state the migrant went to, or out
# in the first wave, the values are:  0 - NA, 1-32 - states in alphabetical order, 66 - same state, 77 - foreign, 98 - dont know, 99 - NR
# in the third waves, the values are: 0 - dead, 1 - same location, 2 - nearby location, 3 - same muni, 4 - same state, 5 - different state, 6 - foreign, NR  
# NOT TRUE AFTER CHANGING TO THE "use.value.labels == FALSE" EDIT: In the second wave we have text. 
# ALSO NOTE: Need an aggregation per year



hh.df$USA_migrant <- hh.df$MEX_migrant <- c(rep(0, length(hh.df$folio)))

# WAVE 1
table(hh.df$traveled_where[hh.df$wave1 == 1])

hh.df$MEX_migrant[hh.df$traveled_where > 0 & hh.df$traveled_where < 70 & hh.df$wave1 == 1] <- 1 
hh.df$USA_migrant[hh.df$traveled_where == 77 & hh.df$wave1 == 1] <- 1

USA_migrant.97.df <- aggregate(as.numeric(hh.df$USA_migrant[hh.df$wave1 == 1]), by = list(Category=hh.df$folio[hh.df$wave1 == 1]), FUN=sum)
names(USA_migrant.97.df) <- c("folio", "USA_Migrant_97")

MEX_migrant.97.df <- aggregate(as.numeric(hh.df$MEX_migrant[hh.df$wave1 == 1]), by = list(Category=hh.df$folio[hh.df$wave1 == 1]), FUN=sum)
names(MEX_migrant.97.df) <- c("folio", "MEX_Migrant_97")

# WAVE 2
table(hh.df$traveled_where[hh.df$wave2 == 1])

hh.df$MEX_migrant[hh.df$traveled_where == 1 & hh.df$wave2 == 1] <- 1 
hh.df$MEX_migrant[hh.df$traveled_where == 2 & hh.df$wave2 == 1] <- 1
hh.df$MEX_migrant[hh.df$traveled_where == 3 & hh.df$wave2 == 1] <- 1
hh.df$MEX_migrant[hh.df$traveled_where == 4 & hh.df$wave2 == 1] <- 1
hh.df$MEX_migrant[hh.df$traveled_where == 5 & hh.df$wave2 == 1] <- 1
hh.df$USA_migrant[hh.df$traveled_where == 6 & hh.df$wave2 == 1] <- 1

USA_migrant.99.df <- aggregate(as.numeric(hh.df$USA_migrant[hh.df$wave2 == 1]), by = list(Category=hh.df$folio[hh.df$wave2 == 1]), FUN=sum)
names(USA_migrant.99.df) <- c("folio", "USA_Migrant_99")

MEX_migrant.99.df <- aggregate(as.numeric(hh.df$MEX_migrant[hh.df$wave2 == 1]), by = list(Category=hh.df$folio[hh.df$wave2 == 1]), FUN=sum)
names(MEX_migrant.99.df) <- c("folio", "MEX_Migrant_99")

# WAVE 3
table(hh.df$traveled_where[hh.df$wave3 == 1])

hh.df$MEX_migrant[hh.df$traveled_where > 0 &  hh.df$traveled_where < 6 & hh.df$wave3 == 1] <- 1 
hh.df$USA_migrant[hh.df$traveled_where == 6 & hh.df$wave3 == 1] <- 1

USA_migrant.00.df <- aggregate(as.numeric(hh.df$USA_migrant[hh.df$wave3 == 1]), by = list(Category=hh.df$folio[hh.df$wave3 == 1]), FUN=sum)
names(USA_migrant.00.df) <- c("folio", "USA_Migrant_00")

MEX_migrant.00.df <- aggregate(as.numeric(hh.df$MEX_migrant[hh.df$wave3 == 1]), by = list(Category=hh.df$folio[hh.df$wave3 == 1]), FUN=sum)
names(MEX_migrant.00.df) <- c("folio", "MEX_Migrant_00")

# merging everything into the hh.df file

hh.df <- merge(hh.df, USA_migrant.97.df, by = "folio", all.x = TRUE)
hh.df <- merge(hh.df, USA_migrant.99.df, by = "folio", all.x = TRUE)
hh.df <- merge(hh.df, USA_migrant.00.df, by = "folio", all.x = TRUE)
hh.df <- merge(hh.df, MEX_migrant.97.df, by = "folio", all.x = TRUE)
hh.df <- merge(hh.df, MEX_migrant.99.df, by = "folio", all.x = TRUE)
hh.df <- merge(hh.df, MEX_migrant.00.df, by = "folio", all.x = TRUE)

hh.df$USA_migrant[hh.df$wave1 == 1] <- hh.df$USA_Migrant_97[hh.df$wave1 == 1] 
hh.df$USA_migrant[hh.df$wave2 == 1] <- hh.df$USA_Migrant_99[hh.df$wave2 == 1] 
hh.df$USA_migrant[hh.df$wave3 == 1] <- hh.df$USA_Migrant_00[hh.df$wave3 == 1] 

hh.df$MEX_migrant[hh.df$wave1 == 1] <- hh.df$MEX_Migrant_97[hh.df$wave1 == 1] 
hh.df$MEX_migrant[hh.df$wave2 == 1] <- hh.df$MEX_Migrant_99[hh.df$wave2 == 1] 
hh.df$MEX_migrant[hh.df$wave3 == 1] <- hh.df$MEX_Migrant_00[hh.df$wave3 == 1] 

# EDUCATION VARIABLES

table(hh.df$highest_grade_completed)
hh.df$edu_yrs <- c(rep(0, length(hh.df$folio)))
hh.df$edu_yrs[hh.df$highest_grade_completed == 12] <- 1
hh.df$edu_yrs[hh.df$highest_grade_completed == 22] <- 2
hh.df$edu_yrs[hh.df$highest_grade_completed == 32 | hh.df$highest_grade_completed == 92] <- 3
hh.df$edu_yrs[hh.df$highest_grade_completed == 42] <- 4
hh.df$edu_yrs[hh.df$highest_grade_completed == 52] <- 5
hh.df$edu_yrs[hh.df$highest_grade_completed == 62 | hh.df$highest_grade_completed == 82] <- 6
hh.df$edu_yrs[hh.df$highest_grade_completed == 13] <- 7
hh.df$edu_yrs[hh.df$highest_grade_completed == 23 | hh.df$highest_grade_completed == 93] <- 8
hh.df$edu_yrs[hh.df$highest_grade_completed == 33 | hh.df$highest_grade_completed == 43 | hh.df$highest_grade_completed == 53 |
                hh.df$highest_grade_completed == 63] <- 9
hh.df$edu_yrs[hh.df$highest_grade_completed == 14 | hh.df$highest_grade_completed == 15 | hh.df$highest_grade_completed == 94 |
                hh.df$highest_grade_completed == 95] <- 10
hh.df$edu_yrs[hh.df$highest_grade_completed == 24 | hh.df$highest_grade_completed == 25] <- 11
hh.df$edu_yrs[hh.df$highest_grade_completed == 34 | hh.df$highest_grade_completed == 35 | hh.df$highest_grade_completed == 45 |
                hh.df$highest_grade_completed == 55 | hh.df$highest_grade_completed == 65] <- 12
hh.df$edu_yrs[hh.df$highest_grade_completed == 44 | hh.df$highest_grade_completed == 54 | hh.df$highest_grade_completed == 64 |
                hh.df$highest_grade_completed == 16] <- 13
hh.df$edu_yrs[hh.df$highest_grade_completed == 26 | hh.df$highest_grade_completed == 96] <- 14
hh.df$edu_yrs[hh.df$highest_grade_completed == 36] <- 15
hh.df$edu_yrs[hh.df$highest_grade_completed == 46] <- 16
hh.df$edu_yrs[hh.df$highest_grade_completed == 17 | hh.df$highest_grade_completed == 56] <- 17
hh.df$edu_yrs[hh.df$highest_grade_completed == 27 | hh.df$highest_grade_completed == 66 | hh.df$highest_grade_completed == 76 |
                hh.df$highest_grade_completed == 86] <- 18
hh.df$edu_yrs[hh.df$highest_grade_completed == 37] <- 19
hh.df$edu_yrs[hh.df$highest_grade_completed == 47 | hh.df$highest_grade_completed == 57 | hh.df$highest_grade_completed == 67] <- 20
hh.df$edu_yrs[hh.df$highest_grade_completed == 18 | hh.df$highest_grade_completed == 28 | hh.df$highest_grade_completed == 38 |
                hh.df$highest_grade_completed == 48 | hh.df$highest_grade_completed == 58 | hh.df$highest_grade_completed == 68] <- 21

hist(hh.df$edu_yrs[hh.df$edu_yrs > 0])


# the 2000 data didn't ask everyone their sex, only new people, so I need to go through and give people in the last time a sex from their earlier info
summary(hh.df$baseSex)
summary(hh.df$sex)
for(i in 1:length(hh.df$folio)){
  if(is.na(hh.df[i,"sex"])) hh.df[i,"sex"] <- hh.df[i,"baseSex"] 
}

table(hh.df$sex)
summary(hh.df$sex)

final.panel.97.99.00.df <- hh.df
save(final.panel.97.99.00.df, file = "Master_Panel.Rda")

# Chapter 6, Making the sex variable into a dummy and Progresa Transfer Amount Inclusion ####

rm(list=ls())
#setwd("C:/Users/Matthew/Desktop/-/ProgrammingDirectory")
load("Master_Panel.Rda")
hh.df <- final.panel.97.99.00.df

hh.df$sex <- hh.df$sex - 1

hh.df$wavenumber <- hh.df$wave1 
hh.df$wavenumber[hh.df$wave2 == 1] <- 2
hh.df$wavenumber[hh.df$wave3 == 1] <- 3
table(hh.df$wavenumber)

# Steps - (A) generate HH level dummy variables equal to one for HH's that recieved the Becas, Suppliment, and Supplies transfers  
#         (B) calculate the progresa cash that each child generates for a mother.
#         (C) aggregate at the household level in both the 1999 and 2000 waves
#         (D) multiply by the becas HH-level dummy
#         (E) include the nut. suppliment interacted with the sup dummy 
#         (F) include the multiply by dummy for apoyo monetrario Progresa
#         (G) set values higher than the ceiling to the ceiling value - 820 pesos a month in 2000, 750 in 1999
#         (H) Convert to a weekly income measure by dividing by 4.




# aggregating the individual level dummies for who recieved progresa assistance to the household level 
# these values are a little muddled, sometimes refereing to the kids who get the transfers and sometimes the parents. I want just a clean HH level var

table(hh.df$progresa_cash_dummy_IND, hh.df$wavenumber)
table(hh.df$progresa_suppliment_dummy_IND, hh.df$wavenumber)
table(hh.df$apoyo_monetario_Progresa_dummy_IND, hh.df$wavenumber)

becas.df <- aggregate(hh.df$progresa_cash_dummy_IND, by=list(hh.df$folio, hh.df$wavenumber), FUN=sum, na.rm=T)
names(becas.df) <- c("folio", "wavenumber", "progresa_cash_dummy_HH")
becas.df$progresa_cash_dummy_HH[becas.df$progresa_cash_dummy_HH >= 1] <- 1

hh.df <- merge(hh.df, becas.df, by = c("folio", "wavenumber"), all.x = T)

apoyo.df <- aggregate(hh.df$apoyo_monetario_Progresa_dummy_IND, by=list(hh.df$folio, hh.df$wavenumber), FUN=sum, na.rm=T)
names(apoyo.df) <- c("folio", "wavenumber", "apoyo_monetario_Progresa_dummy_HH")
apoyo.df$apoyo_monetario_Progresa_dummy_HH[apoyo.df$apoyo_monetario_Progresa_dummy_HH >= 1] <- 1

hh.df <- merge(hh.df, apoyo.df, by = c("folio", "wavenumber"), all.x = T)

sup.df <- aggregate(hh.df$progresa_suppliment_dummy_IND, by=list(hh.df$folio, hh.df$wavenumber), FUN=sum, na.rm=T)
names(sup.df) <- c("folio", "wavenumber", "progresa_suppliment_dummy_HH")
sup.df$progresa_suppliment_dummy_HH[sup.df$progresa_suppliment_dummy_HH >= 1] <- 1

hh.df <- merge(hh.df, sup.df, by = c("folio", "wavenumber"), all.x = T)

# Scholarship ammounts in November 2000

hh.df$progresa_income <- hh.df$progresa_income_total <- c(rep(0, length(hh.df$folio)))

hh.df$progresa_income[hh.df$wavenumber == 3 & hh.df$highest_grade_completed %/% 10 == 1 & hh.df$highest_grade_completed %% 10 == 3 & 
                        hh.df$currently_in_school == 1] <- 90 # + 180
hh.df$progresa_income[hh.df$wavenumber == 3 & hh.df$highest_grade_completed %/% 10 == 1 & hh.df$highest_grade_completed %% 10 == 4 & 
                        hh.df$currently_in_school == 1] <- 105 
hh.df$progresa_income[hh.df$wavenumber == 3 & hh.df$highest_grade_completed %/% 10 == 1 & hh.df$highest_grade_completed %% 10 == 5 & 
                        hh.df$currently_in_school == 1] <- 135 
hh.df$progresa_income[hh.df$wavenumber == 3 & hh.df$highest_grade_completed %/% 10 == 1 & hh.df$highest_grade_completed %% 10 == 6 & 
                        hh.df$currently_in_school == 1] <- 180 

hh.df$progresa_income[hh.df$highest_grade_completed %/% 10 == 2 & hh.df$highest_grade_completed %% 10 == 1 & hh.df$sex == 0 & 
                        hh.df$currently_in_school == 1 & hh.df$wavenumber == 3] <- 260 # + 225
hh.df$progresa_income[hh.df$highest_grade_completed %/% 10 == 2 & hh.df$highest_grade_completed %% 10 == 2 & hh.df$sex == 0 & 
                        hh.df$currently_in_school == 1 & hh.df$wavenumber == 3] <- 275 
hh.df$progresa_income[hh.df$highest_grade_completed %/% 10 == 2 & hh.df$highest_grade_completed %% 10 == 3 & hh.df$sex == 0 & 
                        hh.df$currently_in_school == 1 & hh.df$wavenumber == 3] <- 290
hh.df$progresa_income[hh.df$highest_grade_completed %/% 10 == 2 & hh.df$highest_grade_completed %% 10 == 1 & hh.df$sex == 1 & 
                        hh.df$currently_in_school == 1 & hh.df$wavenumber == 3] <- 275
hh.df$progresa_income[hh.df$highest_grade_completed %/% 10 == 2 & hh.df$highest_grade_completed %% 10 == 2 & hh.df$sex == 1 & 
                        hh.df$currently_in_school == 1 & hh.df$wavenumber == 3] <- 305
hh.df$progresa_income[hh.df$highest_grade_completed %/% 10 == 2 & hh.df$highest_grade_completed %% 10 == 3 & hh.df$sex == 1 & 
                        hh.df$currently_in_school == 1 & hh.df$wavenumber == 3] <- 335

progresa_income_2000.df <- aggregate(hh.df$progresa_income[hh.df$wavenumber == 3], by=list(Category=hh.df$folio[hh.df$wavenumber == 3]), FUN=sum)
names(progresa_income_2000.df) = c("folio", "progresa_income_total_2000")
progresa_income_2000.df$wavenumber <- c(rep(3, length(progresa_income_2000.df$folio)))

hh.df <- merge(hh.df, progresa_income_2000.df, by = c("folio", "wavenumber"), all.x = T)
hh.df$progresa_income_total_2000[is.na(hh.df$progresa_income_total_2000)] <- 0
summary(hh.df$progresa_income_total_2000)

# For November 1999

hh.df$progresa_income[hh.df$wavenumber == 2 & hh.df$highest_grade_completed %/% 10 == 1 & hh.df$highest_grade_completed %% 10 == 3 & 
                        hh.df$currently_in_school == 1] <- 80 # 
hh.df$progresa_income[hh.df$wavenumber == 2 & hh.df$highest_grade_completed %/% 10 == 1 & hh.df$highest_grade_completed %% 10 == 4 & 
                        hh.df$currently_in_school == 1] <- 95 
hh.df$progresa_income[hh.df$wavenumber == 2 & hh.df$highest_grade_completed %/% 10 == 1 & hh.df$highest_grade_completed %% 10 == 5 & 
                        hh.df$currently_in_school == 1] <- 125 
hh.df$progresa_income[hh.df$wavenumber == 2 & hh.df$highest_grade_completed %/% 10 == 1 & hh.df$highest_grade_completed %% 10 == 6 & 
                        hh.df$currently_in_school == 1] <- 165 

hh.df$progresa_income[hh.df$highest_grade_completed %/% 10 == 2 & hh.df$highest_grade_completed %% 10 == 1 & hh.df$sex == 0 & 
                        hh.df$currently_in_school == 1 & hh.df$wavenumber == 2] <- 260 # + 225
hh.df$progresa_income[hh.df$highest_grade_completed %/% 10 == 2 & hh.df$highest_grade_completed %% 10 == 2 & hh.df$sex == 0 & 
                        hh.df$currently_in_school == 1 & hh.df$wavenumber == 2] <- 275 
hh.df$progresa_income[hh.df$highest_grade_completed %/% 10 == 2 & hh.df$highest_grade_completed %% 10 == 3 & hh.df$sex == 0 & 
                        hh.df$currently_in_school == 1 & hh.df$wavenumber == 2] <- 290
hh.df$progresa_income[hh.df$highest_grade_completed %/% 10 == 2 & hh.df$highest_grade_completed %% 10 == 1 & hh.df$sex == 1 & 
                        hh.df$currently_in_school == 1 & hh.df$wavenumber == 2] <- 275
hh.df$progresa_income[hh.df$highest_grade_completed %/% 10 == 2 & hh.df$highest_grade_completed %% 10 == 2 & hh.df$sex == 1 & 
                        hh.df$currently_in_school == 1 & hh.df$wavenumber == 2] <- 305
hh.df$progresa_income[hh.df$highest_grade_completed %/% 10 == 2 & hh.df$highest_grade_completed %% 10 == 3 & hh.df$sex == 1 & 
                        hh.df$currently_in_school == 1 & hh.df$wavenumber == 2] <- 335

progresa_income_1999.df <- aggregate(hh.df$progresa_income[hh.df$wavenumber == 2], by=list(Category=hh.df$folio[hh.df$wavenumber == 2]), FUN=sum, na.rm = T)
names(progresa_income_1999.df) = c("folio", "progresa_income_total_1999")
progresa_income_1999.df$wavenumber <- c(rep(2, length(progresa_income_1999.df$folio)))

hh.df <- merge(hh.df, progresa_income_1999.df, by = c("folio", "wavenumber"), all.x = T)
hh.df$progresa_income_total_1999[is.na(hh.df$progresa_income_total_1999)] <-  0
summary(hh.df$progresa_income_total_1999)

hh.df$progresa_income_total <- hh.df$progresa_income_total_1999 + hh.df$progresa_income_total_2000
summary(hh.df$progresa_income_total)

# Multiplying by the becas dummy
hh.df$progresa_income_total <- hh.df$progresa_income_total*hh.df$progresa_cash_dummy_HH 

# Adding in the Apoyo Monetario cash
# In 2000, elementary school children got 180 and and high school students got 225 
# In 1999, the numbers were 165 and 205
# Step 1: generating a dummy variable for whether the HH had a young child and a dummy for a high school aged child. 
# Step 2: generating how much each kid got for their mom in a period
# Step 3: aggregating at the household level and adding this val to the progresa_income_total variable

# Step 1
hh.df$prog_recipient_young <- hh.df$prog_recipient_old <- c(rep(0, length(hh.df$folio)))
hh.df$prog_recipient_young[hh.df$apoyo_monetario_Progresa_dummy_HH == 1 & hh.df$currently_in_school == 1 &
                             hh.df$highest_grade_completed %/% 10 == 1] <- 1
hh.df$prog_recipient_old[hh.df$apoyo_monetario_Progresa_dummy_HH == 1 & hh.df$currently_in_school == 1 &
                           hh.df$highest_grade_completed %/% 10 == 2] <- 1

# Step 2
hh.df$apoyo_cash <- c(rep(0, length(hh.df$folio)))
hh.df$apoyo_cash[hh.df$prog_recipient_young*hh.df$wave2 == 1] <-   165
hh.df$apoyo_cash[hh.df$prog_recipient_old*hh.df$wave2 == 1] <-   205
hh.df$apoyo_cash[hh.df$prog_recipient_young*hh.df$wave3 == 1] <-   180
hh.df$apoyo_cash[hh.df$prog_recipient_old*hh.df$wave3 == 1] <-   225

# Step 3
apoyo.cash.df <- aggregate(hh.df$apoyo_cash, by = list(hh.df$folio, hh.df$wavenumber), FUN=sum, na.rm=T) 
names(apoyo.cash.df) <- c("folio", "wavenumber", "apoyo_cash_HH")

hh.df <- merge(hh.df, apoyo.cash.df, by = c("folio", "wavenumber"), all.x = T)

hh.df$progresa_income_total <- hh.df$progresa_income_total + hh.df$apoyo_cash_HH

summary(hh.df$progresa_income_total)

# Adding in the Suppliment cash
# In 2000, households got an additional 135 and in 1999, 125

hh.df$progresa_income_total[hh.df$progresa_suppliment_dummy_HH == 1 & hh.df$wavenumber == 2] <- 
  hh.df$progresa_income_total[hh.df$progresa_suppliment_dummy_HH == 1 & hh.df$wavenumber == 2] + 125

hh.df$progresa_income_total[hh.df$progresa_suppliment_dummy_HH == 1 & hh.df$wavenumber == 3] <- 
  hh.df$progresa_income_total[hh.df$progresa_suppliment_dummy_HH == 1 & hh.df$wavenumber == 3] + 135

# Capping max monthly stipened at 820 in 2000, 750 in 1999

hh.df$progresa_income_total[hh.df$wavenumber == 2 & hh.df$progresa_income_total > 750] <- 750
hh.df$progresa_income_total[hh.df$wavenumber == 3 & hh.df$progresa_income_total > 820] <- 820

summary(hh.df$progresa_income_total)

# making it a weekly income measure, not monthly

hh.df$progresa_income_total <- hh.df$progresa_income_total / 4 

summary(hh.df$progresa_income_total)
summary(hh.df$progresa_income_total[hh.df$progresa_income_total > 0])

hist(hh.df$progresa_income_total[hh.df$progresa_income_total > 0 & hh.df$wavenumber == 3],
     col = rgb(1,0,0,.5), main = "Progresa income per household", xlim = c(0, 230))
hist(hh.df$progresa_income_total[hh.df$progresa_income_total > 0 & hh.df$wavenumber == 2], col = rgb(0,0,1,.5), add = T)

#generating a dumy variable for the 307 control households who accidentally got Progresa cash before 2000 

hh.df$treatment_dummy_num <- hh.df$treatment_dummy
hh.df$treatment_dummy[hh.df$treatment_dummy_num == 2] <- "Control" 
hh.df$treatment_dummy[hh.df$treatment_dummy_num == 1] <- "Basal" 

sum(table(unique(hh.df$folio[hh.df$treatment_dummy == "Control" & hh.df$wavenumber == 2 & hh.df$progresa_income_total > 0])))

hh.df$control_early_progresa_cash_drop_dummy <- c(rep(0, length(hh.df$folio)))
hh.df$control_early_progresa_cash_drop_dummy[hh.df$treatment_dummy == "Control" & hh.df$wavenumber == 2 & hh.df$progresa_income_total > 0] <- 1

# Save and quit
final.panel.97.99.00.df <- hh.df
save(final.panel.97.99.00.df, file = "Master_Panel.Rda")

#next step - run the Final Cleaning code. 




