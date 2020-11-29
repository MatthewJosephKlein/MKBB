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
setwd("C:/Users/OWNER/Dropbox/Desktop backup/toot/Programming_Directory")
rm(list=ls())
library("sampleSelection")
library("lfe")
library("glmmML")
library("tidyverse")
# library("ihs")

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

hh.df <- hh.df %>% filter(hh_wages != 0)

# A) BP Function (Hyp 1)
#    A.1) Shadow Earnings function
# B) LPM.Marginal.Fun (Hyp 2, 3)
# C) LPM.Marginal.Fun.Comparison (Hyp 4)
# D) Poisson.Marginal.Fun (Hyp 2)
# E) Accuracy Test Function 
# F) Generate the analog

#### NEURAL NET #### 

data.df <- subset(hh.df,
                  hh.df$age > 15 &
                    hh.df$age <= 65 &
                    hh.df$hh_kids <= 5 & 
                    hh.df$hh_young_kids <= 4 & 
                    hh.df$num_m_adults <= 5 &
                    hh.df$num_f_adults <= 5 & 
                    hh.df$hh_young_kids <= quantile(hh.df$hh_young_kids, 0.95))

data.df <- data.df %>% group_by(sex) %>% # Group by gender, winzorize, scale variables, ungroup
  filter(wages <= quantile(wages, 0.99), 
         wages >= quantile(wages, 0.01)) %>% 
  mutate(scaled.age = ((age - mean(age)) / sd(age)), 
                              scaled.age.squared = (((age^2) - mean((age^2))) / sd((age^2))),
                              scaled.earnings = ((wages - mean(wages)) / sd(wages)),
                              scaled.num.f.adults = ((num_f_adults - mean(num_f_adults)) / sd(num_f_adults)),
                              scaled.num.m.adults = ((num_m_adults - mean(num_m_adults)) / sd(num_m_adults)),
                              scaled.f.kids = ((number_female_kids - mean(number_female_kids)) / sd(number_female_kids)),
                              scaled.m.kids = ((number_male_kids - mean(number_male_kids)) / sd(number_male_kids))) %>%
  ungroup()

summary(data.df$scaled.earnings[data.df$sex ==0])
summary(data.df$scaled.earnings[data.df$sex ==1])

#Make training and test data: 
dt = sort(sample(nrow(data.df), nrow(data.df)*.9))
train.df <- data.df[dt,]
test.df <- data.df[-dt,]

# Predict the values using the neural net package and the same regressors as using the heckman 

# Set the formula to be used in the NN  
earnings.reduced.form <- as.formula("scaled.earnings ~ scaled.age + scaled.age.squared +  scaled.f.kids +
                                    scaled.num.m.adults + literate + scaled.m.kids + 
                                    number_male_kids + sex +
                                    prop_mex_migrant_dummy + prop_usa_migrant_dummy" )

# Fit the Neural Net to the full Sample for now, just to visualize
girls.net <- neuralnet::neuralnet(formula = earnings.reduced.form,
                                  data = filter(data.df, data.df$sex == 1), 
                                  hidden = c(2,2), 
                                  linear.output = TRUE, 
                                  threshold = 0.5)

plot(girls.net, information = TRUE)

summary(girls.test.df.predictions <- predict(girls.net, filter(test.df, test.df$sex == 1)))
summary(NNet.girls <- girls.test.df.predictions*sd(data.df$wages[data.df$sex == 1]) +  
          mean(data.df$wages[data.df$sex == 1]))

# For the lads: 
boys.net <- neuralnet::neuralnet(formula = earnings.reduced.form,
                                 data = filter(data.df, data.df$sex == 0), 
                                 hidden = c(2,2), 
                                 linear.output = TRUE, 
                                 threshold = 0.5)

summary(boys.test.df.predictions <- predict(boys.net, filter(test.df, test.df$sex == 0)))
summary(NNet.boys <- boys.test.df.predictions*sd(data.df$wages[data.df$sex == 0]) +  
          mean(data.df$wages[data.df$sex == 0]))


# Let's fit preliminary Heckman's for men and women too so that we can visualize 
# The differences between the predictions and raw data using these two separate approaches 

# Heckman Regression for MEn
reg.men <- selection(selection = LFP ~ age + I(age^2)  + otherincomeval_dummy + asinh(otherincomeval) + hh_kids + 
                       hh_young_kids + edu_yrs + literate + gov_transfer +
                       indigenous_language + spanish_language + head_dummy + num_f_adults + num_m_adults + #pobextre +  mpcalif  +
                       number_female_kids + number_male_kids   +  prop_mex_migrant_dummy + prop_usa_migrant_dummy  +
                       I(num_m_adults*prop_mex_migrant) +  prop_usa_migrant + prop_mex_migrant + 
                       I(num_m_adults*prop_usa_migrant) + I(num_f_adults*prop_mex_migrant) +
                       I(num_f_adults*prop_usa_migrant) +    
                       as.factor(year_wave_FE) + receive_progresa  +  # FE and Exclusion Restrictions
                       (ER + proportion_need_permission + proportion_need_accompany)*hh_young_kids,  
                     outcome = scaled.earnings ~ age + I(age^2) +  otherincomeval_dummy + asinh(otherincomeval) + hh_kids +
                       hh_young_kids + edu_yrs  + literate + gov_transfer +
                       indigenous_language + spanish_language + head_dummy + num_f_adults + num_m_adults +# pobextre +  mpcalif  +
                       number_female_kids + number_male_kids  +  prop_mex_migrant_dummy + prop_usa_migrant_dummy  +
                       I(num_m_adults*prop_mex_migrant) +  prop_usa_migrant + prop_mex_migrant + 
                       I(num_m_adults*prop_usa_migrant) + I(num_f_adults*prop_mex_migrant) +
                       I(num_f_adults*prop_usa_migrant) +    
                       as.factor(year_wave_FE) + receive_progresa,
                     data = filter(data.df, data.df$sex == 0),
                     method = "ml")

# Heckman Regression for women
reg.women <- selection(selection = LFP ~ age + I(age^2)  + otherincomeval_dummy + asinh(otherincomeval) + hh_kids + 
                         hh_young_kids + edu_yrs + literate + gov_transfer +
                         indigenous_language + spanish_language + head_dummy + num_f_adults + num_m_adults + #pobextre +  mpcalif  +
                         number_female_kids + number_male_kids   +  prop_mex_migrant_dummy + prop_usa_migrant_dummy  +
                         I(num_m_adults*prop_mex_migrant) +  prop_usa_migrant + prop_mex_migrant + 
                         I(num_m_adults*prop_usa_migrant) + I(num_f_adults*prop_mex_migrant) +
                         I(num_f_adults*prop_usa_migrant) +    
                         as.factor(year_wave_FE) + receive_progresa  +  # FE and Exclusion Restrictions
                         (ER + proportion_need_permission + proportion_need_accompany)*hh_young_kids,  
                       outcome = scaled.earnings ~ age + I(age^2) +  otherincomeval_dummy + asinh(otherincomeval) + hh_kids +
                         hh_young_kids + edu_yrs  + literate + gov_transfer +
                         indigenous_language + spanish_language + head_dummy + num_f_adults + num_m_adults +# pobextre +  mpcalif  +
                         number_female_kids + number_male_kids  +  prop_mex_migrant_dummy + prop_usa_migrant_dummy  +
                         I(num_m_adults*prop_mex_migrant) +  prop_usa_migrant + prop_mex_migrant + 
                         I(num_m_adults*prop_usa_migrant) + I(num_f_adults*prop_mex_migrant) +
                         I(num_f_adults*prop_usa_migrant) +    
                         as.factor(year_wave_FE) + receive_progresa,
                       data = filter(data.df, data.df$sex == 1),
                       method = "ml")


summary(heckman.men <- (predict(reg.men, filter(test.df, test.df$sex == 0))*
                          sd(test.df$wages[test.df$sex==0]) +  
                          mean(test.df$wages[test.df$sex==0])))

summary(heckman.women <- (predict(reg.women, filter(test.df, test.df$sex == 1))*
                            sd(test.df$wages[test.df$sex==1]) +  
                            mean(test.df$wages[test.df$sex==1])))


# Visualizing the naive NN predictions veruss the heckman predictions   
ggplot() + geom_density(mapping = aes(x = test.df$wages[test.df$sex==0]), fill = "red", alpha=0.25) +
  geom_density(mapping = aes(x = heckman.men), fill = "blue", alpha=0.25) + 
  geom_density(mapping = aes(x = NNet.boys), fill = "black", alpha=0.25) + 
  theme_bw() + labs(x= "Earnings (Red) v. Heckman Prediction (Blue) v. Neural Network (Black)", 
                    title = "Earnings Versus Predictions for Men") + xlim(0, 700)

heckman.women[heckman.women<0]<-0
ggplot() + geom_density(mapping = aes(x = data.df$wages[data.df$sex==1]), fill = "red", alpha=0.25) +
  geom_density(mapping = aes(x = heckman.women), fill = "blue", alpha=0.25) + 
  geom_density(mapping = aes(x = NNet.girls), fill = "black", alpha=0.25) + 
  theme_bw() + labs(x= "Earnings (Red) v. Heckman Prediction (Blue) v. Neural Network (Black)", 
                    title = "Earnings Versus Predictions for Women") + xlim(0,100)
