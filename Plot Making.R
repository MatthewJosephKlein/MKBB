# PLOT MAKING 

rm(list=ls())
p.df <- read.csv("Bootstrap Results.csv")
library(plotrix)

p.df$Food_Types <- c("Chic", "Milk", "Eggs", "Oran", "Carr", "Leaf.G", "Toma", "Onio", "B/P", "Bana", "Pota",                   
   "Sugar", "Beans", "Appl", "Coff", "Lime", "Tort", "Lard", "W.Bre",  "PanDul", "Rice", "W.Flo", 
   "Veg.O" , "Diges", "Cup.N", "T/S", "Soda", "C.Flo", "Fish",  "Alco", "B.Cer")

par(mfrow=c(1,1))
# Intro: making the food lists ####
LPM.ME.SIG <- c("Chic", "Oran", "Pota", "Bana", "Leaf.G", "Carr", "PanDul", "Milk", "W.Bre",
                "Appl", "B/P", "Soda", "Coff",
                "Cup.N", "Eggs", "Lime",  "Rice", "Toma", 
                "Beans")

LPM.ME.NOT <- c("Lard",  "W.Flo", "B.Cer", "Veg.O", "C.Flo", "Onio",
                "Alco", "Fish", "Diges", "Tort", "Sugar", "T/S")

LPM.order <- c(LPM.ME.SIG, LPM.ME.NOT)

Food_Types <- p.df$Food_Types
rownames(p.df) <- p.df$Food_Types

food_type_organized <- c("Oran", "Bana", "Pota", "Leaf.G", "Carr", "Appl", "Toma", "Lime",  "Onio",
                         "Chic", "Milk", "B/P", "Eggs", "Lard",  "Fish", "T/S", 
                         "W.Bre",  "Cup.N", "W.Flo", "PanDul", "Rice",  "Beans", "B.Cer", "Tort",   "Diges",  "C.Flo",
                         "Soda", "Coff",  "Alco",  "Veg.O",  "Sugar" )

food_type_ev_org <- c("Oran",  "Soda",  "Carr",  "Beans", "Coff", "Tort", "Diges", 
                      "Cup.N", "Leaf.G", "Pota", "Eggs", "Milk", "B/P", "Appl",  "Rice", "C.Flo",
                       "Bana",  "Lime", "Sugar", "Veg.O", "Toma", "Onio", "Chic",  "Fish", "T/S", "Lard", "W.Bre", "PanDul", "W.Flo", "B.Cer", "Alco")

# The LPM.ME graph ####

LPM.ME.Vals <- p.df$LPM.ME.Estimate
lower <- p.df$LPM.ME.Low
upper <- p.df$LPM.ME.High
LPM.m <- as.data.frame(list(LPM.ME.Vals, lower, upper, p.df$Food_Types)) 
colnames(LPM.m) <- c("Val", "lower", "upper", "Food_Types")
rownames(LPM.m) <- Food_Types

LPM.organized <- LPM.m[LPM.order,]

# Decending Order
plotCI(1:31,LPM.organized$Val,li = LPM.organized$lower,ui = LPM.organized$upper,lwd=2,col="red",scol="blue", xlab = '', ylab = '', xaxt = "n",
       yaxt = "n",
       main="LPM Marginal Effects and Bootstrap Confiudence Intervals")
axis(side = 1, labels = LPM.order, at = seq(1, 31, 1), las = 2)
axis(side = 2, labels = seq(-2, 2, .2), at = seq(-2, 2, .2), las = 2)
abline(b=0,a=0)
abline(v=20.5)

LPM.organized <- LPM.m[food_type_organized,] 

#Organized by Food Groups
plotCI(1:31,LPM.organized$Val,li = LPM.organized$lower,ui = LPM.organized$upper,lwd=2,col="red",scol="black", xlab = '', ylab = '', xaxt = "n",
       yaxt = "n",
       main="LPM Marginal Effects and Bootstrap Confidence Intervals")
axis(side = 1, labels = food_type_organized, at = seq(1, 31, 1), las = 2)
axis(side = 2, labels = seq(-2, 2, .2), at = seq(-2, 2, .2), las = 2)
abline(b=0,a=0)
abline(v=9.5)
abline(v=16.5)
abline(v=26.5)

# The Impact Graph ####
#impact <- c(5.89,4.37, 3.33, 3.23, 3.14, 2.76, 2.76,
#            2.57, 2.47, 2.28, 2.19, 2.19, 1.90, 1.81, 1.71, 
#            1.43, 1.33, 1.24, 0.76, 0.67, 0.48, 0.48, 0.29)  
  
#plot(x = seq(1,23,1), y = impact, xaxt = "n", yaxt = "n", ylab = '', xlab = '')
#title(main = "IMPACT", adj = .5, line = -2)
#axis(side = 1, labels = LPM.ME.SIG, at = seq(1, 23, 1), las = 2)
#axis(side = 2, labels = c(1,2,3,4,5,6), las = 2, at = c(1,2,3,4,5,6))


# Poisson Graphs #### 
P.ME.Vals <- p.df$Poisson.ME
lower.p <- p.df$Poisson.ME.Low
upper.p <-  p.df$Poisson.ME.High 
P.m <- as.data.frame(list(P.ME.Vals, lower.p, upper.p)) 
rownames(P.m) <- Food_Types
colnames(P.m) <- c("Val", "lower", "upper")

P.organized <- P.m[food_type_organized,]

# Decending Order
plotCI(1:31,P.m$Val,li = P.m$lower,ui = P.m$upper,lwd=2,col="red",scol="blue", xlab = '', ylab = '', xaxt = "n",
       yaxt = "n",
       main="Poisson Marginal Effects and Bootstrap Confidence Intervals")
axis(side = 1, labels = Food_Types, at = seq(1, 31, 1), las = 2)
axis(side = 2, labels = seq(-2, 5, .5), at = seq(-2, 5, .5), las = 2)
abline(b=0,a=0)
abline(v=23.5)

#Organized by Food Groups
plotCI(1:31,P.organized$Val,li = P.organized$lower,ui = P.organized$upper,lwd=2,col="red",scol="black", xlab = '', ylab = '', xaxt = "n",
       yaxt = "n",
       main="Poisson Marginal Effects and Bootstrap Confidence Intervals")
axis(side = 1, labels = food_type_organized, at = seq(1, 31, 1), las = 2)
axis(side = 2, labels = seq(-2, 7, .5), at = seq(-2, 7, .5), las = 2)
abline(b=0,a=0)
abline(v=9.5)
abline(v=16.5)
abline(v=26.5)


# Overlapping P and LPM Graphs: DOESNT LOOK GOOD #### 

#plotCI(1:34,P.m$Val,li = P.m$lower,ui = P.m$upper,lwd=2,col="red",scol="blue", xlab = '', ylab = '', xaxt = "n",
#       yaxt = "n",
#       main="Poisson Marginal Effects and Bootstrap CI's")
#plotCI(1:34,LPM.ME.Vals,li = lower,ui = upper,lwd=2,col="blue",scol="orange", xlab = '', ylab = '', xaxt = "n",
#       yaxt = "n", add = TRUE,
#       main="LPM Marginal Effects and Bootstrap CI's")
#axis(side = 1, labels = Food_Types, at = seq(1, 34, 1), las = 2)
#axis(side = 2, labels = seq(-2, 5, .5), at = seq(-2, 5, .5), las = 2)
#abline(b=0,a=0)
#abline(v=23.5)



# Hypothesis 3 graph: mu squared coef and SE #### 
lower.Mu <- p.df$LPM.BP2.ME.LOW
upper.Mu <-   p.df$LPM.BP2.ME.High
Mu.ME.Vals <- p.df$LPM.BP2.Point.Estimate
Mu.m <- as.data.frame(list(Mu.ME.Vals, lower.Mu, upper.Mu)) 
rownames(Mu.m) <- Food_Types
colnames(Mu.m) <- c("Mu.ME.Vals" , "lower", "upper")
Mu.organized <- Mu.m[LPM.order,]

# Decending Order
#plotCI(1:34,Mu.m$Val,li = Mu.m$lower,ui = Mu.m$upper,lwd=2,col="orange",scol="blue", xlab = '', ylab = '', xaxt = "n",
#       yaxt = "n",
#       main="Power Squared Coefficient Estimates and Bootstrap Confidence Intervals")
#axis(side = 1, labels = Food_Types, at = seq(1, 34, 1), las = 2)
#axis(side = 2, labels = seq(-4, 5, .5), at = seq(-4, 5, .5), las = 2)
#abline(b=0,a=0)
#abline(v=23.5)

#Organized by Food Groups
plotCI(1:31,Mu.organized$Mu.ME.Vals,li = Mu.organized$lower,ui = Mu.organized$upper,lwd=2,col="red",scol="black", xlab = '', ylab = '', xaxt = "n",
       yaxt = "n",
       main="Power Squared Estimate and Bootstrap Confidence Intervals")
axis(side = 1, labels = alt.order, at = seq(1, 31, 1), las = 2)
axis(side = 2, labels = seq(-7, 5, 1), at = seq(-7, 5, 1), las = 2)
abline(b=0,a=0)
abline(v=13.5)

# Hypothesis 4 graph, two sets of CI's #### 
lower.high <- p.df$LPM.high.inc.LOW
upper.high <-  p.df$LPM.high.inc.high

lower.low <- p.df$LPM.low.inc.LOW
upper.low <- p.df$LPM.low.inc.high

Mu.m <- as.data.frame(list(lower.low, upper.low, lower.high, upper.high)) 
rownames(Mu.m) <- Food_Types
colnames(Mu.m) <- c("lower.low", "upper.low", "lower.high", "upper.high")

food_inc <- c("Milk", "Oran", "Coff", 
              "Carr", "Leaf.G", "Bana", "Beans", "Tort", "W.Bre", "Rice", "Diges", "Soda", "Pota", "Cup.N")

Mu.organized <- Mu.m[food_inc,]


plotCI(1:14, y = (Mu.organized$lower.low+ Mu.organized$upper.low)/2,
       li = Mu.organized$lower.low, ui = Mu.organized$upper.low,
       lwd=2,col="blue",scol="blue", xlab = '', ylab = '', xaxt = "n", yaxt = "n",
       main="High and Low Income Terciles' Confidence Intervals", pch = ".")

plotCI(1:14, y = (Mu.organized$lower.high + Mu.organized$upper.high)/2,
       li = Mu.organized$lower.high,ui = Mu.organized$upper.high,lwd=2,col="orange",scol="orange",
       xlab = '', ylab = '', xaxt = "n", yaxt = "n", add = T, pch = ".")
axis(side = 1, labels = food_inc, at = seq(1, 14, 1), las = 2)
axis(side = 2, labels = seq(-2, 5, .5), at = seq(-2, 5, .5), las s= 2)
abline(b=0,a=0)
abline(v = 3.5)
legend(12.5, 2.5, c("High", "Low"), col = c("orange", "blue"), lty = c(1, 1))



#boxplot(x = t(Mu.organized[,1:2]))
#boxplot(x = t(Mu.organized[,3:4]), add = TRUE)        
        


stargazer::stargazer(p.df[food_type_organized,1:7], summary = FALSE)


stargazer::stargazer(p.df[food_type_organized,c(1,8:13)], summary = FALSE)

