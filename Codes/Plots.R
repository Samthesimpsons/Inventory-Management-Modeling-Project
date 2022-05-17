library(tidyverse)
library(extrafont)
library(truncdist)
font_import()
loadfonts(device = "win")
windowsFonts(A = windowsFont("Times New Roman"))

data <- read_csv("Calculations.csv")
bw <- function(x){2 * IQR(x) / length(x)^(1/3)}

Broccolihistogram <- ggplot(data) +
  geom_histogram(aes(Broccoli,y = ..density..),fill="white",  binwidth = bw,color = "black")+
  geom_density(aes(Broccoli),fill="blue", alpha = .2) +
  geom_vline(xintercept = 2.0, linetype= "dashed", color = "red", size=2) +
  labs(title = "Histogram and Density Plot of Broccoli Demand",x = "Broccoli Demand/Packets", y = "Counts")+
  theme(text=element_text(size=16,  family="Times New Roman"))
Broccolihistogram

Potatohistogram <- ggplot(data) + 
  geom_histogram(aes(Potato,y = ..density..),fill="white", bins=15,color = "black")+
  geom_density(aes(Potato),fill="blue", alpha = .2) +
  geom_vline(xintercept = 180, linetype= "dashed", color = "red", size=2) +
  labs(title = "Histogram and Density Plot of Potato Demand",x = "Potato Demand/Grams", y = "Counts")+
  theme(text=element_text(size=16,  family="Times New Roman"))
Potatohistogram

qqtrunc( data$Broccoli, spec="norm", a=0,mean=2, sd = 5, title="Q-Q Plot of Broccoli",
         xlabel="Truncated Normal Theoretical Values", ylabel="Broccoli Sample Values")

qqtrunc( data$Potato, spec="norm", a=0,mean=-2000, sd = 2600, title="Q-Q Plot of Potato",
         xlabel="Truncated Normal Theoretical Values", ylabel="Potato Sample Values")

# qqplot(x=qgamma(ppoints(131),rate=1/4.509924, shape = 8), y=data$Broccoli, main="Q-Q Plot of Broccoli",
#        xlab="Exponential Theoretical Values", ylab="Broccoli Sample Values", family = "A",
#        cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
# qqline(data$Broccoli, distribution = function(p){qgamma(p, rate =1/4.509924,shape = 8)},col="red")
# 
# qqplot(x=qexp(ppoints(131),rate=1/1361.450382), y=data$Potato, main="Q-Q Plot of Potato",
#         xlab="Exponential Theoretical Values", ylab="Potato Sample Values", family = "A",
#         cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
# qqline(data$Potato, distribution = function(p){qexp(p,rate=1/1361.450382)},col="red")

