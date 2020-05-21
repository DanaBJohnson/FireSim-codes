# --------------------
# Title: Decay.models.plus.loop.R
# Author: TW modified by DBJ
# Date: 2020-May-13
#
# Purpose: Script for fitting two-pool models to exponential decay data
#           + loops

# --------------------
# Install package
# install.packages("minpack.lm")

# Load package
library(minpack.lm)
library(dplyr)
library(ggplot2)
library(tidyverse)
# --------------------

setwd("C:/Users/danab/Box Sync/WhitmanLab/Projects/WoodBuffalo/FireSim2019/data")


### Running model on our own data ##

### Two-pool model ###
# Mt = M1*e^(-k1*t)+M2*e^(-k2*t)  The basic two-pool model
# Mt = 1 = M1+M2    Setting Mt as total initial C to 1, M1+M2 make up total
# Mt = M1*e^(-k1*t)+(1-M1)*e^(-k2*t)    Rearranging equation
# Mt = M1*e^(-k1*t)-M1*e^(-k2*t)+e^(-k2*t)  Rearranging equation

# Describe function form
TwoPoolDecay = function(params,t) params$M1*(exp(-params$k1*t)-exp(-params$k2*t))+exp(-params$k2*t)

## Define residual function (our actual value minus the exponential function)
residualsFun.twopool = function(p, Mt, t) Mt - TwoPoolDecay(p, t)

# List start parameters - can set these however
# Will be important to make sure this decision is not too influential
StartPar.TwoPool = list(k1=0.00001,k2=0.00001,M1=0.9)




# OBJECTIVE: set up input and output files for loop
df <- read.csv("fractional-c-remaining.csv", row.names = 1)
head(df)

# Create a vector of Core IDs
names.df <- df %>%
  subset(DayFactor == 3) %>%
  subset(select = c(Core.ID.full))

# Convert df to vector
names.vec <- names.df[,"Core.ID.full"]

# Check if this worked. 
class(names.vec)
names.vec[1]
names.vec[14]



# --------------------
# Create an output df to populate with coefficients later
coefs.df <- data.frame("Core.ID.full" = "sample", "k1" = 0, "k2" = 0, "M1" = 0, "M2" = 0)
coefs.df

output.fit.df <- data.frame("Core.ID" = "sample", 
                            "Core.ID.full" = "sample", 
                            "Site" = 0,
                            "Burn.trtmt" = "trtmt",
                            "t" = 0, 
                            "Mt" = 0, "Mt.fit" = 0)

# Run the decay.models.R script in a for loop. 
for (i in seq_along(names.vec)) {

  df.x <- df %>%
    rename(t = DayFactor, Mt = fractional.C.remaining) %>%
    subset(Core.ID.full == names.vec[[i]])
  
  TwoPool.nls.fit <- nls.lm(par=StartPar.TwoPool, fn = residualsFun.twopool, 
                            Mt = df.x$Mt, t = df.x$t, 
                            lower = c(0,0,0), upper = c(Inf, Inf, 1),
                            control = nls.lm.control(nprint=1,maxiter=500,ftol=sqrt(.Machine$double.eps)/10))
  
  df.x$Mt.fit <- TwoPoolDecay(as.list(coef(TwoPool.nls.fit)),df.x$t)
  
  ## summary information on parameter estimates
  coefs <- summary(TwoPool.nls.fit) 
  k1 <- coefs$coefficients[[1]]
  k2 <- coefs$coefficients[[2]]
  M1 <- coefs$coefficients[[3]]
  M2 <- df.x$Mt[1] - M1

  coefs.df <- coefs.df %>%
    add_row("Core.ID.full" = names.vec[[i]], "k1" = k1, "k2" = k2, "M1" = M1, "M2" = M2)
  coefs.df
  
  output.fit.df <- rbind(output.fit.df, df.x)
   
}




# --------------------
# Clean up output files
coefs.df <- coefs.df %>%
  subset(Core.ID.full != "sample")

coefs.df <- coefs.df %>%
  separate(Core.ID.full, c("Year", "Project", "Site", "Core", "Incubation.trtmt"), sep = "-") %>%
  unite(Core.ID.full, c(Year, Project, Site, Core, Incubation.trtmt), sep = "-", remove = FALSE)
head(coefs.df)



output.fit.df <- output.fit.df %>%
  subset(Core.ID.full != "sample")

output.fit.df <- output.fit.df %>%
  separate(Core.ID.full, c("Year", "Project", "Site", "Core", "Incubation.trtmt"), sep = "-") %>%
  unite(Core.ID.full, c(Core.ID, Incubation.trtmt), sep = "-", remove = FALSE)
head(output.fit.df)


# Look at the coefficient dataframe
head(coefs.df)
head(output.fit.df)

# Check the coefficient minimums
min(coefs.df$k1)
min(coefs.df$k2)
min(coefs.df$M1)
min(coefs.df$M2)


write.csv(coefs.df, "R_play_data/two-pool-exponential-decay-models/decay-model-coefficients.csv")
write.csv(output.fit.df, "R_play_data/two-pool-exponential-decay-models/decay-model-fit.csv")
# Starting parameters:

# error: (k1=0.001,k2=0.001,M1=0.9)

# success:  (k1=0.00001,k2=0.0001,M1=0.9) -> looks like a one pool model
#           (k1=0.00001,k2=0.0001,M1=0.8) -> looks like a one pool model
#           (k1=0.00001,k2=0.001,M1=0.8) 






#--------------------
head(coefs.df)
head(output.fit.df)

full <- merge(output.fit.df, coefs.df, by = "Core.ID.full")
head(full)

# Create "Incubation.trtmt" column
output.fit.df <- output.fit.df %>%
  separate(Core.ID.full, c("Year", "Project", "Site", "Core", "Incubation.trtmt"), sep = "-") %>%
  unite(Core.ID.full, c(Core.ID, Incubation.trtmt), sep = "-", remove = FALSE)


Site.thirteen <- subset(output.fit.df, Site == 13)
Site.thirteen

# Separate long-term (LIwA) incubation treatments
liwa.inc <- subset(output.fit.df, Incubation.trtmt == "LIwA")
liwa.inc

# Separate long-term (LIn) incubation treatments
lin.inc <- subset(output.fit.df, Incubation.trtmt == "LIn")
lin.inc

# And short-term incubation treatments
st.inc <- subset(output.fit.df, Incubation.trtmt == "SI")
st.inc




#PLOT: Fractional C remaining at one site (13).
p = ggplot(Site.thirteen, aes(x = t, y = Mt, color = Burn.trtmt)) + 
  geom_point() +
  geom_line(aes(x=t, y = Mt.fit)) +
  facet_wrap(~Core.ID.full) +
  labs(x = "Day", 
       y = "C remaining as fraction of initial total C",
       title = "Fractional C remaining during short-term incubation treatments")
p


#PLOT: Fractional C remaining during short-term incubation treatments across all sites
p = ggplot(st.inc, aes(x = t, y = Mt, color = Burn.trtmt)) + 
  geom_point() +
  geom_line(aes(x=t, y = Mt.fit)) +
  facet_wrap(~Site) +
  labs(x = "Day", 
       y = "C remaining as fraction of initial total C",
       title = "Fractional C remaining during short-term incubation treatments")
p


#PLOT: Fractional C remaining during long-term (LIwA) incubation treatments across all sites
p = ggplot(liwa.inc, aes(x = t, y = Mt, color = Burn.trtmt)) + 
  geom_point() +
  geom_line(aes(x=t, y = Mt.fit)) +
  facet_wrap(~Site) +
  labs(x = "Day", 
       y = "C remaining as fraction of initial total C",
       title = "Fractional C remaining during long-term (LIwA) incubation treatments")
p

#PLOT: Fractional C remaining during long-term (LIn) incubation treatments across all sites
p = ggplot(lin.inc, aes(x = t, y = Mt, color = Burn.trtmt)) + 
  geom_point() +
  geom_line(aes(x=t, y = Mt.fit)) +
  facet_wrap(~Site) +
  labs(x = "Day", 
       y = "C remaining as fraction of initial total C",
       title = "Fractional C remaining during long-term (LIn) incubation treatments")
p










