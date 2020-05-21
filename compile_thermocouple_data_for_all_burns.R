# --------------------
# 
# Title: compile_thermocouple_data_all_burns
# Author: Dana Johnson
# Date: 2020-Feb-03
#
# Purpose: This is code to compile all thermocouple data from simulated burns at FPL
# into a single data file "Temp-all-burns"
#
# --------------------

setwd("C:/Users/danab/Box Sync/WhitmanLab/Projects/WoodBuffalo/FireSim2019/data/FireSim2019_Temp/")

# Import packages
# library(dplyr)
# library(ggplot2)

# library(plyr)
# library(readr)

library(tidyverse)
library(stringr)

## Import all .csv files from location (mydir) 
myfiles = list.files(pattern="*.csv", full.names=TRUE)


# Combine all the raw temp .csv files into one (df) and add column with site ID
df <- myfiles %>%
  setNames(nm = .) %>%
  map_df(~read_csv(.x,col_types = cols(), col_names = FALSE), .id = "File_name")

# Rename colums
col.names <- c('Site_ID','time_s','Thermo_mid','Thermo_low')
names(df) <- col.names

class(df$time_s)

# Removing rows in column time_s 
df <- subset(df, time_s != 'time_s')

df$time_s = as.numeric(df$time_s)

# Use only first couple hours of burn time
df <- subset(df, time_s < 18000)

# Get rid of lower temperatures
df <- subset(df, Thermo_mid >20 | Thermo_low >20)

# export new .csv file with compiled temp data
write.csv(df,"C:/Users/danab/Box Sync/WhitmanLab/Projects/WoodBuffalo/WB2019/data/FireSim2019_Temp\\Temp-all-burns.csv")



