# --------------------
# Title: incubations-microbial-respiration.R
# Author: Dana Johnson
# Date: 2020-Nov-21
#
# Purpose: This is code to plot microbial respiration data from short and long term
# incubation of FireSim2019 cores. Similar to ec-alldays.R 
#
# Output: p1 <- day-vs-respiration-rate-SI.png
#         p2 <- day-vs-respiration-rate-LI.png

# --------------------
# Load packages
library(dplyr)
library(ggplot2)
library(tidyr)
# --------------------
setwd("C:/Users/danab/Box Sync/WhitmanLab/Projects/WoodBuffalo/FireSim2019/data")

# Import csv
df = read.csv("raw-data/compiled-conductivity-data.csv")

# Look at top of data
head(df)
tail(df)

?unite
# Create a master ID tag combining core ID and incub treatment
df <- df %>%
  unite(Core.ID.full, c(ID, Incubation.trtmt), sep = "-", remove = FALSE)

# Recorder columns so Core.ID is first
df <- df[,c(2,1,3:18)]
head(df)
# ----------------
# OBJECTIVE: Calculate CO2 absorbed by KOH trap. 
# Make new column that is difference between ref and measurement
# Group by measurement/setup date
# Make new column where we get the reference blank value (mean of all blanks for that date)
# Make new column where we subtract the relevant blank mean for that date


# Calculate CO2 absorbtion in mg using [CO2 absorbed = P*V*C*R*M]
# P = (Change in conductivity)/(Conductivity at starting concentration - conductivity at 1/2 of starting concentration)
# V = Volume (mL) of KOH trap
# C = Concentration of KOH solution
# R = Ratio
# M = Molecular weight
V = 0.015 # L
C = 0.5   # mol/L
R = 0.5
M = 12.01 # g/mol

# Calculate C-CO2 absorbed by KOH trap.
# Adjust for C-CO2 absorbed minus blank.
# Normalize C-CO2 absorbtion by gram of initial dry soil
df = df %>%
  rename(Core.ID = ID) %>% 
  mutate(P = (Initial.cond.mS.cm-Final.cond.mS.cm)/(Initial.cond.mS.cm-Half.M.mS.cm)) %>%
  mutate(C.CO2.absorbed.g = P*V*C*R*M) %>%
  mutate(Blank.C.CO2.absorbed.g = mean(C.CO2.absorbed.g[Incubation.trtmt=="Blank"])) %>%
  mutate(C.CO2.absorbed.minus.blank.g = C.CO2.absorbed.g - Blank.C.CO2.absorbed.g) %>%
  mutate(C.CO2.absorbed.per.gram = C.CO2.absorbed.minus.blank.g/Dry.mass.g)

# Remove the rows for the Blanks (no longer needed)
df = df %>%
    filter(Incubation.trtmt != "Blank")

# Make Site a factor instead of a continuous numeric value
df$Site = as.factor(df$Site)
levels(df$Site)

# Make Day a factor -> "DayFactor"
class(df$Day)
df$DayFactor = as.factor(df$Day)
levels(df$DayFactor)
 
# Display burn treatments in set order: Control, Wet, Dry
df$Burn.trtmt = ordered(df$Burn.trtmt,levels=c("control","wet burn","dry burn"))

# Removing sample that fell on floor post burn treatment
df <- subset(df, Core.ID != "19UW-WB-11-02")
 
head(df)


### Calculate cumulative C-CO2 respired over the course of the incubation
df <- df %>%
  group_by(Core.ID.full) %>%
  arrange(Day, .by_group = TRUE) %>%
  mutate(C.CO2.absorbed.per.gram.cum = cumsum(C.CO2.absorbed.per.gram))
# cusum - order of df is important!


df <- df %>%
  group_by(Core.ID.full)%>%
  mutate(C.CO2.absorbed.per.gram.per.day = C.CO2.absorbed.per.gram/KOH.trap.time.period)

?group_by
?cumsum

head(df)




# ---------------
# OBJECTIVE: Calculate fractional C remaining. So far, the df contains C respired (= CO2.absorbed.minus.blank)
# Later will add in measured initial total C (from Jackson lab EA measurements)
colnames(df)

fr.df <- subset(df, select = c(Core.ID,
                               Core.ID.full,
                               Site,
                               Core,
                               Burn.trtmt,
                               DayFactor,
                               Dry.mass.g,
                               C.CO2.absorbed.minus.blank.g,
                               C.CO2.absorbed.per.gram,
                               C.CO2.absorbed.per.gram.cum))

head(fr.df)
head(fr.df$Core.ID)
tail(fr.df)




# Create a dataframe for total initial C in control cores.


# Import data - thickness of soil horizons from incubations
totC.df <- read.csv("raw-data/incubation-deconstruction-measurements.csv")
head(totC.df)
# This dataset doesn't have the incubation treatments or burn treatments in it. I'll add them. 

# To calculate initial C, I'm using carbon estimates per horizon (from Blonsk and Lasota, 2017)
org.horizon.total.g.C.per.g.soil = 250/1000
mnrl.horizon.total.g.C.per.g.soil = 35/1000

# Calculate total C per horizon
totC.df <- totC.df %>%
  mutate(initial.C.org.horizon.g = Mass.O.horizon.soil.recovered.g * org.horizon.total.g.C.per.g.soil) %>%
  mutate(initial.C.mnrl.horizon.g = Mass.mineral.horizon.soil.recovered.g * mnrl.horizon.total.g.C.per.g.soil) %>%
  mutate(initial.C.total.g = initial.C.org.horizon.g + initial.C.mnrl.horizon.g) %>%
  rename(Core.ID = ï..Sample.ID) %>%
  separate(Core.ID, c("Year", "Project", "Site", "Core", "Incubation.trtmt"), sep = "-") %>%
  unite(Core.ID, c(Year, Project, Site, Core), sep = "-", remove = FALSE) %>%
  unite(Core.ID.full, c(Year, Project, Site, Core, Incubation.trtmt), sep = "-", remove = FALSE)

# Create subset of df that just includes Core.ID and treatment data.
colnames(df)
trtmt.df <- df %>%
  subset(Day == "3") %>%
  subset(select = c(Core.ID.full, Burn.trtmt))

# Merge this new dataframe with totC.df to bring in Burn.trtmt data.
colnames(totC.df)
colnames(trtmt.df)

totC.df <- merge(trtmt.df, totC.df, by = "Core.ID.full")
colnames(totC.df)
head(totC.df)

# Pull out just the control sample from each site. 
totC.df <- totC.df %>%
  subset(Burn.trtmt == "control") %>%
  subset(Incubation.trtmt == "LIn")
head(totC.df)
colnames(totC.df)

# Pull out only the necessary columns
totC.df <- subset(totC.df, select = c(Site,
                                      Incubation.trtmt,
                                      initial.C.org.horizon.g,
                                      initial.C.mnrl.horizon.g,
                                      initial.C.total.g))

# Combine this with C loss via microbial respiration data
colnames(totC.df)
colnames(fr.df)



# "Site" will need to match
class(fr.df$Site)
class(totC.df$Site)
totC.df$Site <- as.factor(totC.df$Site)

totC.df$Site <- as.numeric(totC.df$Site)
fr.df$Site <- as.numeric(fr.df$Site)

# Combine the dataframes to bring in the total initial C values.
fr.df <- merge(fr.df, totC.df, by = "Site")
head(fr.df)

# Calculate the fractional C remaining
fr.df <- fr.df %>%
  mutate(fractional.C.remaining = (initial.C.total.g - C.CO2.absorbed.per.gram.cum)/initial.C.total.g)
colnames(fr.df)

# Format for saving
X <- subset(fr.df, select = c(Core.ID,
                              Core.ID.full,
                              Site,
                              Burn.trtmt,
                              DayFactor,
                              fractional.C.remaining))
write.csv(X, "fractional-c-remaining.csv")




# ---------------
# PLOTS:
colnames(df)
 
df <- df %>%
  mutate(C.CO2.absorbed.per.mg.per.day = C.CO2.absorbed.per.gram.per.day * 1000) %>%
  mutate(C.CO2.absorbed.per.mg.cum = C.CO2.absorbed.per.gram.cum)
  
# Make Site numeric
colnames(df)
class(df$Site)
df$Site <- as.numeric(df$Site)

# Create an SI dataframe
df_SI<- df %>%
  subset(Incubation.trtmt =="SI")

df_LI <- df %>%
  subset(Incubation.trtmt != "SI")

# Making a vector with 3 colours
palette = c("black","orange","red3")


# Rate of microbial respiration by day - short-term incubations
p1 = ggplot(df_SI, aes(x = Day, 
                   y = C.CO2.absorbed.per.mg.per.day, 
                   color = Burn.trtmt, 
                   group = Core.ID.full)) +
  geom_point() +
  geom_line(aes()) +
  scale_color_manual(values = palette) +
  facet_wrap(~Site, scale = "free") +
  labs(x = "Day",
       y = expression(Microbial~respiration~rate~(g~C-CO[2]~g^{"-1"}~day^{"-1"})),
       title = "Microbial respiration during short-term incubation",
       color = "Burn treatment") +
  theme(axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        title = element_text(size = 18))
p1
#ggsave("../figures/day-vs-respiration-rate-SI.png", plot = p1, width = 12, height = 10)




# Rate of microbial respiration by day - LONG-term incubations
p2 = ggplot(df_LI, aes(x = Day, 
                      y = C.CO2.absorbed.per.gram.per.day, 
                      group = Core.ID.full,
                      color = Burn.trtmt)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = palette) +
  facet_wrap(~Site, scale = "free") +
  labs(x = "Day",
       y = expression(Microbial~respiration~rate~(g~CO[2]~g^{"-1"}~day^{"-1"})),
       title = "Microbial respiration during long-term incubation",
       color = "Burn treatment") +
  theme(axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        title = element_text(size = 18))
p2
#ggsave("../figures/day-vs-respiration-rate-LI.png", plot = p2, width = 12, height = 10)














# Create ggplot object (p) from dataframe, with burn treatment as x and EC response as y,
# coloured by burn treatment
p = ggplot(df,aes(x=Burn.trtmt,y=C.CO2.absorbed,color=DayFactor))
# Add boxplot, with x-y point graph overlaid (jittered to imrpove visibility of each point)
p = p +geom_boxplot() # + geom_jitter()
# Plot by different incubation treatments
p = p + facet_wrap(~Incubation.trtmt)
# Adjust appearance
p = p + theme_bw()
# Fix axis labels
p = p + ylab("Microbial respiration (mg CO2)")+ xlab("Burn treatment")
# Fix legend label
p = p + labs(color="Day of Measurement")
# Display plot
p


### Normalizing by gram
# Create ggplot object (p) from dataframe, with burn treatment as x and EC response as y,
# coloured by burn treatment
p = ggplot(df,aes(x=Burn.trtmt,y=CO2.absorbed.per.gram,color=DayFactor))
# Add boxplot, with x-y point graph overlaid (jittered to imrpove visibility of each point)
p = p + geom_boxplot() + geom_jitter()
# Plot by different incubation treatments
p = p + facet_wrap(~Incubation.trtmt)
# Adjust appearance
p = p + theme_bw()
# Fix axis labels
p = p + ylab("Microbial respiration (mg CO2 g-1)")+ xlab("Burn treatment")
# Fix legend label
p = p + labs(color="Burn treatment")
# Display plot
p


colnames(df)

### Plotting normalized data by each day
# Create ggplot object (p) from dataframe, with burn treatment as x and EC response as y,
# coloured by burn treatment
p = ggplot(df,aes(x=DayFactor,y=C.CO2.absorbed.per.gram,color=Burn.trtmt))
# Add boxplot, with x-y point graph overlaid (jittered to imrpove visibility of each point)
#p = p + geom_point() + geom_line()
p = p + geom_jitter() #+ geom_boxplot()  
# Plot by different incubation treatments
p = p + facet_wrap(~Incubation.trtmt)
# Adjust appearance
p = p + theme_bw()
# Fix axis labels
p = p + ylab("Microbial respiration (mg CO2 g-1)")+ xlab("Burn treatment")
# Display plot
p

###### What if we just look at cumulative respiration for each sample?



# Plotting
p = ggplot(df,aes(x=Burn.trtmt,y=CO2.absorbed.per.gram.cum,color=Burn.trtmt))
# Add boxplot, with x-y point graph overlaid (jittered to imrpove visibility of each point)
p = p + geom_boxplot() #+ geom_jitter()
# Plot by different incubation treatments
p = p + facet_wrap(~Incubation.trtmt)
# Adjust appearance
p = p + theme_bw()
# Fix axis labels
p = p + ylab("Microbial respiration (mg CO2 g-1)")+ xlab("Burn treatment")
# Fix legend label
p = p + labs(color="Burn treatment")
# Making a vector with 3 colours
palette = c("gold3","red3","orange")
# Setting that vector to provide the colours in the graph
p = p + scale_color_manual(values=palette)
# Display plot
p


### Plotting cumulative normalized data by each day
# Create ggplot object (p) from dataframe, with burn treatment as x and EC response as y,
# coloured by burn treatment
p = ggplot(df,aes(x=Day,y=C.CO2.absorbed.per.gram.cum,color=Site,group=Core.ID))
# Add boxplot, with x-y point graph overlaid (jittered to imrpove visibility of each point)
p = p + geom_point() + geom_line()
# Plot by different incubation treatments
p = p + facet_grid(~Incubation.trtmt~Burn.trtmt)
# Adjust appearance
p = p + theme_bw()
# Labels
p = p + ylab("Cumulative respiration (mg CO2 g-1 dry soil)")+ xlab("Day")
# Display plot
p


colnames(df)
class(df$Site)
df$Site <- as.numeric(df$Site)
df$Site
org.df <- subset(df, Site == c(11, 12, 7, 14, 17, 4, 15, 8, 13))
p.banks <- subset(df, Site == c(1,3,9,10,16,19))
p.glau <- subset(df, Site == c(4,6,7,12,13,17,18))
p.trem <- subset(df, Site == c(2,5,8,11,14,15))
p.trem$Site

### Looking at each site
# Create ggplot object (p) from dataframe, with burn treatment as x and EC response as y,
# coloured by burn treatment
p = ggplot(p.glau,aes(x=Day,y=C.CO2.absorbed.per.gram.cum,color=Burn.trtmt,shape=Burn.trtmt))
# Add boxplot, with x-y point graph overlaid (jittered to imrpove visibility of each point)
p = p + geom_point() + geom_line(aes(linetype=Incubation.trtmt)) + scale_linetype_manual(values=c("solid","solid","solid"), guide=FALSE)
# Plot by different incubation treatments
p = p + facet_wrap(~Site,scales="free")
# Adjust appearance
p = p + theme_bw()
# Fix axis labels
p = p + ylab("Cumulative microbial respiration (mg CO2-C g-1 dry soil)")+ xlab("Day")
p = p +ylim(0,200)
# Adjust Legend
p = p + scale_color_discrete(name = "Burn Treatment: ", breaks=c("control", "wet burn", "dry burn"), labels = c("Control","Low Severity Burn","High Severity Burn"))
p = p + theme(legend.position = "bottom")

# Making a vector with 3 colours
palette = c("black","orange","red3")
# Setting that vector to provide the colours in the graph
p = p + scale_color_manual(values=palette)
p = p + scale_shape_discrete(breaks=c("control","wet burn","dry burn"))

# Remove Incubation legend or I could just remove Incubation shapes
#p = p + scale_linetype(guide=FALSE)
# Resize graph text
p = p + theme(axis.title.x = element_text(size = 14))
p = p + theme(axis.title.y = element_text(size = 14))
p = p + theme(legend.text = element_text(size=14))
p = p + theme(legend.title = element_text(size=14))
  
# Display plot
p

