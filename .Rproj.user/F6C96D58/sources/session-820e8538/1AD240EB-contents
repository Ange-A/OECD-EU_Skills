################################################################################
# RSHINY ASSIGNMENT 
# Ange Christelle Aduhire, Pauline Chaland 
# MSc DSOB 2023
################################################################################

library(rsconnect)
library(tidyverse)
library(ggplot2)
library(ggcorrplot)
library(corrplot)
library(plotly)
library(dplyr) 
library(shiny)
library(reshape2)



load("OECD skill job level 1.rdata")
load("OECD skill job level 2.rdata")

#========================================
#       Context
#========================================

# You work as a data analyst at The Directorate for Employment,Labor and Social 
# Affairs (ELS ) at the OECD. Your chief will attend a meeting next week where 
# she is going to talk about skills imbalance on the labor market in OECD
# countries.
# Your chief asks you to prepare a dashboard allowing to analyze skill imbalance
# on the labor market in OECD countries.

  #----------------------------------------
  # Def Skill imbalances 
  #----------------------------------------

# A skills imbalance is a misalignment between the demand and supply of skills 
# in an economy, and can involve skills shortages and skills mismatches

#=======================================================
# First look at the data 
#=======================================================

# How many variables do we have?
length (sjd1)

# How many observations?
nrow (sjd1)

# What are the names of the variables?
names (sjd1)

# Some information about the data
summary (sjd1)


#=======================================================
# Chart 1: Global Vision - Bar Chart skills
#=======================================================

# Creation of the dataframe 
df_g1 <- filter(sjd1, skill1 == "Basic Skills (Content)")
skill <- unique(sjd1$skill1)

# Creation of a basic column chart
g <- ggplot(df_g1, aes (x = country, y = value))
g + geom_col()

# Order the countries
df_g1$country <- fct_reorder(df_g1$country, df_g1$value, .desc = FALSE) # I order the the 
#variables "country" (factor) according to "share"

# First, I create a new variable in my DF for comparisons (1 = France; 
#2 = neighboring countries;3 = other countries)
df_g1 <- df_g1 %>%
  mutate(color = case_when (
    country == "Austria" ~ 1,
    country == "Hungary" | country == "Slovenia" | country == "Slovak Republic" |
      country == "Italy" | country == "Switzerland" | country == "Germany" | 
      country == "Czech Republic" ~ 2,
    country != "Austria" & country != "Hungary" & country != "Slovenia" & 
      country != "Slovak Republic" & country != "Italy" & country != "Switzerland" & 
      country != "Germany" & country != "Czech Republic" ~ 3))

# Creation of the bar chart
g1 <- ggplot(df_g1, aes (x = country, y = value, fill = factor (color))) +
  geom_col() +
  scale_fill_manual(name = "Legend : ", labels = c("Selected Country", "Neighboring Countires",
                                                "Other Countires"), values = c("#253494", "#74a9cf", "#bdc9e1")) +
  theme(
    axis.text.x = element_text (color = "black", angle=45, hjust=0.9),
    axis.title.x = element_blank(),
    panel.background = element_rect (fill = NA),
    panel.grid.major = element_line (colour = "#00000010"),
    panel.grid.major.x = element_blank(),
    legend.position = "top") 

# Implementation of title, legend, note ...
g1 + labs(title = "Figure 3: 0.252 of surplus for Basic Skill (content) in Austria",
         subtitle = "Index value representing skill imbalance, 2015",
         caption = "
         Note : The value represent the skill imbalances indicator. It ranges from 
         -1 (surplus skill easy to find) to 1 ( shortage skill hard to find) find).
         
         Lecture : In 2015, in Austria, the index value was around 0.3, which 
         means a shortage of people with basic skill. 
         
         Source : sjd1, 2015")+
  
  theme(
    plot.caption = element_text(hjust = 0, face = "italic")
  )


#rm (g, df)


#=======================================================
# Chart 2: Global Vision - Chart Comparison
#=======================================================

#-------------------------------------------------------
# Skill1
#-------------------------------------------------------

# Creation of the dataframe 
df_g2 <- filter(sjd1, country == "Austria")
skill <- unique(sjd1$skill1)

# Creation of a basic column chart
g <- ggplot(df_g2, aes (x = skill1, y = value))
g + geom_col()

# Order the countries 
df_g2$skill1 <- fct_reorder(df_g2$skill1, df_g2$value, .desc = FALSE) # I order the the 
#variables "skill1" (factor) according to "value"

# First, I create a new variable in my DF for comparisons (1 = France; 
#2 = neighboring countries;3 = other countries)
df_g2 <- df_g2 %>%
  mutate(color = case_when (
    skill1 == "Basic Skills (Content)" ~ 1, 
    skill1 == "Basic Skills (Process)" ~ 2,
    skill1 == "Social Skills" ~ 3, 
    skill1 == "Resource Management Skills"  ~ 4,
    skill1 == "Complex Problem Solving Skills" ~ 5,
    skill1 == "Technical Skills" ~ 6,
    skill1 == "Systems Skills" ~ 7))

# First the bar chart with Austria value
g2 <- ggplot(df_g2, aes (x = skill1, y = value, fill = factor(color))) +
  geom_col() +
  scale_fill_manual(name = "Legend : ", 
                    labels = c("Basic Skills (Content)", "Basic Skills (Process)",
                               "Social Skills", "Complex Problem Solving Skills", 
                               "Technical Skills", "Systems Skills", "Resource Management Skills"), 
                    values = c("#016450", "#02818a", "#3690c0", "#67a9cf", "#a6bddb",
                               "#d0d1e6", "#f6eff7" )) +
  theme(
    axis.text.x = element_text (color = "black", angle=45, hjust=0.9),
    axis.title.x = element_blank(),
    panel.background = element_rect (fill = NA),
    panel.grid.major = element_line (colour = "#00000010"),
    panel.grid.major.x = element_blank(),
    legend.position = "none") 
g2


# Second the plot chart with the European Union value : creation of new df
df_p <- filter(sjd1, country == "European Union")
skill <- unique(sjd1$skill1)
df_p$skill1 <- fct_reorder(df_p$skill1, df_p$value, .desc = FALSE) # I order the the 
#variables "skill1" (factor) according to "value"

# We combine the two chart 
f <- g2 + geom_point(data = df_p, aes(x = skill1, y = value), shape = 18, fill = "grey", size = 3)+
  theme(
    axis.text.x = element_text (color = "black", angle=45, hjust=0.9),
    axis.title.x = element_blank(),
    panel.background = element_rect (fill = NA),
    panel.grid.major = element_line (colour = "#00000010"),
    panel.grid.major.x = element_blank(),
    legend.position = "top") 

# We add some Legend  
f +  labs(title = "Figure 4 : Skills shortage and surplus",
          subtitle = "Austria and Eu/OECD, 2015",
          caption = "
         Note : The value represent the skill imbalances indicator. It ranges from 
         -1 (surplus skill easy to find) to 1 ( shortage skill hard to find) find).
         
         Lecture : In 2015,for Austria, the index value was around similar with EU/OECD
         for almost all the skills.
         
         Source : sjd1, 2015")+
  
  theme(
    plot.caption = element_text(hjust = 0, face = "italic")
  )


#rm (g, df)

#-------------------------------------------------------
# Skill2
#-------------------------------------------------------

# Creation of the dataframe 
df_g3 <- filter(sjd2, country == "Austria")
skill <- unique(sjd2$skill2)

# Creation of a basic column chart
g <- ggplot(df_g3, aes (x = skill2, y = value))
g + geom_col()

# Order the countries 
df_g3$skill2 <- fct_reorder(df_g3$skill2, df_g3$value, .desc = FALSE) # I order the the 
#variables "skill1" (factor) according to "value"

# First, I create a new variable in my DF for comparisons (1 = France; 
#2 = neighboring countries;3 = other countries)
df_g3 <- df_g3 %>%
  mutate(color = case_when (
    skill2 == "Reading Comprehension" | skill2 == "Active Listening" | 
      skill2 == "Writing" | skill2 == "Speaking" | skill2 == "Basic Skills (Process)" | 
      skill2 == "Mathematics Skills" | skill2 == "Science" ~ 1,
    
    skill2 == "Critical Thinking" | skill2 == "Active Learning" |  
      skill2 == "Learning Strategies" | skill2 == "Monitoring" ~ 2,
    
    skill2 == "Social Perceptiveness" | skill2 == "Coordination" |
      skill2 == "Persuasion" | skill2 == "Negotiation" |
      skill2 == "Instructing" | skill2 == "Service Orientation" ~ 3,
    
    skill2 == "Complex Problem Solving Skills" ~ 4,
    skill2 == "Operations Analysis" | skill2 == "Operations Analysis" | 
      skill2 == "Equipment Selection" | skill2 == "Installation" |
      skill2 == "Programming" | skill2 == "Operation Monitoring" |
      skill2 == "Operation and Control" | skill2 == "Equipment Maintenance" |
      skill2 == "Troubleshooting" | skill2 == "Repairing" |
      skill2 == "Quality Control Analysis" ~ 5,
    
    skill2 == "Judgment and Decision Making" | skill2 == "Systems Analysis" |
      skill2 == "Systems Evaluation" ~ 6,
      
    skill2 == "Time Management" | skill2 == "Management of Financial Resources" | 
      skill2 == "Management of Material Resources" | skill2 == "Management of Personnel Resources" ~ 7))

# First the bar chart with Austria value
g3 <- ggplot(df_g3, aes (x = skill2, y = value, fill = factor(color))) +
  geom_col() +
  scale_fill_manual(name = "Legend : ", 
                    labels = c("Basic Skills (Content)", "Basic Skills (Process)",
                               "Social Skills", "Complex Problem Solving Skills", 
                               "Technical Skills", "Systems Skills", "Resource Management Skills"), 
                    values = c("#016450", "#02818a", "#3690c0", "#67a9cf", "#a6bddb",
                               "#d0d1e6", "#f6eff7" )) +
  theme(
    axis.text.x = element_text (color = "black", angle=45, hjust=0.9),
    axis.title.x = element_blank(),
    panel.background = element_rect (fill = NA),
    panel.grid.major = element_line (colour = "#00000010"),
    panel.grid.major.x = element_blank(),
    legend.position = "top") 
g3


# Second the plot chart with the European Union value : creation of new df
df_p <- filter(sjd2, country == "European Union")
skill <- unique(sjd2$skill2)
df_p$skill2 <- fct_reorder(df_p$skill2, df_p$value, .desc = FALSE) # I order the the 
#variables "skill1" (factor) according to "value"

# We combine the two chart 
f2 <- g3 + geom_point(data = df_p, aes(x = skill2, y = value), shape = 18, fill = "grey", size = 3)+
  theme(
    axis.text.x = element_text (color = "black", angle=45, hjust=0.9),
    axis.title.x = element_blank(),
    panel.background = element_rect (fill = NA),
    panel.grid.major = element_line (colour = "#00000010"),
    panel.grid.major.x = element_blank(),
    legend.position = "top") 

# We add some Legend  
f2 +  labs(title = "Figure 5: Skills shortage and surplus (developp)",
          subtitle = "Austria and EU/OECD, 2015",
          caption = "
         Note : The value represent the skill imbalances indicator. It ranges from 
         -1 (surplus skill easy to find) to 1 ( shortage skill hard to find) find).
         
         Lecture : In 2015,for Austria, the index value was similar to EU/OECD. 
         
         Source : sjd2, 2015")+
  
  theme(
    plot.caption = element_text(hjust = 0, face = "italic")
  )



