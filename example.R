rm(list = ls())
#-------------------------------------------#
#### -------------- Load packages ----------####
#-------------------------------------------#
# install.packages("Hmisc") 
# for cleaning data
library(tidyverse)
# for adding label to data.frame()
library(Hmisc)
#-------------------------------------------#
#### -------------- Read data file ----------####
#-------------------------------------------#

cog_df <- read.csv(file = "data/paquid_cog.csv", )
cov_df <- read.csv(file = "data/paquid_cov.csv")

#-------------------------------------------#
#### -------------- Check data ----------####
#-------------------------------------------#
# check data's type 
str(cog_df)
str(cov_df)
# descriptive statistics
summary(cog_df)
summary(cov_df)

# get rid of the first column 'X'
cog_df <- cog_df %>% select(-X)
cov_df <- cov_df %>% select(-X)

# can also use the following code to delete the first column
# cov_df[, -1]


#-------------------------------------------#
#### ---- Generate and label variables -----####
#-------------------------------------------#
# 1. Generate a variable “fu”, which means follow-up time and equals to age-age_init.
head(cov_df)
fu = cov_df$age - cov_df$age_init
cov_df$fu = fu
head(cov_df)

# 2.	Generate a variable “dem_young”, which means age of dementia onset 
# (variable “agedem”) ≤70 years old (use the the if/else statement).

summary(cog_df)
dem_young = ifelse(cog_df$agedem <= 70 , yes = 1, no = 0)
# put dem_young to cog_df 
cog_df$dem_young = dem_young
head(cog_df)

# 3.	Rename variable “CEP” as “education” and change the variable class to factor. 
colnames(cov_df)[colnames(cov_df) == 'CEP'] <- 'education'
head(cov_df)

# 3. Label the variable values as 0=“Below primary school”, 1=“Primary school and above”.
label(cov_df[["education"]]) <- "0='Below primary school', 1='Primary school and above'"
# check if the label is added
View(cov_df)


