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

# actually I would recommend using tibble
# since it gives some nice properties
as_tibble(cog_df)


#----------------------------------------------#
#### ---- Generate and label variables -----####
#----------------------------------------------#
# 1. Generate a variable “fu”, which means follow-up time and equals to age-age_init.
head(cov_df)
fu <- cov_df$age - cov_df$age_init
cov_df$fu <- fu
head(cov_df)

# 2.	Generate a variable “dem_young”, which means age of dementia onset
# (variable “agedem”) =70 years old (use the the if/else statement).

summary(cog_df)
dem_young <- ifelse(cog_df$agedem <= 70, yes = 1, no = 0)
# put dem_young to cog_df
cog_df$dem_young <- dem_young
head(cog_df)

# 3.	Rename variable “CEP” as “education” and change the variable class to factor.
colnames(cov_df)[colnames(cov_df) == "CEP"] <- "education"
head(cov_df)

# 3. Label the variable values as 0=“Below primary school”, 1=“Primary school and above”.
label(cov_df[["education"]]) <- "0='Below primary school', 1='Primary school and above'"
# check if the label is added
View(cov_df)


#----------------------------------------------#
#### ----  Merge and reshape data sets -----####
#----------------------------------------------#

# 4.	Merge datasets “paquid_cog” and “paquid_cov” to a data frame named “paquid”.

# with _join function
paquid <- full_join(x = cog_df, y = cov_df, by = c("ID", "wave", "age"))
head(paquid)
# with merge function
paquid2 <- merge(x = cog_df, y = cov_df)
head(paquid2)
# reorder paquid2 so that it has the same column/row order as paquid
paquid2 <- paquid2[order(paquid2$ID), names(paquid)]
head(paquid2)
# great, the results from two functions are the same.

# 5.	Reshape the “paquid” data to wide format.

# first we need to see how many waves are included
summary.factor(paquid$wave)

head(cog_df)
head(cov_df)
# all the variables in cov_df are constan;
# only variables in cog_df change at different waves
colnames(cog_df)
paquid
# use spread() function
spread(data = paquid, value = "MMSE", key = "wave", sep = "MMSE")
# one short coming of spread function is that it can only spread one column at a time
# if one wants to spread multiple columns, then one needs to spread tables several times then
# join the results together


# use reshape() function
# timevar: the variable in long format that differentiates multiple records from the same group or individual.
# idvar: Columns that will not be affected, stay the same
unchange_column <- c("ID", "age_init", "education", "male", "agedem", "dem", "dem_young")
wide_paquid <- reshape(data = paquid, timevar = "wave", idvar = unchange_column, direction = "wide", sep = "_")
head(wide_paquid)
# it can spread multiple columns at a time
# but one must be careful when define the "idvar" argument.
# spread() function is more useful if we only wants to spread one interested variable (e.g. MMSE)
# reshape() function is mroe useful if we wants to spread multiple columns

#----------------------------------------------#
#### -------  	Row-wise calculation  ------####
#----------------------------------------------#

# 6.	Generate a variable named “MMSE_M”, which is the number of missing values across
# variables “MMSE_1”, “MMSE_2”, …, “MMSE_9” per individual. Label the variable as
# “the number of missing values in MMSE”.
head(wide_paquid)
# convert dataframe to tibble for faster data cleaning
wide_paquid <- as_tibble(wide_paquid)

# (1) use rowSums
paquid_MMSE <- wide_paquid %>% select(c("ID", contains("MMSE")))
paquid_MMSE
MMSE_M <- rowSums(is.na(paquid_MSME))
MMSE_M
# merge the vector to the tibble
paquid_MMSE$MMSE = MMSE_M

# (2) use rowwise()
paquid_MMSE <- wide_paquid %>%
  select(c("ID", contains("MMSE"))) %>%
  rowwise("ID") %>%
  mutate(MMSE_M = sum(is.na(cur_data()))) %>%
  ungroup()
paquid_MMSE
View(paquid_MMSE)

# 7.	View variables that contain “MMSE”.
wide_paquid %>% select(contains("MMSE"))

# 8.	Generate variables “MEM_1”, “MEM_2”, …, “MEM_9”. 
# which equals the mean of “BVRT” and “IST” at each time point.

# this tibble is the "raw material"
wide_paquid %>% select(contains(c("ID","BVRT","IST")))

# let's first state with MEM_1

wide_paquid %>% select(contains(c("ID","BVRT_1","IST_1")))  %>% 
  rowwise("ID") %>% 
  mutate(MEM_1 = mean(c(BVRT_1,IST_1))) %>% 
  ungroup()

# can we do the above process directly on the whole long dataframe?
# Yes!
wide_paquid  %>% 
  rowwise("ID") %>% 
  mutate(MEM_1 = mean(c(BVRT_1,IST_1))) %>% 
  ungroup()


# we can do the same procedure for the rest eight variables by loop and assign() function
for (i in 1:9) {
  BVRT_i = paste0("BVRT_", i)
  IST_i = paste0("IST_", i)
  MEM_i = paste0("MEM_", i)
  wide_paquid = wide_paquid  %>% 
    rowwise("ID") %>% 
    mutate(!!sym(MEM_i) := mean(c(get(BVRT_i),get(IST_i)))) %>% 
    ungroup()
}

# sym(new_col_name) := is a dynamic way of writing MEM_1 = , MEM_2 = ,etc when using functions like mutate()
# in the tidyr package


# 9.	View variables that contain “MEM”, “BVRT”, or “IST”.
wide_paquid %>% select(contains(c("MEM", "BVRT", "IST")))




#----------------------------------------------#
#### ---------  	Summarizing data  --------####
#----------------------------------------------#







