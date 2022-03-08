
#-------------------------------------------#
####-------------- Read data file ----------####
#-------------------------------------------#

cog_df = read.csv(file = 'data/paquid_cog.csv')
cov_df = read.csv(file = 'data/paquid_cov.csv')

#-------------------------------------------#
####-------------- Exercise 1 ----------####
#-------------------------------------------#
# Generate a variable “fu”, which means follow-up time and equals to age-age_init.