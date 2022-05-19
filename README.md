# Rworkshop at Arging Research Center, KI

* This repository is to share the code for the R workshop exercise.
* Data paquid_cog.csv and paquid_cov.csv are available in the data file.
* The code in `example.R` contains solutions for the following questions.
* The slide is available [here](https://bolin-wu.github.io/Rworkshop_KI_slide/#1)


## Generate and label variables

1. Generate a variable “fu”, which means follow-up time and equals to age-age_init.
2. Generate a variable “dem_young”, which means age of dementia onset (variable “agedem”) ≤70 years old (use the the if/else statement).
3. Rename variable “CEP” as “education” and change the variable class to factor. Label the variable values as 0=“Below primary school”, 1=“Primary school and above”.

## Merge and reshape data sets

4. Merge datasets “paquid_cog” and “paquid_cov” to a data frame named “paquid”.
5. Reshape the “paquid” data to wide format.

## Row-wise calculation

6. Generate a variable named “MMSE_M”, which is the number of missing values across variables “MMSE_1”, “MMSE_2”, …, “MMSE_9” per individual. Label the variable as “the number of missing values in MMSE”.
7. View variables that contain “MMSE”.
8. Generate variables “MEM_1”, “MEM_2”, …, “MEM_9”. which equals the mean of “BVRT” and “IST” at each time point.
9. View variables that contain “MEM”, “BVRT”, or “IST”.

## Summarizing data

10. Summarize variable “age_init” (mean, sd, quantiles, etc), summarize “age_init” by variable “male”.
11. Summarize variable “MMSE_1” (mean, sd, quantiles, etc), summarize “age_init” by variable “male”. Note how R deals with missing values.
12. Tabulate variable “male”, tabulate variable “male” and “education”, add row-wise and column-wise proportions.
13. Draw a histogram and a density plot of “MMSE_1”.

## Run simple models and check model output

14. Run a linear regression, with “MMSE_1” as dependent variable and “age_init” and “male” as the independent variables, assuming “MMSE_1” has a normal distribution. Check model output.
15. Run a logistic regression, with “dem_young” as dependent variable and “male” as the independent variables.



