

library(dplyr)
library(haven)
library(miceadds)

# Importing STATA file to look at original data



barrera <- read_dta("data/Public_Data_AEJApp_2010-0132.dta")


# Demographic variables controlled for

# global varbaseline "i.s_teneviv s_utilities s_durables s_infraest_hh s_age_sorteo s_age_sorteo2 s_years_back s_sexo i.s_estcivil s_single s_edadhead s_yrshead s_tpersona s_num18 i.s_estrato s_puntaje s_ingtotal i.grade suba s_over_age";


# To do:
# - Turn into factor variables: s_teneviv s_estcivil s_estrato grade
# - Include as is (maybe add labels etc.): s_utilities s_durables s_infraest_hh s_age_sorteo s_age_sorteo2 s_years_back s_sexo s_single s_edadhead s_yrshead s_tpersona s_num18 s_puntaje s_ingtotal suba s_over_age



# Would be good to add proper labels etc. to this

i.s_teneviv <- factor(barrera$s_teneviv)
i.s_estcivil <- factor(barrera$s_estcivil)
i.s_estrato <- factor(barrera$s_estrato)
i.grade <- factor(barrera$grade)




# Dropping ineligible cases from Suba: Drop if suba == 1 and grade is < 9
#drop if suba == 1 & grade < 9; # !!! I think this should say 6
# Keeping only those who were selected for the survey:
# keep if survey_selected;
# Drop if they are in grade 11
# drop if grade == 11;


# Filtering data to reflect above. I think the STATA code may be incorrect because the text says (p. 179, caption to table 3) that the estimate is for students in grades 6-10. So I think it should say 6 instead of 9 above.

filtered_barrera <- barrera %>% filter(suba == 0, grade >= 6) %>%
  filter(survey_selected == 1) %>%
  filter(grade != 11)

# This filter gives a sample of 5,799 obs, which is in line with the paper.


# Regressions of enrollment for San Cristobal;


# Model 1

# xi: reg at_msamean T1_treat T2_treat if suba == 0, cluster(school_code);



# Regression expansion of categorical variables (xi means everything with the prefix i. will be turned into dummies). Unclear why this is needed, because there are no factor variables in model 1!
# This also seems to be clustered by school, which I wouldn't have guessed based on the table but I guess this is what they mean when they say, "allowed to vary up to the school level".
# What we want is a regression model with at_msamean as the outcome and T1_treat and T2_treat as the explanatory vars, for those who are not in Suba.


mod1 <- miceadds::lm.cluster(data = filtered_barrera, at_msamean ~ T1_treat + T2_treat,  cluster = "school_code")

summary(mod1)

# I think this looks ok.


# Model 2

# xi: reg at_msamean T1_treat T2_treat $varbaseline if suba == 0, cluster(school_code);
# Same model as 1, but including all the demographic variables above



# Model 3

# xi: reg at_msamean T1_treat T2_treat $varbaseline i.school_code if suba == 0, cluster(school_code);
# Same model as 2, but this time using the school code as a factor in the model.








