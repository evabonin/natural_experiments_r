library(dplyr)
library(haven)
library(miceadds)
library(ggplot2)
library(sandwich)
library(lmerTest)

# Importing STATA file to look at original data



barrera <- read_dta("Public_Data_AEJApp_2010-0132.dta")


barrera$T1T2T3 <- case_when(
  barrera$T1_treat == 1 ~ 1,
  barrera$T2_treat == 1 ~ 2,
  barrera$T3_treat == 1 ~ 3,
  barrera$T1_treat == 0 & barrera$T2_treat == 0 & barrera$T3_treat == 0 ~ 0
)


# Demographic variables controlled for

# global varbaseline "i.s_teneviv s_utilities s_durables s_infraest_hh s_age_sorteo s_age_sorteo2 s_years_back s_sexo i.s_estcivil s_single s_edadhead s_yrshead s_tpersona s_num18 i.s_estrato s_puntaje s_ingtotal i.grade suba s_over_age";


# To do:
# - Turn into factor variables: s_teneviv s_estcivil s_estrato grade
# - Include as is (maybe add labels etc.): s_utilities s_durables s_infraest_hh s_age_sorteo s_age_sorteo2 s_years_back s_sexo s_single s_edadhead s_yrshead s_tpersona s_num18 s_puntaje s_ingtotal suba s_over_age








# Dropping ineligible cases from Suba: Drop if suba == 1 and grade is < 9
#drop if suba == 1 & grade < 9; # !!! I think this should say 6
# Keeping only those who were selected for the survey:
# keep if survey_selected;
# Drop if they are in grade 11
# drop if grade == 11;


# Filtering data to reflect above. I think the STATA code may be incorrect because the text says (p. 179, caption to table 3) that the estimate is for students in grades 6-10. So I think it should say 6 instead of 9 above.

filtered_barrera <- barrera %>% filter(suba == 0, grade >= 6) %>% filter(survey_selected == 1) %>% filter(grade != 11)

# This filter gives a sample of 5,799 obs, which is in line with the paper.


# Regressions of enrollment for San Cristobal;


# Model 1

# xi: reg at_msamean T1_treat T2_treat if suba == 0, cluster(school_code);



# Regression expansion of categorical variables (xi means everything with the prefix i. will be turned into dummies). Unclear why this is needed, because there are no factor variables in model 1!
# This also seems to be clustered by school, which I wouldn't have guessed based on the table but I guess this is what they mean when they say, "allowed to vary up to the school level".
# What we want is a regression model with at_msamean as the outcome and T1_treat and T2_treat as the explanatory vars, for those who are not in Suba.

# Model table 3 for subba==0

mod1 <- miceadds::lm.cluster(data = filtered_barrera, at_msamean ~ T1_treat + T2_treat,  cluster = "school_code")


summary(mod1)


# Model table 3 for suba==1 and grades 9 and 10
#####
filtered_barrera2 <- barrera %>% filter(suba == 1, grade >= 6) %>% filter(survey_selected == 1) %>% filter(grade >= 9 ) %>% filter(grade <= 10 )


mod4 <- miceadds::lm.cluster(data = filtered_barrera2, at_msamean ~ T3_treat,  cluster = "school_code")

summary(mod4)


# Would be good to add proper labels etc. to this

barrera$s_teneviv <- as.factor(barrera$s_teneviv)
barrera$s_estcivil <- as.factor(barrera$s_estcivil)
barrera$s_estrato <- as.factor(barrera$s_estrato)
barrera$grade <- as.factor(barrera$grade)
barrera$school_code <- as.factor(barrera$school_code)

# Model 2

# xi: reg at_msamean T1_treat T2_treat $varbaseline if suba == 0, cluster(school_code);
# Same model as 1, but including all the demographic variables above

mod2 <- miceadds::lm.cluster(data = filtered_barrera, at_msamean ~ T1_treat + T2_treat + s_teneviv + s_utilities + s_durables + s_infraest_hh + s_age_sorteo + s_age_sorteo2 + s_years_back + s_sexo + s_estcivil + s_single + s_edadhead + s_yrshead + s_tpersona + s_num18 + s_estrato + s_puntaje + s_ingtotal + grade + suba + s_over_age,  cluster = "school_code")

summary(mod2)



# Model 3



# xi: reg at_msamean T1_treat T2_treat $varbaseline i.school_code if suba == 0, cluster(school_code);
# Same model as 2, but this time using the school code as a factor in the model.

filtered_barrera$school_code <- as.factor(filtered_barrera$school_code)

mod3 <- miceadds::lm.cluster(data = filtered_barrera, at_msamean ~ T1_treat + T2_treat + s_teneviv + s_utilities + s_durables + s_infraest_hh + s_age_sorteo + s_age_sorteo2 + s_years_back + s_sexo + s_estcivil + s_single + s_edadhead + s_yrshead + s_tpersona + s_num18 + s_estrato + s_puntaje + s_ingtotal + grade + suba + s_over_age + school_code,  cluster = "school_code")

model3 <- lm(data = filtered_barrera, at_msamean ~ T1_treat + T2_treat + s_teneviv + s_utilities + s_durables + s_infraest_hh + s_age_sorteo + s_age_sorteo2 + s_years_back + s_sexo + s_estcivil + s_single + s_edadhead + s_yrshead + s_tpersona + s_num18 + s_estrato + s_puntaje + s_ingtotal + grade + suba + s_over_age + school_code)

summary(mod3)



#Predict graph p19 mod3

set.seed(123)
filtered_barrera$predicted_at_msamean <- predict(model1_lmer,filtered_barrera)

ggplot(data=filtered_barrera, aes(x=predicted_at_msamean, y=at_msamean, group=1))+
  geom_point()
View(filtered_barrera$predicted_at_msamean,filtered_barrera$at_msamean)

df <- data.frame(filtered_barrera$predicted_at_msamean,filtered_barrera$at_msamean,filtered_barrera$T1T2T3)
View(df)
summary(df)
ggplot(data=df, aes(x=predicted_at_msamean, y=at_msamean, group=1))+
  geom_point()
 
ggplot(data=df, aes(x=filtered_barrera.predicted_at_msamean, y=filtered_barrera.at_msamean, group=1))+
  geom_point(aes(color=factor(filtered_barrera.T1T2T3)))

df2 <- df %>%
  group_by(filtered_barrera.T1T2T3) %>%
  summarise(mean_run = mean(filtered_barrera.predicted_at_msamean),min_run=min(filtered_barrera.predicted_at_msamean))

View(df2)

##########################################3
model1_lmer <- lmer(data = filtered_barrera, at_msamean ~ T1_treat + T2_treat +school_code)




#plot

barrera$T1T2T3 <- case_when(
  barrera$T1_treat == 1 ~ 1,
  barrera$T2_treat == 1 ~ 2,
  barrera$T3_treat == 1 ~ 3,
  barrera$T1_treat == 0 & barrera$T2_treat == 0 & barrera$T3_treat == 0 ~ 0
)


ggplot(data=filtered_barrera, aes(x=at_baseline, y=at_msamean, color=factor(T1T2T3))) +
  geom_smooth(method="lm", se=FALSE)+
  xlim(0.65, 0.9)+
  scale_color_discrete(name = "Treatments", labels = c("Control", "Basics", "Savings"))+
  labs(y = "Actual Attendance ", x = "Predicted Baseline Attendance")
