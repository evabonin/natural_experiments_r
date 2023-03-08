
#library(knitrBootstrap)
library(dplyr)

# reading STATA files
library(haven)
library(table1)

# lm.cluster function
library(lmtest)

library(miceadds)
library(ggplot2)
library(sandwich)

# visualise residuals for clustered regression
library(visreg)
#library(sjPlot)
#library(sjmisc)
#library(sjlabelled)

library(tidyverse)       # For ggplot, dplyr, and friends
library(broom)           # Convert model objects into data frames


# Importing STATA file

barrera <- read_dta("data/Public_Data_AEJApp_2010-0132.dta")

# Turning variables into factor variables.

barrera$f_teneviv <- factor(barrera$s_teneviv)
barrera$f_estcivil <- factor(barrera$s_estcivil, levels = c(1, 2, 3, 4, 5), labels = c("Free union", "Married", "Widow(er)", "Divorced", "Single"))
barrera$f_estrato <- factor(barrera$s_estrato)
barrera$f_grade <- factor(barrera$grade)
barrera$f_sexo <- factor(barrera$s_sexo, levels = c(0,1), labels = c("Female", "Male"))
barrera$f_single <- factor(barrera$s_single, levels = c(0,1), labels = c("No", "Yes"))
barrera$f_over_age <- factor(barrera$s_over_age, levels = c(0,1), labels = c("No", "Yes"))
barrera$f_suba <- factor(barrera$suba, levels = c(0,1), labels = c("San Cristobal", "Suba"))




# Generate one variable to capture treatment assignment (T1, T2, control)

barrera$T1T2T3 <- case_when(
  barrera$T1_treat == 1 ~ 1,
  barrera$T2_treat == 1 ~ 2,
  barrera$T3_treat == 1 ~ 3,
  barrera$T1_treat == 0 & barrera$T2_treat == 0 & barrera$T3_treat == 0 ~ 0
)

barrera$T1T2T3 <- factor(barrera$T1T2T3, level = c(0, 1, 2, 3), labels = c("Control", "Basic (T1)", "Savings (T2)", "Tertiary (T3"))


# Confirm that this worked

attach(barrera)
check_table <- table(T1T2T3, T1_treat, T2_treat, T3_treat)
ftable(check_table)



# Filtering data in line with the following STATA code operations to reproduce table 3, columns 1-3:
# Dropping ineligible cases from Suba: Drop if suba == 1 and grade is < 9
# drop if suba == 1 & grade < 9; 
# The above seems to be a mistake in the STATA code: this should be grade < 6 instead of < 9. 
# Keeping only those who were selected for the survey:
# keep if survey_selected;
# Drop if they are in grade 11


# Filtered data

filtered_barrera <- barrera %>% filter(suba == 0, grade >= 6, survey_selected == 1, grade != 11)


# Filtering data in line with the following STATA code operations to reproduce table 3, columns 4-6:
# Same as above, but suba == 1 and in rade 9 or 10

filtered_barrera2 <- barrera %>% filter(suba == 1, survey_selected == 1, grade >= 9, grade <= 10)









# Table 1



# Using n = 5,799 to get the sample actually used in our model (for columns 1-3). Variables selected based on Table 1.
# Note: Some factor variables are presented in the paper as scale vars.

# House posessions - f_teneviv
# utilities - s_utilities
# durable goods - s_durables
# physical infrastructure - s_infraest_hh
# age - s_age_sorteo
# gender - s_sexo
# years of education - s_yrs
# single head - s_single
# Age of head - s_edadhead
# years of ed head - s_yrshead
# people in household - s_tpersona
# Member under 18 - s_num18
# estrato - f_estrato
# SISBEN - s_puntaje
# household income - s_ingtotal







table1 <- table1(~ factor(f_teneviv) + s_utilities + s_durables + s_infraest_hh + s_age_sorteo + factor(f_sexo) + s_yrs + factor(f_single) + s_edadhead + s_yrshead + s_tpersona + s_num18 + factor(f_estrato) + s_puntaje + s_ingtotal | ~ factor(T1T2T3), data=filtered_barrera)




# Models


# Model 1

mod1 <- lm.cluster(data = filtered_barrera, at_msamean ~ T1_treat + T2_treat,  cluster = "school_code")
#mod1 <- lm(data = filtered_barrera, at_msamean ~ T1_treat + T2_treat)
summary(mod1)
#summary(mod1, cluster = "school_code")


# Model 2
mod2 <- lm.cluster(data = filtered_barrera, at_msamean ~ T1_treat + T2_treat + f_teneviv + s_utilities + s_durables + s_infraest_hh + s_age_sorteo + s_age_sorteo2 + s_years_back + s_sexo + f_estcivil + s_single + s_edadhead + s_yrshead + s_tpersona + s_num18 + f_estrato + s_puntaje + s_ingtotal + f_grade + suba + s_over_age,  cluster = "school_code")
summary(mod2)



# Model 3
mod3 <- lm.cluster(data = filtered_barrera, at_msamean ~ T1_treat + T2_treat + f_teneviv + s_utilities + s_durables + s_infraest_hh + s_age_sorteo + s_age_sorteo2 + s_years_back + s_sexo + f_estcivil + s_single + s_edadhead + s_yrshead + s_tpersona + s_num18 + f_estrato + s_puntaje + s_ingtotal + f_grade + suba + s_over_age + factor(school_code),  cluster = "school_code")
summary(mod3)


# # Model 4
# mod4 <- lm.cluster(data = filtered_barrera2, at_msamean ~ T3_treat,  cluster = "school_code")
# summary(mod4)
# # Model 5
# mod5 <- lm.cluster(data = filtered_barrera2, at_msamean ~ T3_treat + f_teneviv + s_utilities + s_durables + s_infraest_hh + s_age_sorteo + s_age_sorteo2 + s_years_back + s_sexo + f_estcivil + s_single + s_edadhead + s_yrshead + s_tpersona + s_num18 + f_estrato + s_puntaje + s_ingtotal + f_grade + suba + s_over_age,  cluster = "school_code")
# summary(mod5)
# # Model 6
# mod6 <- lm.cluster(data = filtered_barrera2, at_msamean ~ T3_treat + + f_teneviv + s_utilities + s_durables + s_infraest_hh + s_age_sorteo + s_age_sorteo2 + s_years_back + s_sexo + f_estcivil + s_single + s_edadhead + s_yrshead + s_tpersona + s_num18 + f_estrato + s_puntaje + s_ingtotal + f_grade + suba + s_over_age + factor(school_code),  cluster = "school_code")
# summary(mod6)



#############################################################################################
# Inspecting residuals
#############################################################################################
# From this page: https://evalf21.classes.andrewheiss.com/example/standard-errors/

model3 <- lm(data = filtered_barrera, at_msamean ~ factor(T1T2T3) + s_teneviv + s_utilities + s_durables + s_infraest_hh + s_age_sorteo + s_age_sorteo2 + s_years_back + s_sexo + s_estcivil + s_single + s_edadhead + s_yrshead + s_tpersona + s_num18 + s_estrato + s_puntaje + s_ingtotal + grade + suba + s_over_age + factor(school_code))
tidy(model3, conf.int = TRUE)

# model3_t1 <- lm(data = filtered_barrera, at_msamean ~ T1_treat + s_teneviv + s_utilities + s_durables + s_infraest_hh + s_age_sorteo + s_age_sorteo2 + s_years_back + s_sexo + s_estcivil + s_single + s_edadhead + s_yrshead + s_tpersona + s_num18 + s_estrato + s_puntaje + s_ingtotal + grade + suba + s_over_age)
# model3_t2 <- lm(data = filtered_barrera, at_msamean ~ T2_treat + s_teneviv + s_utilities + s_durables + s_infraest_hh + s_age_sorteo + s_age_sorteo2 + s_years_back + s_sexo + s_estcivil + s_single + s_edadhead + s_yrshead + s_tpersona + s_num18 + s_estrato + s_puntaje + s_ingtotal + grade + suba + s_over_age + factor(school_code))


# Checking for heteroskedasticity

# Plug the original data into the model and find fitted values and
# residuals/errors
fitted_data <- augment(model3, data = filtered_barrera)
# fitted_t1 <- augment(model3_t1, data = filtered_barrera)
# fitted_t2 <- augment(model3, data = filtered_barrera)

# Look at relationship between fitted values and residuals
ggplot(fitted_data, aes(x = .fitted, y = .resid)) + 
  geom_point(aes(color = school_code)) +
  geom_smooth(method = "lm")

# ggplot(fitted_t1, aes(x = .fitted, y = .resid)) + 
#   geom_point(aes(color = T1_treat)) +
#   geom_smooth(method = "lm")

# Are residuals normally distributed? -> no

ggplot(fitted_data, aes(x = .resid)) +
  geom_histogram(binwidth = 0.01, color = "white", boundary = 50000)




# robust standard errors with fixest --> doesn't quite work with all the variables due to collinearity.
library(fixest)

# Because school_code comes after |, it's being treated as a fixed effect
# model_feols <- feols(at_msamean ~ T1_treat + T2_treat + s_teneviv + s_utilities + s_durables + s_infraest_hh + s_age_sorteo + s_age_sorteo2 + s_years_back + s_sexo + s_estcivil + s_single + s_edadhead + s_yrshead + s_tpersona + s_num18 + s_estrato + s_puntaje + s_ingtotal + grade + s_over_age + school_code | school_code,
#                   vcov = "hetero",                  
#                   data = filtered_barrera)
# 
# tidy(model_feols)
# 




#############################################################################################

# Plotting the outcome variable

ggplot(filtered_barrera, aes(x = T1T2T3, y = at_msamean)) +
  geom_boxplot() +    # Box plot for visualization
  labs(x = "Treatment", y = "Attendance %")  # Label the axes


hist(filtered_barrera$at_msamean)


# Create separate histograms of at_msamean for each level of T1T2T3
ggplot(filtered_barrera, aes(x = at_msamean)) +
  geom_histogram(binwidth = 0.07, alpha = 0.5, position = "identity") +
  labs(x = "Attendance %", y = "Frequency") +
  facet_wrap(~T1T2T3, ncol = 3) +
  geom_vline(xintercept = 0.8, color = "red", linetype = "dashed")+
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        panel.spacing = unit(0.5, "lines"))

# Create separate cumulative distribution plots of at_msamean for each level of T1T2T3
ggplot(filtered_barrera, aes(x = at_msamean)) +
  stat_ecdf(aes(color = T1T2T3)) +
  labs(x = "at_msamean", y = "Cumulative Probability") +
  facet_wrap(~T1T2T3, ncol = 3) +
  geom_vline(xintercept = 0.8, color = "red", linetype = "dashed") +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        panel.spacing = unit(0.5, "lines"))

# What proportion in each group falls at or above the target of 80% attendance?

cutoff <- 0.8  # set the cutoff value

filtered_barrera %>% 
  group_by(T1T2T3) %>% 
  summarize(prop_cutoff = sum(at_msamean >= cutoff) / n())


# Calculating new column: is at or above cut-off?

filtered_barrera <- filtered_barrera %>% 
  mutate(above_cutoff = ifelse(at_msamean >= cutoff, 1, 0))

# Running above model but with this as outcome --> note that the residual plots don't pick up the clustered standard errors.
library(clusterSEs) # to get augment method for lm.cluster
library(sandwich)
library(broom) # augment for glm

mod_bi <- glm(data = filtered_barrera, above_cutoff ~ T1_treat + T2_treat + f_teneviv + s_utilities + s_durables + s_infraest_hh + s_age_sorteo + s_age_sorteo2 + s_years_back + s_sexo + f_estcivil + s_single + s_edadhead + s_yrshead + s_tpersona + s_num18 + f_estrato + s_puntaje + s_ingtotal + f_grade + suba + s_over_age + factor(school_code), family = binomial())
summary(mod_bi)
vcov <- vcovCL(mod_bi, cluster = filtered_barrera$school_code)
coeftest(mod_bi, vcov = vcov)


fitted_bi <- augment(mod_bi, data = filtered_barrera, se_fit = TRUE)

plot_bi <- ggplot(fitted_bi, aes(x = .resid)) +
  geom_histogram(binwidth = 0.01, color = "white", boundary = 50000)
plot_bi

# To compare, running linear model as glm:

mod_gau <- glm(data = filtered_barrera, at_msamean ~ T1_treat + T2_treat + f_teneviv + s_utilities + s_durables + s_infraest_hh + s_age_sorteo + s_age_sorteo2 + s_years_back + s_sexo + f_estcivil + s_single + s_edadhead + s_yrshead + s_tpersona + s_num18 + f_estrato + s_puntaje + s_ingtotal + f_grade + suba + s_over_age + factor(school_code), family = gaussian())
summary(mod_gau)
vcov <- vcovCL(mod_gau, cluster = filtered_barrera$school_code)
coeftest(mod_gau, vcov = vcov)

#fitted_gau <- augment(mod_gau, data = filtered_barrera)
fitted_gau <- augment(mod_gau, data = filtered_barrera, se_fit = TRUE)


plot_gau <- ggplot(fitted_gau, aes(x = .resid)) +
  geom_histogram(binwidth = 0.01, color = "white", boundary = 50000)
plot_gau

library(gridExtra) # to show two plots next to each other - I know there is another way!
# arrange plots side by side
grid.arrange(plot_bi, plot_gau, nrow = 2)


# GLM with binary outcome variable performs much better on AIC. But I'm not sure the residual plots help us very much as it's not clustered SE!



# Part of the problem with this model is the outcome variable, which has a ceiling effect (can't go above 100%, and many people have high attendance, with target attendance also being high at 80%.)
# Alternative way of approaching this:
# GLM for skewed data (e.g. log link and gamma function --> would need to fit this more carefully, )
# Binary variable: Whether or not student achieved 80% attendance


#############################################################################################
# Trying again to run this with stata standard errors
library(modelsummary)
#library(fixest)

feols_m1 <- feols(data = filtered_barrera,
               at_msamean ~ T1_treat + T2_treat,
               cluster = ~ school_code)

feols_m2 <- feols(data = filtered_barrera, 
                  at_msamean ~ T1_treat + T2_treat + f_teneviv + s_utilities + s_durables + s_infraest_hh + s_age_sorteo + s_age_sorteo2 + s_years_back + s_sexo + f_estcivil + s_single + s_edadhead + s_yrshead + s_tpersona + s_num18 + f_estrato + s_puntaje + s_ingtotal + f_grade + suba + s_over_age,
                  cluster = ~ school_code)

feols_m3 <- feols(data = filtered_barrera, 
                  at_msamean ~ T1_treat + T2_treat + f_teneviv + s_utilities + s_durables + s_infraest_hh + s_age_sorteo + s_age_sorteo2 + s_years_back + s_sexo + f_estcivil + s_single + s_edadhead + s_yrshead + s_tpersona + s_num18 + f_estrato + s_puntaje + s_ingtotal + f_grade + suba + s_over_age | school_code,
                  cluster = ~ school_code)

# specify hypothesis tests
library(car)
hyp1 <- linearHypothesis(feols_m1, "T1_treat - T2_treat = 0")
hyp2 <- linearHypothesis(feols_m2, "T1_treat - T2_treat")
hyp3 <- linearHypothesis(feols_m3, "T1_treat - T2_treat")


# Combining all model outputs into one table and showing only coefficients on T1_treat and T2_treat

rows <- tribble(~term, ~"(1)", ~"(2)", ~"(3)",
                "Chi-squared", hyp1[,2][2], hyp2[,2][2], hyp3[,2][2],
                "p-value", hyp1[,3][2], hyp2[,3][2], hyp3[,3][2],
                "Demographic controls", 0,1,1)
attr(rows, "position") <- c(5,6,9)

modelsummary(list(feols_m1, feols_m2, feols_m3), 
             coef_omit = -c(2,3), 
             gof_omit = "AIC|BIC|RMSE|R2 W|R2 A", 
             add_rows = rows,
             coef_rename = c("Basic treatment","Savings treatment"),
             title = "Table 3 - Effects on Monitored School Attendance Rates"
             )

library(lmtest)
library(sandwich)

feols <- list(feols_m1, feols_m2, feols_m3)
hyps <- list(hyp1, hyp2, hyp3)


modelsummary(feols,
             coef_omit = -c(2,3), 
             gof_map = c("nobs", "r.squared"), 
)

#############################################################################################



# Graph

ggplot(data=filtered_barrera, aes(x=at_baseline, y=at_msamean, color=factor(T1T2T3))) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)


ggplot(data=filtered_barrera, aes(x=at_baseline, y=at_msamean, color=factor(T1T2T3))) +
  geom_smooth(method="lm", se=FALSE)+
  xlim(0.65, NA) +
  ylim(0.5, NA)












