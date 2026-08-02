#First creating a function to fit regression model for EACH IMPUTATION - basic adjusted
multivariate_MICE <- function(exp, outcome) {
  with(design_analytic, 
       svycoxph(as.formula(
         paste(
           "Surv(permth_int, ",
           outcome,
           ") ~",
           exp,
           "+ Year + rms::rcs(Age, 4) + Gender + Ethnicity + EducationHighest + Povertytoincomeratio + Occupation2  + Diabetes + BMI_class + Hypertension + ChronicKidneyDisease + Hyperlipidemia"
         ))
       ))
}

####################################
#Next, a function for formatting results
pool_and_format_results <- function(fit_mira) {
  # 'MIcombine' applies Rubin's Rules to pool the estimates from each imputed dataset.
  pooled <- mitools::MIcombine(fit_mira)
  
  # Exponentiate the coefficients and confidence intervals to get HRs.
  HR_estimates <- exp(coef(pooled))
  ci_estimates <- exp(confint(pooled))
  
  # Create and return a final data frame, rounding the results for clarity and adding asterisk for p < 0.05.
  results_df <- data.frame(
    HR = round(HR_estimates, 2),
    `2.5%` = round(ci_estimates[, 1], 2),
    `97.5%` = round(ci_estimates[, 2], 2)
  ) %>% 
    mutate(`HR (95% CI)` = paste0(HR, " (", X2.5., ", ", X97.5., ")"),
           sig = case_when(X2.5. > 1 & X97.5. > 1 | X2.5. < 1 & X97.5. < 1 ~ "*",
                           T ~ "")) %>% 
    rename(lower = X2.5.,
           upper = X97.5.) %>% 
    slice(1) %>%
    rownames_to_column('Characteristic') 
  
  return(results_df)
}
####################
#Standard model
bind_rows(
  pool_and_format_results(multivariate_MICE('PFOA_scaled', "Allcausemortality")),
  
  pool_and_format_results(multivariate_MICE('PFOS_scaled', "Allcausemortality")),
  
  pool_and_format_results(multivariate_MICE('PFNA_scaled', "Allcausemortality")),
  
  pool_and_format_results(multivariate_MICE('PFHxS_scaled', "Allcausemortality"))
) %>% 
  gt() %>% 
  tab_footnote(
    "Adjusted for: survey cycle, age (with restricted cubic spline), ethnicity, gender, education, diabetes, employment status, poverty to income ratio, BMI, hypertension, chronic kidney disease and hyperlipidemia"
  ) %>% 
  tab_header(
    "Adjusted estimates from survey-featured Cox regression of PFAS exposure on all-cause mortality: NHANES cycles 1999/00 to 2017/18 (2001/02 omitted)"
  ) %>% 
  gtsave('Outputs/MICE/Adjustedbasic_MICE.docx')

######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
#Function for fitting a Cox model with interaction terms
multivariate_MICE_interaction <- function(exp, outcome, PFAS1, PFAS2, PFAS3) {
  with(design_analytic, 
       svycoxph(as.formula(
         paste(
           "Surv(permth_int, ",
           outcome,
           ") ~",
           exp,
           "+ Year + rms::rcs(Age, 4) + Gender + Ethnicity + EducationHighest + Povertytoincomeratio + Occupation2 + Diabetes + BMI_class + Hypertension + ChronicKidneyDisease + Hyperlipidemia +",
           exp, "*", PFAS1, "+",  exp, "*", PFAS2, "+",  exp, "*", PFAS3
         ))
       ))
}

#Interaction terms added
bind_rows(
  pool_and_format_results(multivariate_MICE_interaction('PFOA_scaled', "Allcausemortality", "PFOS_scaled", "PFNA_scaled", "PFHxS_scaled")),
  
  pool_and_format_results(multivariate_MICE_interaction('PFOS_scaled', "Allcausemortality", "PFOA_scaled", "PFNA_scaled", "PFHxS_scaled")),
  
  pool_and_format_results(multivariate_MICE_interaction('PFNA_scaled', "Allcausemortality", "PFOA_scaled", "PFOS_scaled", "PFHxS_scaled")),
  
  pool_and_format_results(multivariate_MICE_interaction('PFHxS_scaled', "Allcausemortality", "PFOA_scaled", "PFOS_scaled", "PFNA_scaled"))
) %>% 
  gt() %>% 
  tab_footnote(
    "Adjusted for: survey cycle, age (with restricted cubic spline), ethnicity, gender, education, diabetes, employment status, poverty to income ratio, BMI, hypertension, chronic kidney disease and hyperlipidemia"
  ) %>% 
  tab_footnote(
    "Includes PFAS*PFAS interaction terms"
  ) %>% 
  tab_header(
    "Adjusted estimates from survey-featured Cox regression of PFAS exposure on all-cause mortality: NHANES cycles 1999/00 to 2017/18 (2001/02 omitted)"
  ) %>% 
  gtsave('Outputs/MICE/Adjustedinteraction_MICE.docx')




