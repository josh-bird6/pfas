#COMPLETE CASE ANALYSIS
#Note that these functions overwrite the same-named functions elsewhere

#Function for extracting data from Cox models

multivariate <- function(exp, outcome, designobj){
  
  formula <- as.formula(paste("Surv(permth_int, ", outcome, ") ~ ", exp, "+ Year + rms::rcs(Age, 4) + Gender + Ethnicity + EducationHighest + Povertytoincomeratio + Occupation2  + BMI_class "))
  
  svycoxph(formula, design = designobj) %>% 
    tbl_regression(exponentiate = T,
                   include = exp) 
  
}

#Basic adjusted analysis for COMPLETE CASE
tbl_stack(tbls = list(
  multivariate('PFOA_scaled', "Allcausemortality", nhanes_design),
  multivariate('PFOS_scaled', "Allcausemortality", nhanes_design),
  multivariate('PFNA_scaled', "Allcausemortality", nhanes_design),
  multivariate('PFHxS_scaled', "Allcausemortality", nhanes_design)
))%>% 
  as_gt() %>%
  tab_footnote(
    "Adjusted for: survey cycle, age (with restricted cubic spline), ethnicity, gender, education, employment status, poverty to income ratio and BMI"
  ) %>% 
  tab_header(
    "Adjusted estimates from survey-featured Cox regression of PFAS exposure on all-cause mortality: NHANES cycles 1999/00 to 2017/18 (2001/02 omitted)"
  ) %>% 
  gtsave('Outputs/Mediationanalysis/Adjustedbasic_mediation_COMPLETECASE.docx')

######################################################
######################################################
######################################################
######################################################
######################################################
######################################################

#Function for extracting data from Cox models - WITH INTERACTION TERMS

multivariate_interaction <- function(exp, outcome, designobj, PFAS1, PFAS2, PFAS3){
  
  formula <- as.formula(paste("Surv(permth_int, ", outcome, ") ~ ", exp, " + Year + rms::rcs(Age, 4) + Gender + Ethnicity + EducationHighest + Povertytoincomeratio + Occupation2  + BMI_class +",
                              exp, "*", PFAS1, "+",  exp, "*", PFAS2, "+",  exp, "*", PFAS3))
  
  svycoxph(formula, design = designobj) %>% 
    tbl_regression(exponentiate = T,
                   include = exp) 
  
}

#Interaction adjusted analysis for COMPLETE CASE

tbl_stack(
  tbls = list(
    multivariate_interaction('PFOA_scaled', "Allcausemortality", nhanes_design, "PFOS_scaled", "PFNA_scaled", "PFHxS_scaled"),
    
    multivariate_interaction('PFOS_scaled', "Allcausemortality", nhanes_design, "PFOA_scaled", "PFNA_scaled", "PFHxS_scaled"),
    
    multivariate_interaction('PFNA_scaled', "Allcausemortality", nhanes_design, "PFOA_scaled", "PFOS_scaled", "PFHxS_scaled"),
    
    multivariate_interaction('PFHxS_scaled', "Allcausemortality", nhanes_design, "PFOA_scaled", "PFOS_scaled", "PFNA_scaled")
  )
) %>% 
  as_gt() %>%
  tab_footnote(
    "Adjusted for: survey cycle, age (with restricted cubic spline), ethnicity, gender, education, employment status, poverty to income ratio and BMI"
  ) %>% 
  tab_footnote(
    "Includes PFAS*PFAS interaction terms"
  ) %>% 
  tab_header(
    "Adjusted estimates from survey-featured Cox regression of PFAS exposure on all-cause mortality: NHANES cycles 1999/00 to 2017/18 (2001/02 omitted)"
  ) %>% 
  gtsave('Outputs/Mediationanalysis/Adjustedinteraction_mediation_COMPLETECASE.docx')

######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
#MICE

#First creating a function to fit regression model for EACH IMPUTATION - basic adjusted
multivariate_MICE <- function(exp, outcome) {
  with(design_analytic, 
       svycoxph(as.formula(
         paste(
           "Surv(permth_int, ",
           outcome,
           ") ~",
           exp,
           "+ Year + rms::rcs(Age, 4) + Gender + Ethnicity + EducationHighest + Povertytoincomeratio + Occupation2  +  BMI_class "
         ))
       ))
}
#####################################
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
  gtsave('Outputs/Mediationanalysis/Adjustedbasic_mediation_MICE.docx')

##############################
#Interaction
multivariate_MICE_interaction <- function(exp, outcome, PFAS1, PFAS2, PFAS3) {
  with(design_analytic, 
       svycoxph(as.formula(
         paste(
           "Surv(permth_int, ",
           outcome,
           ") ~",
           exp,
           "+ Year + rms::rcs(Age, 4) + Gender + Ethnicity + EducationHighest + Povertytoincomeratio + Occupation2 + BMI_class  +",
           exp, "*", PFAS1, "+",  exp, "*", PFAS2, "+",  exp, "*", PFAS3
         ))
       ))
}

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
  gtsave('Outputs/Mediationanalysis/Adjustedinteraction_mediation_MICE.docx')
