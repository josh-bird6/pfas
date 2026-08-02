
#subgroup analysis - sex


gender_groups <- unique(nhanes_design$variables$gender_subgroup)

#Defining base formula BASIC COMPLETE CASE
multivariate_subgroup <- function(exp, outcome){
  
  formula <- as.formula(paste("Surv(permth_int, ", outcome, ") ~ ", exp, "+ Year + rms::rcs(Age, 4)  + Ethnicity + EducationHighest + Povertytoincomeratio + Occupation2  + Diabetes + BMI_class + Hypertension + ChronicKidneyDisease + Hyperlipidemia"))
  
}


#looping through each group and extracting hazard ratios
all_exposures_results <- lapply(exposures_list, function(current_exp) {
  
  # Inner Loop: Iterate through each age group for the current exposure
  gender_results <- lapply(gender_groups, function(group) {
    
    #Subset design for specific age group
    sub_design <- subset(nhanes_design, gender_subgroup == group)
    
    # Generate formula for this specific exposure
    current_formula <- multivariate_subgroup(current_exp, "Allcausemortality")
    
    fit <- svycoxph(current_formula, design = sub_design)
    
    sum_fit <- summary(fit)
    coefs <- sum_fit$coefficients
    
    hr_results <- data.frame(
      Gender_group = group,
      Variable = rownames(coefs),
      HR = exp(coefs[, "coef"]),
      CI_Lower = exp(coefs[, "coef"] - qnorm(0.975) * coefs[, "robust se"]),
      CI_Upper = exp(coefs[, "coef"] + qnorm(0.975) * coefs[, "robust se"])
    )
    
    # Filter immediately for only the row matching the current exposure variable
    filtered_hr <- hr_results %>% dplyr::filter(Variable == current_exp)
    
    return(filtered_hr)
    
  })
  
  # Concatenate the age groups for this specific exposure
  return(dplyr::bind_rows(gender_results))
}) 

subgroup_analysis_BASIC_COMPLETECASE_gender <- bind_rows(all_exposures_results) %>% 
  remove_rownames()

####################################
####################################
####################################
####################################
####################################
####################################

multivariate_subgroup_INTERACTION <- function(exp, outcome, PFAS1, PFAS2, PFAS3){
  
  formula <- as.formula(paste("Surv(permth_int, ", outcome, ") ~ ", exp, "+ Year + rms::rcs(Age, 4) + Ethnicity + EducationHighest + Povertytoincomeratio + Occupation2  + Diabetes + BMI_class + Hypertension + ChronicKidneyDisease + Hyperlipidemia +",
                              exp, "*", PFAS1, "+",  exp, "*", PFAS2, "+",  exp, "*", PFAS3))
  
}

#Perhaps the clunkiest bit of code ever

subgroup_analysis_BASIC_interaction_gender <- bind_rows(
  
  #PFOA
  bind_rows(
    
    summary(svycoxph(multivariate_subgroup_INTERACTION('PFOA_scaled', 'Allcausemortality', "PFOS_scaled", "PFNA_scaled", "PFHxS_scaled"), subset(nhanes_design, gender_subgroup == 'Male')))$coefficients %>% 
      data.frame() %>% 
      slice(1) %>% 
      mutate(Gender_group = 'Male'),
    
    summary(svycoxph(multivariate_subgroup_INTERACTION('PFOA_scaled', 'Allcausemortality', "PFOS_scaled", "PFNA_scaled", "PFHxS_scaled"), subset(nhanes_design, gender_subgroup == 'Pre-menopausal female')))$coefficients %>% 
      data.frame() %>% 
      slice(1)  %>% 
      mutate(Gender_group = 'Pre-menopausal female'),
    
    summary(svycoxph(multivariate_subgroup_INTERACTION('PFOA_scaled', 'Allcausemortality', "PFOS_scaled", "PFNA_scaled", "PFHxS_scaled"), subset(nhanes_design, gender_subgroup == 'Post-menopausal female')))$coefficients %>% 
      data.frame() %>% 
      slice(1)  %>% 
      mutate(Gender_group = 'Post-menopausal female') 
  ) %>% 
    mutate(Variable = 'PFOA_scaled'),
  
  #PFOS
  bind_rows(
    
    summary(svycoxph(multivariate_subgroup_INTERACTION('PFOS_scaled', 'Allcausemortality', "PFOA_scaled", "PFNA_scaled", "PFHxS_scaled"), subset(nhanes_design, gender_subgroup == 'Male')))$coefficients %>% 
      data.frame() %>% 
      slice(1) %>% 
      mutate(Gender_group = 'Male'),
    
    summary(svycoxph(multivariate_subgroup_INTERACTION('PFOS_scaled', 'Allcausemortality', "PFOA_scaled", "PFNA_scaled", "PFHxS_scaled"), subset(nhanes_design, gender_subgroup == 'Pre-menopausal female')))$coefficients %>% 
      data.frame() %>% 
      slice(1)  %>% 
      mutate(Gender_group = 'Pre-menopausal female') ,
      
      summary(svycoxph(multivariate_subgroup_INTERACTION('PFOS_scaled', 'Allcausemortality', "PFOA_scaled", "PFNA_scaled", "PFHxS_scaled"), subset(nhanes_design, gender_subgroup == 'Post-menopausal female')))$coefficients %>% 
      data.frame() %>% 
      slice(1)  %>% 
      mutate(Gender_group = 'Post-menopausal female') 
  ) %>% 
    mutate(Variable = 'PFOS_scaled'),
  
  #PFNA
  bind_rows(
    
    summary(svycoxph(multivariate_subgroup_INTERACTION('PFNA_scaled', 'Allcausemortality', "PFOA_scaled", "PFOS_scaled", "PFHxS_scaled"), subset(nhanes_design, gender_subgroup == 'Male')))$coefficients %>% 
      data.frame() %>% 
      slice(1) %>% 
      mutate(Gender_group = 'Male'),
    
    summary(svycoxph(multivariate_subgroup_INTERACTION('PFNA_scaled', 'Allcausemortality', "PFOA_scaled", "PFOS_scaled", "PFHxS_scaled"), subset(nhanes_design, gender_subgroup == 'Pre-menopausal female')))$coefficients %>% 
      data.frame() %>% 
      slice(1)  %>% 
      mutate(Gender_group = 'Pre-menopausal female'),
    
    summary(svycoxph(multivariate_subgroup_INTERACTION('PFNA_scaled', 'Allcausemortality', "PFOA_scaled", "PFOS_scaled", "PFHxS_scaled"), subset(nhanes_design, gender_subgroup == 'Post-menopausal female')))$coefficients %>% 
      data.frame() %>% 
      slice(1)  %>% 
      mutate(Gender_group = 'Post-menopausal female') 
  ) %>% 
    mutate(Variable = 'PFNA_scaled'),
  
  #PFHxX
  bind_rows(
    
    summary(svycoxph(multivariate_subgroup_INTERACTION('PFHxS_scaled', 'Allcausemortality', "PFOA_scaled", "PFOS_scaled", "PFNA_scaled"), subset(nhanes_design, gender_subgroup == 'Male')))$coefficients %>% 
      data.frame() %>% 
      slice(1) %>% 
      mutate(Gender_group = 'Male'),
    
    summary(svycoxph(multivariate_subgroup_INTERACTION('PFHxS_scaled', 'Allcausemortality', "PFOA_scaled", "PFOS_scaled", "PFNA_scaled"), subset(nhanes_design, gender_subgroup == 'Pre-menopausal female')))$coefficients %>% 
      data.frame() %>% 
      slice(1)  %>% 
      mutate(Gender_group = 'Pre-menopausal female'),
    
    summary(svycoxph(multivariate_subgroup_INTERACTION('PFHxS_scaled', 'Allcausemortality', "PFOA_scaled", "PFOS_scaled", "PFNA_scaled"), subset(nhanes_design, gender_subgroup == 'Post-menopausal female')))$coefficients %>% 
      data.frame() %>% 
      slice(1)  %>% 
      mutate(Gender_group = 'Post-menopausal female') 
  ) %>% 
    mutate(Variable = 'PFHxS_scaled')
  
) %>% 
  mutate(HR = exp.coef.,
         CI_Lower = exp(coef - qnorm(0.975) * robust.se),
         CI_Upper = exp(coef + qnorm(0.975) * robust.se)) %>% 
  select(Gender_group, Variable, HR, CI_Lower, CI_Upper)

#####

bind_rows(
  
  (subgroup_analysis_BASIC_COMPLETECASE_gender %>% 
     mutate(cat = 'Basic adjusted')),
  
  (subgroup_analysis_BASIC_interaction_gender %>% 
     mutate(cat = 'Interaction'))
) %>% 
  remove_rownames() %>% 
  write_csv('Outputs/Subgroup/gender.csv')
