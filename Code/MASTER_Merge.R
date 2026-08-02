##################################################
#NOW THE FINAL MERGE!

FINAL_BASEDATASET <- bind_rows(
  `1999-00`,
  `2003-04`,
  `2005-06`,
  `2007-08`,
  `2009-10`,
  `2011-12`,
  `2013-14`,
  `2015-16`,
  `2017-18`
) %>%                           #(n = 19,307)
  #defining causes of death (i.e. outcome)
  mutate(
    `Cause of death` = case_when(
      ucod_leading == 1 ~ "Diseases of heart",
      ucod_leading == 2 ~ "Malignant neoplasms",
      ucod_leading == 3 ~ "Chronic lower respiratory diseases",
      ucod_leading == 4 ~ "Accidents (unintentional injuries)",
      ucod_leading == 5 ~ "Cerebrovascular diseases",
      ucod_leading == 6 ~ "Alzheimer’s disease",
      ucod_leading == 7 ~ "Diabetes mellitus",
      ucod_leading == 8 ~ "Influenza and pneumonia",
      ucod_leading == 9 ~ "Nephritis, nephrotic syndrome and nephrosis",
      ucod_leading == 10 ~ "All other causes"
    ),
    #Calibrating weight based on number of survey cycles
    Weight_pool = Weight/9,
    
    #NOW FOR SOME WRANGLING
    #First we are going to transform the exposure scales. They are heavily right-skewed, so going to log transform and then scale (mean = 0, SD = 1)
    PFOA_scaled = as.numeric(scale(log(PFOA))),
    PFOS_scaled = as.numeric(scale(log(PFOS))),
    PFNA_scaled = as.numeric(scale(log(PFNA))),
    PFHxS_scaled = as.numeric(scale(log(PFHxS))),
    #Miscellaneous variable formatting
    Ethnicity = case_when(Ethnicity == 'Other Hispanic' | Ethnicity == 'Mexican American' ~ 'Hispanic',
                          T ~ Ethnicity), 
    Education = str_to_lower(Education),
    Education = case_when(
      Education %in% c(
        "9-11th grade (includes 12th grade with no diploma)",
        "less than 9th grade"
      ) ~ 'Some high school or below',
      Education %in% c(
        'high school graduate/ged or equivalent',
        'high school grad/ged or equivalent'
      ) ~ 'High School/GED or equivalent',
      T ~ Education
    ),
    Education = str_to_title(Education),
    Education2 = str_to_lower(Education2),
    Education3 = case_when(Education2 == 'high school graduate' | Education2 == 'ged or equivalent' ~ 'High School/Ged Or Equivalent',
                           Education2 == 'more than high school' ~ 'Some College Or Aa Degree',
                           Education2 == '9th grade' | Education2 =='10th grade' | Education2 =='11th grade' | Education2 =='12th grade, no diploma' | Education2 =='less than 9th grade' ~ 'Some High School Or Below'),
    EducationHighest = case_when(Age %in% c(18,19) ~ Education3,
                                 Age >=20~ Education),
    BMI_class = case_when(BMI < 25 ~ 'Normal weight',
                          BMI >= 25 & BMI < 30 ~ 'Overweight',
                          BMI >= 30 ~ 'Obese'), 
    Menopause2 = case_when(Menopause == 'No' | Gender == 'Female' & Age >= 60 ~ "Yes",
                           Menopause == 'Yes' | Gender == 'Female' & Age < 60 ~ "No"), 
    Occupation2 = case_when(Occupation == 'Working at a job or business,' ~ 'Employed',
                            Occupation_unemp == 'Taking care of house or family' ~ 'Domestic caregiver',
                            Occupation_unemp == 'Unable to work for health reasons' | Occupation_unemp == 'Disabled' ~ 'Disabled',
                            Occupation_unemp == 'Retired' ~ 'Retired',
                            Occupation == 'With a job or business but not at work,' | Occupation == "Looking for work, or" | Occupation == 'Not working at a job or business?' | Occupation_unemp == 'Going to school' ~ 'Not employed'),
    # Occupation2 = case_when(Occupation == 'Working at a job or business,' ~ 'Employed', Occupation == 'With a job or business but not at work,' | Occupation == "Looking for work, or" | Occupation == 'Not working at a job or business?' ~ 'Not employed',
    #                         T ~ Occupation), 
    #Smoking status, derived from two variables
    SmokingStatus = case_when(Smoking == 'No' ~ 'Never smoked',
                              str_detect(SmokingCurrent, 'Every day') ~ 'Smokes every day',
                              str_detect(SmokingCurrent, 'Some days') ~ 'Smokes some days',
                              str_detect(SmokingCurrent, 'Not at all') ~ 'Former Smoker'),
    #Hyperlipidemia variable
    Hyperlipidemia = case_when(Cholesterol >= 200 ~ "Yes",
                               Cholesterol <200 ~ 'No'),
    #Chronic kidney disease - see function in Functions.R
    eGFR = eGFR_rate(Kidney, Age, Gender),
    ChronicKidneyDisease = case_when(eGFR < 60 ~ "Yes",
                                     eGFR >= 60 ~ 'No'),
    #Replacing all "Refused" and 'Don't know' answers with NA
    across(c(EducationHighest, Diabetes, Occupation2, Diet, Smoking, Heartdisease, Menopause), na_if, "Refused"),
    across(c(Diabetes, Occupation2, Diet, Hypertension, Smoking, Heartdisease, Menopause), na_if, "Don't know"),
    across(c(EducationHighest), na_if, "Don't Know"),
    #Some individuals do not have a weight (n=33) so these are coerced to 0 - they will be filtered out anyway because they have no exposure data
    Weight_pool = case_when(is.na(Weight_pool) ~ 0,
                            T ~ Weight_pool),
    Year2 = case_when(Year == '1999-00' | Year == "2003-04" | Year == "2005-06" | Year == "2007-08" ~ "1999 to 2008",
                      T ~ "2009 to 2018"),
    #Finally, creating a binary flag for all cause mortality (to be explored in secondary analysis)
    Allcausemortality = case_when(!is.na(`Cause of death`) ~ 1,
                                  T ~ 0),
    Age_category = case_when(Age >= 18 & Age <40 ~ "18-39",
                             Age >=40 & Age <60 ~ "40-59",
                             Age >=60 & Age <80 ~ "60-79",
                             T ~ '80 and up'),
    Age_binary = case_when(Age >= 65 ~ "65 and over",
                           T ~ "Under 65"),
    gender_subgroup = case_when(Gender == 'Male' ~ 'Male',
                                Menopause2 == 'No' ~ 'Pre-menopausal female',
                                Menopause2 == 'Yes' ~ 'Post-menopausal female'),
    #Bunch of releveling
    Occupation2 = relevel(as.factor(Occupation2), ref = "Not employed"),
    Ethnicity = relevel(as.factor(Ethnicity), ref = 'Non-Hispanic White'),
    EducationHighest = factor(EducationHighest, levels = c("Some High School Or Below", "High School/Ged Or Equivalent", "Some College Or Aa Degree", "College Graduate Or Above")),
    SmokingStatus = factor(SmokingStatus, levels = c("Never smoked", "Former Smoker", "Smokes some days", "Smokes every day")),
    Diabetes = relevel(as.factor(Diabetes), ref = 'No'),
    Hypertension = relevel(as.factor(Hypertension), ref = 'No'),
    Heartdisease = relevel(as.factor(Heartdisease), ref = 'No'),
    Diet = relevel(as.factor(Diet), ref = 'No'),
    Hyperlipidemia = relevel(as.factor(Hyperlipidemia), ref = 'No'),
    ChronicKidneyDisease = relevel(as.factor(ChronicKidneyDisease), ref = 'No'),
    gender_subgroup = factor(gender_subgroup, levels = c('Male', 'Pre-menopausal female', 'Post-menopausal female')),
  ) %>% 
  select(permth_int, Allcausemortality, Year2, Year, SEQN, PFOA, PFOA_scaled, PFOS, PFOS_scaled, PFNA, PFNA_scaled, PFHxS, PFHxS_scaled, Age, Age_category, Age_binary, Gender, gender_subgroup, Ethnicity, EducationHighest, Income, Occupation2, Diet, Diabetes, BMI_class, Hypertension, ChronicKidneyDisease, Hyperlipidemia, Weight_pool, PseudoPSU, PseudoStratum, mortstat) %>% 
  rename(`Povertytoincomeratio` = Income) %>% 
  mutate() 


#At this point our sample is 19,307
#Dropping unused levels from factor variables
FINAL_BASEDATASET <- droplevels(FINAL_BASEDATASET)
