#tables

varsofinterest <- c('PFOA', 'PFOS', 'PFNA', 'PFHxS', 'Year', "Age", "Gender", "Ethnicity", "EducationHighest", "Povertytoincomeratio", 'Occupation2', 'Diabetes', 'BMI_class', 'Hypertension', 'ChronicKidneyDisease', 'Hyperlipidemia')

#COUNTS
write.csv(print(
  CreateTableOne(
    vars = varsofinterest,
    strata = 'Allcausemortality',
    data = FINAL_BASEDATASET_regression,
    test = F,
    addOverall = T,
    includeNA = T
  ),
  showAllLevels = F,
  print = T,
  format = "f",
  nonnormal = c('PFOA', "PFOS", 'PFNA', 'PFHxS')
),
'Outputs/Table1/Table1_COUNTS.csv')

#PERCENTAGE
write.csv(
  print(
    svyCreateTableOne(
      vars = varsofinterest,
      strata = 'Allcausemortality',
      data = nhanes_design,
      test = F,
      includeNA = T,
      addOverall = T
    ),
    print = T,
    format = 'p',
    nonnormal = c('PFOA', "PFOS", 'PFNA', 'PFHxS')
  ),
  'Outputs/Table1/Table1_PERCENTAGE.csv'
)
