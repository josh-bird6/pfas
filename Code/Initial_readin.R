#################################################
#Reading in mortality data by year
data_mort_9900 <- readfun('1999_2000')
data_mort_0304 <- readfun('2003_2004')
data_mort_0506 <- readfun('2005_2006')
data_mort_0708 <- readfun('2007_2008')
data_mort_0910 <- readfun('2009_2010')
data_mort_1112 <- readfun('2011_2012')
data_mort_1314 <- readfun('2013_2014')
data_mort_1516 <- readfun('2015_2016')
data_mort_1718 <- readfun('2017_2018')

#################################################
#1999-2000
`1999-00` <- left_join(nhanes('SSPFC_A'), nhanes('DEMO'), by = 'SEQN') %>%  #joining lab and demo data
  left_join(nhanes('DIQ'), by = 'SEQN') %>%         #joining diabetes data (questionnaire)
  left_join(nhanes('BMX'), by = 'SEQN') %>%         #joining BMI data (exam)
  left_join(nhanes('SMQ'), by = 'SEQN') %>%         #joining smoking data (questionnaire)
  left_join(nhanes('BPQ'), by = 'SEQN') %>%         #joining hypertension data (questionnaire)
  left_join(nhanes('RHQ'), by = 'SEQN') %>%         #joining menstrual cycle data (questionnaire)
  left_join(nhanes('MCQ'), by = 'SEQN') %>%         #joining heart disease data (questionnaire)
  left_join(nhanes('LAB18'), by = 'SEQN') %>%       #joining kidney data (laboratory data)
  left_join(nhanes('OCQ'), by = 'SEQN') %>%         #joining occupation data (exam)
  left_join(nhanes("DRXTOT"), by = 'SEQN') %>%      #joining diet information (exam)
  left_join(nhanes('LAB13'), by = 'SEQN') %>%       #joining cholesterol information (laboratory data)
  rename(Weight = WTMEC2YR,
         PFOA = SPFOA,
         PFOS = SPFOS,
         PFNA = SPFNA,
         PFHxS = SPFHS,
         Gender = RIAGENDR,
         Age = RIDAGEYR,
         Ethnicity = RIDRETH1,
         Education = DMDEDUC2,
         Education2 = DMDEDUC3,
         Income = INDFMPIR,
         Diabetes = DIQ010,
         BMI = BMXBMI, 
         Smoking = SMQ020,
         SmokingCurrent = SMQ040,
         Hypertension = BPQ020,
         Menopause = RHQ030,
         Heartdisease = MCQ160C,
         Kidney = LBXSCR,
         Cholesterol = LBXTC,
         Occupation = OCQ150,
         Occupation_unemp = OCQ380,
         Diet = DRQ360,
         Pregnancy = RIDEXPRG,
         PseudoPSU = SDMVPSU,
         PseudoStratum = SDMVSTRA) %>% 
  #creating year variable
  mutate(Year = '1999-00') %>%
  #selecting relevant variables
  select(Year, SEQN, PFOA, PFOS, PFNA, PFHxS, Gender, Age, Ethnicity, Education, Education2, Income, Diabetes, BMI, Smoking, SmokingCurrent, Hypertension, Menopause, Heartdisease, Kidney, Cholesterol, Pregnancy, Occupation, Occupation_unemp, Diet, Weight, PseudoPSU, PseudoStratum) %>% 
  #joining mortality data
  left_join(data_mort_9900, by = 'SEQN')

#######################################
#Extracting data for 2003-04 through 2011-12
`2003-04` <- extractfun('L24PFC_C', 'DEMO_C', 'DIQ_C', 'BMX_C', 'SMQ_C', 'SMQ_C', 'BPQ_C', 'RHQ_C', 'MCQ_C', 'L40_C', 'l13_c', "OCQ_C", "DR1TOT_C", 'WTSA2YR', "2003-04", data_mort_0304) 

`2005-06` <- extractfun('PFC_D', 'DEMO_D', 'DIQ_D', 'BMX_D', 'SMQ_D', 'SMQ_D', 'BPQ_D', 'RHQ_D', 'MCQ_D', 'BIOPRO_D', 'TCHOL_D', "OCQ_D", 'DR1TOT_D', 'WTSA2YR', '2005-06', data_mort_0506)

`2007-08` <- extractfun('PFC_E', 'DEMO_E', 'DIQ_E', 'BMX_E', 'SMQ_E', 'SMQ_E', 'BPQ_E', 'RHQ_E', 'MCQ_E', 'BIOPRO_E', 'TCHOL_E', "OCQ_E", 'DR1TOT_E', "WTSC2YR", '2007-08', data_mort_0708)

`2009-10` <- extractfun('PFC_F', 'DEMO_F', 'DIQ_F', 'BMX_F', 'SMQ_F', 'SMQ_F', 'BPQ_F', 'RHQ_F', 'MCQ_F', 'BIOPRO_F', 'TCHOL_F', "OCQ_F", 'DR1TOT_F', "WTSC2YR", '2009-10', data_mort_0910)

`2011-12` <- extractfun('PFC_G', 'DEMO_G', 'DIQ_G', 'BMX_G', 'SMQ_G', 'SMQ_G', 'BPQ_G', 'RHQ_G', 'MCQ_G', 'BIOPRO_G', 'TCHOL_G', "OCQ_G", 'DR1TOT_G', "WTSA2YR", '2011-12', data_mort_1112)

######################
######################
######################
######################
######################
######################
#2013/14 is where it gets very sketchy

#first we need to grab the PFAS data for the four chemicals, which are located in two different survey modules
#Then combine measurements for *PFOA and PFOS ONLY*

`2013-14` <- left_join(
  #First for PFOA and PFOS
  (nhanes('SSPFAS_H') %>% 
     mutate(PFOA = SSNPFOA + SSBPFOA,
            PFOS = SSNPFOS + SSMPFOS) %>% 
     select(SEQN, WTSSBH2Y, PFOA, PFOS)),
  #Then for PFNA, PFHxS, which are single measurements (do not require combining)
  (nhanes('PFAS_H') %>%
     rename(PFNA = LBXPFNA,
            PFHxS = LBXPFHS) %>% 
     select(SEQN, PFNA, PFHxS)),
  by = 'SEQN'
) %>% 
  left_join(nhanes('DEMO_H'), by = 'SEQN') %>%        #joining sociodemographic data
  left_join(nhanes('DIQ_H'), by = 'SEQN') %>%         #joining diabetes data (questionnaire)
  left_join(nhanes('BMX_H'), by = 'SEQN') %>%         #joining BMI data (exam)
  left_join(nhanes('SMQ_H'), by = 'SEQN') %>%         #joining smoking data (laboratory)
  left_join(nhanes('BPQ_H'), by = 'SEQN') %>%         #joining hypertension data (questionnaire)
  left_join(nhanes('RHQ_H'), by = 'SEQN') %>%         #joining menstrual cycle data (questionnaire)
  left_join(nhanes('MCQ_H'), by = 'SEQN') %>%         #joining heart disease data (questionnaire)
  left_join(nhanes('BIOPRO_H'), by = 'SEQN') %>%      #joining kidney data (laboratory data)
  left_join(nhanes('OCQ_H'), by = 'SEQN') %>%         #joining occupation data (exam)
  left_join(nhanes('DR1TOT_H'), by = 'SEQN') %>%      #joining diet information (exam)
  left_join(nhanes('TCHOL_H'), by = 'SEQN') %>%       #joining cholesterol information (laboratory)
  rename(Weight = WTSSBH2Y,
         Gender = RIAGENDR,
         Age = RIDAGEYR,
         Ethnicity = RIDRETH1,
         Education = DMDEDUC2,
         Education2 = DMDEDUC3,
         Income = INDFMPIR,
         Diabetes = DIQ010,
         BMI = BMXBMI, 
         Smoking = SMQ020,
         SmokingCurrent = SMQ040,
         Hypertension = BPQ020,
         Menopause = RHQ031,
         Heartdisease = MCQ160C,
         Kidney = LBXSCR,
         Cholesterol = LBXTC,
         Occupation = OCD150,
         Occupation_unemp = OCQ380,
         Diet = DRD360,
         Pregnancy = RIDEXPRG,
         PseudoPSU = SDMVPSU,
         PseudoStratum = SDMVSTRA) %>% 
  mutate(Year = '2013-14') %>% 
  select(Year, SEQN, PFOA, PFOS, PFNA, PFHxS, Gender, Age, Ethnicity, Education, Education2, Income, Diabetes, BMI, Smoking, SmokingCurrent, Hypertension, Menopause, Heartdisease, Kidney, Cholesterol, Pregnancy, Occupation, Occupation_unemp, Diet,  Weight, PseudoPSU, PseudoStratum) %>% 
  left_join(data_mort_1314, by = 'SEQN')
######################
######################
######################
######################
######################
######################

#2015/16 and 2017/18

`2015-16` <- extractfun_latter('PFAS_I', 'DEMO_I', 'DIQ_I', 'BMX_I', 'SMQ_I', 'BPQ_I', 'RHQ_I', 'MCQ_I', 'BIOPRO_I', 'TCHOL_I', "OCQ_I", 'DR1TOT_I', "2015-16", data_mort_1516)

`2017-18` <- extractfun_latter('PFAS_J', 'DEMO_J', 'DIQ_J', 'BMX_J', 'SMQ_J', 'BPQ_J', 'RHQ_J', 'MCQ_J', 'BIOPRO_J', 'TCHOL_J', "OCQ_J", "DR1TOT_J", "2017-18", data_mort_1718)

