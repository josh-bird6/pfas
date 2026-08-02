#Script for functions
#NOTE: This does not contain certain analysis-specific functions, which are included in analytic scripts 

#Package installation
library(tidyverse)
library(nhanesA)
library(tableone)
library(survival)
library(ggsurvfit)
library(survey)
library(gtsummary)
library(gt)
library(mice)

#Creating function to read in all the mortality data
#Downloaded directly from the CDC website: https://ftp.cdc.gov/pub/Health_Statistics/NCHS/datalinkage/linked_mortality/
readfun <- function(df) {
  read_fwf(
    paste0('Data/NHANES_', df, '_MORT_2019_PUBLIC.dat'),
    col_types = 'iiiiiiii',
    fwf_cols(
      SEQN = c(1, 6),
      eligstat = c(15, 15),
      mortstat = c(16, 16),
      ucod_leading = c(17, 19),
      diabetes = c(20, 20),
      hyperten = c(21, 21),
      permth_int = c(43, 45),
      permth_exm = c(46, 48)
    ),
    na = c('', '.')
  )
}
######################################################
#Function for extracting data ONLY FOR FIRST 5 WAVES OF TIME SERIES (2013/14 to 2011/12)

#Calls for fourteen arguments: 
#the year of laboratory data; year of demographic data; diabetes (questionnaire), BMI (exam), smoking (exam), hypertension (questionnaire), menstraul cycle (questionnaire), heart disease (quesitonnaire), kidney (laboratory), occupation (exam), diet (exam); survey weights FROM THE LABORATORY DATA (as per NHANES documentation); the year, and relevant year of mortality data

extractfun <- function(lab, demo, diabetes, BMI, Smoking, SmokingCurrent, Hypertension, Menopause, Heartdisease, Kidney, Cholesterol, Occupation, Diet, weight, year, mortdat){
  left_join(nhanes(lab), nhanes(demo), by = 'SEQN') %>%  #joining lab and demo data
    left_join(nhanes(diabetes), by = 'SEQN') %>%         #joining diabetes data (questionnaire)
    left_join(nhanes(BMI), by = 'SEQN') %>%              #joining BMI data (exam)
    left_join(nhanes(Smoking), by = 'SEQN') %>%          #joining smoking data (questionnaire)
    left_join(nhanes(Hypertension), by = 'SEQN') %>%     #joining hypertension data (questionnaire)
    left_join(nhanes(Menopause), by = 'SEQN') %>%        #joining menstrual cycle data (questionnaire)
    left_join(nhanes(Heartdisease), by = 'SEQN') %>%     #joining heart disease data (questionnaire)
    left_join(nhanes(Kidney), by = 'SEQN') %>%           #joining kidney data (laboratory data)
    left_join(nhanes(Occupation), by = 'SEQN') %>%       #joining occupation data (exam)
    left_join(nhanes(Diet), by = 'SEQN') %>%             #joining diet information (exam)
    left_join(nhanes(Cholesterol), by = 'SEQN') %>%      #joining cholesterol information (laboratory data)
    rename(Weight = weight,
           PFOA = LBXPFOA,
           PFOS = LBXPFOS,
           PFNA = LBXPFNA,
           PFHxS = LBXPFHS,
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
    #creating year variable
    mutate(Year = year) %>%
    #selecting relevant variables
    select(Year, SEQN, PFOA, PFOS, PFNA, PFHxS, Gender, Age, Ethnicity, Education, Education2, Income, Diabetes, BMI, Smoking, SmokingCurrent, Hypertension, Menopause, Heartdisease, Kidney, Cholesterol, Pregnancy, Occupation, Occupation_unemp, Diet, Weight, PseudoPSU, PseudoStratum) %>% 
    #joining mortality data
    left_join(mortdat, by = 'SEQN')
}



######################################################
#2013/14 is a faff - see Base_wranglingR.R script

#2015/16 and 2017/18 are more straightforward, but require a separate function (because the exposures are not formatted the exact same as in previous years)

extractfun_latter <- function(lab, demo, diabetes, BMI, Smoking, Hypertension, Menopause, Heartdisease, Kidney, Cholesterol, Occupation, Diet, year, mortdata) {
  
  left_join(nhanes(lab), nhanes(demo), by = 'SEQN') %>% 
    left_join(nhanes(diabetes), by = 'SEQN') %>%         #joining diabetes data (questionnaire)
    left_join(nhanes(BMI), by = 'SEQN') %>%              #joining BMI data (exam)
    left_join(nhanes(Smoking), by = 'SEQN') %>%          #joining smoking data (laboratory)
    left_join(nhanes(Hypertension), by = 'SEQN') %>%     #joining hypertension data (questionnaire)
    left_join(nhanes(Menopause), by = 'SEQN') %>%        #joining menstrual cycle data (questionnaire)
    left_join(nhanes(Heartdisease), by = 'SEQN') %>%     #joining heart disease data (questionnaire)
    left_join(nhanes(Kidney), by = 'SEQN') %>%           #joining kidney data (laboratory data)
    left_join(nhanes(Occupation), by = 'SEQN') %>%       #joining occupation data (exam)
    left_join(nhanes(Diet), by = 'SEQN') %>%             #joining diet information (exam)
    left_join(nhanes(Cholesterol), by = 'SEQN') %>%      #joining cholesterol data (exam)
    #Summing PFOA and PFOS (because they are disaggregated)
    mutate(PFOA = LBXNFOA + LBXBFOA,
           PFOS = LBXNFOS + LBXMFOS,
           Year = year) %>% 
    rename(Weight = WTSB2YR,
           PFNA = LBXPFNA,
           PFHxS = LBXPFHS,
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
    select(Year, SEQN, PFOA, PFOS, PFNA, PFHxS, Gender, Age, Ethnicity, Education, Education2, Income, Diabetes, BMI, Smoking, SmokingCurrent, Hypertension, Menopause, Heartdisease, Kidney, Cholesterol, Pregnancy, Occupation, Occupation_unemp, Diet, Weight, PseudoPSU, PseudoStratum) %>% 
    #Joining mortality data
    left_join(mortdata, by = 'SEQN') 
}
######################################################
#Defining funciton for calculating glomerular filtration rate, used to assess presence of chronic kidney disease 
eGFR_rate <- function(creatinine, age, sex) {
  
  # define constant for sex
  k <- ifelse(sex == "Female", 0.7, 0.9)
  a <- ifelse(sex == "Female", -0.241, -0.302)
  factor_sex <- ifelse(sex == "Female", 1.012, 1)
  
  # Calculate eGFR
  egfr <- 142 * (pmin(creatinine / k, 1) ^ a) * 
    (pmax(creatinine / k, 1) ^ -1.200) * 
    (0.9938 ^ age) * factor_sex
  
  return(egfr)
}


######################################################
#EXPOSURE TABLE

#Defining function which extracts and formats exposure data for each analyte
exposure_extraction_fun <- function(Analyte, PFAS){
  
  q1 <- quantile(Analyte, 0.25) %>% data.frame()
  med <- median(Analyte) %>% data.frame()
  q3 <- quantile(Analyte, 0.75) %>% data.frame()
  mini <- min(Analyte) %>% data.frame()
  maxi <- max(Analyte) %>% data.frame()
  
  bind_cols(med, q1,q3, mini, maxi) %>%
    mutate(`Median [1Q-3Q]` = paste0(round(....1, 2), " (", ....2, '-', `....3`, ")"),
           Substance = PFAS) %>%
    rename(Min =  `....4`,
           Max = `....5`) %>%
    select(Substance, `Median [1Q-3Q]`, Min, Max)
  
}

#############################
#Function for creating forest plot
forestplot_fun <- function(df, OUTCOME){
  df %>% 
    filter(outcome == OUTCOME) %>% 
    mutate(cat3 = case_when(cat == 'Univariate' ~ cat,
                            T~ cat3),
           `Missingness Treatment` = as.factor(`Missingness Treatment`),
           `Missingness Treatment` = fct_relevel(`Missingness Treatment`, c('Univariate Analysis \n(n = 13,798)',
                                                                            'Complete Case \n(n = 10,630)',
                                                                            'MICE \n(n = 13,798)'))) %>% 
    ggplot(aes(x = Characteristic, y = HR, ymin = lower, ymax = upper,  color = `Missingness Treatment`, linetype = `Missingness Treatment`)) +
    
    #geom_linerange(position = position_dodge2(width = 0.5, reverse = 2), size = 1, key_glyph = 'path') +
    geom_errorbar(position = position_dodge2(width = 0.5, reverse = 2), key_glyph = 'path') +
    scale_linetype_manual(values = c('solid', 'solid', 'solid')) +
    geom_point(position = position_dodge2(width = 1, reverse = T), size = 2) +
    scale_color_manual(values = c('#66c2a5','#fc8d62', '#8da0cb'))+
    facet_wrap(~factor(cat3, levels = c('Univariate', 'Multivariate Adjusted', 'Multivariate Adjusted + Interaction')), nrow = 3) +
    geom_hline(yintercept = 1) +
    coord_flip() +
    theme_bw()+
    theme(legend.position = 'bottom') +
    scale_y_continuous(limits = c(.68, 1.7)) + 
    labs(title = OUTCOME) +
    geom_text(aes(label = paste0("HR: ", round(HR,2), " [CI: ", round(lower,2), ", ", round(upper,2), "]")), y = 1.44, position = position_dodge2(width = 0.9, reverse = T), hjust = 0, show.legend = F) 
}
