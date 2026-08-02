#missingness by variable
unlist(lapply(FINAL_BASEDATASET, function(x) sum(is.na(x))))/nrow(FINAL_BASEDATASET) 


FINAL_BASEDATASET_regression <- FINAL_BASEDATASET %>% #n=19,307
  #exclusion criteria
  filter(Age >= 18,               #filtering out all observations under 18y/o (n = 16,044),        
         !is.na(PFOA),            #filtering out all observations with missing exposure data (individuals missing one measurement are missing them all, n = 14,907) 
         !is.na(mortstat))        #FINAL N = 14,876


#Survey design object WITH ENTIRE ANALYTIC DATASET
FINAL_BASEDATASET <- 
  FINAL_BASEDATASET %>% 
  mutate(miss = 1)

#adding 0 for folk who do not meet exclusion criteria
FINAL_BASEDATASET$miss[FINAL_BASEDATASET$SEQN %in% FINAL_BASEDATASET_regression$SEQN] <- 0

#defining complex survey object
nhanes_design0 <- svydesign(
  id = ~ PseudoPSU,
  strata = ~ PseudoStratum,
  weights = ~ Weight_pool,
  data = FINAL_BASEDATASET,
  nest = T
)

#removing individuals who are missing
nhanes_design <- subset(nhanes_design0, miss == 0)

#checking missingness
unlist(lapply(FINAL_BASEDATASET_regression, function(x) sum(is.na(x))))/nrow(FINAL_BASEDATASET_regression) 
