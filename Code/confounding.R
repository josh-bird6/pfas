
library(EValue)

#only PFOA was significant in MICE interaction models, therefore e-values only need to be calculated for this compound
#rare disease assumption holds (1,980 deaths out of 14,876 sample ≈ 13%)
evalues.HR(est = .87, lo = .78, hi = .96, rare = T)

#Wald test to juxtapose the interaction adjusted model with standard adjusted model across all PFAS compounds
D1(multivariate_MICE_interaction('PFOA_scaled', "Allcausemortality", "PFOS_scaled", "PFNA_scaled", "PFHxS_scaled"),
   multivariate_MICE('PFOA_scaled', "Allcausemortality"))

D1(multivariate_MICE_interaction('PFOS_scaled', "Allcausemortality", "PFOA_scaled", "PFNA_scaled", "PFHxS_scaled"),
   multivariate_MICE('PFOS_scaled', "Allcausemortality"))

D1(multivariate_MICE_interaction('PFNA_scaled', "Allcausemortality", "PFOA_scaled", "PFOS_scaled", "PFHxS_scaled"),
   multivariate_MICE('PFNA_scaled', "Allcausemortality"))

D1(multivariate_MICE_interaction('PFHxS_scaled', "Allcausemortality", "PFOA_scaled", "PFOS_scaled", "PFNA_scaled"),
   multivariate_MICE('PFHxS_scaled', "Allcausemortality"))
