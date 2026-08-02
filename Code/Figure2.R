####FOREST PLOTS

#For a scaled variable

#Function for extracting complete case models

extraction_function_models <- function(df, category, spec_outcome){
  df %>% 
    as.data.frame() %>% 
    separate_wider_delim(`**95% CI**`, ", ", names = c("lower", "upper")) %>% 
    rename(Characteristic = `**Characteristic**`,
           HR = `**HR**`) %>% 
    mutate(cat = category,
           outcome = spec_outcome,           
           HR = as.numeric(HR),
           lower = as.numeric(lower),
           upper = as.numeric(upper),
           sig = case_when(`**p-value**` <.05 ~ "*",
                           T ~ "")) %>% 
    select(-5)
}

#Function for extracting MICE models
extraction_function_models_MICE <- function(df, category, spec_outcome) {
  df %>% 
    tibble() %>% 
    
    mutate(cat = category,
           outcome = spec_outcome) %>% 
    select(Characteristic, HR, lower, upper, cat, outcome, sig)  
} 

#######

#Extracting data from scaled exposure
scaled_PLOT <- bind_rows(
  
  
  extraction_function_models(univariate('PFOA_scaled', "Allcausemortality"), 'Univariate', 'All cause mortality'),
  extraction_function_models(univariate('PFOS_scaled', "Allcausemortality"), 'Univariate', 'All cause mortality'),
  extraction_function_models(univariate('PFNA_scaled', "Allcausemortality"), 'Univariate', 'All cause mortality'),
  extraction_function_models(univariate('PFHxS_scaled', "Allcausemortality"), 'Univariate', 'All cause mortality'),
  
  #multivariable adjusted 
  extraction_function_models(multivariate('PFOA_scaled', "Allcausemortality", nhanes_design), 'Multivariate adjusted complete case', 'All cause mortality'),
  extraction_function_models(multivariate('PFOS_scaled', "Allcausemortality", nhanes_design), 'Multivariate adjusted complete case', 'All cause mortality'),
  extraction_function_models(multivariate('PFNA_scaled', "Allcausemortality", nhanes_design), 'Multivariate adjusted complete case', 'All cause mortality'),
  extraction_function_models(multivariate('PFHxS_scaled', "Allcausemortality", nhanes_design), 'Multivariate adjusted complete case', 'All cause mortality'),
  
  #multivariable interaction
  extraction_function_models(multivariate_interaction('PFOA_scaled', "Allcausemortality", nhanes_design, "PFOS_scaled", "PFNA_scaled", "PFHxS_scaled"), 'Multivariate interaction complete case', 'All cause mortality'),
  extraction_function_models(multivariate_interaction('PFOS_scaled', "Allcausemortality", nhanes_design, "PFOA_scaled", "PFNA_scaled", "PFHxS_scaled"), 'Multivariate interaction complete case', 'All cause mortality'),
  extraction_function_models(multivariate_interaction('PFNA_scaled', "Allcausemortality", nhanes_design, "PFOA_scaled", "PFOS_scaled", "PFHxS_scaled"), 'Multivariate interaction complete case', 'All cause mortality'),
  extraction_function_models(multivariate_interaction('PFHxS_scaled', "Allcausemortality", nhanes_design, "PFOS_scaled", "PFNA_scaled", "PFNA_scaled"), 'Multivariate interaction complete case', 'All cause mortality'),
  
  #multivariable adjusted MICE
  extraction_function_models_MICE(pool_and_format_results(multivariate_MICE('PFOA_scaled', "Allcausemortality")), 'Multivariate adjusted MICE', 'All cause mortality'),
  extraction_function_models_MICE(pool_and_format_results(multivariate_MICE('PFOS_scaled', "Allcausemortality")), 'Multivariate adjusted MICE', 'All cause mortality'),
  extraction_function_models_MICE(pool_and_format_results(multivariate_MICE('PFNA_scaled', "Allcausemortality")), 'Multivariate adjusted MICE', 'All cause mortality'),
  extraction_function_models_MICE(pool_and_format_results(multivariate_MICE('PFHxS_scaled', "Allcausemortality")), 'Multivariate adjusted MICE', 'All cause mortality'),
  
  #multivariable interaction MICE
  
  extraction_function_models_MICE(pool_and_format_results(multivariate_MICE_interaction('PFOA_scaled', "Allcausemortality", "PFOS_scaled", "PFNA_scaled", "PFHxS_scaled")), 'Multivariate interaction MICE', 'All cause mortality'),
  extraction_function_models_MICE(pool_and_format_results(multivariate_MICE_interaction('PFOS_scaled', "Allcausemortality", "PFOA_scaled", "PFNA_scaled", "PFHxS_scaled")), 'Multivariate interaction MICE', 'All cause mortality'),
  extraction_function_models_MICE(pool_and_format_results(multivariate_MICE_interaction('PFNA_scaled', "Allcausemortality", "PFOA_scaled", "PFOS_scaled", "PFHxS_scaled")), 'Multivariate interaction MICE', 'All cause mortality'),
  extraction_function_models_MICE(pool_and_format_results(multivariate_MICE_interaction('PFHxS_scaled', "Allcausemortality", "PFOA_scaled", "PFOS_scaled", "PFNA_scaled")), 'Multivariate interaction MICE', 'All cause mortality')
  
) %>% 
  mutate(cat = as.factor(cat),
         cat = fct_relevel(cat, c('Multivariate interaction MICE',
                                  'Multivariate adjusted MICE',
                                  'Multivariate interaction complete case',
                                  'Multivariate adjusted complete case',
                                  "Univariate")),
         sig = case_when(Characteristic == "PFHxS_scaled"  & upper >1 & lower <1 ~ " ", 
                         T ~ sig),
         Characteristic = str_replace(Characteristic, "_scaled", ""),
         `MISSINGNESS TREATMENT` = case_when(str_detect(cat, "MICE") ~ "MICE \n(n = 14,876)",
                                             str_detect(cat, 'complete case') ~ "Complete Case \n(n = 13,287)",
                                             T ~ "Univariate Analysis \n(n = 14,876)"),
         # cat3 = case_when(str_detect(cat, "Multivariate adjusted") ~ "Adjusted",
         #                  T ~ "Adjusted with Interaction Terms")) %>% 
         cat3 = case_when(str_detect(cat, 'Univariate') ~ 'Unadjusted',
                          str_detect(cat, 'interaction') ~ 'Adjusted with Interaction Terms',
                          T ~ 'Adjusted')) %>% 
  filter(Characteristic == 'PFOA' | Characteristic == 'PFOS' | Characteristic == 'PFNA' | Characteristic == 'PFHxS')

###########################################

Fig2 <- scaled_PLOT %>% 
  mutate(cat3 = case_when(cat == 'Univariate' ~ 'Unadjusted',
                          T~ cat3),
         `MISSINGNESS TREATMENT` = as.factor(`MISSINGNESS TREATMENT`),
         `MISSINGNESS TREATMENT` = case_when(str_detect(`MISSINGNESS TREATMENT`, "MICE") ~ 'MI \n(n = 14,876)',
                                             T ~ `MISSINGNESS TREATMENT`),
         `MISSINGNESS TREATMENT` = fct_relevel(`MISSINGNESS TREATMENT`, c('Univariate Analysis \n(n = 14,876)',
                                                                          'Complete Case \n(n = 13,287)',
                                                                          'MI \n(n = 14,876)')),
         estimate = paste0("HR: ", HR, " (95% CI: ",lower," ", upper,")")
  ) %>% 
  ggplot(aes(x = Characteristic, y = HR, ymin = lower, ymax = upper,  color = `MISSINGNESS TREATMENT`, linetype = `MISSINGNESS TREATMENT`)) +
  
  #geom_linerange(position = position_dodge2(width = 0.5, reverse = 2), size = 1, key_glyph = 'path') +
  geom_errorbar(position = position_dodge2(width = 0.5, reverse = 2), key_glyph = 'path') +
  scale_linetype_manual(values = c('solid', 'solid', 'solid')) +
  geom_point(position = position_dodge2(width = 1, reverse = T), size = 2) +
  scale_color_manual(values = c('#66c2a5','#fc8d62', '#8da0cb'))+
  facet_wrap(~factor(cat3, levels = c('Unadjusted', 'Adjusted', 'Adjusted with Interaction Terms')), nrow = 3) +
  geom_hline(yintercept = 1) +
  coord_flip() +
  scale_y_continuous(limits = c(.68, 1.5)) + 
  labs(x = '') +
  theme_bw() +
  theme(legend.position = 'bottom',
        plot.title = element_text(face = 'bold', size = 16.5),
        legend.title = element_text(face = 'bold', size = 14),
        axis.text.y = element_text(size = 17, colour = 'black'),
        axis.title.x = element_text(size = 17), colour = 'black',
        axis.text.x = element_text(size = 17, colour = 'black'),
        legend.text = element_text(size = 14, colour = 'black'),
        strip.text = element_text(size = 17, colour = 'black'),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        axis.ticks.y = element_blank()) +
  geom_text(aes(label = estimate, y = 1.3), position = position_dodge2(width = 1, reverse = T), hjust = 0, size = 3.5, show.legend = F)


ggsave('Outputs/Fig2.png', Fig2, width = 9, height = 6, units = 'in', dpi = 600)
