
# Setting working directory -----------------------------------------------


setwd("C:/Bishal_document/MICS dataset/R_analysis")

library(haven)
library(dplyr)
library(writexl)
library(survey)
library(readxl)


# Importation of excel file -----------------------------------------------

cleaned_data <- read_excel(
  "Final cleaned data.xlsx"
)
head(cleaned_data)

# setting data types ------------------------------------------------------

cleaned_data <- cleaned_data |> 
  mutate(
    HH1 = as.integer(HH1),
    HH2 = as.integer(HH2),
    UF3 = as.integer(UF3),
    UF4 = as.integer(UF4),
    dietary_score = as.integer(dietary_score),
    severity_of_child_food_poverty = factor(
      severity_of_child_food_poverty,
      levels = c(
        "Not in child food poverty",
        "Moderate child food poverty",
        "Severe child food poverty"
      ),
      ordered = TRUE
    ),
    fp_binary = as.numeric(fp_binary),
    MMF = as.numeric(MMF),
    MDD = as.numeric(MDD),
    MAD = as.numeric(MAD),
    CAGE = as.integer(CAGE),
    CAGE_group = case_when(
      CAGE >= 6 & CAGE <= 8 ~ "6-8 months",
      CAGE >= 9 & CAGE <= 11 ~ "9-11 months",
      CAGE >= 12 & CAGE <= 17 ~ "12-17 months",
      CAGE >= 18 & CAGE <= 23 ~ "18-23 months",
      TRUE ~ NA_character_
    ),
    CAGE_group = factor(
      CAGE_group,
      levels = c(
        "6-8 months",
        "9-11 months",
        "12-17 months",
        "18-23 months"
      )
    ),
    HL4 = factor(
      HL4,
      levels = c(1, 2),
      labels = c("Male", "Female")
    ),
    DA1 = factor(
      na_if(DA1, 8),
      levels = c(1, 2),
      labels = c("Yes", "No")
    ),
    WB4 = as.integer(WB4),
    MN37U = factor(
      na_if(MN37U, 9),
      levels = c(0, 1, 2),
      labels = c("Immediately", "Hours", "Days")
    ),
    
    MN37N = case_when(
      MN37N %in% c(98, 99) ~ NA_real_,
      TRUE ~ MN37N
    ),
    
    early_initiation = factor(
      early_initiation,
      levels = c("No", "Yes")
    ),
    
    maternal_ED5 = factor(
      case_when(
        maternal_ED5 == 0  ~ "ECE",
        maternal_ED5 == 1  ~ "Class 1",
        maternal_ED5 == 2  ~ "Class 2",
        maternal_ED5 == 3  ~ "Class 3",
        maternal_ED5 == 4  ~ "Class 4",
        maternal_ED5 == 5  ~ "Class 5",
        maternal_ED5 == 6  ~ "Class 6",
        maternal_ED5 == 7  ~ "Class 7",
        maternal_ED5 == 8  ~ "Class 8",
        maternal_ED5 == 9  ~ "Class 9",
        maternal_ED5 == 10 ~ "Class 10",
        maternal_ED5 == 11 ~ "Class 11",
        maternal_ED5 == 12 ~ "Class 12",
        maternal_ED5 == 13 ~ "Bachelors",
        maternal_ED5 == 14 ~ "Masters or above",
        maternal_ED5 %in% c(98, 99) ~ NA_character_,
        TRUE ~ NA_character_
      ),
      levels = c(
        "ECE",
        "Class 1",
        "Class 2",
        "Class 3",
        "Class 4",
        "Class 5",
        "Class 6",
        "Class 7",
        "Class 8",
        "Class 9",
        "Class 10",
        "Class 11",
        "Class 12",
        "Bachelors",
        "Masters or above"
      )
    ),
    
    MN32 = factor(
      case_when(
        MN32 %in% c(8, 9) ~ NA_real_,
        TRUE ~ MN32
      ),
      levels = c(1, 2, 3, 4, 5),
      labels = c(
        "Very large",
        "Larger than average",
        "Average",
        "Smaller than average",
        "Very small"
      )
    ),
    # Mobile phone ownership
    IC1 = factor(
      na_if(IC1, 9),
      levels = c(1, 2),
      labels = c("Yes", "No")
    ),
    
    # Smartphone ownership
    IC2 = factor(
      na_if(IC2, 9),
      levels = c(1, 2),
      labels = c("Yes", "No")
    ),
    
    # Mobile use: 0 none, 1 < weekly, 2 >= weekly, 3 almost daily
    IC3 = factor(
      na_if(IC3, 9),
      levels = c(0, 1, 2, 3),
      labels = c(
        "Not at all",
        "Less than once a week",
        "At least once a week",
        "Almost every day"
      )
    ),
    
    # Ever used internet
    IC10 = factor(
      na_if(IC10, 9),
      levels = c(1, 2),
      labels = c("Yes", "No")
    ),
    
    # Internet use: 0 none, 1 < weekly, 2 >= weekly, 3 almost daily
    IC11 = factor(
      na_if(IC11, 9),
      levels = c(0, 1, 2, 3),
      labels = c(
        "Not at all",
        "Less than once a week",
        "At least once a week",
        "Almost every day"
      )
    ),
    
    # Health insurance
    IN1 = factor(
      na_if(IN1, 9),
      levels = c(1, 2),
      labels = c("Yes", "No")
    ),
    
    # Prenatal care received
    MN2 = factor(
      na_if(MN2, 9),
      levels = c(1, 2),
      labels = c("Yes", "No")
    ),
    
    # Number of prenatal care visits: 98 DK / 99 NR -> NA
    MN5 = case_when(
      MN5 %in% c(98, 99) ~ NA_real_,
      TRUE ~ MN5
    ),
    
    # Place of delivery: 76 DK / 99 NR -> NA where present
    MN20 = factor(
      case_when(
        MN20 %in% c(76, 99) ~ NA_real_,
        TRUE ~ MN20
      )
    ),
    
    # Decision-making variables
    DM3 = factor(
      na_if(DM3, 9),
      levels = c(1, 2, 3, 6),
      labels = c(
        "Respondent",
        "Husband/partner",
        "Jointly",
        "Other"
      )
    ),
    
    DM4 = factor(
      na_if(DM4, 9),
      levels = c(1, 2, 3, 6),
      labels = c(
        "Mainly respondent",
        "Mainly husband/partner",
        "Jointly",
        "Other"
      )
    ),
    # Wealth quintile: categorical in main models
    windex5 = factor(
      windex5,
      levels = c(1, 2, 3, 4, 5),
      labels = c(
        "Poorest",
        "Second",
        "Middle",
        "Fourth",
        "Richest"
      )
    ),
    
    # Continuous wealth score
    wscore = as.numeric(wscore),
    
    # Household-head education
    helevel2 = factor(
      helevel2,
      levels = c(-1, 0, 1, 2, 3, 4, 5, 6, 7, 9, 10),
      labels = c(
        "None",
        "ECED",
        "Basic (Grades 1-8)",
        "Grade 1-5",
        "Grade 6-8",
        "Secondary (Grades 9-12)",
        "Grade 9-10",
        "Grade 11-12",
        "Higher",
        "DK/Missing",
        "No information"
      )
    ),
    
    HHSEX = factor(
      na_if(HHSEX, 9),
      levels = c(1, 2),
      labels = c("Male", "Female")
    ),
    
    # Household size and number of children under 5
    HH48 = as.integer(HH48),
    HH51 = as.integer(HH51),
    
    # Agriculture
    HC15 = factor(
      HC15,
      levels = c(1, 2),
      labels = c("Yes", "No")
    ),
    
    HC16B = as.numeric(HC16B),
    HC16C = as.numeric(HC16C),
    HC16D = as.numeric(HC16D),
    
    # Livestock ownership
    HC17 = factor(
      HC17,
      levels = c(1, 2),
      labels = c("Yes", "No")
    ),
    
    # Livestock counts
    HC18A = as.integer(HC18A),
    HC18B = as.integer(HC18B),
    HC18C = as.integer(HC18C),
    HC18D = as.integer(HC18D),
    HC18E = as.integer(HC18E),
    HC18F = as.integer(HC18F),
    HC18G = as.integer(HC18G),
    HC18H = as.integer(HC18H),
    HH6 = factor(
      HH6,
      levels = c(1, 2),
      labels = c("Urban", "Rural")
    ),
    
    HH7 = factor(
      HH7,
      levels = c(1, 2, 3, 4, 5, 6, 7),
      labels = c(
        "Koshi",
        "Madhesh",
        "Bagmati",
        "Gandaki",
        "Lumbini",
        "Karnali",
        "Sudurpashchim"
      )
    ),
    
    # District and sociocultural variables are categorical.
    # They are left with numeric codes as factor levels.
    HH7A = factor(HH7A),
    HC2 = factor(HC2),
    HC1A = factor(HC1A),
    HC1B = factor(HC1B),
    WS1 = factor(WS1),
    
    WS3 = factor(
      na_if(WS3, 9)
    ),
    
    # Collection time is numeric minutes where recorded.
    # 998/999 are treated as missing if present.
    WS4 = case_when(
      WS4 %in% c(998, 999) ~ NA_real_,
      TRUE ~ WS4
    ),
    
    # Any time in last month without sufficient water
    WS7 = factor(
      case_when(
        WS7 %in% c(8, 9) ~ NA_real_,
        TRUE ~ WS7
      ),
      levels = c(1, 2),
      labels = c("Yes", "No")
    ),
    
    # Water treatment
    WS9 = factor(
      case_when(
        WS9 %in% c(8, 9) ~ NA_real_,
        TRUE ~ WS9
      ),
      levels = c(1, 2),
      labels = c("Yes", "No")
    ),
    
    # Toilet type
    WS11 = factor(
      case_when(
        WS11 == 99 ~ NA_real_,
        TRUE ~ WS11
      )
    ),
    
    # Toilet location
    WS14 = factor(
      case_when(
        WS14 == 9 ~ NA_real_,
        TRUE ~ WS14
      ),
      levels = c(1, 2, 3),
      labels = c(
        "Own dwelling",
        "Own yard/plot",
        "Elsewhere"
      )
    ),
    
    # Shared toilet
    WS15 = factor(
      WS15,
      levels = c(1, 2),
      labels = c("Yes", "No")
    ),
    
    # Handwashing facility
    HW1 = factor(HW1),
    
    # Water at handwashing place
    HW2 = factor(
      na_if(HW2, 9),
      levels = c(1, 2),
      labels = c("Available", "Not available")
    ),
    HW3A = factor(HW3A),
    chweight = as.numeric(chweight),
    PSU = as.integer(PSU),
    PSU.x = as.integer(PSU.x),
    stratum = as.integer(stratum),
    stratum.x = as.integer(stratum.x)
  )

variable_types <- data.frame(
  variable = names(cleaned_data),
  class = sapply(cleaned_data, function(x) class(x)[1]),
  n_unique = sapply(cleaned_data, function(x) dplyr::n_distinct(x)),
  n_missing = sapply(cleaned_data, function(x) sum(is.na(x)))
)

print(variable_types)
options(survey.lonely.psu = "adjust")

analysis_design <- svydesign(
  id = ~PSU,
  strata = ~stratum,
  weights = ~chweight,
  data = cleaned_data,
  nest = TRUE
)

saveRDS(
  cleaned_data,
  "cleaned_data_CFP_CF_MICS_2024_25"
)

# Exposure child ----------------------------------------------------------
#MN37U, MN32

#weighted prevalance
formula_bivariate_cfpb <- function(design, predictors, outcomes) {
  
  results <- list()
  counter <- 1
  
  for (outcome in outcomes) {
    
    for (predictor in predictors) {
      
      # 1. Weighted prevalence
      prev <- svyby(
        formula = as.formula(paste0("~", outcome)),
        by = as.formula(paste0("~", predictor)),
        design = design,
        FUN = svymean,
        na.rm = TRUE
      )
      
      prev <- as.data.frame(prev)
      
      names(prev)[1] <- "category"
      names(prev)[2] <- "prevalence"
      
      prev <- prev |>
        mutate(
          prevalence_percent = prevalence * 100
        )
      
      
      # 2. Survey-weighted logistic regression
      model <- svyglm(
        as.formula(paste0(outcome, " ~ ", predictor)),
        design = design,
        family = quasibinomial()
      )
      
      
      # 3. OR and 95% CI
      coef_table <- summary(model)$coefficients
      
      terms <- rownames(coef_table)
      
      terms <- terms[terms != "(Intercept)"]
      
      OR <- exp(coef(model)[terms])
      
      CI <- exp(
        confint(model)[terms, , drop = FALSE]
      )
      
      
      # 4. Predictor levels
      predictor_data <- model.frame(model)[[predictor]]
      
      levels_predictor <- levels(
        factor(predictor_data)
      )
      
      reference <- levels_predictor[1]
      
      
      # 5. Regression results table
      regression <- data.frame(
        category = levels_predictor,
        COR = NA_real_,
        CI_lower = NA_real_,
        CI_upper = NA_real_
      )
      
      regression$COR[1] <- 1
      
      if (length(terms) > 0) {
        
        regression$COR[-1] <- OR
        regression$CI_lower[-1] <- CI[, 1]
        regression$CI_upper[-1] <- CI[, 2]
      }
      
      
      # 6. Overall p-value
      p_test <- regTermTest(
        model,
        as.formula(paste0("~", predictor))
      )
      
      p_value <- p_test$p
      
      
      # 7. Combine prevalence and regression
      final <- prev |>
        select(
          category,
          prevalence_percent
        ) |>
        left_join(
          regression,
          by = "category"
        ) |>
        mutate(
          outcome = outcome,
          predictor = predictor,
          p_value = p_value
        ) |>
        select(
          outcome,
          predictor,
          category,
          prevalence_percent,
          COR,
          CI_lower,
          CI_upper,
          p_value
        )
      
      
      results[[counter]] <- final
      counter <- counter + 1
    }
  }
  
  bind_rows(results)
}

child_exposures <- c(
  "CAGE_group",
  "HL4",
  "DA1"
)

binary_outcomes <- c(
  "MDD",
  "MMF",
  "MAD"
)

child_bivariate_results <- formula_bivariate_cfpb(
  design = analysis_design,
  predictors = child_exposures,
  outcomes = binary_outcomes
)
child_bivariate_results



# CFP_Child_Ordinal_outcome -----------------------------------------------
formula_bivariate_cfpo <- function(design, predictors) {
  
  results <- list()
  counter <- 1
  
  for (predictor in predictors) {
    
    #------------------------------------------------
    # 1. Weighted prevalence
    #------------------------------------------------
    
    tab <- svytable(
      as.formula(
        paste0(
          "~", predictor,
          " + severity_of_child_food_poverty"
        )
      ),
      design = design
    )
    
    prev <- prop.table(
      tab,
      margin = 1
    ) * 100
    
    prev <- as.data.frame(prev)
    
    names(prev) <- c(
      "category",
      "outcome_category",
      "prevalence_percent"
    )
    
    prev <- prev |>
      mutate(
        category = as.character(category),
        outcome_category = as.character(outcome_category)
      )
    
    
    #------------------------------------------------
    # 2. Survey-weighted ordinal logistic regression
    #------------------------------------------------
    
    model <- svyolr(
      as.formula(
        paste0(
          "severity_of_child_food_poverty ~ ",
          predictor
        )
      ),
      design = design
    )
    
    
    #------------------------------------------------
    # 3. Predictor coefficients ONLY
    #------------------------------------------------
    
    beta <- coef(
      model,
      intercepts = FALSE
    )
    
    
    #------------------------------------------------
    # 4. 95% CI for predictor coefficients ONLY
    #------------------------------------------------
    
    CI <- confint(
      model,
      parm = names(beta)
    )
    
    
    #------------------------------------------------
    # 5. Convert to OR
    #------------------------------------------------
    
    COR <- exp(beta)
    
    CI_lower <- exp(CI[, 1])
    
    CI_upper <- exp(CI[, 2])
    
    
    #------------------------------------------------
    # 6. Predictor levels
    #------------------------------------------------
    
    predictor_levels <- levels(
      droplevels(
        model.frame(model)[[predictor]]
      )
    )
    
    
    #------------------------------------------------
    # 7. Create regression table
    #------------------------------------------------
    
    regression <- data.frame(
      category = predictor_levels,
      COR = NA_real_,
      CI_lower = NA_real_,
      CI_upper = NA_real_
    )
    
    
    # Reference category
    regression$COR[1] <- 1
    
    
    #------------------------------------------------
    # 8. Match each coefficient to its category
    #------------------------------------------------
    
    beta_categories <- sub(
      paste0("^", predictor),
      "",
      names(beta)
    )
    
    for (i in seq_along(beta_categories)) {
      
      row_number <- match(
        beta_categories[i],
        regression$category
      )
      
      if (!is.na(row_number)) {
        
        regression$COR[row_number] <- COR[i]
        
        regression$CI_lower[row_number] <- CI_lower[i]
        
        regression$CI_upper[row_number] <- CI_upper[i]
      }
    }
    
    
    #------------------------------------------------
    # 9. Overall p-value
    #------------------------------------------------
    
    p_value <- regTermTest(
      model,
      as.formula(
        paste0(
          "~", predictor
        )
      )
    )$p
    
    
    #------------------------------------------------
    # 10. Combine prevalence + regression
    #------------------------------------------------
    
    final <- prev |>
      left_join(
        regression,
        by = "category"
      ) |>
      mutate(
        outcome = "severity_of_child_food_poverty",
        predictor = predictor,
        p_value = p_value
      ) |>
      select(
        outcome,
        predictor,
        category,
        outcome_category,
        prevalence_percent,
        COR,
        CI_lower,
        CI_upper,
        p_value
      )
    
    
    #------------------------------------------------
    # 11. Store result
    #------------------------------------------------
    
    results[[counter]] <- final
    
    counter <- counter + 1
  }
  
  # Combine all predictors
  bind_rows(results)
}

ordinal_child_bivariate_results <- formula_bivariate_cfpo(
  design = analysis_design,
  predictors = child_exposures
)

setwd("C:/Bishal_document/MICS dataset/R_Final_analysis/MICS-2024-25")
write_xlsx(ordinal_child_bivariate_results, "Child exposure ordinal outcome.xlsx")
write_xlsx(child_bivariate_results, "logistic child exposure.xlsx")
# Maternal_exposure variables ---------------------------------------------

cleaned_data <- cleaned_data |> 
  mutate(
    #Maternal age
    maternal_age_group = case_when(
      WB4 >= 15 & WB4 <= 19 ~ "15-19",
      WB4 >= 20 & WB4 <= 24 ~ "20-24",
      WB4 >= 25 & WB4 <= 29 ~ "25-29",
      WB4 >= 30 & WB4 <= 34 ~ "30-34",
      WB4 >= 35 & WB4 <= 39 ~ "35-39",
      WB4 >= 40 & WB4 <= 44 ~ "40-44",
      WB4 >= 45 & WB4 <= 49 ~ "45-49",
      TRUE ~ NA_character_
    ),
    
    maternal_age_group = factor(
      maternal_age_group,
      levels = c(
        "15-19",
        "20-24",
        "25-29",
        "30-34",
        "35-39",
        "40-44",
        "45-49"
      )
    ),
    
    #Digital _access
    digital_access = case_when(
      
      IC1 == "Yes" | IC2 == "Yes" ~ "Yes",
      
      IC1 == "No" ~ "No",
      
      TRUE ~ NA_character_
    ),
    digital_access = factor(
      digital_access,
      levels = c("No", "Yes")
    ),
    
    #Institutional delivery
    institutional_delivery = case_when(
      
      as.character(MN20) %in% c(
        "21", "22", "23", "24", "25", "26",
        "31", "32", "36",
        "41", "42", "46"
      ) ~ 1,
      
      !is.na(MN20) ~ 0,
      
      TRUE ~ NA_real_
    ),
    
    #Maternal_education_recode
    maternal_education = factor(
      case_when(
        as.character(maternal_ED5) == "ECE" ~ "ECE",
        
        as.character(maternal_ED5) %in% paste("Class", 1:8) ~ 
          "Basic (Grade 1-8)",
        
        as.character(maternal_ED5) %in% paste("Class", 9:12) ~ 
          "Secondary (Grade 9-12)",
        
        as.character(maternal_ED5) %in% c(
          "Bachelors",
          "Masters or above"
        ) ~ "Higher",
        
        TRUE ~ NA_character_
      ),
      levels = c(
        "ECE",
        "Basic (Grade 1-8)",
        "Secondary (Grade 9-12)",
        "Higher"
      )
    ),
    #Adequate maternal health service
    adequate_maternal_service = case_when(
      
      MN2 == "Yes" &
        !is.na(MN5) &
        MN5 >= 4 &
        institutional_delivery == 1 ~ "Yes",
      
      MN2 == "No" ~ "No",
      
      !is.na(MN5) &
        MN5 < 4 ~ "No",
      
      institutional_delivery == 0 ~ "No",
      
      TRUE ~ NA_character_
    ),
    
    adequate_maternal_service = factor(
      adequate_maternal_service,
      levels = c("No", "Yes")
    )
  )
table(
  cleaned_data$maternal_age_group,
  useNA = "ifany"
)
table(
  cleaned_data$digital_access,
  useNA = "ifany"
)
table(
  cleaned_data$digital_access,
  useNA = "ifany"
)

table(
  cleaned_data$adequate_maternal_service,
  useNA = "ifany"
)

options(survey.lonely.psu = "adjust")

analysis_design <- svydesign(
  id = ~PSU,
  strata = ~stratum,
  weights = ~chweight,
  data = cleaned_data,
  nest = TRUE
)

# Defining and recoding the variable again for multivariate analys --------

cleaned_data <- cleaned_data |>
  mutate(
    maternal_education_3cat = factor(
      case_when(
        maternal_education %in% c(
          "ECE",
          "Basic (Grade 1-8)"
        ) ~ "Basic or less",
        
        maternal_education == "Secondary (Grade 9-12)" ~
          "Secondary",
        
        maternal_education == "Higher" ~
          "Higher",
        
        TRUE ~ NA_character_
      ),
      levels = c(
        "Basic or less",
        "Secondary",
        "Higher"
      )
    ), 
    
    #maternal age
    maternal_age_3cat = factor(
      case_when(
        WB4 >= 15 & WB4 <= 24 ~ "15-24",
        WB4 >= 25 & WB4 <= 34 ~ "25-34",
        WB4 >= 35 & WB4 <= 49 ~ "35-49",
        TRUE ~ NA_character_
      ),
      levels = c(
        "15-24",
        "25-34",
        "35-49"
      )
    ),
    
    #DEcision making variables 
    DM3_3cat = factor(
      case_when(
        DM3 == "Respondent" ~ "Respondent",
        DM3 == "Jointly" ~ "Jointly",
        DM3 %in% c("Husband/partner", "Other") ~
          "Husband/partner or other",
        TRUE ~ NA_character_
      ),
      levels = c(
        "Respondent",
        "Jointly",
        "Husband/partner or other"
      )
    ), 
    
    DM4_3cat = factor(
      case_when(
        DM4 == "Mainly respondent" ~ "Respondent",
        DM4 == "Jointly" ~ "Jointly",
        DM4 %in% c(
          "Mainly husband/partner",
          "Other"
        ) ~ "Husband/partner or other",
        TRUE ~ NA_character_
      ),
      levels = c(
        "Respondent",
        "Jointly",
        "Husband/partner or other"
      )
    )
  )

options(survey.lonely.psu = "adjust")

analysis_design <- svydesign(
  id = ~PSU,
  strata = ~stratum,
  weights = ~chweight,
  data = cleaned_data,
  nest = TRUE
)
maternal_exposures <- c(
  "maternal_education_3cat",
  "maternal_age_3cat",
  "digital_access",
  "IC3",
  "adequate_maternal_service",
  "IN1",
  "DM3_3cat",
  "DM4_3cat"
)

maternal_bivariate_results <- formula_bivariate_cfpb(
  design = analysis_design,
  predictors = maternal_exposures,
  outcomes = binary_outcomes
)

maternal_ordinal_results <- formula_bivariate_cfpo(
  design = analysis_design,
  predictors = maternal_exposures
)



# Socio_economic factors --------------------------------------------------
cleaned_data <- cleaned_data |> mutate(
  #HH_Size_group
  HH_size_group = factor(
    case_when(
      HH48 <= 3 ~ "1-3 members",
      HH48 %in% 4:5 ~ "4-5 members",
      HH48 >= 6 ~ "6+ members",
      TRUE ~ NA_character_
    ),
    levels = c(
      "1-3 members",
      "4-5 members",
      "6+ members"
    )
  ),
  #Having other under 5 childre
  other_under5 = factor(
    case_when(
      HH51 == 1 ~ "No other child <5",
      HH51 >= 2 ~ "At least one other child <5",
      TRUE ~ NA_character_
    ),
    levels = c(
      "No other child <5",
      "At least one other child <5"
    )
  ),
  #HH_Head education
  hh_head_education = factor(
    case_when(
      
      helevel2 %in% c(
        "Grade 1-5",
        "Grade 6-8"
      ) ~ "Basic (Grades 1-8)",
      
      helevel2 %in% c(
        "Grade 9-10",
        "Grade 11-12"
      ) ~ "Secondary (Grades 9-12)",
      
      TRUE ~ NA_character_
    ),
    levels = c(
      "Basic (Grades 1-8)",
      "Secondary (Grades 9-12)"
    )
  )
  
)

socioeconomic_exposures <- c(
  "windex5",
  "hh_head_education",
  "other_under5",
  "HC15",
  "HC17"
)
options(survey.lonely.psu = "adjust")

analysis_design <- svydesign(
  id = ~PSU,
  strata = ~stratum,
  weights = ~chweight,
  data = cleaned_data,
  nest = TRUE
)

Socio_economic_bivariate_results <- formula_bivariate_cfpb(
  design = analysis_design,
  predictors = socioeconomic_exposures,
  outcomes = binary_outcomes
)

Socio_economic_ordinal_results <- formula_bivariate_cfpo(
  design = analysis_design,
  predictors = socioeconomic_exposures
)


# Geographic and socio-cultural factors  ----------------------------------

cleaned_data <- cleaned_data |> 
  mutate(
    #Langague 
    mother_tongue_group = factor(
      case_when(
        HC1B == 1 ~ "Nepali",
        HC1B %in% c(
          2,   # Maithili
          3,   # Bhojpuri
          4,   # Tharu
          6,   # Bajjika
          7,   # Avadhi
          10,  # Doteli
          11,  # Urdu
          14,  # Magahi
          15,  # Baitadeli
          17,  # Achhami
          19,  # Rajbanshi
          21,  # Khash
          22,  # Bajhangi
          23,  # Hindi
          26,  # Ranatharu
          28,  # Bajureli
          30,  # Danuwar
          31,  # Darchuleli
          35,  # Majhi
          38,  # Ganagai
          43,  # Marwadi
          44,  # Dadeldhuri
          46,  # Tajpuriya
          49,  # Musalman
          61,  # Jumli
          70,  # Dailekhi
          79,  # Dolpali
          80,  # Done
          96,  # Sadri
          97,  # English
          102, # Punjabi
          103, # Dhuleli
          112, # Kurmali
          114, # Sindhi
          118, # Malpande
          120, # Sadhani
          121  # Hariyanwi
        ) ~ "Other Indo-European",
        HC1B %in% c(
          5, 8, 9, 12, 13, 16, 18, 20, 24, 25,
          27, 33, 36, 37, 41, 42, 47, 48, 50, 51,
          52, 54, 55, 56, 57, 58, 59, 60, 62, 63,
          64, 65, 66, 67, 68, 71, 72, 73, 74, 76,
          77, 78, 81, 82, 83, 84, 85, 86, 88, 89,
          90, 91, 93, 94, 95, 98, 100, 104, 105,
          106, 107, 108, 109, 110, 111, 113, 115,
          116, 117, 122, 123
        ) ~ "Sino-Tibetan",
        TRUE ~ "Other"
      ),
      levels = c(
        "Nepali",
        "Other Indo-European",
        "Sino-Tibetan",
        "Other"
      )
    ),
    
    #Ethnicity categories 
    ethnicity_8cat = factor(                              #As per census
      case_when(
        
        # 1. Hill Caste
        HC2 %in% c(1, 2, 13, 25) ~ "Hill Caste",
        
        # 2. Madhesh/Tarai Caste
        HC2 %in% c(
          9, 15, 18, 19, 23, 24, 26, 27, 28, 29,
          37, 38, 40, 41, 42, 43, 48, 49, 51, 53,
          54, 55, 57, 58, 60, 61, 63, 64, 67, 68,
          69, 75, 80, 81, 96, 112, 119, 123, 125, 142
        ) ~ "Madhesh/Tarai Caste",
        
        # 3. Hill Dalit
        HC2 %in% c(6, 11, 14, 88, 98) ~ "Hill Dalit",
        
        # 4. Madhesh/Tarai Dalit
        HC2 %in% c(
          17, 20, 22, 33, 34, 39, 50, 73, 74,
          92, 105, 114, 120, 121, 124, 129, 132
        ) ~ "Madhesh/Tarai Dalit",
        
        # 5. Mountain/Hill Janajati
        HC2 %in% c(
          3, 5, 8, 10, 12, 16, 31, 32, 35, 36,
          44, 47, 62, 65, 66, 76, 77, 78, 79, 82,
          85, 86, 87, 89, 90, 91, 93, 95, 97, 99,
          100, 101, 102, 103, 104, 106, 107, 108,
          110, 111, 113, 116, 118, 122, 126, 128,
          131, 133, 135, 136, 137, 138, 139, 140,
          141
        ) ~ "Mountain/Hill Janajati",
        
        # 6. Tarai Janajati
        HC2 %in% c(
          4, 21, 30, 45, 46, 52, 56, 59, 70,
          71, 72, 84, 94, 109, 115, 117, 130, 134
        ) ~ "Tarai Janajati",
        
        # 7. Religious/Linguistic groups
        HC2 %in% c(7, 83, 127) ~ "Religious/Linguistic",
        
        # 8. Others / Foreigners
        HC2 %in% c(143, 996) ~ "Others/Foreigners",
        
        TRUE ~ NA_character_
      ),
      levels = c(
        "Hill Caste",
        "Madhesh/Tarai Caste",
        "Hill Dalit",
        "Madhesh/Tarai Dalit",
        "Mountain/Hill Janajati",
        "Tarai Janajati",
        "Religious/Linguistic",
        "Others/Foreigners"
      )
    )
  )

options(survey.lonely.psu = "adjust")

analysis_design <- svydesign(
  id = ~PSU,
  strata = ~stratum,
  weights = ~chweight,
  data = cleaned_data,
  nest = TRUE
)

Socio_cultural_Geo_Exposures <- c(
  "HH6",
  "HH7",
  "ethnicity_8cat",
  "mother_tongue_group"
)

Socio_geo_cult_bivariate_results <- formula_bivariate_cfpb(
  design = analysis_design,
  predictors = Socio_cultural_Geo_Exposures,
  outcomes = binary_outcomes
)

Socio_geo_cult_ordinal_results <- formula_bivariate_cfpo(
  design = analysis_design,
  predictors = Socio_cultural_Geo_Exposures
)



# Environmental and WASH exposures -------------------------------------------------

cleaned_data <- cleaned_data |> 
  mutate(
    drinking_water_source = factor(
      case_when(
        
        as.character(WS1) %in% c(
          "11", "12", "13", "14"
        ) ~ "Piped water",
        
        as.character(WS1) == "21" ~
          "Tube well/borehole",
        
        as.character(WS1) %in% c(
          "31", "41", "51", "61", "71", "91", "93"
        ) ~ "Other improved source",
        
        as.character(WS1) %in% c(
          "32", "42", "81"
        ) ~ "Unimproved source",
        
        TRUE ~ NA_character_
      ),
      levels = c(
        "Piped water",
        "Tube well/borehole",
        "Other improved source",
        "Unimproved source"
      )
    ),
    water_on_premises = factor(
      case_when(
        
        as.character(WS1) %in% c(
          "11", "12"
        ) ~ "On premises",
        
        as.character(WS3) %in% c(
          "1", "2"
        ) ~ "On premises",
        
        as.character(WS3) == "3" ~
          "Elsewhere",
        
        TRUE ~ NA_character_
      ),
      levels = c(
        "On premises",
        "Elsewhere"
      )
    ),
    water_access = factor(
      case_when(
        
        as.character(WS1) %in% c(
          "11", "12"
        ) |
          as.character(WS3) %in% c(
            "1", "2"
          ) ~ "On premises",
        
        as.character(WS3) == "3" &
          WS4 <= 30 ~
          "Off premises ≤30 min",
        
        as.character(WS3) == "3" &
          WS4 > 30 ~
          "Off premises >30 min",
        
        TRUE ~ NA_character_
      ),
      levels = c(
        "On premises",
        "Off premises ≤30 min",
        "Off premises >30 min"
      )
    ),
    water_treatment = factor(
      WS9,
      levels = c(
        "No",
        "Yes"
      ),
      labels = c(
        "Does not treat water",
        "Treats water"
      )
    ),
    sanitation_service = factor(
      case_when(
        
        as.character(WS11) %in% c(
          "11", "12", "13", "18",
          "21", "22", "24",
          "31", "32"
        ) &
          as.character(WS15) == "No" ~
          "Basic sanitation",
        
        as.character(WS11) %in% c(
          "11", "12", "13", "18",
          "21", "22", "24",
          "31", "32"
        ) &
          as.character(WS15) == "Yes" ~
          "Limited sanitation",
        
        as.character(WS11) %in% c(
          "14", "23", "41", "95", "96"
        ) ~
          "Unimproved/no facility",
        
        TRUE ~ NA_character_
      ),
      levels = c(
        "Basic sanitation",
        "Limited sanitation",
        "Unimproved/no facility"
      )
    ),
    handwashing_service = factor(
      case_when(
        
        as.character(HW1) %in% c(
          "1", "2", "3"
        ) &
          as.character(HW2) == "Available" &
          as.character(HW3A) == "A" ~
          "Water and soap available",
        
        as.character(HW1) %in% c(
          "1", "2", "3"
        ) ~
          "Facility without both water and soap",
        
        as.character(HW1) == "4" ~
          "No handwashing facility",
        
        TRUE ~ NA_character_
      ),
      levels = c(
        "Water and soap available",
        "Facility without both water and soap",
        "No handwashing facility"
      )
    )
  )

#Again 
cleaned_data <- cleaned_data |> 
  mutate(
    #Water_access premise vs off premise
    water_access_2cat = factor(
      case_when(
        water_access == "On premises" ~ "On premises",
        water_access %in% c(
          "Off premises ≤30 min",
          "Off premises >30 min"
        ) ~ "Off premises",
        TRUE ~ NA_character_
      ),
      levels = c(
        "On premises",
        "Off premises"
      )
    ), 
    
    #Hand washing facilities
    handwashing_2cat = factor(
      case_when(
        handwashing_service == "Water and soap available" ~
          "Water and soap available",
        
        handwashing_service %in% c(
          "Facility without both water and soap",
          "No handwashing facility"
        ) ~
          "No basic handwashing service",
        
        TRUE ~ NA_character_
      ),
      levels = c(
        "Water and soap available",
        "No basic handwashing service"
      )
    ) 
  )
options(survey.lonely.psu = "adjust")

analysis_design <- svydesign(
  id = ~PSU,
  strata = ~stratum,
  weights = ~chweight,
  data = cleaned_data,
  nest = TRUE
)

environmental_exposures <- c(
  "drinking_water_source",
  "water_access_2cat", 
  "water_treatment",
  "sanitation_service",
  "handwashing_2cat"
)




Env_wash_bivariate_results <- formula_bivariate_cfpb(
  design = analysis_design,
  predictors = environmental_exposures,
  outcomes = binary_outcomes
)

Env_wash_ordinal_results <- formula_bivariate_cfpo(
  design = analysis_design,
  predictors = environmental_exposures
)

# Creating clean table for bivariate analysis -----------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(writexl)
library(flextable)
library(officer)

ordinal_all <- bind_rows(
  ordinal_child_bivariate_results,
  maternal_ordinal_results,
  Socio_economic_ordinal_results,
  Socio_geo_cult_ordinal_results,
  Env_wash_ordinal_results
)

ordinal_final <- ordinal_all |> 
  mutate(
    Domain = case_when(
      predictor %in% child_exposures ~ "Child characteristics",
      predictor %in% maternal_exposures ~ "Maternal/caregiver characteristics",
      predictor %in% socioeconomic_exposures ~ "Household socioeconomic characteristics",
      predictor %in% Socio_cultural_Geo_Exposures ~ "Geographic and sociocultural characteristics",
      predictor %in% environmental_exposures ~ "Environmental and WASH characteristics"
    ),
    
    Exposure = recode(
      predictor,
      CAGE_group = "Child age",
      HL4 = "Child sex",
      DA1 = "Diarrhoea in previous 2 weeks",
      maternal_education_3cat = "Maternal education",
      maternal_age_3cat = "Maternal age",
      digital_access = "Digital access",
      IC3 = "Frequency of mobile phone use",
      adequate_maternal_service = "Adequate maternal health care",
      IN1 = "Health insurance",
      DM3_3cat = "Decision making for own health care",
      DM4_3cat = "Decision making for seeking reproductive health care",
      windex5 = "Household wealth quintile",
      hh_head_education = "Household head education",
      other_under5 = "Other children aged <5 years",
      HC15 = "Agricultural land ownership",
      HC17 = "Livestock ownership",
      HH6 = "Place of residence",
      HH7 = "Province",
      ethnicity_8cat = "Ethnic group",
      mother_tongue_group = "Mother tongue",
      drinking_water_source = "Drinking-water source",
      water_access_2cat = "Drinking water access",
      water_treatment = "Household water treatment",
      sanitation_service = "Sanitation service",
      handwashing_2cat = "Handwashing service"
    ),
    
    `Prevalence (%)` = sprintf("%.1f", prevalence_percent)
  )

ordinal_final <- ordinal_final |> 
  select(
    Domain, Exposure, category,
    outcome_category, `Prevalence (%)`,
    COR, CI_lower, CI_upper, p_value
  ) |> 
  pivot_wider(
    names_from = outcome_category,
    values_from = `Prevalence (%)`
  ) |> 
  rename(
    Category = category,
    `Not in CFP (%)` = `Not in child food poverty`,
    `Moderate CFP (%)` = `Moderate child food poverty`,
    `Severe CFP (%)` = `Severe child food poverty`
  )




ordinal_final <- ordinal_final |> 
  mutate(
    p_value = as.numeric(p_value),
    
    `cOR (95% CI)` = case_when(
      is.na(COR) ~ "",
      is.na(CI_lower) ~ "1.00 (Ref.)",
      TRUE ~ sprintf("%.2f (%.2f-%.2f)", COR, CI_lower, CI_upper)
    ),
    
    `Overall P` = case_when(
      is.na(p_value) ~ "",
      p_value < 0.001 ~ "<0.001",
      p_value < 0.01  ~ "<0.01",
      p_value < 0.05  ~ "<0.05",
      TRUE ~ sprintf("%.3f", p_value)
    )
  ) |> 
  select(
    Domain, Exposure, Category,
    `Not in CFP (%)`,
    `Moderate CFP (%)`,
    `Severe CFP (%)`,
    `cOR (95% CI)`,
    `Overall P`
  )

#ordering the domain I need
domain_order <- c(
  "Child characteristics",
  "Maternal/caregiver characteristics",
  "Household socioeconomic characteristics",
  "Geographic and sociocultural characteristics",
  "Environmental and WASH characteristics"
)

ordinal_final <- ordinal_final |> 
  mutate(
    Domain = factor(
      Domain,
      levels = domain_order
    )
  ) |> 
  arrange(Domain)



# 1. Show Overall P only once for each exposure
# ------------------------------------------------------------

ordinal_final <- ordinal_final |> 
  group_by(Domain, Exposure) |> 
  mutate(
    `Overall P` = ifelse(
      row_number() == 1,
      `Overall P`,
      ""
    )
  ) |> 
  ungroup()


# ------------------------------------------------------------
# 2. Create a separate section row for each domain
# ------------------------------------------------------------

ordinal_display <- ordinal_final |> 
  group_split(Domain, .keep = TRUE) |> 
  lapply(function(x) {
    
    # Get domain name
    domain_name <- x$Domain[1]
    
    # Create domain header row
    domain_row <- x[1, ]
    
    # Put domain name in EVERY cell temporarily
    # so merge_h() can merge the whole row
    domain_row[] <- domain_name
    
    # Mark this as a domain row
    domain_row$.domain_row <- TRUE
    
    # IMPORTANT:
    # Remove domain name from ordinary data rows
    x$Domain <- ""
    x$.domain_row <- FALSE
    
    # Put domain header above the data rows
    bind_rows(
      domain_row,
      x
    )
    
  }) |> 
  bind_rows()


# ------------------------------------------------------------
# 3. Identify the domain header rows
# ------------------------------------------------------------

domain_rows <- which(
  ordinal_display$.domain_row
)


# ------------------------------------------------------------
# 4. Remove helper variable
# ------------------------------------------------------------

ordinal_table_for_word <- ordinal_display |> 
  select(-Domain, -.domain_row)


# ------------------------------------------------------------
# 5. Create publication-style Word table
# ------------------------------------------------------------

ft1 <- flextable(
  ordinal_table_for_word
) |> 
  
  # Merge the entire domain section row
  merge_h(
    i = domain_rows,
    part = "body"
  ) |> 
  
  # Merge exposure across its category rows
  merge_v(
    j = "Exposure",
    part = "body"
  ) |> 
  
  theme_booktabs() |> 
  
  valign(
    valign = "center",
    part = "all"
  ) |> 
  
  align(
    align = "center",
    part = "all"
  ) |> 
  
  align(
    j = c(
      "Exposure",
      "Category"
    ),
    align = "left",
    part = "all"
  ) |> 
  
  # Bold domain section rows
  bold(
    i = domain_rows,
    part = "body"
  ) |> 
  
  # Bold header
  bold(
    part = "header"
  ) |> 
  
  fontsize(
    size = 9,
    part = "all"
  ) |> 
  
  autofit()


# ------------------------------------------------------------
# Export
# ------------------------------------------------------------

save_as_docx(
  "Table 1. Bivariate associations with child food poverty" = ft1,
  path = "Table_1_Ordinal_CFP.docx"
)


# Table binary outcomes ---------------------------------------------------


# ------------------------------------------------------------
# 1. Manuscript labels and exposure order
# ------------------------------------------------------------

all_predictors_order <- c(
  child_exposures,
  maternal_exposures,
  socioeconomic_exposures,
  Socio_cultural_Geo_Exposures,
  environmental_exposures
)


# ------------------------------------------------------------
# 2. Combine all binary results
# ------------------------------------------------------------

binary_all <- bind_rows(
  child_bivariate_results,
  maternal_bivariate_results,
  Socio_economic_bivariate_results,
  Socio_geo_cult_bivariate_results,
  Env_wash_bivariate_results
)


# ------------------------------------------------------------
# 3. Add manuscript names + format COR and P
# ------------------------------------------------------------

binary_final <- binary_all |> 
  mutate(
    
    Domain = case_when(
      predictor %in% child_exposures ~
        "Child characteristics",
      
      predictor %in% maternal_exposures ~
        "Maternal/caregiver characteristics",
      
      predictor %in% socioeconomic_exposures ~
        "Household socioeconomic characteristics",
      
      predictor %in% Socio_cultural_Geo_Exposures ~
        "Geographic and sociocultural characteristics",
      
      predictor %in% environmental_exposures ~
        "Environmental and WASH characteristics"
    ),
    
    Exposure = recode(
      predictor,
      
      CAGE_group = "Child age",
      HL4 = "Child sex",
      DA1 = "Diarrhoea in previous 2 weeks",
      
      maternal_education_3cat = "Maternal education",
      maternal_age_3cat = "Maternal age",
      digital_access = "Digital access",
      IC3 = "Frequency of mobile phone use",
      adequate_maternal_service = "Adequate maternal health care",
      IN1 = "Health insurance",
      DM3_3cat = "Decision-making for own health care",
      DM4_3cat = "Decision-making for seeking reproductive health care",
      
      windex5 = "Household wealth quintile",
      hh_head_education = "Household head education",
      other_under5 = "Other children aged <5 years",
      HC15 = "Agricultural land ownership",
      HC17 = "Livestock ownership",
      
      HH6 = "Place of residence",
      HH7 = "Province",
      ethnicity_8cat = "Ethnic group",
      mother_tongue_group = "Mother tongue",
      
      drinking_water_source = "Drinking-water source",
      water_access_2cat = "Drinking-water access",
      water_treatment = "Household water treatment",
      sanitation_service = "Sanitation service",
      handwashing_2cat = "Handwashing service"
    ),
    
    Category = as.character(category),
    
    `COR (95% CI)` = ifelse(
      is.na(COR),
      "",
      ifelse(
        is.na(CI_lower),
        "1.00 (Ref.)",
        sprintf(
          "%.2f (%.2f-%.2f)",
          COR,
          CI_lower,
          CI_upper
        )
      )
    )
  )


# ------------------------------------------------------------
# 4. Calculate n/N for each outcome and exposure category
#    n = number with outcome
#    N = number with non-missing outcome
# ------------------------------------------------------------

binary_counts <- map_dfr(
  
  unique(binary_final$predictor),
  
  function(pred) {
    
    map_dfr(
      
      binary_outcomes,
      
      function(out) {
        
        cleaned_data |> 
          filter(
            !is.na(.data[[pred]]),
            !is.na(.data[[out]])
          ) |> 
          group_by(
            Category = as.character(.data[[pred]])
          ) |> 
          summarise(
            n = sum(.data[[out]] == 1),
            N = n(),
            .groups = "drop"
          ) |> 
          mutate(
            predictor = pred,
            outcome = out
          )
      }
    )
  }
)


# ------------------------------------------------------------
# 5. Attach n/N and create display value
# ------------------------------------------------------------

binary_final <- binary_final |> 
  
  left_join(
    binary_counts,
    by = c(
      "predictor",
      "outcome",
      "Category"
    )
  ) |> 
  
  mutate(
    
    `n/N (%)` = paste0(
      n,
      "/",
      N,
      " (",
      sprintf("%.1f", prevalence_percent),
      ")"
    )
    
  )


# ------------------------------------------------------------
# 6. Keep P only once per exposure
# ------------------------------------------------------------

binary_final <- binary_final |> 
  
  group_by(
    predictor,
    outcome
  ) |> 
  
  mutate(
    
    p_value = as.numeric(p_value),
    
    `Overall P` = case_when(
      row_number() > 1 ~ "",
      is.na(p_value) ~ "",
      p_value < 0.001 ~ "<0.001",
      p_value < 0.01  ~ "<0.01",
      p_value < 0.05  ~ "<0.05",
      TRUE ~ sprintf("%.3f", p_value)
    )
    
  ) |> 
  
  ungroup()


# ------------------------------------------------------------
# 7. Make outcomes appear as columns
# ------------------------------------------------------------

binary_final <- binary_final |> 
  
  select(
    Domain,
    Exposure,
    Category,
    outcome,
    `n/N (%)`,
    `COR (95% CI)`,
    `Overall P`,
    predictor
  ) |> 
  
  pivot_wider(
    
    names_from = outcome,
    
    values_from = c(
      `n/N (%)`,
      `COR (95% CI)`,
      `Overall P`
    )
  )


# ------------------------------------------------------------
# 8. Rename outcome columns
# ------------------------------------------------------------

binary_final <- binary_final |> 
  
  rename(
    
    `MDD n/N (%)` =
      `n/N (%)_MDD`,
    
    `MDD COR (95% CI)` =
      `COR (95% CI)_MDD`,
    
    `MDD P` =
      `Overall P_MDD`,
    
    
    `MMF n/N (%)` =
      `n/N (%)_MMF`,
    
    `MMF COR (95% CI)` =
      `COR (95% CI)_MMF`,
    
    `MMF P` =
      `Overall P_MMF`,
    
    
    `MAD n/N (%)` =
      `n/N (%)_MAD`,
    
    `MAD COR (95% CI)` =
      `COR (95% CI)_MAD`,
    
    `MAD P` =
      `Overall P_MAD`
  )


# ------------------------------------------------------------
# 9. Order exposures exactly as your analysis
# ------------------------------------------------------------

domain_order <- c(
  "Child characteristics",
  "Maternal/caregiver characteristics",
  "Household socioeconomic characteristics",
  "Geographic and sociocultural characteristics",
  "Environmental and WASH characteristics"
)

binary_final <- binary_final |> 
  mutate(
    Domain = factor(
      Domain,
      levels = domain_order
    ),
    
    predictor = factor(
      predictor,
      levels = all_predictors_order
    )
  ) |> 
  arrange(
    Domain,
    predictor
  )


# ------------------------------------------------------------
# 10. Create domain section rows
# ------------------------------------------------------------

binary_display <- binary_final |> 
  
  group_split(Domain, .keep = TRUE) |> 
  
  lapply(function(x) {
    
    domain_name <- x$Domain[1]
    
    # Create domain row
    domain_row <- x[1, ]
    
    # Fill row with domain name temporarily
    domain_row[] <- domain_name
    
    domain_row$Domain <- domain_name
    
    domain_row$.domain_row <- TRUE
    
    # Remove domain names from ordinary rows
    x$Domain <- ""
    x$.domain_row <- FALSE
    
    bind_rows(
      domain_row,
      x
    )
    
  }) |> 
  
  bind_rows()


# ------------------------------------------------------------
# 11. Identify domain rows
# ------------------------------------------------------------

domain_rows <- which(
  binary_display$.domain_row
)


# ------------------------------------------------------------
# 12. Remove Domain column from final table
# ------------------------------------------------------------

binary_table_for_word <- binary_display |> 
  select(
    -Domain,
    -.domain_row,
    -predictor
  )


# ------------------------------------------------------------
# 13. Create Word table
# ------------------------------------------------------------

ft2 <- flextable(
  binary_table_for_word
) |> 
  
  # Domain becomes a full-width section row
  merge_h(
    i = domain_rows,
    part = "body"
  ) |> 
  
  # Exposure spans its categories
  merge_v(
    j = "Exposure",
    part = "body"
  ) |> 
  
  theme_booktabs() |> 
  
  valign(
    valign = "center",
    part = "all"
  ) |> 
  
  align(
    align = "center",
    part = "all"
  ) |> 
  
  align(
    j = c(
      "Exposure",
      "Category"
    ),
    align = "left",
    part = "all"
  ) |> 
  
  bold(
    i = domain_rows,
    part = "body"
  ) |> 
  
  bold(
    part = "header"
  ) |> 
  
  fontsize(
    size = 8,
    part = "all"
  ) |> 
  
  autofit()


# ------------------------------------------------------------
# 14. Export
# ------------------------------------------------------------

save_as_docx(
  "Table 2. Bivariate associations with dietary outcomes" = ft2,
  path = "Table_2_Binary_Outcomes.docx"
)

cleaned_data <- cleaned_data |>
  mutate(
    CFP_cut1 = as.numeric(
      severity_of_child_food_poverty != "Not in child food poverty"
    ),
    CFP_cut2 = as.numeric(
      severity_of_child_food_poverty == "Severe child food poverty"
    )
  )


options(survey.lonely.psu = "adjust")

analysis_design <- svydesign(
  id = ~PSU,
  strata = ~stratum,
  weights = ~chweight,
  data = cleaned_data,
  nest = TRUE
)

# Checking proportional odds violation ------------------------------------

# ============================================================
# Function to compare the two cumulative odds ratios
# ============================================================

check_po_all <- function(design, predictors) {
  
  results <- list()
  
  for (pred in predictors) {
    
    # -----------------------------
    # Cut 1
    # -----------------------------
    
    model1 <- svyglm(
      as.formula(
        paste0("CFP_cut1 ~ ", pred)
      ),
      design = design,
      family = quasibinomial()
    )
    
    
    # -----------------------------
    # Cut 2
    # -----------------------------
    
    model2 <- svyglm(
      as.formula(
        paste0("CFP_cut2 ~ ", pred)
      ),
      design = design,
      family = quasibinomial()
    )
    
    
    # -----------------------------
    # Extract coefficients
    # -----------------------------
    
    b1 <- coef(model1)
    b2 <- coef(model2)
    
    b1 <- b1[names(b1) != "(Intercept)"]
    b2 <- b2[names(b2) != "(Intercept)"]
    
    
    # -----------------------------
    # Keep terms appearing in both
    # -----------------------------
    
    common_terms <- intersect(
      names(b1),
      names(b2)
    )
    
    b1 <- b1[common_terms]
    b2 <- b2[common_terms]
    
    
    # -----------------------------
    # Convert to OR
    # -----------------------------
    
    OR1 <- exp(b1)
    OR2 <- exp(b2)
    
    
    # -----------------------------
    # Compare the two ORs
    # -----------------------------
    
    results[[pred]] <- data.frame(
      
      predictor = pred,
      
      term = common_terms,
      
      OR_cut1 = OR1,
      
      OR_cut2 = OR2,
      
      OR_ratio = OR1 / OR2,
      
      log_OR_difference =
        abs(log(OR1) - log(OR2))
      
    )
  }
  
  bind_rows(results)
}

all_ordinal_predictors <- unique(
  c(
    child_exposures,
    maternal_exposures,
    socioeconomic_exposures,
    Socio_cultural_Geo_Exposures,
    environmental_exposures
  )
)

po_diagnostic <- check_po_all(
  design = analysis_design,
  predictors = all_ordinal_predictors
)

View(po_diagnostic)

po_diagnostic <- po_diagnostic |>
  mutate(
    PO_screen = case_when(
      OR_ratio >= 1.5 |
        OR_ratio <= 1 / 1.5 ~ "Review",
      TRUE ~ "Similar"
    )
  )

write_xlsx(po_diagnostic, "Proportionalodds diagnostic.xlsx")




# Multivariable analysis  -------------------------------------------------


# Child
child_vars <- c(
  "CAGE_group",
  "HL4",
  "DA1"
)

# Maternal/caregiver
maternal_vars <- c(
  "maternal_education_3cat",
  "maternal_age_3cat",
  "digital_access",
  "IC3",
  "adequate_maternal_service",
  "IN1",
  "DM3_3cat"
)

# Household socioeconomic
ses_vars <- c(
  "windex5",
  "other_under5",
  "HC15",
  "HC17"
)

# Geographic / sociocultural
geo_vars <- c(
  "HH6",
  "HH7",
  "ethnicity_8cat"
)

# Environmental / WASH
wash_vars <- c(
  "drinking_water_source",
  "water_access_2cat",
  "water_treatment",
  "sanitation_service",
  "handwashing_2cat"
)

library(svyVGAM)   # proportional odds ratio
Model_child <- svy_vglm(
  severity_of_child_food_poverty ~
    CAGE_group +
    HL4 +
    DA1,
  
  design = analysis_design,
  
  family = VGAM::cumulative(
    link = "logitlink",
    reverse = TRUE,
    
    parallel = TRUE ~
      DA1 + HL4 - 1
  )
)

summary(Model_child)
predict(Model_child, type = "response")
fitted_values <- fitted(Model_child)

summary(fitted_values)
range(fitted_values, na.rm = TRUE)
exp(coef(Model_child))
exp(confint(Model_child))



# Model child model -------------------------------------------------------

Model_2 <- svy_vglm(
  severity_of_child_food_poverty ~
    
    # Child characteristics
    CAGE_group +
    HL4 +
    DA1 +
    
    # Maternal/caregiver characteristics
    maternal_education_3cat +
    maternal_age_3cat +
    digital_access +
    IC3 +
    adequate_maternal_service +
    IN1 +
    DM3_3cat,
  
  design = analysis_design,
  
  family = VGAM::cumulative(
    link = "logitlink",
    reverse = TRUE,
    
    # Parallel predictors
    parallel = TRUE ~
      HL4 +
      DA1 +
      maternal_education_3cat +
      maternal_age_3cat +
      digital_access +
      IC3 +
      adequate_maternal_service +
      IN1 +
      DM3_3cat - 1
  )
)
summary(Model_2)


# Model 3: Child and mother and hh ----------------------------------------

Model_3 <- svy_vglm(
  severity_of_child_food_poverty ~
    
    # Child characteristics
    CAGE_group +
    HL4 +
    DA1 +
    
    # Maternal/caregiver characteristics
    maternal_education_3cat +
    maternal_age_3cat +
    digital_access +
    IC3 +
    adequate_maternal_service +
    IN1 +
    DM3_3cat +
    
    # Household socioeconomic characteristics
    windex5 +
    other_under5 +
    HC15 +
    HH6 +
    HH7 +
    ethnicity_8cat +
    HC17,
  
  design = analysis_design,
  
  family = VGAM::cumulative(
    link = "logitlink",
    reverse = TRUE,
    
    # Only CAGE is non-parallel
    parallel = TRUE ~
      HL4 +
      DA1 +
      maternal_education_3cat +
      maternal_age_3cat +
      digital_access +
      IC3 +
      adequate_maternal_service +
      IN1 +
      DM3_3cat +
      other_under5 +
      HH7 +
      ethnicity_8cat +
      HC15 +
      HC17 - 1
  )
)
summary(Model_3)


# Model 4: Child, mother, hh, and environmental ---------------------------

Model_4 <- svy_vglm(
  severity_of_child_food_poverty ~
    
    # Child characteristics
    CAGE_group +
    HL4 +
    DA1 +
    
    # Maternal/caregiver characteristics
    maternal_education_3cat +
    maternal_age_3cat +
    digital_access +
    IC3 +
    adequate_maternal_service +
    IN1 +
    DM3_3cat +
    
    # Household socioeconomic characteristics
    windex5 +
    other_under5 +
    HC15 +
    HC17 +
    HH6 +
    HH7 +
    ethnicity_8cat +
    
    # Environmental and WASH characteristics
    drinking_water_source +
    water_access_2cat +
    water_treatment +
    sanitation_service +
    handwashing_2cat,
  
  design = analysis_design,
  
  family = VGAM::cumulative(
    link = "logitlink",
    reverse = TRUE,
    
    # Parallel predictors
    parallel = TRUE ~
      HL4 +
      DA1 +
      maternal_education_3cat +
      maternal_age_3cat +
      digital_access +
      IC3 +
      adequate_maternal_service +
      IN1 +
      DM3_3cat +
      windex5 +
      other_under5 +
      HC15 +
      HC17 +
      HH7 +
      ethnicity_8cat +
      water_access_2cat +
      water_treatment +
      sanitation_service +
      handwashing_2cat - 1
  )
)
summary(Model_4)



# Creating model for ordinal regression -----------------------------------


make_model_df <- function(model) {
  
  # Coefficients
  beta <- coef(model)
  
  # Standard errors
  se <- sqrt(diag(vcov(model)))
  
  # 95% CI
  ci <- confint(model)
  
  # P-values
  p <- 2 * pnorm(-abs(beta / se))
  
  # Create dataframe
  df <- data.frame(
    term = names(beta),
    aOR = exp(beta),
    CI_lower = exp(ci[, 1]),
    CI_upper = exp(ci[, 2]),
    P = p,
    stringsAsFactors = FALSE
  )
  
  # Remove intercepts
  df <- df[
    !grepl("^\\(Intercept\\)", df$term),
  ]
  
  # Identify whether estimate is Cut 1, Cut 2, or parallel
  df$cut <- ifelse(
    grepl(":1$", df$term),
    "Cut 1",
    ifelse(
      grepl(":2$", df$term),
      "Cut 2",
      "Parallel"
    )
  )
  
  # Create combined aOR (95% CI)
  df$aOR_95CI <- sprintf(
    "%.2f (%.2f-%.2f)",
    df$aOR,
    df$CI_lower,
    df$CI_upper
  )
  
  # Format P-value
  df$P_display <- ifelse(
    df$P < 0.001,
    "<0.001",
    sprintf("%.3f", df$P)
  )
  
  df
}

model1_df <- make_model_df(Model_child)
model2_df <- make_model_df(Model_2)
model3_df <- make_model_df(Model_3)
model4_df <- make_model_df(Model_4)

library(dplyr)
library(tidyr)
library(purrr)
library(flextable)
library(officer)
library(stringr)

model1_df$Model <- "Model 1"
model2_df$Model <- "Model 2"
model3_df$Model <- "Model 3"
model4_df$Model <- "Model 4"

all_models <- bind_rows(
  model1_df,
  model2_df,
  model3_df,
  model4_df
)

model_order <- c(
  "Model 1",
  "Model 2",
  "Model 3",
  "Model 4"
)

cut_order <- c(
  "Cut 1",
  "Parallel",
  "Cut 2"
)

all_models <- all_models |>
  mutate(
    Model = factor(
      Model,
      levels = model_order
    ),
    
    cut = factor(
      cut,
      levels = cut_order
    )
  )



all_models <- all_models |>
  mutate(
    
    term_clean = sub(
      ":(1|2)$",
      "",
      term
    ),
    
    predictor = str_extract(
      term_clean,
      paste(
        c(
          "CAGE_group",
          "HL4",
          "DA1",
          "maternal_education_3cat",
          "maternal_age_3cat",
          "digital_access",
          "IC3",
          "adequate_maternal_service",
          "IN1",
          "DM3_3cat",
          "windex5",
          "other_under5",
          "HC15",
          "HC17",
          "HH6",
          "HH7",
          "ethnicity_8cat",
          "drinking_water_source",
          "water_access_2cat",
          "water_treatment",
          "sanitation_service",
          "handwashing_2cat"
        ),
        collapse = "|"
      )
    ),
    
    Category = str_remove(
      term_clean,
      paste0("^", predictor)
    )
  )




all_models <- all_models |>
  select(
    predictor,
    Category,
    Model,
    cut,
    aOR_95CI,
    P_display
  ) |>
  pivot_wider(
    names_from = c(Model, cut),
    values_from = c(aOR_95CI, P_display)
  )



all_predictors <- c(
  "CAGE_group",
  "HL4",
  "DA1",
  "maternal_education_3cat",
  "maternal_age_3cat",
  "digital_access",
  "IC3",
  "adequate_maternal_service",
  "IN1",
  "DM3_3cat",
  "windex5",
  "other_under5",
  "HC15",
  "HC17",
  "HH6",
  "HH7",
  "ethnicity_8cat",
  "drinking_water_source",
  "water_access_2cat",
  "water_treatment",
  "sanitation_service",
  "handwashing_2cat"
)

table_rows <- map_dfr(
  all_predictors,
  function(x) {
    data.frame(
      predictor = x,
      Category = levels(
        cleaned_data[[x]]
      )
    )
  }
)



ordinal_table <- table_rows |>
  left_join(
    all_models,
    by = c(
      "predictor",
      "Category"
    )
  )


ordinal_table <- ordinal_table |>
  group_by(predictor) |>
  mutate(
    reference = row_number() == 1
  ) |>
  ungroup()


ordinal_table <- ordinal_table |>
  mutate(
    
    M1_Cut1 = ifelse(
      reference,
      "1.00 Ref.",
      coalesce(
        `aOR_95CI_Model 1_Cut 1`,
        `aOR_95CI_Model 1_Parallel`,
        ""
      )
    ),
    
    M1_P1 = ifelse(
      reference,
      "",
      coalesce(
        `P_display_Model 1_Cut 1`,
        `P_display_Model 1_Parallel`,
        ""
      )
    ),
    
    M1_Cut2 = ifelse(
      reference,
      "1.00 Ref.",
      coalesce(
        `aOR_95CI_Model 1_Cut 2`,
        `aOR_95CI_Model 1_Parallel`,
        ""
      )
    ),
    
    M1_P2 = ifelse(
      reference,
      "",
      coalesce(
        `P_display_Model 1_Cut 2`,
        `P_display_Model 1_Parallel`,
        ""
      )
    ),
    
    
    M2_Cut1 = ifelse(
      reference,
      "1.00 Ref.",
      coalesce(
        `aOR_95CI_Model 2_Cut 1`,
        `aOR_95CI_Model 2_Parallel`,
        ""
      )
    ),
    
    M2_P1 = ifelse(
      reference,
      "",
      coalesce(
        `P_display_Model 2_Cut 1`,
        `P_display_Model 2_Parallel`,
        ""
      )
    ),
    
    M2_Cut2 = ifelse(
      reference,
      "1.00 Ref.",
      coalesce(
        `aOR_95CI_Model 2_Cut 2`,
        `aOR_95CI_Model 2_Parallel`,
        ""
      )
    ),
    
    M2_P2 = ifelse(
      reference,
      "",
      coalesce(
        `P_display_Model 2_Cut 2`,
        `P_display_Model 2_Parallel`,
        ""
      )
    ),
    
    
    M3_Cut1 = ifelse(
      reference,
      "1.00 Ref.",
      coalesce(
        `aOR_95CI_Model 3_Cut 1`,
        `aOR_95CI_Model 3_Parallel`,
        ""
      )
    ),
    
    M3_P1 = ifelse(
      reference,
      "",
      coalesce(
        `P_display_Model 3_Cut 1`,
        `P_display_Model 3_Parallel`,
        ""
      )
    ),
    
    M3_Cut2 = ifelse(
      reference,
      "1.00 Ref.",
      coalesce(
        `aOR_95CI_Model 3_Cut 2`,
        `aOR_95CI_Model 3_Parallel`,
        ""
      )
    ),
    
    M3_P2 = ifelse(
      reference,
      "",
      coalesce(
        `P_display_Model 3_Cut 2`,
        `P_display_Model 3_Parallel`,
        ""
      )
    ),
    
    
    M4_Cut1 = ifelse(
      reference,
      "1.00 Ref.",
      coalesce(
        `aOR_95CI_Model 4_Cut 1`,
        `aOR_95CI_Model 4_Parallel`,
        ""
      )
    ),
    
    M4_P1 = ifelse(
      reference,
      "",
      coalesce(
        `P_display_Model 4_Cut 1`,
        `P_display_Model 4_Parallel`,
        ""
      )
    ),
    
    M4_Cut2 = ifelse(
      reference,
      "1.00 Ref.",
      coalesce(
        `aOR_95CI_Model 4_Cut 2`,
        `aOR_95CI_Model 4_Parallel`,
        ""
      )
    ),
    
    M4_P2 = ifelse(
      reference,
      "",
      coalesce(
        `P_display_Model 4_Cut 2`,
        `P_display_Model 4_Parallel`,
        ""
      )
    )
  )


ordinal_table <- ordinal_table |>
  mutate(
    Exposure = recode(
      predictor,
      
      CAGE_group = "Child age",
      HL4 = "Child sex",
      DA1 = "Diarrhoea in previous 2 weeks",
      
      maternal_education_3cat = "Maternal education",
      maternal_age_3cat = "Maternal age",
      digital_access = "Digital access",
      IC3 = "Frequency of mobile phone use",
      adequate_maternal_service = "Adequate maternal health care",
      IN1 = "Health insurance",
      DM3_3cat = "Decision-making for own health care",
      
      windex5 = "Household wealth quintile",
      other_under5 = "Other children aged <5 years",
      HC15 = "Agricultural land ownership",
      HC17 = "Livestock ownership",
      HH6 = "Place of residence",
      HH7 = "Province",
      ethnicity_8cat = "Ethnic group",
      
      drinking_water_source = "Drinking-water source",
      water_access_2cat = "Drinking-water access",
      water_treatment = "Household water treatment",
      sanitation_service = "Sanitation service",
      handwashing_2cat = "Handwashing service"
    )
  )

ordinal_table <- ordinal_table |>
  mutate(
    Domain = case_when(
      
      predictor %in% c(
        "CAGE_group",
        "HL4",
        "DA1"
      ) ~ "Child characteristics",
      
      predictor %in% c(
        "maternal_education_3cat",
        "maternal_age_3cat",
        "digital_access",
        "IC3",
        "adequate_maternal_service",
        "IN1",
        "DM3_3cat"
      ) ~ "Maternal/caregiver characteristics",
      
      predictor %in% c(
        "windex5",
        "other_under5",
        "HC15",
        "HC17",
        "HH6",
        "HH7",
        "ethnicity_8cat"
      ) ~ "Household socioeconomic characteristics",
      
      predictor %in% c(
        "drinking_water_source",
        "water_access_2cat",
        "water_treatment",
        "sanitation_service",
        "handwashing_2cat"
      ) ~ "Environmental and WASH characteristics"
    )
  )


domain_order <- c(
  "Child characteristics",
  "Maternal/caregiver characteristics",
  "Household socioeconomic characteristics",
  "Environmental and WASH characteristics"
)

ordinal_table <- ordinal_table |>
  mutate(
    
    Domain = factor(
      Domain,
      levels = domain_order
    ),
    
    predictor = factor(
      predictor,
      levels = all_predictors
    )
  ) |>
  arrange(
    Domain,
    predictor
  ) |>
  mutate(
    Domain = as.character(Domain),
    predictor = as.character(predictor)
  )



ordinal_table <- ordinal_table |>
  select(
    Domain,
    Exposure,
    Category,
    
    M1_Cut1,
    M1_P1,
    M1_Cut2,
    M1_P2,
    
    M2_Cut1,
    M2_P1,
    M2_Cut2,
    M2_P2,
    
    M3_Cut1,
    M3_P1,
    M3_Cut2,
    M3_P2,
    
    M4_Cut1,
    M4_P1,
    M4_Cut2,
    M4_P2
  )

View(ordinal_table)

# ============================================================
# Create domain rows
# ============================================================

ordinal_display <- ordinal_table |>
  mutate(
    Domain = factor(
      Domain,
      levels = domain_order
    )
  ) |>
  arrange(Domain) |>
  group_split(Domain, .keep = TRUE) |>
  lapply(function(x) {
    
    domain_name <- x$Domain[1]
    
    # Create one domain row
    domain_row <- x[1, ]
    domain_row[] <- domain_name
    
    # Blank domain in normal rows
    x$Domain <- ""
    
    bind_rows(
      domain_row,
      x
    )
  }) |>
  bind_rows()

# ============================================================
# Prepare table for Word
# ============================================================


# Create a flag BEFORE removing Domain
ordinal_table_for_word <- ordinal_display |>
  mutate(
    is_domain = Exposure %in% domain_order
  ) |>
  select(
    -Domain
  )


# Find domain rows
domain_rows <- which(
  ordinal_table_for_word$is_domain
)

# Check this
domain_rows


# Remove helper column
table_for_word <- ordinal_table_for_word |>
  select(
    -is_domain
  )


# ============================================================
# Create flextable
# ============================================================

ft <- flextable(
  table_for_word
)


# ============================================================
# Merge Exposure first
# ============================================================

ft <- merge_v(
  ft,
  j = "Exposure",
  part = "body"
)


# ============================================================
# Merge domain rows
# ============================================================

ft <- merge_h(
  ft,
  i = domain_rows,
  part = "body"
)


# ============================================================
# Formatting
# ============================================================

ft <- theme_booktabs(ft)

ft <- valign(
  ft,
  valign = "center",
  part = "all"
)

ft <- align(
  ft,
  align = "center",
  part = "all"
)

ft <- align(
  ft,
  j = c("Exposure", "Category"),
  align = "left",
  part = "all"
)

ft <- bold(
  ft,
  i = domain_rows,
  part = "body"
)

ft <- bold(
  ft,
  part = "header"
)

ft <- fontsize(
  ft,
  size = 8,
  part = "all"
)

ft <- autofit(ft)


# ============================================================
# View table
# ============================================================

ft
save_as_docx(
  "Table X. Multivariable associations with severity of child food poverty" = ft,
  path = "Multivariable_Ordinal_CFP_Table.docx"
)




# Binary multivariate analysis --------------------------------------------
options(survey.lonely.psu = "adjust")

analysis_design <- svydesign(
  id = ~PSU,
  strata = ~stratum,
  weights = ~chweight,
  data = cleaned_data,
  nest = TRUE
)



# Model 1: Child
model1_vars <- c(
  child_vars
)

# Model 2: Child + maternal
model2_vars <- c(
  child_vars,
  maternal_vars
)

# Model 3: Child + maternal + household + geographic
model3_vars <- c(
  child_vars,
  maternal_vars,
  ses_vars,
  geo_vars
)

# Model 4: Child + maternal + household + geographic + WASH
model4_vars <- c(
  child_vars,
  maternal_vars,
  ses_vars,
  geo_vars,
  wash_vars
)

MDD_Model1_Child <- svyglm(
  reformulate(
    child_vars,
    response = "MDD"
  ),
  design = analysis_design,
  family = quasibinomial()
)
summary(MDD_Model1_Child)
class(MDD_Model1_Child)
family(MDD_Model1_Child)
AIC(MDD_Model1_Child)
anyNA(coef(MDD_Model1_Child))

run_binary_models <- function(outcome) {
  
  model1 <- svyglm(
    reformulate(model1_vars, response = outcome),
    design = analysis_design,
    family = quasibinomial()
  )
  
  model2 <- svyglm(
    reformulate(model2_vars, response = outcome),
    design = analysis_design,
    family = quasibinomial()
  )
  
  model3 <- svyglm(
    reformulate(model3_vars, response = outcome),
    design = analysis_design,
    family = quasibinomial()
  )
  
  model4 <- svyglm(
    reformulate(model4_vars, response = outcome),
    design = analysis_design,
    family = quasibinomial()
  )
  
  list(
    Model_1 = model1,
    Model_2 = model2,
    Model_3 = model3,
    Model_4 = model4
  )
}

binary_models <- list(
  
  MDD = run_binary_models("MDD"),
  
  MMF = run_binary_models("MMF"),
  
  MAD = run_binary_models("MAD")
)


# ============================================================
# MULTIVARIABLE TABLE FOR BINARY OUTCOMES
# ============================================================

library(dplyr)
library(tidyr)
library(purrr)
library(flextable)
library(officer)


# ============================================================
# 1. Function to extract aOR, 95% CI and overall P
# ============================================================

make_binary_df <- function(
    model,
    predictors,
    model_name,
    outcome_name
) {
  
  beta <- coef(model)
  ci <- confint(model)
  
  # Remove intercept
  keep <- !grepl(
    "^\\(Intercept\\)",
    names(beta)
  )
  
  beta <- beta[keep]
  ci <- ci[
    keep,
    ,
    drop = FALSE
  ]
  
  # Basic results
  df <- data.frame(
    term = names(beta),
    aOR = exp(beta),
    CI_lower = exp(ci[, 1]),
    CI_upper = exp(ci[, 2]),
    stringsAsFactors = FALSE
  )
  
  
  # ----------------------------------------------------------
  # Identify predictor and category
  # ----------------------------------------------------------
  
  df$predictor <- NA_character_
  df$Category <- NA_character_
  
  for (v in predictors) {
    
    index <- startsWith(
      df$term,
      v
    )
    
    df$predictor[index] <- v
    
    df$Category[index] <- sub(
      paste0("^", v),
      "",
      df$term[index]
    )
  }
  
  
  # ----------------------------------------------------------
  # Overall P-value for each predictor
  # ----------------------------------------------------------
  
  df$P <- NA_character_
  
  for (v in predictors) {
    
    index <- df$predictor == v
    
    if (any(index)) {
      
      p <- regTermTest(
        model,
        as.formula(
          paste0("~", v)
        )
      )$p
      
      df$P[index] <- ifelse(
        p < 0.001,
        "<0.001",
        sprintf("%.3f", p)
      )
    }
  }
  
  
  # ----------------------------------------------------------
  # Combined aOR and CI
  # ----------------------------------------------------------
  
  df$aOR_95CI <- sprintf(
    "%.2f (%.2f-%.2f)",
    df$aOR,
    df$CI_lower,
    df$CI_upper
  )
  
  
  df$Model <- model_name
  df$Outcome <- outcome_name
  
  df
}


# ============================================================
# 2. Predictor sets for Models 1-4
# ============================================================

model_vars <- list(
  
  Model_1 = child_vars,
  
  Model_2 = c(
    child_vars,
    maternal_vars
  ),
  
  Model_3 = c(
    child_vars,
    maternal_vars,
    ses_vars,
    geo_vars
  ),
  
  Model_4 = c(
    child_vars,
    maternal_vars,
    ses_vars,
    geo_vars,
    wash_vars
  )
)


# ============================================================
# 3. Extract results from all 16 models
# ============================================================

binary_results <- purrr::map_dfr(
  
  names(binary_models),
  
  function(outcome) {
    
    purrr::map_dfr(
      
      names(binary_models[[outcome]]),
      
      function(model_name) {
        
        make_binary_df(
          model =
            binary_models[[outcome]][[model_name]],
          
          predictors =
            model_vars[[model_name]],
          
          model_name =
            model_name,
          
          outcome_name =
            outcome
        )
      }
    )
  }
)


# Check results
View(binary_results)


# ============================================================
# 4. Create all predictor/category combinations
# ============================================================

all_predictors <- model4_vars

table_rows <- purrr::map_dfr(
  
  all_predictors,
  
  function(v) {
    
    data.frame(
      predictor = v,
      Category = levels(
        cleaned_data[[v]]
      ),
      stringsAsFactors = FALSE
    )
  }
)


# ============================================================
# 5. Create model-specific rows
#    This prevents variables from appearing in models
#    before they were actually introduced.
# ============================================================

binary_table <- tidyr::expand_grid(
  
  table_rows,
  
  Outcome = names(binary_models),
  
  Model = names(model_vars)
) |>
  
  filter(
    purrr::map2_lgl(
      predictor,
      Model,
      function(v, m) {
        v %in% model_vars[[m]]
      }
    )
  ) |>
  
  left_join(
    binary_results,
    by = c(
      "predictor",
      "Category",
      "Outcome",
      "Model"
    )
  )


# ============================================================
# 6. Reference categories
# ============================================================

binary_table <- binary_table |>
  
  mutate(
    
    reference = purrr::map2_lgl(
      predictor,
      Category,
      function(v, cat) {
        cat ==
          levels(cleaned_data[[v]])[1]
      }
    ),
    
    Estimate = case_when(
      
      !is.na(aOR) ~ aOR_95CI,
      
      reference ~ "1.00 Ref.",
      
      TRUE ~ ""
    ),
    
    P_display = ifelse(
      is.na(P),
      "",
      P
    )
  )


# ============================================================
# 7. Readable exposure names
# ============================================================

binary_table <- binary_table |>
  
  mutate(
    
    Exposure = recode(
      predictor,
      
      CAGE_group =
        "Child age",
      
      HL4 =
        "Child sex",
      
      DA1 =
        "Diarrhoea in previous 2 weeks",
      
      maternal_education_3cat =
        "Maternal education",
      
      maternal_age_3cat =
        "Maternal age",
      
      digital_access =
        "Digital access",
      
      IC3 =
        "Frequency of mobile phone use",
      
      adequate_maternal_service =
        "Adequate maternal health care",
      
      IN1 =
        "Health insurance",
      
      DM3_3cat =
        "Decision-making for own health care",
      
      windex5 =
        "Household wealth quintile",
      
      other_under5 =
        "Other children aged <5 years",
      
      HC15 =
        "Agricultural land ownership",
      
      HC17 =
        "Livestock ownership",
      
      HH6 =
        "Place of residence",
      
      HH7 =
        "Province",
      
      ethnicity_8cat =
        "Ethnic group",
      
      drinking_water_source =
        "Drinking-water source",
      
      water_access_2cat =
        "Drinking-water access",
      
      water_treatment =
        "Household water treatment",
      
      sanitation_service =
        "Sanitation service",
      
      handwashing_2cat =
        "Handwashing service"
    )
  )


# ============================================================
# 8. Add domains
# ============================================================

binary_table <- binary_table |>
  
  mutate(
    
    Domain = case_when(
      
      predictor %in% child_vars ~
        "Child characteristics",
      
      predictor %in% maternal_vars ~
        "Maternal/caregiver characteristics",
      
      predictor %in% ses_vars ~
        "Household socioeconomic characteristics",
      
      predictor %in% geo_vars ~
        "Household socioeconomic characteristics",
      
      predictor %in% wash_vars ~
        "Environmental and WASH characteristics"
    )
  )


# ============================================================
# 9. Put outcomes and models in correct order
# ============================================================

binary_table$Outcome <- factor(
  binary_table$Outcome,
  levels = c(
    "MDD",
    "MMF",
    "MAD"
  )
)

binary_table$Model <- factor(
  binary_table$Model,
  levels = c(
    "Model_1",
    "Model_2",
    "Model_3",
    "Model_4"
  )
)


# ============================================================
# 10. Create wide table
# ============================================================

binary_table <- binary_table |>
  
  select(
    Domain,
    Exposure,
    Category,
    Outcome,
    Model,
    Estimate,
    P_display
  ) |>
  
  pivot_wider(
    names_from = c(
      Outcome,
      Model
    ),
    values_from = c(
      Estimate,
      P_display
    ),
    names_glue = "{.value}_{Outcome}_{Model}"
  )


# ============================================================
# 11. Replace missing cells with blank
# ============================================================

binary_table <- binary_table |>
  
  mutate(
    across(
      everything(),
      ~ ifelse(
        is.na(.x),
        "",
        .x
      )
    )
  )


# ============================================================
# 12. Put columns in exact order
# ============================================================

desired_columns <- c(
  
  "Domain",
  "Exposure",
  "Category",
  
  unlist(
    lapply(
      c("MDD", "MMF", "MAD"),
      
      function(outcome) {
        
        unlist(
          lapply(
            c(
              "Model_1",
              "Model_2",
              "Model_3",
              "Model_4"
            ),
            
            function(model) {
              
              c(
                paste0(
                  "Estimate_",
                  outcome,
                  "_",
                  model
                ),
                
                paste0(
                  "P_display_",
                  outcome,
                  "_",
                  model
                )
              )
            }
          )
        )
      }
    )
  )
)


binary_table <- binary_table |>
  
  select(
    all_of(desired_columns)
  )


# Check the final dataframe
View(binary_table)


# ============================================================
# 13. Create domain rows
# ============================================================

binary_display <- binary_table |>
  
  mutate(
    Domain = factor(
      Domain,
      levels = domain_order
    )
  ) |>
  arrange(Domain) |> 
  group_split(
    Domain,
    .keep = TRUE
  ) |>
  
  lapply(
    function(x) {
      
      domain_name <- as.character(x$Domain[1])
      
      # Create one domain row
      domain_row <- x[1, ]
      
      domain_row[] <- domain_name
      
      # Remove domain name from normal rows
      x$Domain <- ""
      
      bind_rows(
        domain_row,
        x
      )
    }
  ) |>
  
  bind_rows()


# ============================================================
# 14. Identify domain rows
# ============================================================

domain_rows <- which(
  binary_display$Exposure %in%
    domain_order
)


# ============================================================
# 15. Prepare dataframe for Word
# ============================================================

table_for_word <- binary_display |>
  
  select(
    -Domain
  )


# ============================================================
# 16. Create flextable
# ============================================================

ft_binary <- flextable(
  table_for_word
)


# ============================================================
# 17. Add outcome header
# ============================================================

ft_binary <- add_header_row(
  x = ft_binary,
  
  values = c(
    "",
    "",
    "MDD",
    "MMF",
    "MAD"
  ),
  
  colwidths = c(
    1,
    1,
    8,
    8,
    8
  )
)


# ============================================================
# 18. Rename lower header
# ============================================================

ft_binary <- set_header_labels(
  x = ft_binary,
  
  Exposure = "Exposure",
  Category = "Category",
  
  Estimate_MDD_Model_1 = "Model 1 aOR (95% CI)",
  P_display_MDD_Model_1 = "P",
  Estimate_MDD_Model_2 = "Model 2 aOR (95% CI)",
  P_display_MDD_Model_2 = "P",
  Estimate_MDD_Model_3 = "Model 3 aOR (95% CI)",
  P_display_MDD_Model_3 = "P",
  Estimate_MDD_Model_4 = "Model 4 aOR (95% CI)",
  P_display_MDD_Model_4 = "P",
  
  Estimate_MMF_Model_1 = "Model 1 aOR (95% CI)",
  P_display_MMF_Model_1 = "P",
  Estimate_MMF_Model_2 = "Model 2 aOR (95% CI)",
  P_display_MMF_Model_2 = "P",
  Estimate_MMF_Model_3 = "Model 3 aOR (95% CI)",
  P_display_MMF_Model_3 = "P",
  Estimate_MMF_Model_4 = "Model 4 aOR (95% CI)",
  P_display_MMF_Model_4 = "P",
  
  Estimate_MAD_Model_1 = "Model 1 aOR (95% CI)",
  P_display_MAD_Model_1 = "P",
  Estimate_MAD_Model_2 = "Model 2 aOR (95% CI)",
  P_display_MAD_Model_2 = "P",
  Estimate_MAD_Model_3 = "Model 3 aOR (95% CI)",
  P_display_MAD_Model_3 = "P",
  Estimate_MAD_Model_4 = "Model 4 aOR (95% CI)",
  P_display_MAD_Model_4 = "P"
)


# ============================================================
# 19. Bold domain rows BEFORE merging
# ============================================================

ft_binary <- bold(
  x = ft_binary,
  i = domain_rows,
  part = "body"
)


# ============================================================
# 20. Merge Exposure
# ============================================================

ft_binary <- merge_v(
  x = ft_binary,
  j = "Exposure",
  part = "body"
)


# ============================================================
# 21. Merge domain rows
# ============================================================

ft_binary <- merge_h(
  x = ft_binary,
  i = domain_rows,
  part = "body"
)


# ============================================================
# 22. Final formatting
# ============================================================

ft_binary <- theme_booktabs(
  x = ft_binary
)

ft_binary <- valign(
  x = ft_binary,
  valign = "center",
  part = "all"
)

ft_binary <- align(
  x = ft_binary,
  align = "center",
  part = "all"
)

ft_binary <- align(
  x = ft_binary,
  j = c(
    "Exposure",
    "Category"
  ),
  align = "left",
  part = "all"
)

ft_binary <- bold(
  x = ft_binary,
  part = "header"
)

ft_binary <- fontsize(
  x = ft_binary,
  size = 7,
  part = "all"
)

ft_binary <- autofit(
  x = ft_binary
)


# ============================================================
# 23. View final table
# ============================================================

ft_binary


# ============================================================
# 24. Export to Word
# ============================================================

save_as_docx(
  "Table X. Multivariable associations with binary dietary outcomes" =
    ft_binary,
  path =
    "Table_Multivariable_Binary_Dietary_Outcomes.docx"
)

write_xlsx(ft_binary, path = "Table_Multivariable_Binary_Dietary_Outcomes.xlsx")





