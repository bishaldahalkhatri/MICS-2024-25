
# Setting working directory -----------------------------------------------

setwd("C:/Bishal_document/MICS dataset/R_analysis")

library(haven)
library(dplyr)
library(writexl)
library(survey)


# Importation and conversion to excel -------------------------------------


mics_dir <- paste0(
  "C:/Bishal_document/MICS dataset/mics_dataset/",
  "NPL_2024_MICS7_v01_M/NPL_2024_MICS7_v01_M/",
  "NPL_2024_MICS7_Datasets/Nepal MICS7 Datasets/",
  "Nepal MICS7 Datasets/Nepal MICS7 SPSS Datasets"
)

bh <- read_sav(file.path(mics_dir, "bh.sav"))
ch <- read_sav(file.path(mics_dir, "ch.sav"))
fs <- read_sav(file.path(mics_dir, "fs.sav"))
hh <- read_sav(file.path(mics_dir, "hh.sav"))
hl <- read_sav(file.path(mics_dir, "hl.sav"))
mn <- read_sav(file.path(mics_dir, "mn.sav"))
wm <- read_sav(file.path(mics_dir, "wm.sav"))

data_list <- list(
  bh = bh,
  ch = ch,
  fs = fs,
  hh = hh,
  hl = hl,
  mn = mn,
  wm = wm
)
get_codebook <- function(df, dataset_name) {
  
  variables <- names(df)
  
  data.frame(
    Dataset = dataset_name,
    Variable = variables,
    Variable_Label = vapply(
      variables,
      function(x) {
        lab <- attr(df[[x]], "label")
        if (is.null(lab)) "" else as.character(lab)
      },
      character(1)
    ),
    Value_Labels = vapply(
      variables,
      function(x) {
        labs <- attr(df[[x]], "labels")
        
        if (is.null(labs)) {
          ""
        } else {
          paste(
            paste(names(labs), as.character(labs), sep = " = "),
            collapse = "; "
          )
        }
      },
      character(1)
    ),
    stringsAsFactors = FALSE
  )
}

codebook <- bind_rows(
  lapply(
    names(data_list),
    function(x) get_codebook(data_list[[x]], x)
  )
)

write_xlsx(
  list(Codebook = codebook),
  "MICS_codebook_export.xlsx"
)



# Merging dataset ---------------------------------------------------------

wm_for_child <- wm |>
  rename(
    maternal_ED5 = ED5
  )

child_mother <- ch |>
  left_join(
    wm_for_child,
    by = c(
      "HH1" = "HH1",
      "HH2" = "HH2",
      "UF4" = "WM3"
    ),
    relationship = "many-to-one"
  )

child_mother_HH <- child_mother |>
  left_join(
    hh,
    by = c(
      "HH1" = "HH1",
      "HH2" = "HH2"
    ),
    relationship = "many-to-one"
  )

"ED5" %in% names(child_mother_HH)
# Defining my analytical sample -------------------------------------------

eligible_child_raw <- child_mother_HH |>
  filter(
    UF17 == 1,
    CAGE >= 6,
    CAGE <= 23
  ) |>
  mutate(
    child_id = paste(HH1, HH2, UF3, sep = "_")
  )

cat("\nNumber of eligible children:",
    nrow(eligible_child_raw), "\n")

eligible_child <- eligible_child_raw |>
  mutate(
    across(
      where(haven::is.labelled),
      haven::zap_labels
    )
  )

# Creating food groups  ---------------------------------------------------

food_group <- function(...) {
  
  x <- cbind(...)
  
  apply(x, 1, function(z) {
    
    if (any(z == 1, na.rm = TRUE)) {
      return(1)
    }
    
    if (any(z %in% c(8, 9), na.rm = TRUE)) {
      return(NA_real_)
    }
    
    observed <- z[!is.na(z)]
    
    if (length(observed) > 0 && all(observed == 2)) {
      return(0)
    }
    
    NA_real_
  })
}


eligible_child <- eligible_child |>
  mutate(
    
    # 1. Breastmilk
    fg_breastmilk = case_when(
      BD3 == 1 ~ 1,
      BD3 == 2 ~ 0,
      BD2 == 2 ~ 0,
      BD3 %in% c(8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    # 2. Grains, roots, tubers and plantains
    fg_grains_roots =
      food_group(BD8B, BD8D),
    
    # 3. Legumes and nuts
    fg_legumes_nuts =
      food_group(BD8N),
    
    # 4. Dairy products
    # infant formula, animal milk, yogurt drink,
    # yogurt and cheese
    fg_dairy =
      food_group(BD7B, BD7C, BD7D, BD8A, BD8O),
    
    # 5. Flesh foods
    # organ meat, meat/poultry and fish/shellfish
    fg_flesh =
      food_group(BD8I, BD8K, BD8M),
    
    # 6. Eggs
    fg_eggs =
      food_group(BD8L),
    
    # 7. Vitamin-A-rich fruits and vegetables
    # pumpkin/carrots/squash + dark green leafy vegetables
    # + ripe mangoes/papayas etc.
    fg_vitaminA =
      food_group(BD8C, BD8E, BD8G),
    
    # 8. Other fruits and vegetables
    fg_other_fruits_veg =
      food_group(BD8F, BD8H)
  )
food_group_check <- eligible_child |>
  summarise(
    across(
      starts_with("fg_"),
      ~ mean(.x == 1, na.rm = TRUE) * 100
    )
  )
cat("\nPercentage consuming each food group:\n")
print(food_group_check)


# Calculating dietary score -----------------------------------------------

eligible_child <- eligible_child |>
  mutate(
    dietary_score = rowSums(
      across(starts_with("fg_")),
      na.rm = FALSE
    )
  )
cat("\nDietary score distribution:\n")
print(
  table(
    eligible_child$dietary_score,
    useNA = "ifany"
  )
)


# Calculating child food poverty ------------------------------------------

eligible_child <- eligible_child |>
  mutate(
    
    severity_of_child_food_poverty = case_when(
      dietary_score >= 0 & dietary_score <= 2 ~
        "Severe child food poverty",
      
      dietary_score >= 3 & dietary_score <= 4 ~
        "Moderate child food poverty",
      
      dietary_score >= 5 & dietary_score <= 8 ~
        "Not in child food poverty",
      
      TRUE ~ NA_character_
    ),
    
    fp_binary = case_when(
      dietary_score >= 0 & dietary_score <= 4 ~ 1,
      dietary_score >= 5 & dietary_score <= 8 ~ 0,
      TRUE ~ NA_real_
    )
  ) |>
  mutate(
    
    severity_of_child_food_poverty = factor(
      severity_of_child_food_poverty,
      levels = c(
        "Severe child food poverty",
        "Moderate child food poverty",
        "Not in child food poverty"
      ),
      ordered = TRUE
    ),
    
    fp_binary_label = factor(
      fp_binary,
      levels = c(0, 1),
      labels = c(
        "No child food poverty",
        "Child food poverty"
      )
    )
  )

cat("\nChild food poverty distribution:\n")
print(
  table(
    eligible_child$severity_of_child_food_poverty,
    useNA = "ifany"
  )
)

cat("\nBinary child food poverty distribution:\n")
print(
  table(
    eligible_child$fp_binary_label,
    useNA = "ifany"
  )
)


# Breastfeeding status ----------------------------------------------------

eligible_child <- eligible_child |>
  mutate(
    breastfeeding_status = case_when(
      
      BD3 == 1 ~ "Breastfeeding",
      
      BD3 == 2 ~ "Not breastfeeding",
      
      BD2 == 2 ~ "Not breastfeeding",
      
      BD3 %in% c(8, 9) ~ NA_character_,
      
      TRUE ~ NA_character_
    )
  )

eligible_child <- eligible_child |>
  mutate(
    early_initiation = case_when(
      
      # Immediately = within 1 hour
      MN37U == 0 ~ "Yes",
      
      # Hours: 0 or 1 hour = within 1 hour
      MN37U == 1 & MN37N <= 1 ~ "Yes",
      
      # Hours: more than 1 hour
      MN37U == 1 & MN37N > 1 ~ "No",
      
      # Days = more than 24 hours
      MN37U == 2 ~ "No",
      
      # Special / unknown
      MN37U == 9 ~ NA_character_,
      
      TRUE ~ NA_character_
    ),
    
    early_initiation = factor(
      early_initiation,
      levels = c("No", "Yes")
    )
  )

cat("\nBreastfeeding status:\n")
print(
  table(
    eligible_child$breastfeeding_status,
    useNA = "ifany"
  )
)


# Solid food frequency ----------------------------------------------------

solid_food_items <- c(
  "BD8A", "BD8B", "BD8C", "BD8D",
  "BD8E", "BD8F", "BD8G", "BD8H",
  "BD8I", "BD8J", "BD8K", "BD8L",
  "BD8M", "BD8N", "BD8O",
  "BD8P", "BD8Q", "BD8X"
)

eligible_child <- eligible_child |>
  mutate(
    
    semi_solid_food_others = case_when(
      BD8P == 1 | BD8Q == 1 | BD8X == 1 ~ 1,
      BD8P == 2 & BD8Q == 2 & BD8X == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    all_solid_food_items_no = if_all(
      all_of(solid_food_items),
      ~ .x == 2
    ),
    
    solid_food_freq_fg = case_when(
      
      BD9 %in% 1:7 ~ BD9,
      
      BD9 %in% c(8, 9) ~ NA_real_,
      
      is.na(BD9) & all_solid_food_items_no ~ 0,
      
      TRUE ~ NA_real_
    )
  )

cat("\nSolid/semi-solid/soft food frequency:\n")
print(
  table(
    eligible_child$solid_food_freq_fg,
    useNA = "ifany"
  )
)


# Milk feeding frequencies ------------------------------------------------

eligible_child <- eligible_child |>
  mutate(
    
    freq_formula = case_when(
      BD7B == 2 ~ 0,
      BD7B == 1 & BD7B1 %in% 1:7 ~ BD7B1,
      BD7B %in% c(8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    freq_animal_milk = case_when(
      BD7C == 2 ~ 0,
      BD7C == 1 & BD7C1 %in% 1:7 ~ BD7C1,
      BD7C %in% c(8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    freq_yogurt = case_when(
      BD7D == 2 ~ 0,
      BD7D == 1 & BD7D1 %in% 1:7 ~ BD7D1,
      BD7D %in% c(8, 9) ~ NA_real_,
      TRUE ~ NA_real_
    )
  )
cat("\nFormula frequency:\n")
print(table(eligible_child$freq_formula, useNA = "ifany"))

cat("\nAnimal milk frequency:\n")
print(table(eligible_child$freq_animal_milk, useNA = "ifany"))

cat("\nYogurt-drink frequency:\n")
print(table(eligible_child$freq_yogurt, useNA = "ifany"))



# non-breast feed milk frequencies ----------------------------------------

eligible_child <- eligible_child |>
  mutate(
    
    total_feed_freq_nonbreastfed = case_when(
      
      breastfeeding_status == "Not breastfeeding" &
        !is.na(solid_food_freq_fg) &
        !is.na(freq_formula) &
        !is.na(freq_animal_milk) &
        !is.na(freq_yogurt) ~
        
        solid_food_freq_fg +
        freq_formula +
        freq_animal_milk +
        freq_yogurt,
      
      TRUE ~ NA_real_
    ),
    
    milk_feed_freq_nonbreastfed = case_when(
      
      breastfeeding_status == "Not breastfeeding" &
        !is.na(freq_formula) &
        !is.na(freq_animal_milk) &
        !is.na(freq_yogurt) ~
        
        freq_formula +
        freq_animal_milk +
        freq_yogurt,
      
      TRUE ~ NA_real_
    ),
    
    milk_feeding_2plus = case_when(
      
      breastfeeding_status == "Not breastfeeding" &
        !is.na(milk_feed_freq_nonbreastfed) &
        milk_feed_freq_nonbreastfed >= 2 ~ 1,
      
      breastfeeding_status == "Not breastfeeding" &
        !is.na(milk_feed_freq_nonbreastfed) &
        milk_feed_freq_nonbreastfed < 2 ~ 0,
      
      TRUE ~ NA_real_
    )
  )

cat("\nTotal feeds among non-breastfed children:\n")
print(
  table(
    eligible_child$total_feed_freq_nonbreastfed,
    useNA = "ifany"
  )
)

cat("\nAt least 2 milk feeds among non-breastfed children:\n")
print(
  table(
    eligible_child$milk_feeding_2plus,
    useNA = "ifany"
  )
)



# calculating minimum meal frequencies ------------------------------------

eligible_child <- eligible_child |>
  mutate(
    
    MMF = case_when(
      
      breastfeeding_status == "Breastfeeding" &
        CAGE >= 6 & CAGE <= 8 &
        !is.na(solid_food_freq_fg) &
        solid_food_freq_fg >= 2 ~ 1,
      
      breastfeeding_status == "Breastfeeding" &
        CAGE >= 6 & CAGE <= 8 &
        !is.na(solid_food_freq_fg) &
        solid_food_freq_fg < 2 ~ 0,
      
      breastfeeding_status == "Breastfeeding" &
        CAGE >= 9 & CAGE <= 23 &
        !is.na(solid_food_freq_fg) &
        solid_food_freq_fg >= 3 ~ 1,
      
      breastfeeding_status == "Breastfeeding" &
        CAGE >= 9 & CAGE <= 23 &
        !is.na(solid_food_freq_fg) &
        solid_food_freq_fg < 3 ~ 0,
      
      breastfeeding_status == "Not breastfeeding" &
        !is.na(total_feed_freq_nonbreastfed) &
        !is.na(solid_food_freq_fg) &
        total_feed_freq_nonbreastfed >= 4 &
        solid_food_freq_fg >= 1 ~ 1,
      
      breastfeeding_status == "Not breastfeeding" &
        !is.na(total_feed_freq_nonbreastfed) &
        !is.na(solid_food_freq_fg) &
        (
          total_feed_freq_nonbreastfed < 4 |
            solid_food_freq_fg == 0
        ) ~ 0,
      
      TRUE ~ NA_real_
    ),
    
    MMF_label = factor(
      MMF,
      levels = c(0, 1),
      labels = c(
        "Does not meet minimum meal frequency",
        "Meets minimum meal frequency"
      )
    )
  )

cat("\nMMF distribution:\n")
print(table(eligible_child$MMF_label, useNA = "ifany"))



# calculating MDD ---------------------------------------------------------

eligible_child <- eligible_child |>
  mutate(
    
    MDD = case_when(
      dietary_score >= 5 ~ 1,
      dietary_score <= 4 ~ 0,
      TRUE ~ NA_real_
    ),
    
    MDD_label = factor(
      MDD,
      levels = c(0, 1),
      labels = c(
        "Does not meet minimum dietary diversity",
        "Meets minimum dietary diversity"
      )
    )
  )

cat("\nMDD distribution:\n")
print(table(eligible_child$MDD_label, useNA = "ifany"))



# Minimum acceptable diet -------------------------------------------------

eligible_child <- eligible_child |>
  mutate(
    
    MAD = case_when(
      
      # Breastfed
      breastfeeding_status == "Breastfeeding" &
        !is.na(MDD) &
        !is.na(MMF) &
        MDD == 1 &
        MMF == 1 ~ 1,
      
      breastfeeding_status == "Breastfeeding" &
        !is.na(MDD) &
        !is.na(MMF) &
        (MDD == 0 | MMF == 0) ~ 0,
      
      # Non-breastfed
      breastfeeding_status == "Not breastfeeding" &
        !is.na(MDD) &
        !is.na(MMF) &
        !is.na(milk_feeding_2plus) &
        MDD == 1 &
        MMF == 1 &
        milk_feeding_2plus == 1 ~ 1,
      
      breastfeeding_status == "Not breastfeeding" &
        !is.na(MDD) &
        !is.na(MMF) &
        !is.na(milk_feeding_2plus) &
        (
          MDD == 0 |
            MMF == 0 |
            milk_feeding_2plus == 0
        ) ~ 0,
      
      TRUE ~ NA_real_
    ),
    
    MAD_label = factor(
      MAD,
      levels = c(0, 1),
      labels = c(
        "Does not meet minimum acceptable diet",
        "Meets minimum acceptable diet"
      )
    )
  )

cat("\nMAD distribution:\n")
print(table(eligible_child$MAD_label, useNA = "ifany"))

options(survey.lonely.psu = "adjust")

wt_eligible_child <- svydesign(
  id = ~PSU,
  strata = ~stratum,
  weights = ~chweight,
  data = eligible_child,
  nest = TRUE
)

MDD_overall <- svymean(
  ~MDD,
  design = wt_eligible_child,
  na.rm = TRUE
)

MDD_bf <- svymean(
  ~MDD,
  design = subset(
    wt_eligible_child,
    breastfeeding_status == "Breastfeeding"
  ),
  na.rm = TRUE
)

MDD_nbf <- svymean(
  ~MDD,
  design = subset(
    wt_eligible_child,
    breastfeeding_status == "Not breastfeeding"
  ),
  na.rm = TRUE
)

cat("\n==============================\n")
cat("WEIGHTED MDD\n")
cat("==============================\n")
cat("Overall:", coef(MDD_overall) * 100, "%\n")
cat("Breastfeeding:", coef(MDD_bf) * 100, "%\n")
cat("Not breastfeeding:", coef(MDD_nbf) * 100, "%\n")



# Weighted MMF ------------------------------------------------------------

MMF_overall <- svymean(
  ~MMF,
  design = wt_eligible_child,
  na.rm = TRUE
)

MMF_bf <- svymean(
  ~MMF,
  design = subset(
    wt_eligible_child,
    breastfeeding_status == "Breastfeeding"
  ),
  na.rm = TRUE
)

MMF_nbf <- svymean(
  ~MMF,
  design = subset(
    wt_eligible_child,
    breastfeeding_status == "Not breastfeeding"
  ),
  na.rm = TRUE
)

cat("\n==============================\n")
cat("WEIGHTED MMF\n")
cat("==============================\n")
cat("Overall:", coef(MMF_overall) * 100, "%\n")
cat("Breastfeeding:", coef(MMF_bf) * 100, "%\n")
cat("Not breastfeeding:", coef(MMF_nbf) * 100, "%\n")


# MAD result weighted -----------------------------------------------------

MAD_overall <- svymean(
  ~MAD,
  design = wt_eligible_child,
  na.rm = TRUE
)

MAD_bf <- svymean(
  ~MAD,
  design = subset(
    wt_eligible_child,
    breastfeeding_status == "Breastfeeding"
  ),
  na.rm = TRUE
)

MAD_nbf <- svymean(
  ~MAD,
  design = subset(
    wt_eligible_child,
    breastfeeding_status == "Not breastfeeding"
  ),
  na.rm = TRUE
)

MAD_results <- data.frame(
  Group = c(
    "Overall",
    "Currently breastfeeding",
    "Currently not breastfeeding"
  ),
  
  Weighted_MAD_percent = c(
    coef(MAD_overall) * 100,
    coef(MAD_bf) * 100,
    coef(MAD_nbf) * 100
  ),
  
  Lower_95CI = c(
    confint(MAD_overall)[1] * 100,
    confint(MAD_bf)[1] * 100,
    confint(MAD_nbf)[1] * 100
  ),
  
  Upper_95CI = c(
    confint(MAD_overall)[2] * 100,
    confint(MAD_bf)[2] * 100,
    confint(MAD_nbf)[2] * 100
  )
) |>
  mutate(
    Weighted_MAD_percent = round(Weighted_MAD_percent, 1),
    Lower_95CI = round(Lower_95CI, 1),
    Upper_95CI = round(Upper_95CI, 1)
  )

cat("\n==============================\n")
cat("WEIGHTED MAD\n")
cat("==============================\n")
print(MAD_results)





# listing required variables for exposure analysis ------------------------

#child factors CAGE, HL4, birthint from bh, DA1, MN37, MN32, other under 5 child in HH
#maternal factors ED5, WB4, magebrt from bh, media variables, maternal occupation, IC1, IC2, IC3, IC11, IC10, IN1, MN5+MN20, DM3, DM4, CM11
#socio-economic variables windex5, HH size, no of children less than 5, HC15, land size, HC17, HC18A
#Geographic and socio-cultural factors HH6, HH7, HH7A, HC2, HC1A, HC1B
#Environmental factors WS1, WS3, WS4, WS7, WS9, WS11, WS14, WS15, HW1, HW2, HW3A, WQ26

required_vars <- c(
  #Identifier
  "HH1", "HH2", "UF3", "UF4",
  #outcome indicator
  "dietary_score",
  "severity_of_child_food_poverty",
  "fp_binary",
  "MMF",
  "MDD",
  "MAD",
  #child indicator
  "CAGE",
  "HL4",
  "DA1",
  "early_initiation",
  #mother indicator
  "maternal_ED5",
  "WB4",
  "MN37U",
  "MN37N",
  "MN32",
  "IC1",
  "IC2",
  "IC3",
  "IC10",
  "IC11",
  "IN1",
  "MN2",
  "MN5",
  "MN20",
  "DM3",
  "DM4",
  # HOUSEHOLD SES
  "windex5",
  "wscore",
  "helevel2",
  "HHSEX",
  "HH48",
  "HH51",
  "HC15",
  "HC16B",
  "HC16C",
  "HC16D",
  "HC17",
  "HC18A",
  "HC18B",
  "HC18C",
  "HC18D",
  "HC18E",
  "HC18F",
  "HC18G",
  "HC18H",
  # GEOGRAPHIC / SOCIOCULTURAL
  "HH6",
  "HH7",
  "HH7A",
  "HC2",
  "HC1A",
  "HC1B",
  # ENVIRONMENT
  "WS1",
  "WS3",
  "WS4",
  "WS7",
  "WS9",
  "WS11",
  "WS14",
  "WS15",
  "HW1",
  "HW2",
  "HW3A",
  # SURVEY DESIGN
  "chweight",
  "PSU",
  "PSU.x",
  "stratum",
  "stratum.x"
)


variable_check <- data.frame(
  variable = required_vars,
  present = required_vars %in% names(eligible_child)
)
variable_check

cleaned_data <- eligible_child |> 
  transmute(
    #Identifier
    HH1 = HH1,
    HH2 = HH2,
    UF3 = UF3,
    UF4 = UF4,
    #outcome indicator
    dietary_score = dietary_score,
    severity_of_child_food_poverty =
      severity_of_child_food_poverty,
    fp_binary = fp_binary,
    MMF = MMF,
    MDD = MDD,
    MAD = MAD,
    #child indicator
    CAGE = CAGE,
    HL4 = HL4,
    DA1 = DA1,
    early_initiation = early_initiation,
    #mother indicator
    maternal_ED5 = maternal_ED5,
    WB4 = WB4,
    MN37U = MN37U,
    MN37N = MN37N,
    MN32 = MN32,
    IC1 = IC1,
    IC2 = IC2,
    IC3 = IC3,
    IC10 = IC10,
    IC11 = IC11,
    IN1 = IN1,
    MN2 = MN2,
    MN5 = MN5,
    MN20 = MN20,
    DM3 = DM3,
    DM4 = DM4,
    # HOUSEHOLD SES
    windex5 = windex5,
    wscore = wscore,
    helevel2 = helevel2,
    HHSEX = HHSEX,
    HH48 = HH48,
    HH51 = HH51,
    HC15 = HC15,
    HC16B = HC16B,
    HC16C = HC16C,
    HC16D = HC16D,
    HC17 = HC17,
    HC18A = HC18A,
    HC18B = HC18B,
    HC18C = HC18C,
    HC18D = HC18D,
    HC18E = HC18E,
    HC18F = HC18F,
    HC18G = HC18G,
    HC18H = HC18H,
    # GEOGRAPHIC / SOCIOCULTURAL
    HH6 = HH6,
    HH7 = HH7,
    HH7A = HH7A,
    HC2 = HC2,
    HC1A = HC1A,
    HC1B = HC1B,
    # ENVIRONMENT
    WS1 = WS1,
    WS3 = WS3,
    WS4 = WS4,
    WS7 = WS7,
    WS9 = WS9,
    WS11 = WS11,
    WS14 = WS14,
    WS15 = WS15,
    HW1 = HW1,
    HW2 = HW2,
    HW3A = HW3A,
    # SURVEY DESIGN
    chweight = chweight,
    PSU = PSU,
    PSU.x = PSU.x,
    stratum = stratum,
    stratum.x = stratum.x
  )

write_xlsx(cleaned_data, "Final cleaned data.xlsx")
saveRDS(
  cleaned_data,
  "cleaned_data_CFP_CF_MICS_2024_25"
)
# Child level factor ------------------------------------------------------


