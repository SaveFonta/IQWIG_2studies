#############################################
##            02_data_cleaning             ##
#############################################




library(readxl)
library(dplyr)
library(tidyr)

## ---- Read data ----
Excel_name <- "IQWiG-MA-Datenbank_Stand2025.xlsx"



#--------------------------------------------------------
# Remember that the original file that Leonie sent by email is saved locally: "Original_IQWiG-MA-Datenbank_Stand2025.xlsx". 

#  Here are the changes applied to it, and the new file is Excel_name: 

# 1) Since the meta analysis: A20-93 || Abb. 12 || HbA1c was reported with estimates:

  #PIONEER6 = -0.66
  #SUSTAIN6 = 0.87
  #However, according to IQWiG Project A20-93, it should be changed by hand to:
  
  #PIONEER6 = -0.66
  #SUSTAIN6 = -0.87

# 2) for clearness, the old file had some empty lines at the end of Dossier and at EVB sheets, I cancelled them by hand



# True path, inside Input
path_to_excel <- file.path("Input", Excel_name)

sheets <- excel_sheets(path_to_excel)
iqwig_list <- lapply(sheets, function(x) read_excel(path_to_excel, sheet = x))
names(iqwig_list) <- sheets

## ---- Set column types ----
char_cols <- c(
  "Index", "Studie", "Bewertungsart", "Vergleich", "Vergleichstyp",
  "Endpunkt", "Kategorie", "Projektnummer", "Abbildung", "Mass"
)

num_cols <- c(
  "binaer_ai", "binaer_ni", "binaer_ac", "binaer_nc",
  "effekt_est", "effekt_se", "n_i", "n_c",
  "stetig_mittel_i", "stetig_sd_i", "stetig_n_i",
  "stetig_mittel_c", "stetig_sd_c", "stetig_n_c",
  "endpunkt_x1_md", "endpunkt_x1_spool", "endpunkt_x1_n_i", "endpunkt_x1_n_c",
  "AnzahlStudien"
)

for (nm in names(iqwig_list)) {
  df <- iqwig_list[[nm]]
  
  for (cl in char_cols) {
    if (cl %in% names(df)) {
      df[[cl]] <- as.character(df[[cl]])
    }
  }
  
  for (cl in num_cols) {
    if (cl %in% names(df)) {
      df[[cl]] <- as.numeric(df[[cl]])
    }
  }
  
  iqwig_list[[nm]] <- df
}

## ---- Add sheet_name and bind rows ----
iqwig_all_list <- list()
for (nm in names(iqwig_list)) {
  sheet <- iqwig_list[[nm]]
  sheet$sheet_name <- nm
  iqwig_all_list[[nm]] <- sheet
}
iqwig_all <- bind_rows(iqwig_all_list)

## ---- Create MA_id ----
iqwig_all <- iqwig_all %>%
  mutate(
    MA_id = paste(Projektnummer, Abbildung, Endpunkt, sep = " || ")
  )

iqwig_all$no <- as.numeric(as.factor(iqwig_all$MA_id))

## ---- Flag and remove rows with no usable estimate ----
iqwig_all <- iqwig_all %>%
  mutate(
    has_effekt = !is.na(effekt_est) & !is.na(effekt_se),
    has_2x2 = if_all(
      c(binaer_ai, binaer_ni, binaer_ac, binaer_nc),
      ~ !is.na(.x)
    ),
    has_continuous = if_any(
      c(stetig_mittel_i, stetig_sd_i, stetig_n_i,
        stetig_mittel_c, stetig_sd_c, stetig_n_c),
      ~ !is.na(.x)
    ),
    has_x1_md = if_any(
      c(endpunkt_x1_md, endpunkt_x1_spool,
        endpunkt_x1_n_i, endpunkt_x1_n_c),
      ~ !is.na(.x)
    ),
    no_estimate = !has_effekt & !has_2x2 & !has_continuous & !has_x1_md
  )

rows_no_estimate <- iqwig_all %>%
  filter(no_estimate)

## ---- Recompute AnzahlStudien ---- (useless step)
drop_counts <- rows_no_estimate %>%
  count(MA_id, name = "n_drop")

iqwig_all <- iqwig_all %>%
  left_join(drop_counts, by = "MA_id") %>%
  mutate(
    n_drop = ifelse(is.na(n_drop), 0L, n_drop),
    AnzahlStudien_orig = AnzahlStudien,
    AnzahlStudien = AnzahlStudien - n_drop
  )

## ---- Keep only usable rows ----
iqwig_all <- iqwig_all %>%
  filter(!no_estimate)

iqwig_all <- select(
  iqwig_all,
  -has_effekt, -has_2x2, -has_continuous, -has_x1_md,
  -n_drop, -AnzahlStudien_orig, -no_estimate
)

## ---- Make Studie unique within each MA ----
keys_study <- c("Studie", "MA_id")  # Define before using

iqwig_all <- iqwig_all %>%
  group_by(across(all_of(keys_study))) %>%
  mutate(
    dup_index = row_number(),
    n_in_group = n(),
    Studie_unique = ifelse(
      n_in_group == 1,
      Studie,
      paste0(Studie, ".", dup_index)
    )
  ) %>%
  ungroup()

iqwig_all <- iqwig_all %>%
  mutate(
    Studie_orig = Studie,
    Studie = Studie_unique
  ) %>%
  select(-Studie_unique, -dup_index, -n_in_group, -Studie_orig)

## ---- Extract two-study MAs ----
data_two_studies <- iqwig_all[iqwig_all$AnzahlStudien == 2, ]


#Define output file name
file_name <- "data_two_studies.rds"
file_name_all <- "data_all_studies.rds"

#Ouput directory 
out <- file.path("Output", file_name)
out_all <- file.path("Output", file_name_all)

saveRDS(data_two_studies, file = out)
saveRDS(iqwig_all, file = out_all)




# save the 2 studies also in Excel 
library(writexl)

out_excel <- file.path("Output", "data_two_studies.xlsx")

write_xlsx(data_two_studies, path = out_excel)
