############################################################
## IQWiG meta-analysis database – exploratory script   ##
## Author: Saverio Fontana      ##
############################################################


#NOTE: 
#this file is just an exploratory analysis, there is no necessity to run it cause its noly 
#purpose is to understand what cleaning step to perform in the data_cleaning file

library(readxl)
library(dplyr)
library(tidyr)


## ---- Path to the Excel file ----
path_to_excel <- file.path("Input", "IQWiG-MA-Datenbank_Stand2024.xlsx"  )


## Read sheets

# extract the name of the sheets
sheets <- excel_sheets(path_to_excel)

iqwig_list <- lapply(sheets, function(x) read_excel(path_to_excel, sheet = x))
names(iqwig_list) <- sheets


cat("Sheets found:\n")
print(names(iqwig_list))
cat("\n")




## ---- Check if all sheets have the same columns ----

# get the column names for each sheet
cols_list <- lapply(iqwig_list, names)

# use the first sheet as reference
ref_cols <- cols_list[[1]]

# check identical column names (including order)
same_cols <- sapply(
  cols_list, function(x) identical(ref_cols, x))


cat("Do all sheets have exactly the same columns (including order)?\n")
print(same_cols)
cat("\n")




## ---- Check column types across sheets ----

# Collect all column names
all_cols <- cols_list %>% 
  unlist() %>% 
  unique()

# Build a data frame of classes: rows = sheets, cols = columns
class_matrix <- do.call(
  rbind,
  lapply(iqwig_list, function(df) {
    # initialise vector of NA for all columns
    out <- setNames(rep(NA_character_, length(all_cols)), all_cols)
    # fill in the classes for columns present in this sheet
    for (nm in names(df)) {
      out[nm] <- class(df[[nm]])[1]
    }
    out
  })
)
row.names(class_matrix) <- names(iqwig_list)

# Find columns with inconsistent classes (ignoring NA)
cols_with_mixed_class <- sapply(
  seq_along(all_cols),
  function(j) {
    col_classes <- unique(na.omit(class_matrix[, j]))
    length(col_classes) > 1
  }
)
names(cols_with_mixed_class) <- all_cols

mixed_cols <- names(cols_with_mixed_class)[cols_with_mixed_class]

cat("Columns with differing classes across sheets:\n")
print(mixed_cols)
cat("\n")






## ---- Set column types ----

# target classes by role
char_cols <- c(
  "Index",
  "Studie",
  "Bewertungsart",
  "Vergleich",
  "Vergleichstyp",
  "Endpunkt",
  "Kategorie",
  "Projektnummer",
  "Abbildung",
  "Mass"
)

num_cols <- c(
  "binaer_ai",
  "binaer_ni",
  "binaer_ac",
  "binaer_nc",
  "effekt_est",
  "effekt_se",
  "n_i",
  "n_c",
  "stetig_mittel_i",
  "stetig_sd_i",
  "stetig_n_i",
  "stetig_mittel_c",
  "stetig_sd_c",
  "stetig_n_c",
  "endpunkt_x1_md",
  "endpunkt_x1_spool",
  "endpunkt_x1_n_i",
  "endpunkt_x1_n_c",
  "AnzahlStudien"
)

# loop over sheets and enforce classes
for (nm in names(iqwig_list)) {
  df <- iqwig_list[[nm]]
  
  ## 1) character columns
  for (cl in char_cols) {
    if (cl %in% names(df)) {
      df[[cl]] <- as.character(df[[cl]])
    }
  }
  
  ## 2) numeric columns
  for (cl in num_cols) {
    if (cl %in% names(df)) {
      old <- df[[cl]]
      # coerce everything to numeric; logical/character become numeric
      new <- (as.numeric(old))
      
      # quick check
      bad <- !is.na(old) & is.na(new) & !is.logical(old)
      if (any(bad)) {
        cat("WARNING: non-numeric values in column", cl, "in sheet", nm, "\n")
        print(unique(old[bad]))
      }
      
      df[[cl]] <- new
    }
  }
  
  iqwig_list[[nm]] <- df
}




## ---- Add sheet_name and bind rows ----
iqwig_all_list <- list()

#rewrite each sheet adding his sheet name
for (nm in names(iqwig_list)) {
  sheet <- iqwig_list[[nm]]
  sheet$sheet_name <- nm
  iqwig_all_list[[nm]] <- sheet
}

iqwig_all <- bind_rows(iqwig_all_list)

nrow(iqwig_all)




























################################################
##              CONSISTENCY CHECKS            ##
################################################



## ---- Check MA keys and MAs w/ different endpoints ----

# are there duplicated rows? 
n_row_dups <- sum(duplicated(iqwig_all))
cat("Number of duplciates:", n_row_dups, "\n" )



# are there duplicated rows (if we dont consider sheet_name and Bewertungsart)
n_row_dups <- iqwig_all %>%
  select (-c(sheet_name, Bewertungsart, Index, Kategorie, AnzahlStudien)) %>% 
  duplicated() %>% 
  sum()
n_row_dups


#following the guidelines, this should uniquely define a MA
keys_MA <- c("Projektnummer", "Abbildung")

n_ma <- iqwig_all %>%
  summarise(
    n_ma = n_distinct(across(all_of(keys_MA)))
  ) %>%
  pull(n_ma) #pull is just "$"

cat("Number of distinct MA (Projektnummer + Abbildung):", n_ma, "\n\n")


# let's see if there are same identical studies with different Endpoints

keys_MA_endpoint <- c("Projektnummer", "Abbildung", "Endpunkt")

n_ma_endpoint <- iqwig_all %>%
  summarise(
    n_ma_endpoint = n_distinct(across(all_of(keys_MA_endpoint)))
  ) %>%
  pull(n_ma_endpoint)

cat("Number of distinct MA (Projektnummer + Abbildung + Endpunkt):", n_ma_endpoint, "\n")
cat("Difference:", n_ma_endpoint - n_ma, "\n\n")




# ----  Inspect MAs w\ more than one endpoint  ----

ma_by_endpoint <- iqwig_all %>%
  group_by(Projektnummer, Abbildung) %>%
  summarise(
    n_endpunkte = n_distinct(Endpunkt),
    endpoints   = paste(sort(unique(Endpunkt)), collapse = " | "),
    .groups     = "drop"
  )
#small check that everything is in order
nrow(ma_by_endpoint) # should be 2144


#inspect 
ma_multi_endpoint <- ma_by_endpoint %>%
  filter(n_endpunkte > 1)

cat("Number of MA (Projektnummer + Abbildung) with >1 Endpunkt:", 
    nrow(ma_multi_endpoint), "\n\n")

print(ma_multi_endpoint)

#since there are only two, we can singularly inspect them  

#A12-19 
A12.19 <- iqwig_all[iqwig_all$Projektnummer == "A12-19" & iqwig_all$Abbildung== "Abb. 1", ]
print(A12.19)
#this is ok, also the number of individuals is perfect, but 
# AnzahlStudien = 6, so the point is, with different endopoints, are these 3 different metanalysi 
# with 2 studies, or a single MA w 6 studies? 



#N17-01A 
N17.01A <- iqwig_all[iqwig_all$Projektnummer == "N17-01A" & iqwig_all$Abbildung == "Abb. 26", ]
print(N17.01A)

#same as before, AnzahlStudien = 4.

# I can just change number of studies and that's it
# Talking w/ Samuel, don't bother to change at the moment, let's follow what IQWIG did

#So we comment this

#iqwig_all[iqwig_all$Projektnummer == "A12.19" & iqwig_all$Abbildung == "Abb. 1", "AnzahlStudien"]<- 2
#iqwig_all[iqwig_all$Projektnummer == "N17-01A" & iqwig_all$Abbildung == "Abb. 26", "AnzahlStudien"]<- 2





## ---- Create ID for each meta-analyis ("identifier" in Quarto df) ----
iqwig_all <- iqwig_all %>%
  mutate(
    MA_id = paste(Projektnummer, Abbildung, Endpunkt, sep = " || ")# one id per (Projektnummer, Abbildung, Endpunkt) --> OUT NEW KEY
  )

#verify --> there should be 2147 IDs
iqwig_all$MA_id %>%
  unique() %>%
  length
 

# create also IDs with numbers 
iqwig_all$no <- as.numeric(as.factor(iqwig_all$MA_id))







## ---- Flag rows with no usable estimate ----
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

cat("Rows with NO usable estimate:", nrow(rows_no_estimate), "\n")

#here is the rows wwithout estimaets:
# N18-03 || Abb. 64 || SF-36 körperliche Rollenfunktion
#N18-03 || Abb. 65 || SF-36 emotionale Rollenfunktion



## ---- Recompute AnzahlStudien after dropping unusable rows ----

#find how many rows we are dropping for each MA id
drop_counts <- rows_no_estimate %>%
  count(MA_id, name = "n_drop")

iqwig_all <- iqwig_all %>%
  left_join(drop_counts, by = "MA_id") %>%
  mutate(
    n_drop = ifelse(is.na(n_drop), 0L, n_drop),
    AnzahlStudien_orig = AnzahlStudien,
    AnzahlStudien      = AnzahlStudien - n_drop
  )

## ---- Keep only usable rows for analysis ----
iqwig_all <- iqwig_all %>%
  filter(!no_estimate)


# delete the flag columns... maybe some of them I can use in the future? 
iqwig_all <- select(iqwig_all, -has_effekt, -has_2x2, -has_continuous, -has_x1_md, -n_drop, -AnzahlStudien_orig, -no_estimate)








# ---- studies with something wrong ----
# Now the idea is to look at singular studies, cause I'd like to have also a study_name which is unique,
# since in the Quarto we have the column study, to identify the singular study in the metanalyisis


# Summary for each metanalyi
ma_summary <- iqwig_all %>%
  group_by(MA_id) %>% 
  summarise(
    declared_studies   = first(AnzahlStudien), #I already checked that AnzahlStudien iis constant withing each MA
    n_rows             = n(),
    n_unique_study_names   = n_distinct(Studie),
    name_duplicated     = n_rows > n_unique_study_names, # TRUE if one study name is duplicated
    .groups = "drop"
  )


#fast check that there are no MA with less than 2 studies
ma_summary %>%
  filter(declared_studies < 2) %>%
  nrow()

#
ma_problem <- ma_summary %>%
  filter(
    n_unique_study_names != declared_studies |
      n_rows != declared_studies |
      n_unique_study_names != n_rows
  )





# --- Better inspect singular studies ----
keys_study <- c("Studie", "MA_id") 
#this should uniquely define a study inside a meta analyisis, but it doesnt since we have the MA in ma_problem



# Look  duplicated rows
dup_indices_df <- iqwig_all %>%
  mutate(index_row = row_number()) %>%
  group_by(across(all_of(keys_study))) %>%
  filter(n() > 1) %>%  #keep only duplicated combinations of the key
  ungroup() %>%
  arrange(across(all_of(keys_study)), index_row) %>%
  left_join(
    ma_summary,
    by = ("MA_id")
  )




# I conclude they only have the same name, but the nuumbers are different
#so I need to add something (like .1, .2 to duplicated name in order so that each name is study name is singuular)



## ---- Make Studie unique within each MA+endpoint ----


iqwig_all <- iqwig_all %>%
  group_by(across(all_of(keys_study))) %>%
  mutate(
    dup_index = row_number(), #row number withing group 
    n_in_group = n(), #number of rows within group
    Studie_unique = ifelse(
      n_in_group == 1,
      Studie,                      # only one row → keep original name
      paste0(Studie, ".", dup_index)  # duplicated → Studie.1, Studie.2, ...
    )
  ) %>%
  ungroup()



iqwig_all <- iqwig_all %>%
  mutate(Studie_orig = Studie,   # keep a copy if you want
         Studie      = Studie_unique) %>%
  select(-Studie_unique, -dup_index, -n_in_group, -Studie_orig)







#CHECK: 

dup_after <- iqwig_all %>%
  group_by(Studie, Projektnummer, Abbildung, Endpunkt) %>%
  filter(n() > 1) %>%
  ungroup()

cat("Rows still duplicated after renaming:", nrow(dup_after), "\n")














# ---- OVERVIEW  ----
ma_size_dist <- ma_summary %>%
  group_by(declared_studies) %>%
  summarise(n_ma = n(), .groups = "drop") %>%
  arrange(declared_studies)

print(ma_size_dist)




# Exctract just the two studies MA
data_2_studies <- iqwig_all[iqwig_all$AnzahlStudien == 2, ]
cat ("Number of MA with 2 studies", length(unique(data_two_studies$MA_id)))



# put this as last so I can use source in my master file
data_2_studies
