# Packages
All packages used can installed with 
```{r}
pacs <- c(
  "meta", "metafor", "bayesmeta", "irr",
  "dplyr", "tidyr", "tibble", "stringr", "purrr",
  "ggplot2", "gt", "patchwork", "ggbeeswarm", "ggridges", "ggforce", "plotly", "ggh4x",
  "future", "future.apply", "progressr", "pbapply", "parallelly", "quarto", "remotes"
)

install.packages(pacs)
```

**Notably**, `confMeta` package is update regularly, to not miss out on the updates, first delete the package and then reinstall it:

```{r}
remove.packages("confMeta")

remotes::install_github("SaveFonta/confMeta")
```

# To Run

1) Create an `Input` folder in the main directory and add there your data, with the name: `IQWiG-MA-Datenbank_Stand2025.xlsx`

2) Create the `Ouput` folder, where outputs will be saved

3) Don't directly run the files starting with `00.`
 
4) Run `02_data_cleaning.R` to perform all necessary cleaning steps. Note that `01_exploratory_analysis.R` is for inspection and is not required for the pipeline.

5) if you want to run `04_....qmd` files or `presentation_17_02.qmd`, run `03_data_processing` for their data preparation

6) if you  want to run `06_Index_MA.qmd` file, you need to have locally all the singular reports in a specific folder. Run `05_master_for_generating.qmd` (**COMPUTATIONAL AND MEMORY HEAVY**) to generate them


