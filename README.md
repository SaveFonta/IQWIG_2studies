# To Run

1) Create an `Input` folder in the main directiory and add there your data, with the name: `IQWiG-MA-Datenbank_Stand2024.xlsx`

2) Create the `Ouput` folder, where output will be loaded

3) Don't directly run the files starting with `00.`
 
4) If you don't want to look at it, don't bother to run  `01_exploratory_analysis.R` since all the important cleaning steps are performed in `02_data_cleaning.R`) 

5) The summary report can be rendered using `Summary_2.studies.qmd`

Note: the `master_for_generating.R` creates a singular report for EACH meta Analisis, so your output folder will be pretty full (around 1.4 GB) 

In the end it also generates an index file inside the Output that can be used to inspect singular MAs.
