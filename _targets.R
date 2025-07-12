# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tidyverse", "googlesheets4"), # packages that your targets need to run
  format = "rds" # default storage format
)


# Run the R scripts in the R/ folder with your custom functions:
source("2-code/respiration.R")
source("2-code/weom.R")
source("2-code/xct.R")

# Replace the target list below with your own:
list(
  # import metadata
  tar_target(corekey, read_sheet("https://docs.google.com/spreadsheets/d/1qpfho6Z7aHYg9zkT0dMtQy7Oth5MkVtFc7O7rB45_t4")),
  
  # respiration
  tar_target(licor_map, read_sheet("https://docs.google.com/spreadsheets/d/1CkMjaIOUSHGJJloa4W4ILtFO3CVoeRCgK3Cf1GTom5o/", 
                                   sheet = "Sheet1", col_types = "c")),
  tar_target(licor_data, import_licor_data(FILEPATH = "1-data/respiration_subset")),
  tar_target(licor_processed_ppm, process_licor_data(licor_data, licor_map, corekey)),
  tar_target(licor_processed_rates, compute_rates(licor_processed_ppm)),
  tar_target(licor_cumulative, cumulative_evolution(licor_processed_rates)),

  
  # weom-mbc
  tar_target(subsampling, read_sheet("https://docs.google.com/spreadsheets/d/1Zab9xJK-ACkxCwRfqAXps8NCVfx0ZkX2RyhmJ_92sWc/edit?gid=1453401056#gid=1453401056", sheet = "subsampling-USE THIS")),
  tar_target(weoc_data, import_weoc_mbc(FILEPATH = "1-data/weom", PATTERN = "summary")),
  tar_target(weoc_processed, process_weoc(weoc_data, subsampling)),
  
  tar_target(mbc_data, import_weoc_mbc(FILEPATH = "1-data/mbc", PATTERN = "summary")),
  tar_target(mbc_processed, process_mbc(mbc_data, subsampling)),
  
  # tc-tn
  tar_target(tctn_data, import_tctn(FILEPATH = "1-data/tctn", PATTERN = ".csv")),
  tar_target(tctn_processed, process_tctn(tctn_data, subsampling)),
  
  # xct
  ## xct summary files
  tar_target(xct_summary, import_xct_summaries(FILEPATH = "1-data/xct/csv-summary", PATTERN = ".csv")),
  tar_target(xct_summary_processed, process_xct_summaries(xct_summary, corekey)),
  
  ## xct pnm files
  tar_target(xct_pnm, import_xct_pnm(FILEPATH = "1-data/xct/csv-pnm", PATTERN = ".csv")),
  tar_target(xct_pnm_processed, process_xct_pnm(xct_pnm, corekey)),
  
  # combined chemistry data
  tar_target(combined_chemistry, combine_chemistry_data(weoc_processed, mbc_processed, tctn_processed)),
  
  tar_render(report, path = "3-reports/egu25.Rmd")
  
  
)
