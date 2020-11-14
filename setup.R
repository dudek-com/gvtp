# load libraries ----
if (!require(librarian)){
  install.packages(librarian)
  library(librarian)
}
# load libraries, install if missing
shelf(
  # tables
  DT,
  # plots
  ggplot2, trelliscopejs, gapminder, # kassambara/easyGgplot2
  # sdm
  corrplot, dismo, mgcv, randomForest, sdm,
  # data wrangling
  dplyr, purrr, readr, readxl, tidyr,
  # utilities
  fs, glue, here, htmltools, skimr)
# sdm::installAll() # run once
select = dplyr::select

# paths & variables ----
dir_gdrive <- case_when(
  Sys.info()[['user']] == "bbest" ~ "/Volumes/GoogleDrive/My Drive/projects/dudek")
d_xls    <- path(dir_gdrive, "data/gvtp_hab_char_data_compiled_Final.xls")
cols_xls <- path(dir_gdrive, "data/gvtp_hab_char_columns.xlsx")
# excel_sheets(d_xls)

# read sheets of data
plot   <- read_excel(d_xls, "Plot")
spp    <- read_excel(d_xls, "Species")
soil   <- read_excel(d_xls, "Soil Layers")
veg    <- read_excel(d_xls, "Veg")
herbht <- read_excel(d_xls, "Herb Height")
gvtpht <- read_excel(d_xls, "GVTP Height Heads")
rdm    <- read_excel(d_xls, "RDM")