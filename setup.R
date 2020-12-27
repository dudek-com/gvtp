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
  broom, corrplot, dismo, mgcv, randomForest, sdm,
  # data wrangling
  dplyr, purrr, readr, readxl, tidyr,
  # utilities
  fs, glue, here, htmltools, skimr, stringr, units)
# sdm::installAll() # run once
select = dplyr::select

# paths & variables ----
dir_gdrive <- case_when(
  Sys.info()[['user']] == "bbest" ~ "/Volumes/GoogleDrive/My Drive/projects/dudek")
d_xls        <- path(dir_gdrive, "data/gvtp_hab_char_data_compiled_Final.xls")
cols_csv     <- path(dir_gdrive, "data/gvtp_hab_char_columns.csv")
cols_new_csv <- path(dir_gdrive, "data/gvtp_columns.csv")
d_csv        <- here("data/pred_resp.csv")
# excel_sheets(d_xls)

# read sheets of data ----
plot   <- read_excel(d_xls, "Plot", na = c("na", "n d"))
spp    <- read_excel(d_xls, "Species")
soil   <- read_excel(d_xls, "Soil Layers")
veg    <- read_excel(d_xls, "Veg")
herbht <- read_excel(d_xls, "Herb Height")
gvtpht <- read_excel(d_xls, "GVTP Height Heads")
rdm    <- read_excel(d_xls, "RDM")

# Soil Layers	
# plot	5x5-meter plot unique ID (1-40)
# spatial_data	"Gaviota tarplant 2019 areal extent (n=20): As mapped during focused surveys in 2019;
# Gaviota tarplant CNDDB occurrences (n=10): Gaviota tarplant 2019 areal extent excluded;
# Gaviota tarplant suitable but unoccupied habitat (n=10): Suitable habitat defined for these purposes as all native and non-native grassland communities, and all soil textures occupied by Gaviota tarplant on site.
# "
# soil_layer	Depth of the layer in inches starting from the soil surface.
# soil_texture	Soil texture of layer using USDA NRCS's "A flow diagram for teaching texture by feel analysis"
# notes_soil	notes regarding soil texture analysis

# transformations ----
plot <- plot %>% 
  # Important vars:
  # X Soil texture: soil_texture
  # X Soil disturbance (5x5): soil_dist_cover
  # X Soil disturbance cir (20m diameter): soil_dist_cover_cir
  # X Bare ground: bare_grnd_cover
  # X Average weight of Residual dry matter: dry_wt_lbs_acre
  # X Herbaceous plant height: herbheight_avg_cm
  # X Total plant species cover: sum(Species.percent_cover_abs) = pct_plant_cover
  # X Non-native plant species cover: TODO: sum(native), sum(naturalized): pct_native_cover, pct_nonnative_cover
  # X GVTP abundance: gvtp_count
  # X GVTP cover: gvtp_cover
  # X GVTP plant heights (average): gvtp_height_cm_avg
  # X Number of GVTP inflorescences per plant (average): gvtp_heads
  filter(!is.na(plot)) %>% 
  select(
    # not for modeling
    -spatial_data, -photo_sq_plot, -photo_circular_plot, -soil_dist_cause, -notes_plot, -`sample ID`, -description, 
    # skipping
    -lime) %>% 
  mutate(
    # RESPONSE: gvtp_count, gvtp_presence, gvtp_performance, gvtp_cover, gvtp_height_cm_avg, gvtp_heads, gvtp_reproductive_potential
    gvtp_count         = recode(gvtp_count, "100-499"="299") %>% as.numeric(),
    gvtp_presence      = as.integer(gvtp_count > 0),
    gvtp_cover         = as.numeric(gvtp_cover),
    gvtp_height_cm_avg = as.numeric(gvtp_height_cm_avg),
    gvtp_heads         = as.numeric(heads_no_avg),
    gvtp_percent_vegetative  = as.numeric(gvtp_percent_vegetative),
    gvtp_percent_flowering   = as.numeric(gvtp_percent_flowering),
    gvtp_percent_fruiting    = as.numeric(gvtp_percent_fruiting),
   ) %>% 
  replace_na(list(
    gvtp_performance   = 0,
    gvtp_height_cm_avg = 0,
    gvtp_heads         = 0,
    molybdenum = 0, 
    aluminum   = 0, 
    chromium   = 0, 
    mercury    = 0, 
    selenium   = 0, 
    silver     = 0)) %>% 
  mutate(
    gvtp_reproductive_potential = gvtp_count * gvtp_heads,
    # SOIL:
    `estimated soil texture`     = as.factor(`estimated soil texture`),
    `relative infiltration rate` = as.factor(`relative infiltration rate`),
    `organic matter`             = as.factor(`organic matter`),
    # LANDSCAPE: aspect_north, aspect_east, aspect_cir_north, aspect_cir_east, slope_degrees, slope_degrees_cir
    slope_degrees     = as.integer(slope_degrees),
    slope_degrees_cir = as.integer(slope_degrees_cir),
    # aspect
    aspect_deg   = recode(
      aspect, 
      N=0, NW=45, W=90, SW=135, S=180, SE=225, E=270, NE=315) %>% 
      as_units("degrees"),
    aspect       = as.factor(aspect),
    aspect_rad   = set_units(aspect_deg, "radians"),
    aspect_north = cos(aspect_rad) %>% as.numeric(),
    aspect_east  = sin(aspect_rad) %>% as.numeric(),
    aspect_deg   = as.numeric(aspect_deg),
    aspect_rad   = as.numeric(aspect_rad),
    aspect_cir_deg   = recode(
      aspect_cir, 
      N=0, NW=45, W=90, SW=135, S=180, SE=225, E=270, NE=315) %>% 
      as_units("degrees"),
    aspect_cir       = as.factor(aspect_cir),
    aspect_cir_rad   = set_units(aspect_cir_deg, "radians"),
    aspect_cir_north = cos(aspect_cir_rad) %>% as.numeric(),
    aspect_cir_east  = sin(aspect_cir_rad) %>% as.numeric(),
    aspect_cir_deg   = as.numeric(aspect_cir_deg),
    aspect_cir_rad   = as.numeric(aspect_cir_rad),
    # LANDSCAPE: concavity, concavity_cir, slope_degrees
    slope_shape     = recode(
      slope_shape,
      concave=1, flat=0, convex=-1),
    slope_shape_cir = recode(
      slope_shape_cir,
      concave=1, flat=0, convex=-1),
    slope_degrees   = as.integer(slope_degrees),
    slope_position  = recode(
      slope_position,
      "top of slope"=1, "mid-slope"=0, "toe of slope"=-1),
    # LANDSCAPE: soil_dist_cover_cir
    #   Q: [soil_dist_cover_cir] as abs percentage (%)
    soil_dist_cover_cir = as.integer(soil_dist_cover_cir),
    # BIOTIC: bare_grnd_cover, soil_dist_cover
    #   Q: [bare_grnd_cover] as abs percentage (%)
    #   Q: [soil_dist_cover] as abs percentage (%)
    bare_grnd_cover = as.integer(bare_grnd_cover),
    soil_dist_cover = as.integer(soil_dist_cover)) %>% 
  # BIOTIC: pct_plant_cover
  left_join(
    spp %>% 
    group_by(plot) %>% 
    summarize(
      pct_plant_cover = sum(percent_cover_abs, na.rm = T)),
    by = "plot") %>% 
  # BIOTIC: pct_native
  #   Q: does [pct_native] work vs original ask for [Native] & [Naturalized] "as relative cover like we did during the meeting"
  #   TODO: sum(native), sum(naturalized)
  left_join(
    spp %>% 
      group_by(plot, `Native/Naturalized`) %>% 
      summarize(
        pct_cover = sum(percent_cover_abs, na.rm = T)) %>% 
      pivot_wider(names_from = `Native/Naturalized`, values_from = pct_cover) %>% 
      replace_na(list(NATIVE = 0, NATURALIZED = 0)) %>% 
      mutate(
        pct_nativity    = NATIVE / (NATIVE + NATURALIZED)),
    by = "plot") %>% 
  rename(
    pct_native_cover    = NATIVE,
    pct_nonnative_cover = NATURALIZED) %>% 
  # BIOTIC: dry_wt_g
  left_join(
    rdm %>% 
      group_by(Plot) %>% 
      summarize(
        dry_wt_g            = mean(`Weight Sample (g)`, na.rm=T)) %>% 
      mutate(
        dry_wt_lbs_acre = dry_wt_g * 96.03341929),
    by = c("plot" = "Plot")) %>% 
  # transformed vars w/ diff't names 
  select(-heads_no_avg)
    
# ensure all columns converted to numeric or factor
stopifnot(ncol(select_if(plot, is.character)) == 0)

# drop columns without variance: mercury, selenium, silver
plot <- select_if(plot, function(x) var(as.numeric(x), na.rm=T) != 0)

# setup main data frame (d) ----
responses  <- select(plot, starts_with("gvtp")) %>% names()
predictors <- setdiff(names(plot), c("plot", responses)) # %>% sort()

d <- plot %>% 
  select(
    plot,
    all_of(responses), 
    all_of(predictors))

# abbreviate names
d_cols_abbr <- abbreviate(names(d), minlength=4)
d_cols <- tibble(
  column_original    = names(d_cols_abbr),
  column_abbreviated = d_cols_abbr)
#write_csv(d_cols, cols_csv)

d_cols <- read_csv(cols_new_csv) %>% 
  select(cat = category, new = column_new, old = column_original) %>% 
  arrange(cat, new)

stopifnot(all(names(d) %in% d_cols$old))

cols_resp  <- d_cols %>% 
  filter(
    cat == "_response",
    old %in% names(d)) %>% 
  pull(new)
cols_pred  <- d_cols %>% 
  filter(
    str_detect(cat, "^[^_]"),
    old %in% names(d))%>% 
  pull(new)

names(d) = d_cols$new[match(names(d), d_cols$old)]

# get columns ordered by category, variable
d_all <- select(d, any_of(d_cols$new))
d <- d_all
write_csv(d_all, d_csv)


get_resp_pred_num_long <- function(d_all, col_resp, cols_pred){
  # col_resp = "g_pres"
  d_all %>% 
    select(all_of(col_resp), any_of(cols_pred)) %>% 
    select_if(is.numeric) %>% 
    pivot_longer(-all_of(col_resp), "var") %>%
    arrange(var, all_of(col_resp), value)
}

get_resp_pred_smry_stats <- function(d_resp_pred_num_long, col_resp){
  d_resp_pred_num_long %>% 
    group_by(var, all_of(col_resp)) %>% 
    summarize(
      min = min(value, na.rm=T), 
      avg = mean(value, na.rm=T), 
      max = max(value, na.rm=T),
      sd  = sd(value, na.rm=T),
      .groups="drop")
}

get_var_ttest <- function(d_resp_pred_num_long, col_pred){
  # col_pred = "g_cnt"; d_resp_pred_num_long = d_cnt_lng
  
  frmla <- as.formula(glue("value ~ {col_pred}"))
  
  d_resp_pred_num_long %>% 
    group_by(var) %>% 
    summarize(
      t_test = oneway.test(frmla, data = ., var.equal = F)$p.value) %>% 
    arrange(desc(t_test))
}

d_pres_num_lng <- get_resp_pred_num_long(d_all, "g_pres", cols_pred)

d_pres_num_avg <- get_resp_pred_smry_stats(d_pres_num_lng, "g_pres")

