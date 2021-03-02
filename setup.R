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
  # modeling
  broom, caret, corrplot, corrr, dismo, mgcv, randomForest, sdm,
  # data wrangling
  dplyr, purrr, readr, readxl, tibble, tidyr, 
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
dir_models   <- here("data/models")
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

d_cols <- read_csv(cols_new_csv, col_types = cols()) %>% 
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

cols_cat  <- unique(d_cols$cat) %>% setdiff(c("_other", "_response")) %>% sort()
cols_resp <- cols_resp %>% setdiff("g_perf") %>% sort()
# cols_pred

# functions ----
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

get_pred1_mdls <- function(cat, resp){
  
  mdls <- read_csv(here("data/pred_lm_p05.csv"), col_types = cols()) %>% 
    left_join(
      d_cols %>% 
        filter(!str_starts(cat, "_")) %>% 
        select(pred = new, cat), by = "pred")
  
  mdls %>% 
    filter(
      # cat  == "landscape",
      # resp == "g_pres") %>% 
      cat  == !!cat,
      resp == !!resp) %>% 
    # get lowest AIC of both linear models, without (lm1) and with x^2 (lm2) [n = 8 to 6]
    arrange(pred, AIC) %>% 
    group_by(pred) %>% 
    summarize(
      mdl = first(mdl),
      AIC = first(AIC))
}

get_cor <- function(d, preds, cor_threshold = 0.7){
  # preds = preds_lnd$pred; cor_threshold = 0.7
  
  d %>% 
    select(all_of(preds)) %>% 
    select(!where(is.factor)) %>% # drop: asp_cir;
    # TODO: figure out factors
    correlate(method = "spearman") %>%
    shave %>% 
    gather(-term, key = "term2", value = "cor") %>% 
    filter(abs(cor) > cor_threshold) %>% 
    rowid_to_column(var = "cor_id") %>% 
    pivot_longer(
      cols = starts_with("term"),
      names_to="term", values_to="pred") %>% 
    select(-term)
}

filt_mdls_cor <- function(d_mdls, d_cor){
  # d_mdls = pred1_mdls; d_cor = cor_pred1
  
  # choose lowest AIC between auto-correlated predictors
  mdls_cor <- d_mdls %>% 
    left_join(d_cor, by="pred")
  
  preds_notcor <- mdls_cor %>% 
    filter(is.na(cor_id)) %>% 
    pull(pred)
  
  preds_cor <- mdls_cor %>% 
    filter(!is.na(cor_id)) %>% 
    arrange(AIC) %>% 
    pull(pred) %>% 
    unique()
  
  i = 2
  while(length(preds_cor > 0)){
    message(glue("filt_mdls_cor() pass {i} (n={length(preds_cor)}): {paste(preds_cor, collapse = ' + ')}"))
    
    d_cor_w <- get_cor(d, preds_cor)
    
    preds_cor <- d_mdls %>% 
      filter(pred %in% d_cor_w$pred) %>% 
      left_join(d_cor_w, by="pred") %>% 
      arrange(AIC) %>% 
      group_by(cor_id) %>%
      summarize(
        pred = first(pred),
        mdl  = first(mdl),
        AIC  = first(AIC)) %>% 
      pull(pred) %>% 
      unique()
    
    if (length(preds_cor) == 1){
      preds_notcor <- c(preds_notcor, preds_cor)
      break()
    }
    
    d_cor_w <- get_cor(d, preds_cor)
    
    preds_notcor <- c(
      preds_notcor,
      d_cor_w %>% 
        filter(is.na(cor_id)) %>% 
        pull(pred))
    
    preds_cor <- d_cor_w %>% 
        filter(!is.na(cor_id)) %>% 
        pull(pred) %>% 
      unique()
    
    if (length(preds_cor) == 1){
      preds_notcor <- c(preds_notcor, preds_cor)
      break()
    }
    
    i <- i+1
  }
  
  d_mdls %>% 
    filter(pred %in% preds_notcor)
}

d_pres_num_lng <- get_resp_pred_num_long(d_all, "g_pres", cols_pred)

d_pres_num_avg <- get_resp_pred_smry_stats(d_pres_num_lng, "g_pres")

is_binary <- function(x){ length(unique(x)) == 2 }

get_preds_sign_notcor <- function(cat, resp){
  
  # get significant predictors based individual models: 
  #   resp ~ pred (lm1); or resp ~ pred + pred^2 (lm2)
  pred1_mdls <- get_pred1_mdls(resp = resp, cat = cat)
  
  if (nrow(pred1_mdls) <= 1)
    return(pred1_mdls)
  
  # evaluate correlation between predictors
  cor_pred1 <- get_cor(d, pred1_mdls$pred)
  
  # of auto-correlated predictors, choose one with lowest AIC
  pred1_mdls_notcor <- filt_mdls_cor(pred1_mdls, cor_pred1)
  
  # check that no correlations still exist
  pred1_mdls_notcor2 <- get_cor(d, pred1_mdls_notcor$pred)
  stopifnot(nrow(pred1_mdls_notcor2) == 0)
  
  pred1_mdls_notcor
}

run_maxent <- function(cat, resp, preds, use_cache = T){
  # silent args: d, dir_models
  # cat   = "landscape"
  # resp  = "g_pres"
  # preds = d_preds$pred
  
  library(dismo)
  
  dir_mx       <- glue("{dir_models}/{cat}.{resp}.mx")
  factors_csv  <- glue("{dir_mx}/data_factors.csv")
  train_csv    <- glue("{dir_mx}/data_train.csv")
  test_csv     <- glue("{dir_mx}/data_test.csv")
  fit_rds      <- glue("{dir_mx}/maxent_fit.rds")
  evaluate_rds <- glue("{dir_mx}/maxent_evaluate.rds")
  dir_create(dir_mx)
  
  has_cache <- all(file_exists(c(
    factors_csv, train_csv, test_csv, fit_rds, evaluate_rds)))
  
  if (has_cache & use_cache){
    
    d_train     <- read_csv(train_csv, col_types = cols())
    d_test      <- read_csv(test_csv, col_types = cols())
    d_preds_fac <- read_csv(factors_csv, col_types = cols())
    preds_fac   <- unique(d_preds_fac$pred)
    mx          <- read_rds(fit_rds)
    e           <- read_rds(evaluate_rds)
    
    return(list(
      d_train     = d_train,
      d_test      = d_test,
      preds_fac   = preds_fac,
      d_preds_fac = d_preds_fac,
      maxent      = mx,
      evaluate    = e))
  }
  
  # prep data ----
  
  d_preds   <- d %>% 
    select(all_of(preds))
  # flag predictors that are of class factor
  preds_fac <- d_preds %>% 
    select_if(is.factor) %>% 
    names()
  d_preds_fac <- tibble(
    pred = preds_fac) %>% 
    mutate(
      levels = map(pred, function(x){ 
        tibble(
          level = levels(d[[x]])) %>% 
          rowid_to_column("integer")})) %>% 
    unnest(levels)
  preds_fac <- unique(d_preds_fac$pred)
  # convert factor predictors to integer for maxent to run
  d_preds   <- d_preds %>% 
    mutate(
      across(where(is.factor), ~ as.integer(.x)))
  
  # combine response and predictors
  d_rp <- d %>% 
    select(all_of(resp)) %>% 
    bind_cols(
      d_preds)
  
  # split data into training (to fit maxent) and test (to evaluate), 80% and 20% respectively
  i_k     <- kfold(d_rp, 5)
  d_train <- d_rp[i_k != 1, ]
  d_test  <- d_rp[i_k == 1, ]
  d_train     %>% write_csv(train_csv)
  d_test      %>% write_csv(test_csv)
  d_preds_fac %>% write_csv(factors_csv)
  
  message("d_train response counts:")
  print(table(d_train[[resp]]))
  message("d_test response counts:")
  print(table(d_test[[resp]]))
  
  
  # run maxent ----
  # setup input args for maxent
  v_resp  <- d_train %>% pull(resp)
  d_preds <- d_train %>% select(-all_of(resp))
  
  mx <- maxent(
    p       = v_resp,
    x       = d_preds, 
    factors = preds_fac,
    path    = dir_mx)
  write_rds(mx, fit_rds)
  
  # evaluate ----
  d_test_present <- d_test %>% 
    filter(across(all_of(resp), ~ .x == 1)) %>% 
    select(-all_of(resp))
  d_test_absent <- d_test %>% 
    filter(across(all_of(resp), ~ .x == 0)) %>% 
    select(-all_of(resp))
  
  e <- evaluate(
    p = d_test_present, 
    a = d_test_absent, 
    mx)
  write_rds(e, evaluate_rds)

  list(
    d_train     = d_train,
    d_test      = d_test,
    preds_fac   = preds_fac,
    d_preds_fac = d_preds_fac,
    maxent      = mx,
    evaluate    = e)
}

run_randomForest <- function(cat, resp, preds, use_cache = T){
  # silent args: d, dir_models
  # cat   = "landscape"; resp  = "g_pres"; preds = d_preds$pred; use_cache = F
  
  dir_mx       <- glue("{dir_models}/{cat}.{resp}.rf")
  train_csv    <- glue("{dir_mx}/data_train.csv")
  test_csv     <- glue("{dir_mx}/data_test.csv")
  fit_rds      <- glue("{dir_mx}/randomForest_fit.rds")
  evaluate_rds <- glue("{dir_mx}/randomForest_evaluate.rds")
  dir_create(dir_mx)
  
  has_cache <- all(file_exists(c(
    train_csv, test_csv, fit_rds, evaluate_rds)))
  
  if (has_cache & use_cache){
    
    return(list(
      d_train      = read.csv(train_csv, stringsAsFactors = T) %>% tibble(),
      d_test       = read.csv(test_csv,  stringsAsFactors = T) %>% tibble(),
      randomForest = read_rds(fit_rds),
      evaluate     = read_rds(evaluate_rds)))
  }
  
  # prep data ----
  d_rp   <- d %>% 
    select(all_of(resp), all_of(preds))
  if (!is_binary(d[[resp]])){
    ## http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/
    ## https://stat-ata-asu.github.io/MachineLearningToolbox/classification-models-fitting-them-and-evaluating-their-performance.html#customizing-traincontrol
    rf_trControl <- trainControl(
      method = "cv", number = 10, verboseIter = F)
  } else {
    d_rp[[resp]] <- factor(c('absent','present')[d_rp$g_pres + 1])
    rf_trControl <- trainControl(
      method = "cv", number = 10, verboseIter = F,
      # https://stat-ata-asu.github.io/MachineLearningToolbox/classification-models-fitting-them-and-evaluating-their-performance.html#customizing-traincontrol
      summaryFunction = twoClassSummary, classProbs = T)
  }
  
  # split data into training (to fit randomForest) and test (to evaluate), 80% and 20% respectively
  i_k     <- kfold(d_rp, 5)
  d_train <- d_rp[i_k != 1, ]
  d_test  <- d_rp[i_k == 1, ]
  d_train %>% write_csv(train_csv)
  d_test  %>% write_csv(test_csv)
  
  # run randomForest ----
  rf_formula <- as.formula(glue("{resp} ~ ."))
  # OLD: direct randomForest
  # rf <- randomForest(
  #   as.formula(rf_formula_str),
  #   data = d_train,
  #   importance = T, proximity = T)
  
  # NEW: caret with 10-fold cross-validation
  rf <- train(
    rf_formula, d_train,
    method = "rf",
    trControl = rf_trControl,
    preProcess = c("center", "scale", "YeoJohnson", "nzv"))
    
  # rf$finalModel
  write_rds(rf, fit_rds)
  
  e <- postResample(pred = predict(rf, d_test), obs = d_test[[resp]])
  write_rds(e, evaluate_rds)
  # binary response:
  ##  Accuracy     Kappa 
  ## 0.8460000 0.6081345
  #
  # continuous response:
  ##      RMSE  Rsquared       MAE 
  ## 4.0927043 0.8234427 2.8163731
  #
  # https://stats.stackexchange.com/questions/22344/which-performance-measure-to-use-when-using-svm-mse-or-mae
  #   MAE≤RMSE≤MAE2  (for regression)
  #     - if RMSE is close to MAE, the model makes many relatively small errors
  #     - if RMSE is close to MAE2, the model makes few but large errors
  #   https://www.datatechnotes.com/2019/02/regression-model-accuracy-mae-mse-rmse.html
  
  list(
    d_train      = d_train,
    d_test       = d_test,
    randomForest = rf,
    evaluate     = e)
}

get_mdl_y <- function(mdl){
  # mdl <- rf
  m <- ifelse(
    "randomForest" %in% names(mdl),
    mdl['randomForest'],
    mdl['maxent'])
  
  d_p <- bind_rows(
    tibble(
      mdl$d_test,
      partition = "test"),
    tibble(
      mdl$d_train,
      partition = "train")) %>%
    rename(observed = 1)
  d_p %>% 
    mutate(
      predicted = predict(m, d_p)[[1]]) %>% 
    select(partition, observed, predicted)
}

get_nrmse <- function(cat, resp, mdl="rf", method = "sd"){
  # cat = "biotic"; resp = "g_cnt"; mdl="rf"
  
  # normalize root mean square error for comparing performance of models with different response terms
  # * [How to normalize the RMSE](https://www.marinedatascience.co/blog/2019/01/07/normalizing-the-rmse/)
  # * [nrmse: Normalized Root Mean Square Error in saskiaotto/INDperform: Evaluation of Indicator Performances for Assessing Ecosystem States](https://rdrr.io/github/saskiaotto/INDperform/man/nrmse.html)
  # * [saskiaotto/INDperform: INDperform is an R package for evaluating ecological state indicators and assessing the ecological status based on a suite of indicators.](https://github.com/saskiaotto/INDperform/)
  shelf(saskiaotto/INDperform)
  
  fxn     <- ifelse(mdl == "rf", "randomForest", "maxent")
  dir_mdl <- glue("{dir_models}/{cat}.{resp}.{mdl}")
  m       <- read_rds(glue("{dir_mdl}/{fxn}_fit.rds"))
  d_test  <- read_csv(glue("{dir_mdl}/data_test.csv"), col_types = cols())
  
  nrmse(
    pred   = predict(m, d_test),
    obs    = d_test %>% pull(resp),
    method = method)
}

get_terms <- function(cat, resp, mdl="rf"){
  # cat = "biotic"; resp = "g_cnt"; mdl="rf"
  # cat = "landscape"; resp = "g_repro"; mdl="rf"
  # cat = "landscape"; resp = "g_repro"; mdl="rf"
  
  fxn     <- ifelse(mdl == "rf", "randomForest", "maxent")
  dir_mdl <- glue("{dir_models}/{cat}.{resp}.{mdl}")
  m       <- read_rds(glue("{dir_mdl}/{fxn}_fit.rds"))
  paste(attr(terms(m), "term.labels"), collapse = " + ")
}

get_accuracy <- function(cat, resp, mdl="rf"){
  fxn     <- ifelse(mdl == "rf", "randomForest", "maxent")
  dir_mdl <- glue("{dir_models}/{cat}.{resp}.{mdl}")
  e       <- read_rds(glue("{dir_mdl}/{fxn}_evaluate.rds"))
  if(mdl == "rf")
    a <- e[["Accuracy"]]
  
  if (mdl == "mx"){
    thr    <- threshold(e, 'spec_sens')
    m      <- read_rds(glue("{dir_models}/{cat}.{resp}.{mdl}/{fxn}_fit.rds"))
    d_test <- read_csv(glue("{dir_models}/{cat}.{resp}.{mdl}/data_test.csv"), col_types = cols())
    y_prob <- suppressWarnings(predict(m, d_test))
    y_int <- as.integer(y_prob > thr)
    y_yes <- d_test[,1] == y_int
    a <- sum(y_yes)/nrow(d_test)
  }
  a
}


get_stat <- function(cat, resp, mdl="rf", fxn="evaluate", stat = "RMSE"){
  # cat="landscape"; resp="g_pres"; mdl="rf"; fxn="evaluate"; stat = "Accuracy"
  # cat="landscape"; resp="g_pres"; mdl="rf"; fxn="fit"; stat = "Accuracy"
  # cat="landscape"; resp="g_pres"; mdl="mx"; fxn="evaluate"; stat = "Accuracy"
  fit <- ifelse(mdl == "rf", "randomForest", "maxent")
  o_rds <- glue("{dir_models}/{cat}.{resp}.{mdl}/{fit}_{fxn}.rds")
  o <- read_rds(o_rds)
  if (mdl == "mx" & stat == "Accuracy"){
    thr    <- threshold(o, 'spec_sens')
    m      <- read_rds(glue("{dir_models}/{cat}.{resp}.{mdl}/{fit}_fit.rds"))
    d_test <- read_csv(glue("{dir_models}/{cat}.{resp}.{mdl}/data_test.csv"), col_types = cols())
    y_prob <- suppressWarnings(predict(m, d_test))
    y_int <- as.integer(y_prob > thr)
    y_yes <- d_test[,1] == y_int
    Accuracy <- sum(y_yes)/nrow(d_test)
    return(Accuracy)
  }
  o[[stat]]
}
