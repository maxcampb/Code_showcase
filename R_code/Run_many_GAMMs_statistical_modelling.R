
# =======================================================================
# Author: Max D Campbell
#
# This script contains some examples of copy pasted code from my previous 
# projects (written by myself). This showcase script is not designed to run. 
# Some of the major skills demonstrated here are: 
# working with lists, statistical modelling with gams, metaprogramming,
# functional programming, subsetting/indexing , etc.
# 
# Feel free to contact me with any queries or for more information.
# ========================================================================


# Specify modelling variables
response <- expr(Y)
smoother_types <- c("tp", "ts", "cr", "gp")
k_set_vect <- c(3,4,5, 6)
knots <- list(month_centre = c(0.5, 12.5))
delta_vars <- c("X2_s", "X3_s", "X4_s") # specify which variables to compute deltas for 

# Make splits using generalised split function
train_window <- 10
test_window <- 3
include_row <- !is.na(envdata[[deparse1(response)]]) # check for nas is vars
gensplit_output <- generalised_split(dat = envdata[include_row,], 
                                     train_window = train_window, 
                                     test_window = test_window )
model_test_df <- gensplit_output[[2]]
split_list <- gensplit_output[[1]]


mod_exprs <- list(
  # Model with penalising smoothness on the 1st derivative
  gam_penal_first = expr({
    gam(!!response ~ X1 +
          s(X2_s, k = k_set, bs = smooth_ty, m = 1) + 
          s(X3_s, k = k_set, bs = smooth_ty, m = 1) + 
          s(X4_s, k = k_set, bs = smooth_ty, m = 1) + 
          s(month_centre, k = 5, bs = "cc" ) + 
          s(year, k = train_window-1, by = X1, m = 1) + 
          s(site_id, bs = "re"), knots = knots,
        data = train#, select = TRUE#, method = "REML"
    )}),
  
  # model with default selection
  gam_norm_penalty = expr({
    gam(!!response ~ X1 +
          s(X2_s, k = k_set, bs = smooth_ty) + 
          s(X3_s, k = k_set, bs = smooth_ty) + 
          s(X4_s, k = k_set, bs = smooth_ty) + 
          s(month_centre, k = 5, bs = "cc" ) + 
          s(year, k = train_window-1, by = X1) + 
          s(site_id, bs = "re"), knots = knots,
        data = train#, select = TRUE#, method = "REML"
    )}),
  
  # model with select = TRUE comparison
  gam_heavy_penalty = expr({
    gam(!!response ~ X1 +
          s(X2_s, k = k_set, bs = smooth_ty) + 
          s(X3_s, k = k_set, bs = smooth_ty) + 
          s(X4_s, k = k_set, bs = smooth_ty) + 
          s(month_centre, k = 5, bs = "cc" ) + 
          s(year, k = train_window-1, by = X1) +
          s(site_id, bs = "re"), knots = knots,
        data = train, select = TRUE#, method = "REML"
    )})
  
)

if (NEED_RERUN){
  
  # Create data frame and training data ids to take advantage of the structure of 
  # the problem - this is a bit abstract but it saves us having to refit the same model
  # when the same training data is used
  train_dfs <- map(.x = seq_along(split_list), ~ split_list[[.x]][["train"]])
  train_ids <- match(train_dfs, train_dfs)
  train_dfs <- train_dfs[unique(train_ids)] %>% setNames(nm = unique(train_ids))
  mods_run <- expand_grid(smoother_types, k_set_vect, model_name = names(mod_exprs), 
                          train_id = unique(train_ids))
  
  # Fit all the models we need to for the experiment using purrr
  system.time({models <- map(seq_len(nrow(mods_run)), function(x) {
    
    with(mods_run, {
      
      train <- train_dfs[[as.character(train_id[[x]])]]
      k_set <- k_set_vect[[x]]
      smooth_ty <- smoother_types[[x]]
      
      return(eval(mod_exprs[[ model_name[[x]] ]]))
      
    })
  })})
  
  # Create dataframe which matches the models with the training/testing datasets
  model_comparisons <- mods_run %>% mutate(.,model_no = seq_len(nrow(.))) %>% 
    rename(k_set = k_set_vect, smooth_ty = smoother_types) %>% 
    left_join(data.frame(train_id = train_ids, split_no = seq_along(split_list),
                         model_test_df), by = "train_id") # add training and testing year start
  
  # Run the all_years prediction function using the method where the models are already 
  # fitted - this is where we save a lot of computing time
  system.time({model_comparison_dat <- 
    pmap_df(select(model_comparisons, model_name, model_no, split_no), 
            .f = function(model_name, model_no, split_no) { 
              all_years(mod_name = model_name, mod = models[[model_no]],
                        train = split_list[[split_no]]$train,
                        test = split_list[[split_no]]$test,
                        delta_vars = delta_vars)})
  })
  
  # Add other variables
  model_comparison_dat <- model_comparison_dat %>% 
    cbind(select(model_comparisons, k_set, smooth_ty, train_start_year, test_start_year)) %>% 
    mutate(train_window = train_window) 
  
  
  # Run intercept only model for comparisons using the all_fit method with model expression -
  # only a few models here so not worth speeding it up
  intercept_models <- cbind(model_test_df,
                            # run prediction variables for all models
                            map_df(.x = seq_along(split_list), ~
                                     all_years(mod_name = "intercept_only",
                                               mod_expr = expr({
                                                 gam(!!response ~ 1,
                                                     data = train#, select = TRUE#, method = "REML"
                                                 )}),
                                               train = split_list[[.x]]$train,
                                               test = split_list[[.x]]$test, 
                                               delta_vars = delta_vars))) %>%
    within({ # Add model variables
      
      
      k_set <- NA
      smooth_ty <- NA
      train_window <- train_window
      
    })
  
  # Add in the intercept only model to the data
  model_comparison_dat <- rbind(model_comparison_dat, intercept_models) 
  
  save(intercept_models, model_comparison_dat,
       file = sprintf("Shared/Data/%s_model_datasets_002A_method_train%s_test%s.RDA",
                      deparse1(response), train_window, test_window))
  
}