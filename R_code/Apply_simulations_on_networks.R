
# =======================================================================
# Author: Max D Campbell
#
# This script contains some examples of copy pasted code from my previous 
# projects (written by myself). This showcase script is not designed to run. 
# Some of the major skills demonstrated here are: 
# loops, parallel processing, simulations, metaprogramming, vectorisation,
# functional programming, subsetting/indexing, network analysis, 
# 
# Feel free to contact me with any queries or for more information.
# ========================================================================


library(tidyverse)
library(igraph)
library(foreach)
library(doParallel)

# source functions 
source("Functions/Conversion_functions.R")
source("Functions/Stochastic_disp_model.R")
source("Functions/Stochastic_rec_model.R")
source("Functions/Seed_functions.R")
source("Functions/Generate_graphs_functions.R")


######################## Set Parameters ##########################################

# Network sizes
size_vect <- c(240, seq(30, 225, by = 15)) #by 15

# Number of networks to test
n_networks <- 100

# Average degree of the networks
av_deg_vect <- c(24, seq(2, 22, by = 2)) #2

# Setup the stressor treatments if running on server
# Choose light and temp levels plus name
if(!("on_cloud" %in% ls())){
  
  Experiment_name <- "2022-02-11_Experiment_3_100reps_temp32_light2-4"
  temp_lev <- c( "normal" = 30.5, "high" =  32) 
  light_lev <- c("normal" = 1000, "low" = 2.4*1000/24)
  templight <- expand.grid( T. = temp_lev, I. = light_lev)
  
}

# SPECIFY MODELS TO GENERATE GRAPHS FROM FOR experiment
sample_models <- list()
sample_models[[1]] <- expression(sample_pa(n = size, m = av_deg/2, power = 0, directed = FALSE))
sample_models[[2]] <- expression(sample_pa(n = size, m = av_deg/2, power = 1, directed = FALSE))
sample_models[[3]] <- expression(sample_random_connected(n = size, average_degree = av_deg)) 
sample_models[[4]] <- expression(sample_smallworld(1, size = size, nei = av_deg/2, p = 0.5))

# Initialize storage vectors
graph_info <- NULL
graph_list <- NULL
model_results <- NULL
treatment_vector <- NULL

#setup parallel back end to use many processors
cores <- 15 # Change this to 20 for the cloud
cl <- parallel::makeCluster(cores, setup_strategy = "sequential")
registerDoParallel(cl)

set.seed(100)


# Function to run both models on the network with certain parameters
run_models_on_l<- function(l, temp_params){
  
  temp_params[["Q"]] <- as.matrix(temp_graphs[[2]][[l]])
  
  # Disturb all the ones that the epicentre is connected to
  temp_params[["B_init"]] <- ifelse(temp_params[["Q"]][,epicentres[[l]]], 0, 667 )
  temp_params[["B_init"]][epicentres[[l]]] <- 0 # disturb the epicentre
  # Make initial biomass start at equilibrium
  temp_params[["B_init"]] <- Find_B_star(params = temp_params, keep_perturbed = TRUE)
  
  result <- list(c(list(
    Stochastic_disp_model(temp_params, keep_timeseries = FALSE)[
      c("model", "type","disturbance_type", "time")]), 
    list(Stochastic_rec_model(temp_params, keep_timeseries = FALSE)[
      c("model", "type", "disturbance_type", "time")])))
  result
}

for (i in seq_along(size_vect)){
  for(j in seq_along(av_deg_vect)){
    
    # Specify how many nodes (patches)
    size <- size_vect[[i]]
    
    # Specify average degree
    av_deg <- av_deg_vect[[j]]
    
    # Specify average degree ()
    this_param_set <- list(
      
      # Patch parameters
      B.max = rep(667, size), 
      T. = rep(35, size),      # Temp     
      I. = rep(1000, size),    # Irradiance
      M = rep(0.004, size), # Default 0.004
      B_init = rep(667, size),
      
      # Inherent parameters
      PT.max = mg.C.per.h_to_g.dry.wt.per.d(4.7),  
      T.opt = 34.9, # optimum temperature   
      T.max = 44.5, # Maximum temperature 
      Ik = 319,     # Saturation irradiance       
      R.max = mg.C.per.h_to_g.dry.wt.per.d(1.1), #,  # Maximum respiration
      RT.opt = 39.1,  # Resp optimum temperature
      RT.max = 45.6,
      
      # Global parameters
      n = size,
      tmax = 365*100*2, # default 100 yrs
      dt = 1, # Step size
      Q = NA, # Transfer per biomass matrix
      E = 2, # Extinction threshold population - has to be less than 1% of B.max to repopulate
      
      # Seed transfer function parameters
      nu = 0.01, # steepness default 0.01 
      phi = 500, # phase shift of curve default 500
      psi = 1, # generalised logistic function parameter: logistic function = 1, Gompertz = 0+.
      d = 0.01 * 667, # Re-population biomass # needs d/dt > E otherwise can't repopulate
      l_max = 0.2 # Maximum probability of recovery in a year
    )
    
    
    # Generate adjacency matrices for size i and average degree j
    temp_graphs <- simulate_graphs(sample_models, reps = n_networks)
    
    # Make a random perturbation
    epicentres <- sample(seq_len(size), size = n_networks*length(sample_models), 
                         replace = TRUE)
    
    # Compute local graph statistics
    local_stats <- map2_df(.x = temp_graphs[[2]], .y = epicentres, 
                           ~ get_local_statistics(g = .x, epicentre = .y))
    
    # Save graph info
    graph_info <- rbind(graph_info, within(cbind(temp_graphs[[1]], local_stats), 
                                           epicentre <- epicentres))
    graph_list <- c(graph_list, temp_graphs[[2]])
    
    
    # For each treatment
    for (k in 1:nrow(templight)){
      
      # Update treatment parameters
      temp_params <- this_param_set
      temp_params$T. <- as.vector(templight[k,, drop = FALSE]$T.)
      temp_params$I. <- as.vector(templight[k,, drop = FALSE]$I.)
      
      model_list <- foreach(l = seq_along(temp_graphs[[2]]), .combine='c', .inorder=TRUE ,
                            .packages = c('tidyverse')) %dopar% 
        run_models_on_l(i, temp_params)
      
      model_results <- c(model_results, model_list)
    }
  }
}

stopCluster(cl)


save(model_results, file = 'Data/model_results.RDA')
