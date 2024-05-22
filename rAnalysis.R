#__________________________________________________________________________

# Loading packages, data, etc. --------------------------------------------

#__________________________________________________________________________

# Turn off scientific notation
options(scipen=999)
# Install libraries
install.packages(c("ggplot2", "RTransferEntropy", "lme4", "lmerTest", "future", 
                   "performance", "stringr", "tidyr"))

# if this doesn't work, try changing to: version = "1.11.0"
devtools::install_version("rEDM", version= "0.7.1")

# Import libraries
library(ggplot2)
library(RTransferEntropy)
library(rEDM)
library(lme4)
library(lmerTest)
library(future)
library(performance)
library(stringr)
library(tidyr)

# Import sliding window-averaged data
rms_timeseries = read.csv("Data/rms_timeseries.csv")
#tonalcentroid_timeseries_raw = read.csv("Data/tonal_centroid_timeseries.csv")
#spectralflatness_timeseries = read.csv("Data/spectral_flatness_timeseries.csv")
# Import datasets with info about the trials
musicianDataset = read.csv("Data/endImpro_4R_booths_3_trio_12.csv", sep=";")
trialDataset = read.csv("Data/endingDataset_fromThomas_withconditions.csv", sep=";")

# 22050 samples per second; 3520 samples per window.
windowSizeInSeconds = (1/22050) * 3520

#_____________________________________________________________________________________

# Further preprocessing of Tonnetz data ----------------------------------------------

#_____________________________________________________________________________________
# 
# # Computing our 'tonnetz distance' measure. At each window, it computes the Euclidean distance between
# # the current and the previous tonnetz (both of which are 6-dimensional vectors). This is our measure of
# # harmonic change in a musician's playing.
# tonnetzdistance_timeseries = data.frame("g1_t1_b1" = rep(0, 3000)) # construct a sufficiently large DF.
# 
# for(group in 1:12) {
#   for(trial in 1:16) {
#     for(booth in 1:3) {
#       # Create string featuring the identifier of the current time series
#       colname = sprintf("g%s_t%s_b%s", group, trial, booth)
#       # Create empty vector, which will contain distance values between each windows tonnetz TONt and 
#       # the previous window's tonnetz, TONt-1.
#       distVec = c()
#       
#       curBooth = tonalcentroid_timeseries_raw[,grepl(colname, names(tonalcentroid_timeseries_raw))]
#       
#       for(row in 1:3000){
#         tonnetz = unname(unlist(curBooth[row,]))
#         if(row==1){
#           # For the very first window's tonnetz, compute the tonnetz distance as simply the distance between the current tonnetz and an all-zeroes tonnetz.
#           distance = dist(rbind(tonnetz, rep(0, 6)))[1]
#         } else {
#           # for all subsequent windows, compute the Euclidean distance between the current and previous tonnetz. 
#           # This represents a measure of harmonic change from one window to the next.
#           prevTonnetz = unname(unlist(curBooth[row-1,]))
#           distance = dist(rbind(tonnetz, prevTonnetz))[1]
#         }
#         # Append to our vector of tonnetz distances
#         distVec = c(distVec, distance)
#       }
#       # Add distance to dataframe
#       tonnetzdistance_timeseries[[colname]] = distVec
#     }
#   }
# }

#_____________________________________________________________________________________

# Plotting examples of acoustic feature time series ------------

#_____________________________________________________________________________________

# For each acoustic feature, make an example line plot of the 3 musicians in trio 1, trial 1.
# See figure 1 in text.

# # RMS amplitude
# rms_timeseries_example_b1 = rms_timeseries$g12_t1_b1[!is.na(rms_timeseries$g12_t1_b1)]
# rms_timeseries_example_b2 = rms_timeseries$g12_t1_b2[!is.na(rms_timeseries$g12_t1_b1)]
# rms_timeseries_example_b3 = rms_timeseries$g12_t1_b3[!is.na(rms_timeseries$g12_t1_b1)]
# rms_timeseries_example = data.frame(seconds=seq(length(rms_timeseries_example_b1)) * windowSizeInSeconds,
#                                                 b1=rms_timeseries_example_b1,
#                                                 b2=rms_timeseries_example_b2,
#                                                 b3=rms_timeseries_example_b3)[0:60/windowSizeInSeconds,]
# ggplot(data=rms_timeseries_example, aes(x=seconds)) +
#   geom_line(aes(y=b1), color="steelblue") +
#   geom_line(aes(y=b2), color="red") +
#   geom_line(aes(y=b3), color="black") +
#   ylab("RMS amplitude") +
#   theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 22))
# 
# # Tonnetz distance
# tonnetzdistance_timeseries_example_b1 = tonnetzdistance_timeseries$g12_t1_b1[!is.na(tonnetzdistance_timeseries$g12_t1_b1)]
# tonnetzdistance_timeseries_example_b2 = tonnetzdistance_timeseries$g12_t1_b2[!is.na(tonnetzdistance_timeseries$g12_t1_b1)]
# tonnetzdistance_timeseries_example_b3 = tonnetzdistance_timeseries$g12_t1_b3[!is.na(tonnetzdistance_timeseries$g12_t1_b1)]
# tonnetzdistance_timeseries_example = data.frame(seconds=seq(length(tonnetzdistance_timeseries_example_b1)) * windowSizeInSeconds,
#                                     b1=tonnetzdistance_timeseries_example_b1,
#                                     b2=tonnetzdistance_timeseries_example_b2,
#                                     b3=tonnetzdistance_timeseries_example_b3)[0:60/windowSizeInSeconds,]
# ggplot(data=tonnetzdistance_timeseries_example, aes(x=seconds)) +
#   geom_line(aes(y=b1), color="steelblue") +
#   geom_line(aes(y=b2), color="red") +
#   geom_line(aes(y=b3), color="black") +
#   ylab("Tonnetz distance") +
#   theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 22))
# 
# # Spectral flatness
# spectralflatness_timeseries_example_b1 = spectralflatness_timeseries$g12_t1_b1[!is.na(spectralflatness_timeseries$g12_t1_b1)]
# spectralflatness_timeseries_example_b2 = spectralflatness_timeseries$g12_t1_b2[!is.na(spectralflatness_timeseries$g12_t1_b1)]
# spectralflatness_timeseries_example_b3 = spectralflatness_timeseries$g12_t1_b3[!is.na(spectralflatness_timeseries$g12_t1_b1)]
# spectralflatness_timeseries_example = data.frame(seconds=seq(length(spectralflatness_timeseries_example_b1)) * windowSizeInSeconds,
#                                                 b1=spectralflatness_timeseries_example_b1,
#                                                 b2=spectralflatness_timeseries_example_b2,
#                                                 b3=spectralflatness_timeseries_example_b3)[0:60/windowSizeInSeconds,]
# ggplot(data=spectralflatness_timeseries_example, aes(x=seconds)) +
#   geom_line(aes(y=b1), color="steelblue") +
#   geom_line(aes(y=b2), color="red") +
#   geom_line(aes(y=b3), color="black") +
#   ylab("Spectral flatness") +
#   theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 22))

#_____________________________________________________________________________________

# ETE for real pairs / trials and random pairs ------------

#_____________________________________________________________________________________

# Enable parallel processing for all calc_ete() calls.
plan(multisession)

# REAL PAIRS + TRIALS
# Calculate ETE values for all 'real pairs' of musicians; i.e., same group, same trial. Also calculate
# ETE for trials by computing mean of all pairwise ETEs a trial. This will come in handy in RQ2.

# Create empty lists
rms_ETEvalues_perpair = list()
#spectralflatness_ETEvalues_perpair = list()
#tonnetzdistance_ETEvalues_perpair = list()

rms_ETEvalues_pertrial = list()
#spectralflatness_ETEvalues_pertrial = list()
#tonnetzdistance_ETEvalues_pertrial = list()

# Get all combinations of booths: booth 1-2, booth 1-3, booth 2-3
# pairs = combn(c(1:3), m=2)
# 
# for(group in 1:12){
#   for(trial in 1:16){
#       trial_rms_ETE = c()
#       trial_spectralflatness_ETE = c()
#       trial_tonnetzdistance_ETE = c()
#       
#       # Loop over all pairs in trio
#       for(pair in 1:3){
#         colname1 = sprintf("g%s_t%s_b%s", group, trial, pairs[1,pair])
#         colname2 = sprintf("g%s_t%s_b%s", group, trial, pairs[2,pair])
#         
#         # calculate effective transfer entropy for all pairs of RMS amplitude time series within the trial
#         
#         rmsETExy = 0
#         rmsETEyx = 0
#         
#         # Run calc_ete() with Markov orders from 1 up to 20, to mitigate the issue of
#         # having a fixed time delay. calc_ete() calculates effective transfer entropy (ETE).
#         for(markov_order in 1:20){
#           ETExy = calc_ete(rms_timeseries[,colname1],
#                           rms_timeseries[,colname2], lx=markov_order, ly=markov_order, shuffles=20, seed=TRUE)
#           ETEyx = calc_ete(rms_timeseries[,colname2],
#                           rms_timeseries[,colname1], lx=markov_order, ly=markov_order, shuffles=20, seed=TRUE)
#           rmsETExy = rmsETExy + ETExy
#           rmsETEyx = rmsETEyx + ETEyx
#         }
#         
#         # We have calculated 20 ETE values both ways for this pair. Divide by 20 to average over
#         # the different Markov order settings.
#         rmsETExy = rmsETExy / 20
#         rmsETEyx = rmsETEyx / 20
#         
#         # Store results in list, for use in RQ2 and RQ4.
#         rms_ETEvalues_perpair[sprintf("%s-%s", colname1, colname2)] = rmsETExy
#         rms_ETEvalues_perpair[sprintf("%s-%s", colname2, colname1)] = rmsETEyx
#         
#         # We will later take the mean of this vector to get one final ETE value for the whole trial
#         trial_rms_ETE = c(trial_rms_ETE, rmsETExy, rmsETEyx)
# 
#         # calculate effective transfer entropy for all pairs of tonnetz distance time series within the trial
#         
#         tonnetzdistanceETExy = 0
#         tonnetzdistanceETEyx = 0
#         
#         for(markov_order in 1:20){
#           ETExy = calc_ete(tonnetzdistance_timeseries[,colname1],
#                                       tonnetzdistance_timeseries[,colname2], lx=markov_order, ly=markov_order, shuffles=20, seed=TRUE)
#           ETEyx = calc_ete(tonnetzdistance_timeseries[,colname2],
#                                         tonnetzdistance_timeseries[,colname1], lx=markov_order, ly=markov_order, shuffles=20, seed=TRUE)
#           tonnetzdistanceETExy = tonnetzdistanceETExy + ETExy
#           tonnetzdistanceETEyx = tonnetzdistanceETEyx + ETEyx
#         }
#         
#         tonnetzdistanceETExy = tonnetzdistanceETExy / 20
#         tonnetzdistanceETEyx = tonnetzdistanceETEyx / 20
#         
#         tonnetzdistance_ETEvalues_perpair[sprintf("%s-%s", colname1, colname2)] = tonnetzdistanceETExy
#         tonnetzdistance_ETEvalues_perpair[sprintf("%s-%s", colname2, colname1)] = tonnetzdistanceETEyx
#         
#         trial_tonnetzdistance_ETE = c(trial_tonnetzdistance_ETE, tonnetzdistanceETExy, tonnetzdistanceETEyx)
#         
#         # calculate effective transfer entropy for all pairs of spectral flatness time series within the trial
#         
#         spectralflatnessETExy = 0
#         spectralflatnessETEyx = 0
#         
#         for(markov_order in 1:20){
#           ETExy = calc_ete(spectralflatness_timeseries[,colname1],
#                          spectralflatness_timeseries[,colname2], lx=markov_order, ly=markov_order, shuffles=20, seed=TRUE)
#           ETEyx = calc_ete(spectralflatness_timeseries[,colname2],
#                          spectralflatness_timeseries[,colname1], lx=markov_order, ly=markov_order, shuffles=20, seed=TRUE)
#           spectralflatnessETExy = spectralflatnessETExy + ETExy
#           spectralflatnessETEyx = spectralflatnessETEyx + ETEyx
#         }
#         
#         spectralflatnessETExy = spectralflatnessETExy / 20
#         spectralflatnessETEyx = spectralflatnessETEyx / 20
#         
#         spectralflatness_ETEvalues_perpair[sprintf("%s-%s", colname1, colname2)] = spectralflatnessETExy
#         spectralflatness_ETEvalues_perpair[sprintf("%s-%s", colname2, colname1)] = spectralflatnessETEyx
#         
#         trial_spectralflatness_ETE = c(trial_spectralflatness_ETE, spectralflatnessETExy, spectralflatnessETEyx)
#       }
#       
#       # Get trial-wide ETE values by averaging over the 6 pairwise ETE values for each trial. Store in list.
#       rms_ETEvalues_pertrial[sprintf("g%s_t%s", group, trial)] = mean(trial_rms_ETE)
#       tonnetzdistance_ETEvalues_pertrial[sprintf("g%s_t%s", group, trial)] = mean(trial_tonnetzdistance_ETE)
#       spectralflatness_ETEvalues_pertrial[sprintf("g%s_t%s", group, trial)] = mean(trial_spectralflatness_ETE)
#   }
# }
# 
# # RANDOM PAIRS
# # Calculate ETE values for 'random pairs' of musicians; i.e., same group, different trial.
# 
# rms_ETEvalues_random = c()
# spectralflatness_ETEvalues_random = c()
# tonnetzdistance_ETEvalues_random = c()
#  
# rms_ETEvalues_random_pertrial = c()
# spectralflatness_ETEvalues_random_pertrial = c()
# tonnetzdistance_ETEvalues_random_pertrial = c()
# 
# for(group in 1:12){
#   for(trial in 1:16){
#     trial_rms_ETE = c()
#     trial_tonnetzdistance_ETE = c()
#     trial_spectralflatness_ETE = c()
#     for(pair in 1:3){
#       # Create random pairs by matching g1_t1_b1 to g1_t2_b2, g1_t1_b1 to g1_t2_b3, g1_t1_b2 to g1_t2_b3, and so on.
#       trialRandom = if (trial != 16) trial+1 else 1
#       colname1 = sprintf("g%s_t%s_b%s", group, trial, pairs[1,pair])
#       colname2 = sprintf("g%s_t%s_b%s", group, trialRandom, pairs[2,pair])
#       
#       # Calculate effective transfer entropy for random pairs of RMS amplitude time series
#       rmsETExy = 0
#       rmsETEyx = 0
#       
#       for(markov_order in 1:20){
#         ETExy = calc_ete(rms_timeseries[,colname1],
#                        rms_timeseries[,colname2], lx=markov_order, ly=markov_order, shuffles=20, seed=TRUE)
#         ETEyx = calc_ete(rms_timeseries[,colname2],
#                        rms_timeseries[,colname1], lx=markov_order, ly=markov_order, shuffles=20, seed=TRUE)
#         rmsETExy = rmsETExy + ETExy
#         rmsETEyx = rmsETEyx + ETEyx
#       }
#       rmsETExy = rmsETExy / 20
#       rmsETEyx = rmsETEyx / 20
#       
#       # add all random-pair ETE values to a vector
#       rms_ETEvalues_random = c(rms_ETEvalues_random, rmsETExy, rmsETEyx)
#       trial_rms_ETE = c(trial_rms_ETE, rmsETExy, rmsETEyx)
#       
#       # calculate effective transfer entropy for random pairs of tonnetz distance time series
#       tonnetzdistanceETExy = 0
#       tonnetzdistanceETEyx = 0
#       
#       for(markov_order in 1:20){
#         ETExy = calc_ete(tonnetzdistance_timeseries[,colname1],
#                        tonnetzdistance_timeseries[,colname2], lx=markov_order, ly=markov_order, shuffles=20, seed=TRUE)
#         ETEyx = calc_ete(tonnetzdistance_timeseries[,colname2],
#                        tonnetzdistance_timeseries[,colname1], lx=markov_order, ly=markov_order, shuffles=20, seed=TRUE)
#         tonnetzdistanceETExy = tonnetzdistanceETExy + ETExy
#         tonnetzdistanceETEyx = tonnetzdistanceETEyx + ETEyx
#       }
#       tonnetzdistanceETExy = tonnetzdistanceETExy / 20
#       tonnetzdistanceETEyx = tonnetzdistanceETEyx / 20
#       
#       tonnetzdistance_ETEvalues_random = c(tonnetzdistance_ETEvalues_random, tonnetzdistanceETExy, tonnetzdistanceETEyx)
#       trial_tonnetzdistance_ETE = c(trial_tonnetzdistance_ETE, tonnetzdistanceETExy, tonnetzdistanceETEyx)
#       
#       # calculate effective transfer entropy for random pairs of spectral flatness time series
# 
#       spectralflatnessETExy = 0
#       spectralflatnessETEyx = 0
#       
#       for(markov_order in 1:20){
#         ETExy = calc_ete(spectralflatness_timeseries[,colname1],
#                        spectralflatness_timeseries[,colname2], lx=markov_order, ly=markov_order, shuffles=20, seed=TRUE)
#         ETEyx = calc_ete(spectralflatness_timeseries[,colname2],
#                        spectralflatness_timeseries[,colname1], lx=markov_order, ly=markov_order, shuffles=20, seed=TRUE)
#         spectralflatnessETExy = spectralflatnessETExy + ETExy
#         spectralflatnessETEyx = spectralflatnessETEyx + ETEyx
#       }
#       spectralflatnessETExy = spectralflatnessETExy / 20
#       spectralflatnessETEyx = spectralflatnessETEyx / 20
#       
#       spectralflatness_ETEvalues_random = c(spectralflatness_ETEvalues_random, spectralflatnessETExy, spectralflatnessETEyx)
#       trial_spectralflatness_ETE = c(trial_spectralflatness_ETE, spectralflatnessETExy, spectralflatnessETEyx)
#     }
#     rms_ETEvalues_random_pertrial = c(rms_ETEvalues_random_pertrial, mean(trial_rms_ETE))
#     tonnetzdistance_ETEvalues_random_pertrial = c(tonnetzdistance_ETEvalues_random_pertrial, mean(trial_tonnetzdistance_ETE))
#     spectralflatness_ETEvalues_random_pertrial = c(spectralflatness_ETEvalues_random_pertrial, mean(trial_spectralflatness_ETE))
#   }
# }
# 
# # Get just the values from each key-value pair in the list, so that we can perform our statistical tests
# rms_ETEvalues_raw = unname(unlist(rms_ETEvalues_pertrial))
# tonnetzdistance_ETEvalues_raw = unname(unlist(tonnetzdistance_ETEvalues_pertrial))
# spectralflatness_ETEvalues_raw = unname(unlist(spectralflatness_ETEvalues_pertrial))

#_____________________________________________________________________________________

# Rho for real trios and random trios ------------

#_____________________________________________________________________________________

# REAL TRIOS

# Create empty lists
rms_rhovalues = list()
#tonnetzdistance_rhovalues = list()
#spectralflatness_rhovalues = list()

# This list will contain, for each trial, the window at which at least one of the musicians has stopped playing.
endPoints = list()

for(group in 1:12){
  for(trial in 1:16){
    # Get the three recordings from the trial
    colnames = c(sprintf("g%s_t%s_b1", group, trial), 
                 sprintf("g%s_t%s_b2", group, trial), 
                 sprintf("g%s_t%s_b3", group, trial))
    
    # Get the ending of the performance (i.e. window at which the first musician stops playing)
    endPoint = min(c(which(is.na(rms_timeseries[colnames[1]]))[1], 
                      which(is.na(rms_timeseries[colnames[2]]))[1], 
                      which(is.na(rms_timeseries[colnames[3]]))[1]))
    endPoints[sprintf("g%s_t%s", group, trial)] = endPoint
    
    # Calculate rho for all real pairs of RMS amplitude time series
    
    rmsDF = cbind(rms_timeseries[colnames[1]], rms_timeseries[colnames[2]], rms_timeseries[colnames[3]])
    trialRMSRho = c()
    
    # For each trial, calculate three values of rho, each with a different booth as the target
    for(colname in colnames){
      rhos_forecast_times = c()
      # Calculate rho values from 1 lag up to 20 lags
      for(forecast_time in 1:20){
        # First, we use the first half of the performance to construct the attractor manifold
        # and use the second half to predict
        simplex_output1 = block_lnlp(rmsDF, lib = c(1, floor(endPoint / 2)), pred = c(floor(endPoint/2)+1, endPoint),
                                     target_column = colname, tp = forecast_time, stats_only = FALSE, first_column_time = FALSE, silent = TRUE)
        # Then, we turn it around; we use the second half to construct the manifold and the first half
        # for prediction purposes
        simplex_output2 = block_lnlp(rmsDF, lib = c(floor(endPoint/2)+1, endPoint), pred = c(1, floor(endPoint / 2)),
                                     target_column = colname, tp = forecast_time, stats_only = FALSE, first_column_time = FALSE, silent = TRUE)
        # Average the two rho values we got out of this and append to rhos_forecast_times vector
        rhos_forecast_times = c(rhos_forecast_times, mean(simplex_output1$stats$rho$rho, simplex_output2$stats$rho$rho))
      }
      # Average rhos_forecast_times (a vector of length 20); append to trialRMSRho,
      # which we will later average to get the group-level predictability
      trialRMSRho = c(trialRMSRho, mean(rhos_forecast_times))
    }
    # 
    # # Calculate rho for all real pairs of tonnetz distance time series
    # tonnetzdistanceDF = cbind(tonnetzdistance_timeseries[colnames[1]], tonnetzdistance_timeseries[colnames[2]], tonnetzdistance_timeseries[colnames[3]])
    # trialTonnetzDistanceRho = c()
    # 
    # for(colname in colnames){
    #    rhos_forecast_times = c()
    #    for(forecast_time in 1:20){
    #    simplex_output1 = block_lnlp(tonnetzdistanceDF, lib = c(1, floor(endPoint / 2)), pred = c(floor(endPoint/2)+1, endPoint),
    #                                 target_column = colname, tp = forecast_time, stats_only = FALSE, first_column_time = FALSE, silent = TRUE)
    #    simplex_output2 = block_lnlp(tonnetzdistanceDF, lib = c(floor(endPoint/2)+1, endPoint), pred = c(1, floor(endPoint / 2)),
    #                                 target_column = colname, tp = forecast_time, stats_only = FALSE, first_column_time = FALSE, silent = TRUE)
    #    rhos_forecast_times = c(rhos_forecast_times, mean(simplex_output1$stats$rho$rho, simplex_output2$stats$rho$rho))
    #    }
    #  trialTonnetzDistanceRho = c(trialTonnetzDistanceRho, mean(rhos_forecast_times))
    # }
    # 
    # # Calculate rho for all real pairs of spectral flatness time series
    # spectralFlatnessDF = cbind(spectralflatness_timeseries[colnames[1]], spectralflatness_timeseries[colnames[2]], spectralflatness_timeseries[colnames[3]])
    # trialSpectralFlatnessRho = c()
    # 
    # for(colname in colnames){
    #   rhos_forecast_times = c()
    #   for(forecast_time in 1:20){
    #     simplex_output1 = block_lnlp(spectralFlatnessDF, lib = c(1, floor(endPoint / 2)), pred = c(floor(endPoint/2)+1, endPoint),
    #                                target_column = colname, tp = forecast_time,  stats_only = FALSE, first_column_time = FALSE, silent = TRUE)
    #     simplex_output2 = block_lnlp(spectralFlatnessDF, lib = c(floor(endPoint/2)+1, endPoint), pred = c(1, floor(endPoint / 2)),
    #                                 target_column = colname, tp = forecast_time,  stats_only = FALSE, first_column_time = FALSE, silent = TRUE)
    #     rhos_forecast_times = c(rhos_forecast_times, mean(simplex_output1$stats$rho$rho, simplex_output2$stats$rho$rho))
    #    }
    #    trialSpectralFlatnessRho = c(trialSpectralFlatnessRho, mean(rhos_forecast_times))
    # }
    ## The vectors trialRMSrho, trialSpectralFlatnessRho and trialTonnetzDistanceRho each contain 3 rho values;
    ## one for each target (i.e. each musician). Average them to get group-level predictability.
    rms_rhovalues[sprintf("g%s_t%s", group, trial)] = mean(trialRMSRho)
    #spectralflatness_rhovalues[sprintf("g%s_t%s", group, trial)] = mean(trialSpectralFlatnessRho)
    # tonnetzdistance_rhovalues[sprintf("g%s_t%s", group, trial)] = mean(trialTonnetzDistanceRho)
  }
}

# Get raw values to plug into Wilcoxon tests
rms_rhovalues_raw = unname(unlist(rms_rhovalues))
#tonnetzdistance_rhovalues_raw = unname(unlist(tonnetzdistance_rhovalues))
#spectralflatness_rhovalues_raw = unname(unlist(spectralflatness_rhovalues))

# RANDOM TRIOS

# Create empty vectors
rms_rhovalues_random = c()
#spectralflatness_rhovalues_random = c()
#tonnetzdistance_rhovalues_random = c()

for(group in 1:12){
  for(trial in 1:16){
    # Same procedure as before for creating random pairs of musicians
    trialRandom1 = if (trial != 16) trial+1 else 1
    trialRandom2 = if (trial != 1) trial-1 else 16
    
    colnames = c(sprintf("g%s_t%s_b1", group, trial),
                 sprintf("g%s_t%s_b2", group, trialRandom1),
                 sprintf("g%s_t%s_b3", group, trialRandom2))
    # This gives us the point, for this random trio, at which at least 1 musician has stopped playing for good.
    endPoint = min(c(which(is.na(rms_timeseries[colnames[1]]))[1], 
                     which(is.na(rms_timeseries[colnames[2]]))[1], 
                     which(is.na(rms_timeseries[colnames[3]]))[1]))
    
    # Calculate rho for random pairs of RMS amplitude time series

    rmsDF = cbind(rms_timeseries[colnames[1]], rms_timeseries[colnames[2]], rms_timeseries[colnames[3]])
    trialRMSRho = c()

    for(colname in colnames){
      rhos_forecast_times = c()
      for(forecast_time in 1:20){
        simplex_output1 = block_lnlp(rmsDF, lib = c(1, floor(endPoint / 2)), pred = c(floor(endPoint/2)+1, endPoint),
                                     target_column = colname, tp = forecast_time, stats_only = FALSE, first_column_time = FALSE, silent = TRUE)
        simplex_output2 = block_lnlp(rmsDF, lib = c(floor(endPoint/2)+1, endPoint), pred = c(1, floor(endPoint / 2)),
                                     target_column = colname, tp = forecast_time, stats_only = FALSE, first_column_time = FALSE, silent = TRUE)
        rhos_forecast_times = c(rhos_forecast_times, mean(simplex_output1$stats$rho$rho, simplex_output2$stats$rho$rho))
      }
      trialRMSRho = c(trialRMSRho, mean(rhos_forecast_times))
    }
    
    # Calculate rho for random pairs of tonnetz distance time series
    
    # tonnetzdistanceDF = cbind(tonnetzdistance_timeseries[colnames[1]], tonnetzdistance_timeseries[colnames[2]], tonnetzdistance_timeseries[colnames[3]])
    # trialTonnetzDistanceRho = c()
    #  
    # for(colname in colnames){
    #    rhos_forecast_times = c()
    #    for(forecast_time in 1:20){
    #    simplex_output1 = block_lnlp(tonnetzdistanceDF, lib = c(1, floor(endPoint / 2)), pred = c(floor(endPoint/2)+1, endPoint),
    #                                 target_column = colname, tp = forecast_time, stats_only = FALSE, first_column_time = FALSE, silent = TRUE)
    #    simplex_output2 = block_lnlp(tonnetzdistanceDF, lib = c(floor(endPoint/2)+1, endPoint), pred = c(1, floor(endPoint / 2)),
    #                                 target_column = colname, tp = forecast_time, stats_only = FALSE, first_column_time = FALSE, silent = TRUE)
    #    rhos_forecast_times = c(rhos_forecast_times, mean(simplex_output1$stats$rho$rho, simplex_output2$stats$rho$rho))
    #    }
    #  trialTonnetzDistanceRho = c(trialTonnetzDistanceRho, mean(rhos_forecast_times))
    # }
    # 
    # # Calculate rho for random pairs of spectral flatness time series
    # 
    # spectralFlatnessDF = cbind(spectralflatness_timeseries[colnames[1]], spectralflatness_timeseries[colnames[2]], spectralflatness_timeseries[colnames[3]])
    # trialSpectralFlatnessRho = c()
    #  
    # for(colname in colnames){
    #    rhos_forecast_times = c()
    #    for(forecast_time in 1:20){
    #      simplex_output1 = block_lnlp(spectralFlatnessDF, lib = c(1, floor(endPoint / 2)), pred = c(floor(endPoint/2)+1, endPoint),
    #                                 target_column = colname, tp = forecast_time, stats_only = FALSE, first_column_time = FALSE, silent = TRUE)
    #      simplex_output2 = block_lnlp(spectralFlatnessDF, lib = c(floor(endPoint/2)+1, endPoint), pred = c(1, floor(endPoint / 2)),
    #                                   target_column = colname, tp = forecast_time, stats_only = FALSE, first_column_time = FALSE, silent = TRUE)
    #      rhos_forecast_times = c(rhos_forecast_times, mean(simplex_output1$stats$rho$rho, simplex_output2$stats$rho$rho))
    #    }
    #   trialSpectralFlatnessRho = c(trialSpectralFlatnessRho, mean(rhos_forecast_times))
    # }
    rms_rhovalues_random = c(rms_rhovalues_random, mean(trialRMSRho))
    #tonnetzdistance_rhovalues_random = c(tonnetzdistance_rhovalues_random, mean(trialTonnetzDistanceRho))
    #spectralflatness_rhovalues_random = c(spectralflatness_rhovalues_random, mean(trialSpectralFlatnessRho))
  }
}

#_____________________________________________________________________________________

# RQ1 ------------

#_____________________________________________________________________________________

# RQ1a - compare real and fake ETE values

# RMS amplitude ETE
# 
# # Test for normality
# qqnorm(rms_ETEvalues_raw)
# # Descriptive statistics
# mean(rms_ETEvalues_raw)
# sd(rms_ETEvalues_raw)
# mean(rms_ETEvalues_random_pertrial)
# sd(rms_ETEvalues_random_pertrial)
# # Test for significant difference between ETE values for RMS amplitude time series of real vs random pairs
# wilcox.test(rms_ETEvalues_raw, rms_ETEvalues_random_pertrial)
# 
# # Tonnetz distance ETE
# 
# qqnorm(tonnetzdistance_ETEvalues_raw)
# mean(tonnetzdistance_ETEvalues_raw)
# sd(tonnetzdistance_ETEvalues_raw)
# mean(tonnetzdistance_ETEvalues_random_pertrial)
# sd(tonnetzdistance_ETEvalues_random)
# wilcox.test(tonnetzdistance_ETEvalues_raw, tonnetzdistance_ETEvalues_random_pertrial)
# 
# # Spectral flatness ETE
# 
# qqnorm(spectralflatness_ETEvalues_raw)
# mean(spectralflatness_ETEvalues_raw)
# sd(spectralflatness_ETEvalues_raw)
# mean(spectralflatness_ETEvalues_random_pertrial)
# sd(spectralflatness_ETEvalues_random_pertrial)
# wilcox.test(spectralflatness_ETEvalues_raw, spectralflatness_ETEvalues_random_pertrial)

# Plot results of ETE on real vs random trios (figure 3 in text)
# RQ1a.df = data.frame(rms_ETEvalues_raw, rms_ETEvalues_random_pertrial, 
#                        tonnetzdistance_ETEvalues_raw, tonnetzdistance_ETEvalues_random_pertrial,
#                        spectralflatness_ETEvalues_raw, spectralflatness_ETEvalues_random_pertrial)
# names(RQ1a.df) = c("RMS real", "RMS random", "Tonn. dist. real", "Tonn. dist. random", "Spec. fltn. real", "Spec. fltn. random")
# RQ1a.df = gather(RQ1a.df)
# ggplot(RQ1a.df, aes(x=value, y=key, fill=key)) + 
#   geom_violin() +
#   coord_flip() +
#   xlab("ETE") +
#   ylab("") +
#   stat_summary(fun = "mean", geom = "point", colour = "black") +
#   theme(legend.position="none", text = element_text(size = 17)) +
#   scale_fill_manual(values=c("tomato2", "tomato2", "mediumturquoise", "mediumturquoise", "darkolivegreen3", "darkolivegreen3"))
# 
# RQ1a.df = data.frame(rms_TEvalues_raw, rms_TEvalues_random_pertrial, 
#                      tonnetzdistance_TEvalues_raw, tonnetzdistance_TEvalues_random_pertrial,
#                      spectralflatness_TEvalues_raw, spectralflatness_TEvalues_random_pertrial,
#                      fractaldimension_ETEvalues_raw, fractaldimension_ETEvalues_random_pertrial,
#                      spectralcentroid_ETEvalues_raw, spectralcentroid_ETEvalues_random_pertrial,
#                      zerocrossing_ETEvalues_raw, zerocrossing_ETEvalues_random_pertrial)
# names(RQ1a.df) = c("RMS real", "RMS rand", "Ton. dist. real", "Ton. dist. rand", "Spec. flt. real", "Spec. flt. rand", 
#                    "Frac. dim. real", "Frac. dim. rand", "Spec. cen. real", "Spec. cen. rand", "ZCR real", "ZCR rand")
# RQ1a.df = gather(RQ1a.df)
# 
# RQ1a.df$key = factor(RQ1a.df$key, levels=c("RMS real", "RMS rand", "Ton. dist. real", "Ton. dist. rand", "Spec. flt. real", "Spec. flt. rand", 
#                                            "Frac. dim. real", "Frac. dim. rand", "Spec. cen. real", "Spec. cen. rand", "ZCR real", "ZCR rand"),
#                      ordered=TRUE)
# ggplot(RQ1a.df, aes(x=value, y=key, fill=key)) + 
#   geom_violin() +
#   coord_flip() +
#   xlab("ETE") +
#   ylab("") +
#   stat_summary(fun = "mean", geom = "point", colour = "black") +
#   theme(legend.position="none", text = element_text(size = 18)) +
#   scale_fill_manual(values=c("tomato3", "tomato1", "steelblue3", "steelblue1", "springgreen4", "springgreen3",
#                                       "yellow3", "yellow1", "mediumpurple3","mediumpurple1", "burlywood3", "burlywood2"))

# RQ1b - same as before, now with rho values

# RMS amplitude rho
qqnorm(rms_rhovalues_raw)
mean(rms_rhovalues_raw)
sd(rms_rhovalues_raw)
mean(rms_rhovalues_random)
sd(rms_rhovalues_random)
wilcox.test(rms_rhovalues_raw, rms_rhovalues_random)
# 
# # Tonnetz distance rho
# qqnorm(tonnetzdistance_rhovalues_raw)
# mean(tonnetzdistance_rhovalues_raw)
# sd(tonnetzdistance_rhovalues_raw)
# mean(tonnetzdistance_rhovalues_random)
# sd(tonnetzdistance_rhovalues_random)
# wilcox.test(tonnetzdistance_rhovalues_raw, tonnetzdistance_rhovalues_random)
# 
# # Spectral flatness rho
# qqnorm(spectralflatness_rhovalues_raw)
# mean(spectralflatness_rhovalues_raw)
# sd(spectralflatness_rhovalues_raw)
# mean(spectralflatness_rhovalues_random)
# sd(spectralflatness_rhovalues_random)
# wilcox.test(spectralflatness_rhovalues_raw, spectralflatness_rhovalues_random)

# Plot results of EDM on real vs random trios (figure 3 in text)
# RQ1b.df = data.frame(rms_rhovalues_raw, rms_rhovalues_random, 
#                      tonnetzdistance_rhovalues_raw, tonnetzdistance_rhovalues_random,
#                      spectralflatness_rhovalues_raw, spectralflatness_rhovalues_random,
#                      fractaldimension_rhovalues_raw, fractaldimension_rhovalues_random,
#                      spectralcentroid_rhovalues_raw, spectralcentroid_rhovalues_random,
#                      zerocrossing_rhovalues_raw, zerocrossing_rhovalues_random)
# names(RQ1b.df) = c("RMS real", "RMS rand", "Ton. dist. real", "Ton. dist. rand", "Spec. flt. real", "Spec. flt. rand", 
#                    "Frac. dim. real", "Frac. dim. rand", "Spec. cen. real", "Spec. cen. rand", "ZCR real", "ZCR rand")
# RQ1b.df = gather(RQ1b.df)
# RQ1b.df$key = factor(RQ1b.df$key, levels=c("RMS real", "RMS rand", "Ton. dist. real", "Ton. dist. rand", "Spec. flt. real", "Spec. flt. rand", 
#                                            "Frac. dim. real", "Frac. dim. rand", "Spec. cen. real", "Spec. cen. rand", "ZCR real", "ZCR rand"),
#                      ordered=TRUE)
# ggplot(RQ1b.df, aes(x=value, y=key, fill=key)) + 
#   geom_violin() +
#   coord_flip() +
#   xlab(expression(rho)) +
#   ylab("") +
#   stat_summary(fun = "mean", geom = "point", colour = "black") +
#   theme(legend.position="none", text = element_text(size = 18)) +
#   scale_fill_manual(values=c("tomato3", "tomato1", "steelblue3", "steelblue1", "springgreen4", "springgreen3",
#                                       "yellow3", "yellow1", "mediumpurple3","mediumpurple1", "burlywood3", "burlywood2"))

#_____________________________________________________________________________________

# RQ2 ------------

#_____________________________________________________________________________________

# Descriptive statistics for Likert scale ratings
mean(musicianDataset$likedImpro, na.rm=TRUE)
sd(musicianDataset$likedImpro, na.rm=TRUE)

# Smoothing factor to avoid divide-by-zero errors when calculating unidirectionality index. This smoothing factor
# is equal to the smallest pairwise ETE value.
smooth = min(unlist(rms_ETEvalues_perpair)[unlist(rms_ETEvalues_perpair) > 0])

for (row in 1:nrow(trialDataset)){
  # Add trial-wide rho and ETE values to dataframe
  group = trialDataset[row, "trio"]
  trial = trialDataset[row, "take"]
  trialDataset$rho_full[row] = rms_rhovalues[[sprintf("g%s_t%s", group, trial)]] + tonnetzdistance_rhovalues[[sprintf("g%s_t%s", group, trial)]] + spectralflatness_rhovalues[[sprintf("g%s_t%s", group, trial)]]
  trialDataset$ETE_full[row] = rms_ETEvalues_pertrial[[sprintf("g%s_t%s", group, trial)]] + tonnetzdistance_ETEvalues_pertrial[[sprintf("g%s_t%s", group, trial)]] + spectralflatness_ETEvalues_pertrial[[sprintf("g%s_t%s", group, trial)]]
  trialDataset$ETE_rms_full[row] = rms_ETEvalues_pertrial[[sprintf("g%s_t%s", group, trial)]]
  trialDataset$rho_rms_full[row] = rms_rhovalues[[sprintf("g%s_t%s", group, trial)]]
  
  unidirectionality_indices = c()
  
  # Get all pairs within trial
  for (pair in 1:3){
    colname1 = sprintf("g%s_t%s_b%s", group, trial, pairs[1,pair])
    colname2 = sprintf("g%s_t%s_b%s", group, trial, pairs[2,pair])
    
    # Retrieve pairwise ETE values from list. Add smoothing factor to each pairwise ETE value.
    pairETExy = rms_ETEvalues_perpair[[sprintf("%s-%s", colname1, colname2)]] + smooth
    pairETEyx = rms_ETEvalues_perpair[[sprintf("%s-%s", colname2, colname1)]] + smooth 
    
    # Calculate the unidirectionality index of a pair. Unidirectionality index is always a value of 
    # at least 1, with 1 signifying perfect bidirectionality.
    unidirectionality_index_pair = max(c(pairETExy / pairETEyx, pairETEyx / pairETExy))
    # Since there were a handful of extreme outliers (including values that exceeded 1000), we cap pairwise 
    # unidirectionality indices at 10.
    if(unidirectionality_index_pair > 10){
      unidirectionality_index_pair = 10
    }
    # Append pair unidirectionality index to vector of unidirectionality indices in the trial.
    unidirectionality_indices = c(unidirectionality_indices, unidirectionality_index_pair)
  }
  
  # Take average of unidirectionality indices within the trial to get the unidirectionality
  # index for the whole trial. 'full' indicates that we take ETE over the whole performance,
  # not just the part after the prompt.
  trialDataset$unidirectionality_index_full[row] = mean(unidirectionality_indices)
}

# To ensure the data can be input into the models
for(col in names(trialDataset)){
  trialDataset$col = unlist(trialDataset$col)
}

# RQ2
# Do amount of information flow, directionality of information flow and group-level 
# predict subjective quality of improvisations?
RQ2.1 = lmer(goodImproAll ~ TE_rms_full + unidirectionality_index_full + rho_rms_full + (1|trio),
            trialDataset[trialDataset$take > 4,])
summary(RQ2.1)
# Check whether model meets assumptions of mixed models
check_model(RQ2.1)

RQ2.2 = lmer(goodImproAll ~ rho_rms_full + ETE_rms_full + (1|trio),
            trialDataset[trialDataset$take > 4,])
summary(RQ2.2)

RQ2.3 = lmer(goodImproAll ~ rho_rms_full + (1|trio),
             trialDataset[trialDataset$take > 4,])
summary(RQ2.3)

# Compare the three models by BIC, marg. R2 and cond. R2
compare_performance(RQ2.1, RQ2.2, RQ2.3)

# Plot the relationships (these plots were not used in the text)
# ETE - subjective quality
RQ2ETE = lmer(goodImproAll ~ ETE_rms_full + (1|trio),
             trialDataset[trialDataset$take > 4,])
RQ2ETE_df = data.frame(trialDataset[trialDataset$take > 4,]$goodImproAll, trialDataset[trialDataset$take > 4,]$ETE_rms_full)
RQ2ETE_df = RQ2ETE_df[complete.cases(RQ2ETE_df),]
names(RQ2ETE_df) = c("Subjective_quality", "ETE")
RQ2ETE_df$predlmer = predict(RQ2ETE)
ggplot(RQ2ETE_df, aes(x=ETE, y=Subjective_quality)) + 
  geom_point(na.rm=TRUE) +
  geom_smooth(aes(y = predlmer), size = 1) +
  scale_y_continuous(breaks=seq(1,7)) + 
  theme(text = element_text(size = 17))

# Unidirectionality - subjective quality
RQ2Uni = lmer(goodImproAll ~ unidirectionality_index_full + (1|trio),
             trialDataset[trialDataset$take > 4,])
RQ2Uni_df = data.frame(trialDataset[trialDataset$take > 4,]$goodImproAll, trialDataset[trialDataset$take > 4,]$unidirectionality_index_full)
RQ2Uni_df = RQ2Uni_df[complete.cases(RQ2Uni_df),]
names(RQ2Uni_df) = c("Subjective_quality", "Unidirectionality_index")
RQ2Uni_df$predlmer = predict(RQ2Uni)
ggplot(RQ2Uni_df, aes(x=Unidirectionality_index, y=Subjective_quality)) + 
  geom_point(na.rm=TRUE) +
  geom_smooth(aes(y = predlmer), size = 1) +
  scale_y_continuous(breaks=seq(1,7)) + 
  theme(text = element_text(size = 17))

# Rho - subjective quality
RQ2Rho_df = data.frame(trialDataset[trialDataset$take > 4,]$goodImproAll, trialDataset[trialDataset$take > 4,]$rho_rms_full)
RQ2Rho_df = RQ2Rho_df[complete.cases(RQ2Rho_df),]
names(RQ2Rho_df) = c("Subjective_quality", "Rho")
RQ2Rho_df$predlmer = predict(RQ2.3)
ggplot(RQ2Rho_df, aes(x=Rho, y=Subjective_quality)) + 
  geom_point(na.rm=TRUE) +
  geom_smooth(aes(y = predlmer), size = 1) +
  scale_y_continuous(breaks=seq(1,7)) + 
  theme(text = element_text(size = 17))

#_____________________________________________________________________________________

# Compute post-prompt ETE, post-prompt rho --------

#_____________________________________________________________________________________

# Add empty columns to dataframe
trialDataset$ETE_after_prompt = rep(NA, nrow(trialDataset))
trialDataset$ETE_before_prompt = rep(NA, nrow(trialDataset))
# Initialize empty list of pairwise post-prompt ETE values
ETE_perpair_after_prompt = list()

for(group in 1:12) {
  for(trial in 1:16) {
    # Get info about the current trial
    idx = which(trialDataset$trio == group & trialDataset$take == trial)
    promptTime = trialDataset$promptTime[idx]
    promptWindow = ceiling(promptTime / windowSizeInSeconds)
    promptType = trialDataset$type[idx]
    promptNumber = trialDataset$number[idx]
    endPoint = endPoints[[sprintf("g%s_t%s", group, trial)]]
    
    # Get ETE of the part of the improvisation that occurred after the prompt.
    # Only include trials where all 3 musicians continued playing for at least 10 seconds after the prompt
    if (!(promptWindow %in% c(0, NA)) & endPoint - promptWindow >= round(10/windowSizeInSeconds)) {
      ETE_after_prompt = c()
      ETE_before_prompt = c()
      for(pair in 1:3){
        colname1 = sprintf("g%s_t%s_b%s", group, trial, pairs[1,pair])
        colname2 = sprintf("g%s_t%s_b%s", group, trial, pairs[2,pair])
        ETExyPair = 0
        ETEyxPair = 0
        for(markov_order in 1:20){
          ETExy = calc_ete(rms_timeseries[,colname1][promptWindow:endPoint],
                         rms_timeseries[,colname2][promptWindow:endPoint], lx=markov_order, ly=markov_order, seed=TRUE)
          ETEyx = calc_ete(rms_timeseries[,colname2][promptWindow:endPoint],
                         rms_timeseries[,colname1][promptWindow:endPoint], lx=markov_order, ly=markov_order, seed=TRUE)
          ETExyPair = ETExyPair + ETExy
          ETEyxPair = ETEyxPair + ETEyx
        }
      
        # Save pairwise post-prompt ETE values
        ETE_perpair_after_prompt[sprintf("%s-%s", colname1, colname2)] = ETExyPair / 20
        ETE_perpair_after_prompt[sprintf("%s-%s", colname2, colname1)] = ETEyxPair / 20
        ETE_after_prompt = c(ETE_after_prompt, ETExyPair, ETEyxPair)
      # Save trial-wide post-prompt ETE value
      trialDataset$ETE_after_prompt[idx] = mean(ETE_after_prompt) / 20 # divide by 20 to average over Markov orders
      }
  }
  }
}

# Add empty column to DF
trialDataset$rho_after_prompt = rep(NA, nrow(trialDataset))

for(group in 1:12){
  # Only need trials that feature prompts, so can exclude the first 4 trials for each trio
  for(trial in 5:16){
    # Get info about the current trial
    idx = which(trialDataset$trio == group & trialDataset$take == trial)
    promptTime = trialDataset$promptTime[idx]
    promptWindow = ceiling(promptTime / windowSizeInSeconds)
    promptType = trialDataset$type[idx]
    promptNumber = trialDataset$number[idx]
    endPoint = endPoints[[sprintf("g%s_t%s", group, trial)]]
    
    colnames = c(sprintf("g%s_t%s_b1", group, trial), 
                 sprintf("g%s_t%s_b2", group, trial), 
                 sprintf("g%s_t%s_b3", group, trial))
    rmsDF = cbind(rms_timeseries[colnames[1]], rms_timeseries[colnames[2]], rms_timeseries[colnames[3]])
    
    # Again, select only those trials with at least 10 s of group performance after the prompt
    if (!(promptWindow %in% c(0, NA)) & endPoint - promptWindow >= round(10/windowSizeInSeconds)) {
      for(colname in colnames){
        rhosAfterPrompt = c()
        for(forecast_time in 1:20){
          # calculate post-prompt Rho using everything before the prompt as the manifold
          simplex_after_prompt = block_lnlp(rmsDF, lib = c(1, promptWindow), pred = c(promptWindow+1, endPoint),
                                       target_column = colname, tp = forecast_time, stats_only = FALSE, first_column_time = FALSE, silent = TRUE)
          rhosAfterPrompt = c(rhosAfterPrompt, simplex_after_prompt$stats$rho$rho)
        }
        trialRhoAfterPrompt = mean(rhosAfterPrompt)
      }
      # add to DF
      trialDataset$rho_after_prompt[idx] = trialRhoAfterPrompt
    }
  }
}

# Get raw values for statistical tests
trialDataset$ETE_after_prompt = unlist(trialDataset$ETE_after_prompt)
trialDataset$rho_after_prompt = unlist(trialDataset$rho_after_prompt)

#_____________________________________________________________________________________

# RQ3 ------------

#_____________________________________________________________________________________

# Make levels of prompt type more easily interpretable
trialDataset$type[trialDataset$type == 0] = "NO"
trialDataset$type[trialDataset$type == 1] = "ME"
trialDataset$type[trialDataset$type == 2] = "WE"

# Turn into factors
trialDataset$type = factor(trialDataset$type)
trialDataset$trio = factor(trialDataset$trio)

# RQ3a
RQ3a.1 = lmer(sqrt(ETE_after_prompt) ~ number + type + number:type + (1|trio),
              trialDataset[!is.na(trialDataset$ETE_after_prompt),])
summary(RQ3a.1)
check_model(RQ3a.1) # Model did not meet normality of residuals assumption; hence square-root transformation

ggplot(trialDataset[!is.na(trialDataset$ETE_after_prompt),],aes(number, sqrt(ETE_after_prompt), col=trio )) + 
  facet_grid(~type) +
  geom_line(aes(y=predict(RQ3a.1)), size=0.8) +
  geom_point(alpha = 0.3) +
  theme_bw() +
  labs(y="Post-prompt ETE (sqrt-transformed)", x="Prompt number", title="Prompt type") +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5, size=17), text = element_text(size = 17)) +
  scale_x_continuous(breaks=c(1,2,3))

RQ3a.2 = lmer(sqrt(ETE_after_prompt) ~ type + number:type + (1|trio),
              trialDataset[!is.na(trialDataset$ETE_after_prompt),])
summary(RQ3a.2)

RQ3a.3 = lmer(sqrt(ETE_after_prompt) ~ type + (1|trio),
              trialDataset[!is.na(trialDataset$ETE_after_prompt),])
summary(RQ3a.3)

compare_performance(RQ3a.1, RQ3a.2, RQ3a.3)

# RQ3b
RQ3b.1 = lmer(rho_after_prompt ~ number + type + number:type + (1|trio),
                 trialDataset[!is.na(trialDataset$rho_after_prompt),])
summary(RQ3b.1)
check_model(RQ3b.1)

ggplot(trialDataset[!is.na(trialDataset$rho_after_prompt),],aes(number, rho_after_prompt, col=trio)) + 
  facet_grid(~type) +
  geom_line(aes(y=predict(RQ3b.1)), size=0.8) +
  geom_point(alpha = 0.3) +
  theme_bw() +
  labs(y=expression(rho), x="Prompt number", title="Prompt type") +
  theme(legend.position="none", plot.title = element_text(hjust = 0.5, size=17), text = element_text(size = 17)) +
  scale_x_continuous(breaks=c(1,2,3))

RQ3b.2 = lmer(rho_after_prompt ~ number:type + type + (1 |trio), 
              trialDataset[!is.na(trialDataset$rho_after_prompt),])
summary(RQ3b.2)

RQ3b.3 = lmer(rho_after_prompt ~ number + (1|trio), 
              trialDataset[!is.na(trialDataset$rho_after_prompt),])
summary(RQ3b.3)

compare_performance(RQ3b.1, RQ3b.2, RQ3b.3)

#_____________________________________________________________________________________

# RQ4 ------------

#_____________________________________________________________________________________

# Create empty vectors
from = c()
to = c()
from_prompt = c()
to_prompt = c()
TE_pair_full = c()
TE_pair_after_prompt = c()
directionality_afterprompt = c()

# Cast to factor variable
musicianDataset$booth = factor(musicianDataset$booth)
levels(musicianDataset$booth) = c(1,2,3)

for(group in 1:12) {
  for(trial in 1:16) {
    for(pair in 1:3){
      
      # What prompt did each of the musicians in the pair hear?
      prompt_heard_1 = musicianDataset[musicianDataset$trio == group & musicianDataset$take == trial & musicianDataset$booth == pairs[1,pair],]$prompt_heard
      prompt_heard_2 = musicianDataset[musicianDataset$trio == group & musicianDataset$take == trial & musicianDataset$booth == pairs[2,pair],]$prompt_heard
      
      if (length(prompt_heard_1) != 0 & length(prompt_heard_2) != 0){
        colname1 = sprintf("g%s_t%s_b%s", group, trial, pairs[1,pair])
        colname2 = sprintf("g%s_t%s_b%s", group, trial, pairs[2,pair])
        from = c(from, colname1, colname2)
        to = c(to, colname2, colname1)
        from_prompt = c(from_prompt, prompt_heard_1, prompt_heard_2)
        to_prompt = c(to_prompt, prompt_heard_2, prompt_heard_1)
        # Retrieve pairwise ETE for the full performance, for RQ4c
        ETE_pair_full = c(ETE_pair_full,
                         rms_ETEvalues_perpair[sprintf("%s-%s", colname1, colname2)], 
                         rms_ETEvalues_perpair[sprintf("%s-%s", colname2, colname1)])
        # Retrieve post-prompt pairwise ETE, for use in RQ4a
        ETE_pair_after_prompt = c(ETE_pair_after_prompt)
        
        RQ4_pair_df[RQ4_pair_df$from == colname1 & RQ4_pair_df$to == colname2,]$ETE_pair_after_prompt = ETE_perpair_after_prompt[sprintf("%s-%s", colname1, colname2)]
        RQ4_pair_df[RQ4_pair_df$from == colname2 & RQ4_pair_df$to == colname1,]$ETE_pair_after_prompt = ETE_perpair_after_prompt[sprintf("%s-%s", colname2, colname1)]
      }
    }
  }
}

# Create dataframe with the new variables
RQ4_pair_df = data.frame(from = from)
RQ4_pair_df$to = to
RQ4_pair_df$ETE_pair_full = ETE_pair_full
RQ4_pair_df$from_prompt = from_prompt
RQ4_pair_df$to_prompt = to_prompt

# Add empty columns
RQ4_pair_df$ETE_pair_after_prompt = rep(NA, nrow(RQ4_pair_df))
RQ4_pair_df$directionality_ratio_after_prompt = rep(NA, nrow(RQ4_pair_df))

for(group in 1:12) {
  # Only trials that featured prompts
  for(trial in 5:16) {
    # Get trial info
    idx = which(trialDataset$trio == group & trialDataset$take == trial)
    promptTime = trialDataset$promptTime[idx]
    promptWindow = ceiling(promptTime / windowSizeInSeconds)
    endPoint = endPoints[[sprintf("g%s_t%s", group, trial)]]
    for(pair in 1:3){
      prompt_heard_1 = musicianDataset[musicianDataset$trio == group & musicianDataset$take == trial & musicianDataset$booth == pairs[1,pair],]$prompt_heard
      prompt_heard_2 = musicianDataset[musicianDataset$trio == group & musicianDataset$take == trial & musicianDataset$booth == pairs[2,pair],]$prompt_heard
      if(length(prompt_heard_1) != 0 & length(prompt_heard_2) != 0){
        colname1 = sprintf("g%s_t%s_b%s", group, trial, pairs[1,pair])
        colname2 = sprintf("g%s_t%s_b%s", group, trial, pairs[2,pair])
        # Add post-prompt pairwise ETE to RQ4_pair_df
        RQ4_pair_df[RQ4_pair_df$from == colname1 & RQ4_pair_df$to == colname2,]$ETE_pair_after_prompt = ETE_perpair_after_prompt[sprintf("%s-%s", colname1, colname2)]
        RQ4_pair_df[RQ4_pair_df$from == colname2 & RQ4_pair_df$to == colname1,]$ETE_pair_after_prompt = ETE_perpair_after_prompt[sprintf("%s-%s", colname2, colname1)]
        # Select trials with at least 10 s of group playing after prompt
        if (!(promptWindow %in% c(0, NA)) & endPoint - promptWindow >= round(10/windowSizeInSeconds)) {
          dir_ratio = (ETE_perpair_after_prompt[[sprintf("%s-%s", colname1, colname2)]] + smooth) / (ETE_perpair_after_prompt[[sprintf("%s-%s", colname2, colname1)]] + smooth)
          # Cap directionality ratio at 10, just like directionality indices computed earlier
          if(dir_ratio > 10){
            RQ4_pair_df[RQ4_pair_df$from == colname1 & RQ4_pair_df$to == colname2,]$directionality_ratio_after_prompt = 10
          } else {
            RQ4_pair_df[RQ4_pair_df$from == colname1 & RQ4_pair_df$to == colname2,]$directionality_ratio_after_prompt = dir_ratio
          }
        }
      }
    }
  }
}

# Add column for individual predictability after the prompt and during the full trial
RQ4_pair_df$individual_predictability_full = rep(NA, nrow(RQ4_pair_df))
RQ4_pair_df$individual_predictability_after_prompt = rep(NA, nrow(RQ4_pair_df))

for(group in 1:12){
  for (trial in 1:16){
    # Retrieve info from trial
    idx = which(trialDataset$trio == group & trialDataset$take == trial)
    promptTime = trialDataset$promptTime[idx]
    promptWindow = ceiling(promptTime / windowSizeInSeconds)
    endPoint = endPoints[[sprintf("g%s_t%s", group, trial)]]
    colnames = c(sprintf("g%s_t%s_b1", group, trial), 
                 sprintf("g%s_t%s_b2", group, trial), 
                 sprintf("g%s_t%s_b3", group, trial))
    rmsDF = cbind(rms_timeseries[colnames[1]], rms_timeseries[colnames[2]], rms_timeseries[colnames[3]])
    
      # Calculate individual predictability for each musician, using only their own playing as the
      # state space history
      for(colname in colnames){
        if(colname %in% RQ4_pair_df$from){
        total_rho = c()
        rmsTS = unlist(rms_timeseries[colname])
        for(forecast_time in 1:20){
          simplex_output1 = block_lnlp(rmsTS, lib = c(1, floor(endPoint / 2)), pred = c(floor(endPoint/2)+1, endPoint), tp = forecast_time, stats_only = FALSE, first_column_time = FALSE, silent = TRUE)
          simplex_output2 = block_lnlp(rmsTS, lib = c(floor(endPoint/2)+1, endPoint), pred = c(1, floor(endPoint / 2)), tp = forecast_time, stats_only = FALSE, first_column_time = FALSE, silent = TRUE)
          total_rho = c(total_rho, mean(simplex_output1$stats$rho$rho, simplex_output2$stats$rho$rho))
        }
        RQ4_pair_df[RQ4_pair_df$from == colname,]$individual_predictability_full = mean(total_rho)
      }
      
      # For musicians in trials that continued for at least 10 seconds after prompt, calculate
      # their individual predictability following the prompt.
      if (!(promptWindow %in% c(0, NA)) & endPoint - promptWindow >= round(10/windowSizeInSeconds)) {
        for(colname in colnames){
          if(colname %in% RQ4_pair_df$from){
            rmsTS = unlist(rms_timeseries[colname])
            total_rho_after_prompt = c()
            for(forecast_time in 1:20){
               simplex_after_prompt = block_lnlp(rmsTS, lib = c(1, promptWindow), pred = c(promptWindow+1, endPoint), tp=forecast_time, stats_only = FALSE, first_column_time = FALSE, silent = TRUE)
               total_rho_after_prompt = c(total_rho_after_prompt, simplex_after_prompt$stats$rho$rho)
           }
           individual_predictability_after_prompt = mean(total_rho_after_prompt, na.rm=TRUE)
           RQ4_pair_df[match(colname, RQ4_pair_df$from),]$individual_predictability_after_prompt = individual_predictability_after_prompt
          }
        }
      }
    }
  }
}

RQ4_pair_df$ETE_pair_full = unlist(RQ4_pair_df$ETE_pair_full)

# Turn into factors
RQ4_pair_df$from_prompt = factor(RQ4_pair_df$from_prompt)
RQ4_pair_df$to_prompt = factor(RQ4_pair_df$to_prompt)

# Construct the dataframe that contains, for each musician in each trial, their individual predictability throughout the performance
# and their average ETE to their partners.
RQ4c_df = RQ4_pair_df[match(unique(RQ4_pair_df$from), RQ4_pair_df$from),]
for(row in 1:nrow(RQ4c_df)){
  RQ4c_df$ETE_from_full[row] = sum(RQ4_pair_df[RQ4_pair_df$from==RQ4c_df$from[row],]$ETE_pair_full)
}

# RQ4a
RQ4a = lm(log(directionality_ratio_after_prompt) ~ relevel(from_prompt, ref="No-Goal") + relevel(to_prompt, ref="No-Goal"), RQ4_pair_df[!is.na(RQ4_pair_df$directionality_ratio_after_prompt),])
check_model(RQ4a) # Used log transform, because residuals highly non-normal 
summary(RQ4a)

# RQ4b
RQ4b = lm(individual_predictability_after_prompt ~ relevel(from_prompt, ref="No-Goal"), RQ4_pair_df[!is.na(RQ4_pair_df$individual_predictability_after_prompt),])
summary(RQ4b)

mean(RQ4_pair_df[RQ4_pair_df$from_prompt == "Me-Goal",]$individual_predictability_after_prompt, na.rm=TRUE)
sd(RQ4_pair_df[RQ4_pair_df$from_prompt == "Me-Goal",]$individual_predictability_after_prompt, na.rm=TRUE)
mean(RQ4_pair_df[RQ4_pair_df$from_prompt == "We-Goal",]$individual_predictability_after_prompt, na.rm=TRUE)
sd(RQ4_pair_df[RQ4_pair_df$from_prompt == "We-Goal",]$individual_predictability_after_prompt, na.rm=TRUE)

wilcox.test(RQ4_pair_df[RQ4_pair_df$from_prompt == "Me-Goal",]$individual_predictability_after_prompt, RQ4_pair_df[RQ4_pair_df$from_prompt == "We-Goal",]$individual_predictability_after_prompt)

ggplot(RQ4_pair_df[!is.na(RQ4_pair_df$individual_predictability_after_prompt),], aes(x=relevel(from_prompt, ref="No-Goal"), y=individual_predictability_after_prompt)) + 
  geom_violin() +
  xlab("Condition") +
  ylab("Post-prompt individual predictability") +
  stat_summary(fun = "mean", geom = "point", colour = "black") +
  theme(legend.position="none", text = element_text(size = 17))

# RQ4c
RQ4c = lm(ETE_from_full ~ individual_predictability_full, RQ4c_df)
summary(RQ4c)
