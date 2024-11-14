### rice/N/CH4
### H2O AutoML
### Min Gon Chung
### 11/22/2023

rm(list=ls())

num <- commandArgs(trailingOnly = TRUE)
num <- as.numeric(num)

## library
library(h2o)   
library(raster)
library(data.table)
library(stringr)
library(dplyr)
library(sf)
library(tibble)

setwd("[Working Directory here]")

## read csv
# observation, train & test
ob <- read.csv("Rice_CH4_N_raw.csv", header = TRUE, stringsAsFactors = FALSE)

# sum of control + treatment
ob.cnt <- ob[,c(1:7)]
ob.trt <- ob[,c(1:2,8,10:13)] 

names(ob.cnt) <- c("Data.point", "TN.g.kg", "N.kgN.ha", "mean.kg.ha.day", "sd.kg.ha.day", "yield.Mg.ha", "yield.sd.Mg.ha")
names(ob.trt) <- c("Data.point", "TN.g.kg", "N.kgN.ha", "mean.kg.ha.day", "sd.kg.ha.day", "yield.Mg.ha", "yield.sd.Mg.ha")

ob.sum <- rbind(ob.cnt, ob.trt)

# global, simulation
wrld <- read.csv("Rice_CH4_N_simulation.csv", header = TRUE, stringsAsFactors = FALSE)

###########
## input data
## ob.CH4.all
ob.CH4.all <- ob.sum[,c("Data.point", "TN.g.kg", "N.kgN.ha", "mean.kg.ha.day")]

## ob.YD.all
ob.YD.all <- ob.sum[,c("Data.point", "TN.g.kg", "N.kgN.ha", "yield.Mg.ha")]


### simplify input
input.model <- list(ob.CH4.all, ob.YD.all)
input.h2o <- input.model[[num]]

input.list <- c("CH4_all","YD_all")
input.name <- input.list[[num]]

# Set names for h2o
if(num < 2) {
  names(input.h2o)[3:4] <- c("N.kgN.ha","CH4.kg.ha.day")
  y <- "CH4.kg.ha.day"
} else {
  names(input.h2o)[3:4] <- c("N.kgN.ha","YD.MG.ha")
  y <- "YD.MG.ha"  
} 
x <- c("TN.g.kg","N.kgN.ha") 


#################
## H2O AutoML
## train & test
# Split into training (8), validation and test sets (2)
h2o.init()

# Convert your data frame to an H2O data frame
input.h2o.hex <- as.h2o(input.h2o)

# Split the data frame
splits <- h2o.splitFrame(input.h2o.hex, ratios = 0.8, seed = 160617)

# The first element of the splits object is the training set
train <- splits[[1]]

# The second element of the splits object is the testing set
test <- splits[[2]]

# x = x: The names of our feature columns.
# y = y: The name of our target column.
# training_frame = train_h2o: Our training set consisting of data from 2010 to start of 2018.
# validation_frame = valid_h2o: Our validation set consisting of data in the year 2018. H2O uses this to ensure the model does not overfit the data.
# leaderboard_frame = test_h2o: The models get ranked based on MAE performance against this set.
# max_runtime_secs = 60: We supply this to speed up H2O's modeling. The algorithm has a large number of complex # models so we want to keep things moving at the expense of some accuracy.
# stopping_metric = "deviance": Use deviance as the stopping metric, which provides very good results for MAPE
# linear regression model used, but can use any model
set.seed(160617)

automl.h2o <- h2o.automl(
  x = x, 
  y = y, 
  training_frame = train,
  nfolds = 5,
  max_runtime_secs = 0, # 0 (no limit) in HPC 
  seed = 160617) 

# Extract leader model
automl_leader <- automl.h2o@leader
print(automl_leader)

# Generate predictions on the test data
pred.h2o <- h2o.predict(automl_leader, newdata = test)
pred.h2o.df <- as.data.frame(pred.h2o)

# root mean squared error (RMSE) and mean absolute error (MAE)
print(h2o.performance(automl_leader, newdata = test))

# Investigate test error
if(num < 2) {
  error_rice <- as.data.frame(test) %>% 
    tibble::add_column(pred = pred.h2o.df %>% as.tibble() %>% pull(predict)) %>%
    rename(actual = CH4.kg.ha.day) %>% #CH4.kg.ha.day; YD.MG.ha
    mutate(
      error     = actual - pred,
      error_pct = error / actual * 100
  ) 
} else {
  error_rice <- as.data.frame(test) %>% 
    tibble::add_column(pred = pred.h2o.df %>% as.tibble() %>% pull(predict)) %>%
    rename(actual = YD.MG.ha) %>% #CH4.kg.ha.day; YD.MG.ha
    mutate(
      error     = actual - pred,
      error_pct = error / actual * 100
    ) 
}

print(error_rice)

error_rice_sum <- error_rice %>%
  summarise(
    me   = mean(error),
    rmse = mean(error^2)^0.5,
    mae  = mean(abs(error)),
    mape = mean(abs(error_pct)),
    mpe  = mean(error_pct)
  )

###############
## write.csv
write.csv(error_rice, paste0("./prediction/h2o_error_test_",input.name,"_Nfrt.csv"), row.names=F)
write.csv(error_rice_sum, paste0("./prediction/h2o_error_summary_",input.name,"_Nfrt.csv"), row.names=F)

###########################
###### World predict ######
### N fertilizer original
Nsoil.depth <- c("0_5","5_15","0_15","100_200")

for(kk in 1:4) {
    
  kk.num <- kk + 5
  
  # subset  
  wrld.re <- wrld[,c(1:5, kk.num)]
  
  # N fertilizer 1, 1.5, 2
  wrld.re$nfrt.kgN.ha <- wrld.re$nfrt.kgN.ha*1
  
  # CH4.kg.ha.day; N.kgN.ha; YD.MG.ha
  ## rename columns
  names(wrld.re) <- c("V1","YD.MG.ha","x","y","N.kgN.ha", "TN.g.kg")
  
  wrld.re.h2o <- as.h2o(wrld.re)
  
  ## predict grid level
  siml_h2o <- h2o.predict(automl_leader, newdata = wrld.re.h2o)
  siml_h2o.df <- as.data.frame(siml_h2o)
  
  ###############
  ## write.csv
  ##!!!!!!!!!!!!!
  write.csv(siml_h2o.df, paste0("./prediction/h2o_predict_",input.name,"_",Nsoil.depth[[kk]],"_Nfrt100.csv"), row.names=F)

}


## N fertilizer 150%
for(kk in 1:4) {
  
  kk.num <- kk + 5
  
  # subset  
  wrld.re <- wrld[,c(1:5, kk.num)]
  
  # N fertilizer 0.5, 1, 1.5, 2
  wrld.re$nfrt.kgN.ha <- wrld.re$nfrt.kgN.ha*0.5
  
  # CH4.kg.ha.day; N.kgN.ha; YD.MG.ha
  ## rename columns
  names(wrld.re) <- c("V1","YD.MG.ha","x","y","N.kgN.ha", "TN.g.kg")
  
  wrld.re.h2o <- as.h2o(wrld.re)
  
  ## predict grid level
  siml_h2o <- h2o.predict(automl_leader, newdata = wrld.re.h2o)
  siml_h2o.df <- as.data.frame(siml_h2o)
  
  ###############
  ## write.csv
  ##!!!!!!!!!!!!!
  write.csv(siml_h2o.df, paste0("./prediction/h2o_predict_",input.name,"_",Nsoil.depth[[kk]],"_Nfrt50.csv"), row.names=F)
  
}

## N fertilizer 150%
for(kk in 1:4) {
  
  kk.num <- kk + 5
  
  # subset  
  wrld.re <- wrld[,c(1:5, kk.num)]
  
  # N fertilizer 1, 1.5, 2
  wrld.re$nfrt.kgN.ha <- wrld.re$nfrt.kgN.ha*1.5
  
  # CH4.kg.ha.day; N.kgN.ha; YD.MG.ha
  ## rename columns
  names(wrld.re) <- c("V1","YD.MG.ha","x","y","N.kgN.ha", "TN.g.kg")
  
  wrld.re.h2o <- as.h2o(wrld.re)
  
  ## predict grid level
  siml_h2o <- h2o.predict(automl_leader, newdata = wrld.re.h2o)
  siml_h2o.df <- as.data.frame(siml_h2o)
  
  ###############
  ## write.csv
  ##!!!!!!!!!!!!!
  write.csv(siml_h2o.df, paste0("./prediction/h2o_predict_",input.name,"_",Nsoil.depth[[kk]],"_Nfrt150.csv"), row.names=F)
  
}

## N fertilizer 200%
for(kk in 1:4) {
  
  kk.num <- kk + 5
  
  # subset  
  wrld.re <- wrld[,c(1:5, kk.num)]
  
  # N fertilizer 1, 1.5, 2
  wrld.re$nfrt.kgN.ha <- wrld.re$nfrt.kgN.ha*2
  
  # CH4.kg.ha.day; N.kgN.ha; YD.MG.ha
  ## rename columns
  names(wrld.re) <- c("V1","YD.MG.ha","x","y","N.kgN.ha", "TN.g.kg")
  
  wrld.re.h2o <- as.h2o(wrld.re)
  
  ## predict grid level
  siml_h2o <- h2o.predict(automl_leader, newdata = wrld.re.h2o)
  siml_h2o.df <- as.data.frame(siml_h2o)
  
  ###############
  ## write.csv
  ##!!!!!!!!!!!!!
  write.csv(siml_h2o.df, paste0("./prediction/h2o_predict_",input.name,"_",Nsoil.depth[[kk]],"_Nfrt200.csv"), row.names=F)
  
}