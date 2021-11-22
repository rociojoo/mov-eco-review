# libraries
library(tidyverse)
library(tm)
library(tidytext)
library(topicmodels)
library(gridExtra)
library(grid)
library(parallel)

# paths
path_data <- "./Data/ProcessedQueries/References/" #path to the file of clean data ready for Topic Modeling
path_topics <- "./Data/Topics/" # path to where we'll store data and model outputs
path.plots <- "./Rocio/Plots/" # path to where we'll store plots

# READ DATA ---------------------------------------------------------------
datosFinales <- read.csv(paste0(path_data,"cleaned_filtered_TidyData_TopicModeling.csv"), stringsAsFactors = FALSE)

# glimpse(datosFinales)


# DOCUMENT TERM MATRIX ----------------------------------------------------
datosFreq <- datosFinales %>%
  # Obtain word frequencies
  count(paper, word) 

# Document Term Matrix
datos_dtm <- datosFreq %>%
  # Specify the token
  cast_dtm(document = paper, term = word, value = n)

free_cores <- 4
N_rep <- 20
N_topics <- 15
alpha_par <- 1 # 0.25
method_par <- "VEM"

  # Calculate the number of cores
  no_cores <- detectCores() - free_cores # leave 4 free
  # Initiate cluster
  cl <- makeCluster(no_cores, type="FORK")
  # clusterExport(cl, "dta")
  test <- parLapply(cl, 1:N_rep, # 20 replicates
                    function(x){
                      res1 <- LDA(x = datos_dtm, k = N_topics, method = method_par, 
                                  control = list(alpha = alpha_par)) #, delta= 0.1))
                      return(res1)
                    }
  )  
  stopCluster(cl)
  
  if (method_par == "Gibbs"){
    BestModel <- test[[order(rapply(test, function(x) x@loglikelihood), decreasing = TRUE)[1]]]
  }else{
    BestModel <- test[[order(rapply(test, function(x) max(x@loglikelihood)), decreasing = TRUE)[1]]]
  }
  
  saveRDS(BestModel, file=paste0(path_topics,'NewBestTopicModel',N_topics,
                                 '_alpha_',alpha_par,"_method_",method_par,'_filtered_II.rds'))
  
