library(combinat)
library(party)
library(plotly)
library(tidyverse)
library(cluster)
library(shiny)
library(plotly)
library(DT)
library(shinydashboard)
# library(tidymodels)

source('R/cluster_calculation_functions.R')
source('R/create_cluster_object.R')
source('R/app_helpers_functions.R')
source('R/create_cluster_app.R')
source('R/data_transformation_functions.R')
source('R/classify_cluster.R')

# importar dataset
df <- readRDS("/mnt/2892419C92416F7E/R/df_playlist.rds")

# transformar variáveis

# gerar objeto cluster
clusters_obj <- create_cluster_object(df = df[1:500,], k_test = 2:8,
                                  fixed_variables = c('energy'),
                                  optional_variables = c('danceability', 'instrumentalness'))

# gerar objeto app
app_cluster <- create_cluster_app(cluster_object = clusters_obj, screen_height = '1000px')


# gerar modelo para categorizar out-of-sample
nnet_cluster <- classify_cluster(cluster_object = clusters_obj)
predict(nnet_cluster, newdata = df[-c(1:500),], type = "class")
