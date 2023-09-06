library(here)
library(tidymodels)
library(tidyverse)

# Import Data ----------------------------------------

data_raw = readr::read_csv(file = here::here('data/raw/Sleep_health_and_lifestyle_dataset.csv'))

#data_raw |> head(5)
#summary(data_raw)
#colnames(data_raw)

# Rename Columns --------------------------------
colnames(data_raw)=lapply(colnames(data_raw),
                      function(x) stringr::str_replace_all(string=x,pattern=" ", repl="_")) |>
                      tolower()

#data_raw |> head(5)

data_renamed = data_raw

# Transform character and integer in factors---------------------------
summary(data_renamed)
data_renamed |> head(5) |> View()
transform_in_factor=c("gender","occupation","bmi_category","sleep_disorder",'stress_level','quality_of_sleep')

for(x in transform_in_factor){
  data_renamed[x] = factor(data_renamed[[x]])
}

rm(x)
# summary(data_renamed)

rm(transform_in_factor)
data_renamed_factored = data_renamed

# What to do with blood presure? -------------------------------
data_rename_factored_new_feature= data_renamed_factored |>  separate_wider_delim(blood_pressure, "/", names = c("systolic_pressure", "diastolic_pressure"))

#data_rename_factored_new_feature |> summary()

for(x in c("systolic_pressure", "diastolic_pressure")) {
  data_rename_factored_new_feature[x] = as.numeric(data_rename_factored_new_feature[[x]])
}

rm(x)

#data_rename_factored_new_feature |> summary()
#data_rename_factored_new_feature |> View()


# EDA ---------------------------------------
data_eda = data_rename_factored_new_feature
data_eda = data_eda |>  dplyr::select(-c('person_id'))

columns_numeric = data_eda |> dplyr::select_if(is.numeric) |> colnames()
#columns_numeric

for( x in columns_numeric){
  g = data_eda |> ggplot() +
    geom_histogram(aes(.data[[x]]), bins = 30) +
    xlab(x)
  
  print(g)
    
}

rm(columns_numeric,x,g)

#data_eda |> summary()

# Sleep Disorder Distribution
columns_factor = data_eda |> dplyr::select_if(is.factor) |>  dplyr:: select(-c(sleep_disorder)) |> colnames()

for( x in columns_factor){
  g = data_eda |>
    ggplot() +
    geom_bar(aes(x=sleep_disorder,
                 fill=.data[[x]]),
             position ='dodge2')
  
  print(g)
  }

rm(columns_factor,x,g)

saveRDS(data_eda,file=here('code/RDATA/data_eda.R'))

rm(list=ls())
