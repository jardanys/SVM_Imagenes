rm(list = ls())
library(R.utils); library(dplyr)
source("fun.R")


## Enlaces a los datasets
train_img_link <- "http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz"
train_labels_link <- "http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz"

test_img_link <- "http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz"
test_labels_link <- "http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz"


## Descarga información sólo si es necesario
if(!file.exists("./MNIST")){
      dir.create("MNIST")
      
      ## Datasets imagenes
      download.file(url = train_img_link, destfile = "./MNIST/Train_img.gz")
      download.file(url = test_img_link, destfile = "./MNIST/Test_img.gz")
      
      ## labels datasets 
      download.file(url = train_labels_link, destfile = "./MNIST/Train_labels.gz")
      download.file(url = test_labels_link, destfile = "./MNIST/Test_labels.gz")
} else{
      if(!file.exists("./MNIST/Train_img.gz")) download.file(url = train_img_link, destfile = "./MNIST/Train_img.gz")
      if(!file.exists("./MNIST/Test_img.gz")) download.file(url = train_img_link, destfile = "./MNIST/Test_img.gz")
      if(!file.exists("./MNIST/Train_labels.gz")) download.file(url = train_img_link, destfile = "./MNIST/Train_labels.gz")
      if(!file.exists("./MNIST/Test_labels.gz")) download.file(url = train_img_link, destfile = "./MNIST/Test_labels.gz")
}


## Descomprimir gz files
gunzip("./MNIST/Train_img.gz") 
gunzip("./MNIST/Test_img.gz") 
gunzip("./MNIST/Train_labels.gz") 
gunzip("./MNIST/Test_labels.gz")


## cargue imagenes
train <- load_image_file("./MNIST/Train_img")
test  <- load_image_file("./MNIST/Test_img")

# Cargue labels
train$label = as.factor(load_label_file("./MNIST/Train_labels"))
test$label  = as.factor(load_label_file("./MNIST/Test_labels"))

## Ordenamos los datasets
train <- train %>% arrange(label)
test <- test %>% arrange(label)

#*********************************************
## Cómo saber cuáles filas son cada número?
#*********************************************
## Train
id_train <- train %>%
            arrange(label) %>%
            mutate(id = 1:length(train$label)) %>%
            group_by(label) %>%
            summarise(fila_desde = min(as.numeric(id)), fila_hasta = max(as.numeric(id)))
id_train

## Test
id_test <- test %>%
           arrange(label) %>%
           mutate(id = 1:length(test$label)) %>%
           group_by(label) %>%
           summarise(fila_desde = min(as.numeric(id)), fila_hasta = max(as.numeric(id)))
id_test


#******************************************************************************
## Usando las tablas anteriores, podemos graficar un número cualquiera:  
#******************************************************************************

# train: el número 5
show_digit(train[30598, ])

# test: el número 5
show_digit(test[5140, ])


#*********************
## Guarda Rds
#*********************
if(!file.exists("./RDS")) {
      dir.create("RDS")
}

# train
for(i in 1:length(unique(train$label))){
      if(!file.exists(paste0("./RDS/Train_", i-1, ".rds"))) {
            saveRDS(filter(train, label==i-1), file = paste0("./RDS/Train_", i-1, ".rds"))
      }
}

# test
for(i in 1:length(unique(test$label))){
      if(!file.exists(paste0("./RDS/Test_", i-1, ".rds"))) {
            saveRDS(filter(test, label==i-1), file = paste0("./RDS/Test_", i-1, ".rds"))
      }
}

