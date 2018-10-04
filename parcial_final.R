library(e1071) 
options(scipen = 999)
set.seed(1024514151)

train_0 <- readRDS("./RDS/Train_0.rds")
train_1 <- readRDS("./RDS/Train_1.rds")
train_2 <- readRDS("./RDS/Train_2.rds")
train_3 <- readRDS("./RDS/Train_3.rds")
train_4 <- readRDS("./RDS/Train_4.rds")
train_5 <- readRDS("./RDS/Train_5.rds")
train_6 <- readRDS("./RDS/Train_6.rds")
train_7 <- readRDS("./RDS/Train_7.rds")
train_8 <- readRDS("./RDS/Train_8.rds")
train_9 <- readRDS("./RDS/Train_9.rds")

l0 <- length(train_0$X1)
l1 <- length(train_1$X1)
l2 <- length(train_2$X1)
l3 <- length(train_3$X1)
l4 <- length(train_4$X1)
l5 <- length(train_5$X1)
l6 <- length(train_6$X1)
l7 <- length(train_7$X1)
l8 <- length(train_8$X1)
l9 <- length(train_9$X1)

test_0 <- readRDS("./RDS/Test_0.rds")
test_1 <- readRDS("./RDS/Test_1.rds")
test_2 <- readRDS("./RDS/Test_2.rds")
test_3 <- readRDS("./RDS/Test_3.rds")
test_4 <- readRDS("./RDS/Test_4.rds")
test_5 <- readRDS("./RDS/Test_5.rds")
test_6 <- readRDS("./RDS/Test_6.rds")
test_7 <- readRDS("./RDS/Test_7.rds")
test_8 <- readRDS("./RDS/Test_8.rds")
test_9 <- readRDS("./RDS/Test_9.rds")

lt0 <- length(test_0$X1)
lt1 <- length(test_1$X1)
lt2 <- length(test_2$X1)
lt3 <- length(test_3$X1)
lt4 <- length(test_4$X1)
lt5 <- length(test_5$X1)
lt6 <- length(test_6$X1)
lt7 <- length(test_7$X1)
lt8 <- length(test_8$X1)
lt9 <- length(test_9$X1)

# train <- rbind(train_0, train_1, train_2)[,c(1:784)]
# test <- rbind(test_0, test_1, test_2)[,c(1:784)]

train <- rbind(train_0, train_1, train_2, train_3, train_4, train_5, train_6, train_7, train_8, train_9)[,c(1:784)]
test <- rbind(test_0, test_1, test_2, test_3, test_4, test_5, test_6, test_7, test_8, test_9)[,c(1:784)]

# Centrado de datos
centro <- colMeans(train)
train <- train - matrix(centro, nrow(train), ncol(train), byrow=TRUE)
test <- test - matrix(centro, nrow(test), ncol(test), byrow=TRUE)

pc <- prcomp(train)
pc_test <- predict(pc, test)
plot(cumsum(pc$sdev^2)/sum(pc$sdev^2), xlab = "Componentes", ylab = "Varianza Acumulada", main = "% Acum Varianza")
abline(h=0.5, col="red")
abline(h=0.8, col="blue")
abline(h=0.9, col="green")
abline(h=0.95, col="yellow")
abline(h=0.99, col="pink")

X <- pc$x
X_test <- pc_test
y <- c(rep(0, l0), rep(1, l1), rep(2, l2), rep(3, l3), rep(4, l4), rep(5, l5), rep(6, l6), rep(7, l7), 
       rep(8, l8), rep(9, l9))

table(y)

y0 <- c(rep(1, l0), rep(0, l1+l2+l3+l4+l5+l6+l7+l8+l9))
y1 <- c(rep(0, l0), rep(1, l1), rep(0, l2+l3+l4+l5+l6+l7+l8+l9))
y2 <- c(rep(0, l0+l1), rep(1, l2), rep(0, l3+l4+l5+l6+l7+l8+l9))
y3 <- c(rep(0, l0+l1+l2), rep(1, l3), rep(0, l4+l5+l6+l7+l8+l9))
y4 <- c(rep(0, l0+l1+l2+l3), rep(1, l4), rep(0, l5+l6+l7+l8+l9))
y5 <- c(rep(0, l0+l1+l2+l3+l4), rep(1, l5), rep(0, l6+l7+l8+l9))
y6 <- c(rep(0, l0+l1+l2+l3+l4+l5), rep(1, l6), rep(0, l7+l8+l9))
y7 <- c(rep(0, l0+l1+l2+l3+l4+l5+l6), rep(1, l7), rep(0, l8+l9))
y8 <- c(rep(0, l0+l1+l2+l3+l4+l5+l6+l7), rep(1, l8), rep(0, l9))
y9 <- c(rep(0, l0+l1+l2+l3+l4+l5+l6+l7+l8), rep(1, l9))

y_test <- c(rep(0, lt0), rep(1, lt1), rep(2, lt2), rep(3, lt3), rep(4, lt4), rep(5, lt5), rep(6, lt6), rep(7, lt7), 
            rep(8, lt8), rep(9, lt9))

y_test_0 <- c(rep(1, lt0), rep(0, lt1+lt2+lt3+lt4+lt5+lt6+lt7+lt8+lt9))
y_test_1 <- c(rep(0, lt0), rep(1, lt1), rep(0, lt2+lt3+lt4+lt5+lt6+lt7+lt8+lt9))
y_test_2 <- c(rep(0, lt0+lt1), rep(1, lt2), rep(0, lt3+lt4+lt5+lt6+lt7+lt8+lt9))
y_test_3 <- c(rep(0, lt0+lt1+lt2), rep(1, lt3), rep(0, lt4+lt5+lt6+lt7+lt8+lt9))
y_test_4 <- c(rep(0, lt0+lt1+lt2+lt3), rep(1, lt4), rep(0, lt5+lt6+lt7+lt8+lt9))
y_test_5 <- c(rep(0, lt0+lt1+lt2+lt3+lt4), rep(1, lt5), rep(0, lt6+lt7+lt8+lt9))
y_test_6 <- c(rep(0, lt0+lt1+lt2+lt3+lt4+lt5), rep(1, lt6), rep(0, lt7+lt8+lt9))
y_test_7 <- c(rep(0, lt0+lt1+lt2+lt3+lt4+lt5+lt6), rep(1, lt7), rep(0, lt8+lt9))
y_test_8 <- c(rep(0, lt0+lt1+lt2+lt3+lt4+lt5+lt6+lt7), rep(1, lt8), rep(0, lt9))
y_test_9 <- c(rep(0, lt0+lt1+lt2+lt3+lt4+lt5+lt6+lt7+lt8), rep(1, lt9))

#model_svm_prob <- svm(X[,1:40], as.factor(y), kernel="radial", cost=0.1, probability = TRUE)
#pred_prob <- predict(model_svm, X_test[,1:15], decision.values = TRUE, probability = TRUE)
#pred_prob

l <- l0+l1+l2+l3+l4+l5+l6+l7+l8+l9
p0 <- c((l0+1):l)
p1 <- c(c(1:l0),c((l0+l1+1):l))
p2 <- c(c(1:(l0+l1)),c((l0+l1+l2+1):l))
p3 <- c(c(1:(l0+l1+l2)),c((l0+l1+l2+l3+1):l))
p4 <- c(c(1:(l0+l1+l2+l3)),c((l0+l1+l2+l3+l4+1):l))
p5 <- c(c(1:(l0+l1+l2+l3+l4)),c((l0+l1+l2+l3+l4+l5+1):l))
p6 <- c(c(1:(l0+l1+l2+l3+l4+l5)),c((l0+l1+l2+l3+l4+l5+l6+1):l))
p7 <- c(c(1:(l0+l1+l2+l3+l4+l5+l6)),c((l0+l1+l2+l3+l4+l5+l6+l7+1):l))
p8 <- c(c(1:(l0+l1+l2+l3+l4+l5+l6+l7)),c((l0+l1+l2+l3+l4+l5+l6+l7+l8+1):l))
p9 <- c(c(1:(l0+l1+l2+l3+l4+l5+l6+l7+l8)))
p_0 <- c(1:l0)
p_1 <- c((l0+1):(l0+l1))
p_2 <- c((l0+l1+1):(l0+l1+l2))
p_3 <- c((l0+l1+l2+1):(l0+l1+l2+l3))
p_4 <- c((l0+l1+l2+l3+1):(l0+l1+l2+l3+l4))
p_5 <- c((l0+l1+l2+l3+l4+1):(l0+l1+l2+l3+l4+l5))
p_6 <- c((l0+l1+l2+l3+l4+l5+1):(l0+l1+l2+l3+l4+l5+l6))
p_7 <- c((l0+l1+l2+l3+l4+l5+l6+1):(l0+l1+l2+l3+l4+l5+l6+l7))
p_8 <- c((l0+l1+l2+l3+l4+l5+l6+l7+1):(l0+l1+l2+l3+l4+l5+l6+l7+l8))
p_9 <- c((l0+l1+l2+l3+l4+l5+l6+l7+l8+1):(l0+l1+l2+l3+l4+l5+l6+l7+l8+l9))
length(p0) + l0
length(p1) + l1
length(p2) + l2
length(p3) + l3
length(p4) + l4
length(p5) + l5
length(p6) + l6
length(p7) + l7
length(p8) + l8
length(p9) + l9

p0_s <- sample(p0, l0)
p1_s <- sample(p1, l0)
p2_s <- sample(p2, l0)
p3_s <- sample(p3, l0)
p4_s <- sample(p4, l0)
p5_s <- sample(p5, l0)
p6_s <- sample(p6, l0)
p7_s <- sample(p7, l0)
p8_s <- sample(p8, l0)
p9_s <- sample(p9, l0)

t0 <- Sys.time()
model_svm_0 <- svm(X[c(p_0,p0_s),1:40], as.factor(y0[c(p_0,p0_s)]), kernel="radial", cost=0.1, probability = TRUE)
t1 <- Sys.time(); t1-t0
predictivas_train_0 <- predict(model_svm_0, X[c(p_0,p0_s),1:40])
predictivas_test_0 <- predict(model_svm_0, X_test[,1:40])
residuales_train_0 <- ifelse(y0[c(p_0,p0_s)] == predictivas_train_0,0,1)
residuales_test_0 <- ifelse(y_test_0 == predictivas_test_0,0,1)
err_train_0 <- mean(residuales_train_0); err_train_0
err_test_0 <- mean(residuales_test_0); err_test_0

t0 <- Sys.time()
model_svm_1 <- svm(X[c(p_1,p1_s),1:40], as.factor(y1[c(p_1,p1_s)]), kernel="radial", cost=0.1, probability = TRUE)
t1 <- Sys.time(); t1-t0
predictivas_train_1 <- predict(model_svm_1, X[c(p_1,p1_s),1:40])
predictivas_test_1 <- predict(model_svm_1, X_test[,1:40])
residuales_train_1 <- ifelse(y1[c(p_1,p1_s)] == predictivas_train_1,0,1)
residuales_test_1 <- ifelse(y_test_1 == predictivas_test_1,0,1)
err_train_1 <- mean(residuales_train_1); err_train_1
err_test_1 <- mean(residuales_test_1); err_test_1

t0 <- Sys.time()
model_svm_2 <- svm(X[c(p_2,p2_s),1:40], as.factor(y2[c(p_2,p2_s)]), kernel="radial", cost=0.1, probability = TRUE)
t1 <- Sys.time(); t1-t0
predictivas_train_2 <- predict(model_svm_2, X[c(p_2,p2_s),1:40])
predictivas_test_2 <- predict(model_svm_2, X_test[,1:40])
residuales_train_2 <- ifelse(y2[c(p_2,p2_s)] == predictivas_train_2,0,1)
residuales_test_2 <- ifelse(y_test_2 == predictivas_test_2,0,1)
err_train_2 <- mean(residuales_train_2); err_train_2
err_test_2 <- mean(residuales_test_2); err_test_2

t0 <- Sys.time()
model_svm_3 <- svm(X[c(p_3,p3_s),1:40], as.factor(y3[c(p_3,p3_s)]), kernel="radial", cost=0.1, probability = TRUE)
t1 <- Sys.time(); t1-t0
predictivas_train_3 <- predict(model_svm_3, X[c(p_3,p3_s),1:40])
predictivas_test_3 <- predict(model_svm_3, X_test[,1:40])
residuales_train_3 <- ifelse(y3[c(p_3,p3_s)] == predictivas_train_3,0,1)
residuales_test_3 <- ifelse(y_test_3 == predictivas_test_3,0,1)
err_train_3 <- mean(residuales_train_3); err_train_3
err_test_3 <- mean(residuales_test_3); err_test_3

t0 <- Sys.time()
model_svm_4 <- svm(X[c(p_4,p4_s),1:40], as.factor(y4[c(p_4,p4_s)]), kernel="radial", cost=0.1, probability = TRUE)
t1 <- Sys.time(); t1-t0
predictivas_train_4 <- predict(model_svm_4, X[c(p_4,p4_s),1:40])
predictivas_test_4 <- predict(model_svm_4, X_test[,1:40])
residuales_train_4 <- ifelse(y4[c(p_4,p4_s)] == predictivas_train_4,0,1)
residuales_test_4 <- ifelse(y_test_4 == predictivas_test_4,0,1)
err_train_4 <- mean(residuales_train_4); err_train_4
err_test_4 <- mean(residuales_test_4); err_test_4

t0 <- Sys.time()
model_svm_5 <- svm(X[c(p_5,p5_s),1:40], as.factor(y5[c(p_5,p5_s)]), kernel="radial", cost=0.1, probability = TRUE)
t1 <- Sys.time(); t1-t0
predictivas_train_5 <- predict(model_svm_5, X[c(p_5,p5_s),1:40])
predictivas_test_5 <- predict(model_svm_5, X_test[,1:40])
residuales_train_5 <- ifelse(y5[c(p_5,p5_s)] == predictivas_train_5,0,1)
residuales_test_5 <- ifelse(y_test_5 == predictivas_test_5,0,1)
err_train_5 <- mean(residuales_train_5); err_train_5
err_test_5 <- mean(residuales_test_5); err_test_5

t0 <- Sys.time()
model_svm_6 <- svm(X[c(p_6,p6_s),1:40], as.factor(y6[c(p_6,p6_s)]), kernel="radial", cost=0.1, probability = TRUE)
t1 <- Sys.time(); t1-t0
predictivas_train_6 <- predict(model_svm_6, X[c(p_6,p6_s),1:40])
predictivas_test_6 <- predict(model_svm_6, X_test[,1:40])
residuales_train_6 <- ifelse(y6[c(p_6,p6_s)] == predictivas_train_6,0,1)
residuales_test_6 <- ifelse(y_test_6 == predictivas_test_6,0,1)
err_train_6 <- mean(residuales_train_6); err_train_6
err_test_6 <- mean(residuales_test_6); err_test_6

t0 <- Sys.time()
model_svm_7 <- svm(X[c(p_7,p7_s),1:40], as.factor(y7[c(p_7,p7_s)]), kernel="radial", cost=0.1, probability = TRUE)
t1 <- Sys.time(); t1-t0
predictivas_train_7 <- predict(model_svm_7, X[c(p_7,p7_s),1:40])
predictivas_test_7 <- predict(model_svm_7, X_test[,1:40])
residuales_train_7 <- ifelse(y7[c(p_7,p7_s)] == predictivas_train_7,0,1)
residuales_test_7 <- ifelse(y_test_7 == predictivas_test_7,0,1)
err_train_7 <- mean(residuales_train_7); err_train_7
err_test_7 <- mean(residuales_test_7); err_test_7

t0 <- Sys.time()
model_svm_8 <- svm(X[c(p_8,p8_s),1:40], as.factor(y8[c(p_8,p8_s)]), kernel="radial", cost=0.1, probability = TRUE)
t1 <- Sys.time(); t1-t0
predictivas_train_8 <- predict(model_svm_8, X[c(p_8,p8_s),1:40])
predictivas_test_8 <- predict(model_svm_8, X_test[,1:40])
residuales_train_8 <- ifelse(y8[c(p_8,p8_s)] == predictivas_train_8,0,1)
residuales_test_8 <- ifelse(y_test_8 == predictivas_test_8,0,1)
err_train_8 <- mean(residuales_train_8); err_train_8
err_test_8 <- mean(residuales_test_8); err_test_8

t0 <- Sys.time()
model_svm_9 <- svm(X[c(p_9,p9_s),1:40], as.factor(y9[c(p_9,p9_s)]), kernel="radial", cost=0.1, probability = TRUE)
t1 <- Sys.time(); t1-t0
predictivas_train_9 <- predict(model_svm_9, X[c(p_9,p9_s),1:40])
predictivas_test_9 <- predict(model_svm_9, X_test[,1:40])
residuales_train_9 <- ifelse(y9[c(p_9,p9_s)] == predictivas_train_9,0,1)
residuales_test_9 <- ifelse(y_test_9 == predictivas_test_9,0,1)
err_train_9 <- mean(residuales_train_9); err_train_9
err_test_9 <- mean(residuales_test_9); err_test_9

pred_prob_train_0 <- predict(model_svm_0, X[c(p_0,p0_s),1:40], decision.values = TRUE, probability = TRUE)
pred_prob_train_1 <- predict(model_svm_1, X[c(p_1,p1_s),1:40], decision.values = TRUE, probability = TRUE)
pred_prob_train_2 <- predict(model_svm_2, X[c(p_2,p2_s),1:40], decision.values = TRUE, probability = TRUE)
pred_prob_train_3 <- predict(model_svm_3, X[c(p_3,p3_s),1:40], decision.values = TRUE, probability = TRUE)
pred_prob_train_4 <- predict(model_svm_4, X[c(p_4,p4_s),1:40], decision.values = TRUE, probability = TRUE)
pred_prob_train_5 <- predict(model_svm_5, X[c(p_5,p5_s),1:40], decision.values = TRUE, probability = TRUE)
pred_prob_train_6 <- predict(model_svm_6, X[c(p_6,p6_s),1:40], decision.values = TRUE, probability = TRUE)
pred_prob_train_7 <- predict(model_svm_7, X[c(p_7,p7_s),1:40], decision.values = TRUE, probability = TRUE)
pred_prob_train_8 <- predict(model_svm_8, X[c(p_8,p8_s),1:40], decision.values = TRUE, probability = TRUE)
pred_prob_train_9 <- predict(model_svm_9, X[c(p_9,p9_s),1:40], decision.values = TRUE, probability = TRUE)

pred_prob_test_0 <- predict(model_svm_0, X_test[,1:40], decision.values = TRUE, probability = TRUE)
pred_prob_test_1 <- predict(model_svm_1, X_test[,1:40], decision.values = TRUE, probability = TRUE)
pred_prob_test_2 <- predict(model_svm_2, X_test[,1:40], decision.values = TRUE, probability = TRUE)
pred_prob_test_3 <- predict(model_svm_3, X_test[,1:40], decision.values = TRUE, probability = TRUE)
pred_prob_test_4 <- predict(model_svm_4, X_test[,1:40], decision.values = TRUE, probability = TRUE)
pred_prob_test_5 <- predict(model_svm_5, X_test[,1:40], decision.values = TRUE, probability = TRUE)
pred_prob_test_6 <- predict(model_svm_6, X_test[,1:40], decision.values = TRUE, probability = TRUE)
pred_prob_test_7 <- predict(model_svm_7, X_test[,1:40], decision.values = TRUE, probability = TRUE)
pred_prob_test_8 <- predict(model_svm_8, X_test[,1:40], decision.values = TRUE, probability = TRUE)
pred_prob_test_9 <- predict(model_svm_9, X_test[,1:40], decision.values = TRUE, probability = TRUE)

DB_train <- data.frame(predict_0=predictivas_train_0, predict_1=predictivas_train_1, predict_2=predictivas_train_2,
                       predict_3=predictivas_train_3, predict_4=predictivas_train_4, predict_5=predictivas_train_5,
                       predict_6=predictivas_train_6, predict_7=predictivas_train_7, predict_8=predictivas_train_8,
                       predict_9=predictivas_train_9, real=y)


t0 <- Sys.time()
model_svm_0 <- svm(X[,1:40], as.factor(y0), kernel="radial", cost=0.1, probability = TRUE)
t1 <- Sys.time(); t1-t0
predictivas_train_0 <- predict(model_svm_0, X[,1:40])
predictivas_test_0 <- predict(model_svm_0, X_test[,1:40])
residuales_train_0 <- ifelse(y0 == predictivas_train_0,0,1)
residuales_test_0 <- ifelse(y_test_0 == predictivas_test_0,0,1)
err_train_0 <- mean(residuales_train_0)
err_test_0 <- mean(residuales_test_0) 

t0 <- Sys.time()
model_svm_1 <- svm(X[,1:40], as.factor(y1), kernel="radial", cost=0.1, probability = TRUE)
t1 <- Sys.time(); t1-t0
predictivas_train_1 <- predict(model_svm_1, X[,1:40])
predictivas_test_1 <- predict(model_svm_1, X_test[,1:40])
residuales_train_1 <- ifelse(y1 == predictivas_train_1,0,1)
residuales_test_1 <- ifelse(y_test_1 == predictivas_test_1,0,1)
err_train_1 <- mean(residuales_train_1)
err_test_1 <- mean(residuales_test_1) 

t0 <- Sys.time()
model_svm_2 <- svm(X[,1:40], as.factor(y2), kernel="radial", cost=0.1, probability = TRUE)
t1 <- Sys.time(); t1-t0
predictivas_train_2 <- predict(model_svm_2, X[,1:40])
predictivas_test_2 <- predict(model_svm_2, X_test[,1:40])
residuales_train_2 <- ifelse(y2 == predictivas_train_2,0,1)
residuales_test_2 <- ifelse(y_test_2 == predictivas_test_2,0,1)
err_train_2 <- mean(residuales_train_2)
err_test_2 <- mean(residuales_test_2) 

t0 <- Sys.time()
model_svm_3 <- svm(X[,1:40], as.factor(y3), kernel="radial", cost=0.1, probability = TRUE)
t1 <- Sys.time(); t1-t0
predictivas_train_3 <- predict(model_svm_3, X[,1:40])
predictivas_test_3 <- predict(model_svm_3, X_test[,1:40])
residuales_train_3 <- ifelse(y3 == predictivas_train_3,0,1)
residuales_test_3 <- ifelse(y_test_3 == predictivas_test_3,0,1)
err_train_3 <- mean(residuales_train_3)
err_test_3 <- mean(residuales_test_3) 

t0 <- Sys.time()
model_svm_4 <- svm(X[,1:40], as.factor(y4), kernel="radial", cost=0.1, probability = TRUE)
t1 <- Sys.time(); t1-t0
predictivas_train_4 <- predict(model_svm_4, X[,1:40])
predictivas_test_4 <- predict(model_svm_4, X_test[,1:40])
residuales_train_4 <- ifelse(y4 == predictivas_train_4,0,1)
residuales_test_4 <- ifelse(y_test_4 == predictivas_test_4,0,1)
err_train_4 <- mean(residuales_train_4)
err_test_4 <- mean(residuales_test_4) 

t0 <- Sys.time()
model_svm_5 <- svm(X[,1:40], as.factor(y5), kernel="radial", cost=0.1, probability = TRUE)
t1 <- Sys.time(); t1-t0
predictivas_train_5 <- predict(model_svm_5, X[,1:40])
predictivas_test_5 <- predict(model_svm_5, X_test[,1:40])
residuales_train_5 <- ifelse(y5 == predictivas_train_5,0,1)
residuales_test_5 <- ifelse(y_test_5 == predictivas_test_5,0,1)
err_train_5 <- mean(residuales_train_5)
err_test_5 <- mean(residuales_test_5) 

t0 <- Sys.time()
model_svm_6 <- svm(X[,1:40], as.factor(y6), kernel="radial", cost=0.1, probability = TRUE)
t1 <- Sys.time(); t1-t0
predictivas_train_6 <- predict(model_svm_6, X[,1:40])
predictivas_test_6 <- predict(model_svm_6, X_test[,1:40])
residuales_train_6 <- ifelse(y6 == predictivas_train_6,0,1)
residuales_test_6 <- ifelse(y_test_6 == predictivas_test_6,0,1)
err_train_6 <- mean(residuales_train_6)
err_test_6 <- mean(residuales_test_6) 

t0 <- Sys.time()
model_svm_7 <- svm(X[,1:40], as.factor(y7), kernel="radial", cost=0.1, probability = TRUE)
t1 <- Sys.time(); t1-t0
predictivas_train_7 <- predict(model_svm_7, X[,1:40])
predictivas_test_7 <- predict(model_svm_7, X_test[,1:40])
residuales_train_7 <- ifelse(y7 == predictivas_train_7,0,1)
residuales_test_7 <- ifelse(y_test_7 == predictivas_test_7,0,1)
err_train_7 <- mean(residuales_train_7)
err_test_7 <- mean(residuales_test_7) 

t0 <- Sys.time()
model_svm_8 <- svm(X[,1:40], as.factor(y8), kernel="radial", cost=0.1, probability = TRUE)
t1 <- Sys.time(); t1-t0
predictivas_train_8 <- predict(model_svm_8, X[,1:40])
predictivas_test_8 <- predict(model_svm_8, X_test[,1:40])
residuales_train_8 <- ifelse(y8 == predictivas_train_8,0,1)
residuales_test_8 <- ifelse(y_test_8 == predictivas_test_8,0,1)
err_train_8 <- mean(residuales_train_8)
err_test_8 <- mean(residuales_test_8) 

t0 <- Sys.time()
model_svm_9 <- svm(X[,1:40], as.factor(y9), kernel="radial", cost=0.1, probability = TRUE)
t1 <- Sys.time(); t1-t0
predictivas_train_9 <- predict(model_svm_9, X[,1:40])
predictivas_test_9 <- predict(model_svm_9, X_test[,1:40])
residuales_train_9 <- ifelse(y9 == predictivas_train_9,0,1)
residuales_test_9<- ifelse(y_test_9 == predictivas_test_9,0,1)
err_train_9 <- mean(residuales_train_9)
err_test_9 <- mean(residuales_test_9) 

pred_prob_train_0 <- predict(model_svm_0, X[,1:40], decision.values = TRUE, probability = TRUE)
pred_prob_train_1 <- predict(model_svm_1, X[,1:40], decision.values = TRUE, probability = TRUE)
pred_prob_train_2 <- predict(model_svm_2, X[,1:40], decision.values = TRUE, probability = TRUE)
pred_prob_train_3 <- predict(model_svm_3, X[,1:40], decision.values = TRUE, probability = TRUE)
pred_prob_train_4 <- predict(model_svm_4, X[,1:40], decision.values = TRUE, probability = TRUE)
pred_prob_train_5 <- predict(model_svm_5, X[,1:40], decision.values = TRUE, probability = TRUE)
pred_prob_train_6 <- predict(model_svm_6, X[,1:40], decision.values = TRUE, probability = TRUE)
pred_prob_train_7 <- predict(model_svm_7, X[,1:40], decision.values = TRUE, probability = TRUE)
pred_prob_train_8 <- predict(model_svm_8, X[,1:40], decision.values = TRUE, probability = TRUE)
pred_prob_train_9 <- predict(model_svm_9, X[,1:40], decision.values = TRUE, probability = TRUE)

pred_prob_test_0 <- predict(model_svm_0, X_test[,1:40], decision.values = TRUE, probability = TRUE)
pred_prob_test_1 <- predict(model_svm_1, X_test[,1:40], decision.values = TRUE, probability = TRUE)
pred_prob_test_2 <- predict(model_svm_2, X_test[,1:40], decision.values = TRUE, probability = TRUE)
pred_prob_test_3 <- predict(model_svm_3, X_test[,1:40], decision.values = TRUE, probability = TRUE)
pred_prob_test_4 <- predict(model_svm_4, X_test[,1:40], decision.values = TRUE, probability = TRUE)
pred_prob_test_5 <- predict(model_svm_5, X_test[,1:40], decision.values = TRUE, probability = TRUE)
pred_prob_test_6 <- predict(model_svm_6, X_test[,1:40], decision.values = TRUE, probability = TRUE)
pred_prob_test_7 <- predict(model_svm_7, X_test[,1:40], decision.values = TRUE, probability = TRUE)
pred_prob_test_8 <- predict(model_svm_8, X_test[,1:40], decision.values = TRUE, probability = TRUE)
pred_prob_test_9 <- predict(model_svm_9, X_test[,1:40], decision.values = TRUE, probability = TRUE)


DB_train <- data.frame(predict_0=predictivas_train_0[1:10000], predict_1=predictivas_train_1[1:10000], predict_2=predictivas_train_2[1:10000],
                       predict_3=predictivas_train_3[1:10000], predict_4=predictivas_train_4[1:10000], predict_5=predictivas_train_5[1:10000],
                       predict_6=predictivas_train_6[1:10000], predict_7=predictivas_train_7[1:10000], predict_8=predictivas_train_8[1:10000],
                       predict_9=predictivas_train_9[1:10000], real=y[1:10000])

head(DB_train)
