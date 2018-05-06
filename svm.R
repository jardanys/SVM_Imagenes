library(e1071) 

train_3 <- readRDS("./RDS/Train_3.rds")
train_8 <- readRDS("./RDS/Train_8.rds")

test_3 <- readRDS("./RDS/Test_3.rds")
test_8 <- readRDS("./RDS/Test_8.rds")

train_3_8 <- rbind(train_3, train_8)
test_3_8 <- rbind(test_3, test_8)

pc <- prcomp(train_3_8[,c(1:784)])
pc_test <- predict(pc, test_3_8[,c(1:784)])
plot(cumsum(pc$sdev^2)/sum(pc$sdev^2), xlab = "Componentes", ylab = "Varianza Acumulada", main = "% Acum Varianza")
abline(h=0.5, col="red")
abline(h=0.8, col="blue")
abline(h=0.9, col="green")
abline(h=0.95, col="yellow")
abline(h=0.99, col="pink")
X <- pc$x
X_test <- pc_test
y <- as.numeric(as.character(train_3_8[,785]))
y_test <- as.numeric(as.character(test_3_8[,785]))

resultados <- data.frame(Componentes=0, Kernel=0, C=0, err_train=0, err_test=0)
Componentes <- c(2, 10, 40, 60, 80, 100)
kernel <- c("linear", "polynomial", "radial", "sigmoid")
C <- c(0.1, 1, 10, 100, 1000)
for(k in Componentes){
      for(i in kernel){
            for(j in C){
                  model_svm <- svm(X[,1:k], as.factor(y), kernel=i, cost=C)
                  predictivas_train <- predict(model_svm, X[,1:k])
                  predictivas_test <- predict(model_svm, X_test[,1:k])
                  residuales_train <- ifelse(y == predictivas_train,0,1)
                  residuales_test <- ifelse(y_test == predictivas_test,0,1)
                  err_train <- mean(residuales_train)
                  err_test <- mean(residuales_test) 
                  resultados <- rbind(resultados, c(k, i, j, err_train, err_test))
            }
      }
}

resultados <- resultados[-1,]
resultado <- resultados
resultados$iteraciones <- c(1:120) 
resultados$err_train <- as.numeric(resultados$err_train)
resultados$err_test <- as.numeric(resultados$err_test)
resultados

saveRDS(resultados, "resultados.rds")

ggplot(resultados, aes(x=iteraciones, y=err_train, colour="resid train")) + geom_line() +
      geom_line(aes(x=iteraciones, y=err_test, colour="resid test")) + xlab("Iteraciones SVM") +
      ylab("Resid Train & Test") + labs(title = "Total Iteraciones Supper Vector Machina", 
                                          subtitle = "Resid Train vs Test")

which.min(resultados$err_test) + 1

resultado[which.min(resultado$err_test),]



