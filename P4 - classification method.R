set.seed(1000)
data(iris)
# Three classes
x <- iris[1:150, c("Sepal.Length", "Sepal.Width", "Species")]
head(x)
plot(x[,1:2], col = x[,3])

decisionplot <- function(model, data, class = NULL, predict_type = "class",
                         resolution = 100, showgrid = TRUE, ...) {
  if(!is.null(class)) cl <- data[,class] else cl <- 1
  data <- data[,1:2]
  k <- length(unique(cl))
  plot(data, col = as.integer(cl)+1L, pch = as.integer(cl)+1L, ...)
  # make grid
  r <- sapply(data, range, na.rm = TRUE)
  xs <- seq(r[1,1], r[2,1], length.out = resolution)
  ys <- seq(r[1,2], r[2,2], length.out = resolution)
  g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
  colnames(g) <- colnames(r)
  g <- as.data.frame(g)
  ### guess how to get class labels from predict
  ### (unfortunately not very consistent between models)
  p <- predict(model, g, type = predict_type)
  if(is.list(p)) p <- p$class
  p <- as.factor(p)
  if(showgrid) points(g, col = as.integer(p)+1L, pch = ".")
  z <- matrix(as.integer(p), nrow = resolution, byrow = TRUE)
  contour(xs, ys, z, add = TRUE, drawlabels = FALSE,
          lwd = 2, levels = (1:(k-1))+.5)
  invisible(z)
}

library(caret)
library(lattice)
library(ggplot2)
## Loading required package: lattice
## Loading required package: ggplot2
win.graph()
plot(x[,1:2], col = x[,3])
model <- knn3(Species ~ ., data=x, k = 1)
decisionplot(model, x, class = "Species", main = "KNN (1)")
ypredk1 = predict(model, x, type="class")
acck1 = mean(ypredk1==iris$Species);acck1
tk1 = table(ypredk1, iris$Species);tk1

#KNN k=10
win.graph()
plot(x[,1:2], col = x[,3])
model <- knn3(Species ~ ., data=x, k = 10)
decisionplot(model, x, class = "Species", main = "kNN (10)")
ypredk10 = predict(model, x, type="class")
acck10 = mean(ypredk10==iris$Species);acck10
tk10 = table(ypredk10, iris$Species);tk10

#KNN k=5
win.graph()
plot(x[,1:2], col = x[,3])
model <- knn3(Species ~ ., data=x, k = 5)
decisionplot(model, x, class = "Species", main = "kNN (5)")
ypredk5 = predict(model, x, type="class")
acck5 = mean(ypredk5==iris$Species);acck5
tk5 = table(ypredk5, iris$Species);tk5

#Naive Bayes
library(e1071)
win.graph()
plot(x[,1:2], col = x[,3])
model <- naiveBayes(Species ~ ., data=x)
decisionplot(model, x, class = "Species", main = "naive Bayes")
yprednb = predict(model, x, type="class")
accnb = mean(yprednb==iris$Species);accnb
tnb = table(yprednb, iris$Species);tnb

#Linear Drisciminant Analysis
library(MASS)
win.graph()
plot(x[,1:2], col = x[,3])
model <- lda(Species ~ ., data=x)
decisionplot(model, x, class = "Species", main = "LDA")
ypredlda = predict(model, x, type="class")
acclda = mean(ypredlda$class==iris$Species);acclda
tlda = table(ypredlda$class, iris$Species);tlda

#Logistic regression
model <- glm(Species ~., data = x, family=binomial(link='logit'))
## Warning: glm.fit: algorithm did not converge
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
win.graph()
plot(x[,1:2], col = x[,3])
class(model) <- c("lr", class(model))
predict.lr <- function(object, newdata, ...)
  predict.glm(object, newdata, type = "response") > .5
decisionplot(model, x, class = "Species", main = "Logistic Regression")

library(nnet)
model = multinom(Species ~ ., data=x)
decisionplot(model, x, class = "Species", main = "Logistic Regression")
ypredrl = predict(model, x, type="class")
accrl = mean(ypredrl==iris$Species);accrl
trl = table(ypredrl, iris$Species);trl

#Decision tree
library("rpart")
win.graph()
plot(x[,1:2], col = x[,3])
model <- rpart(Species ~ ., data=x)
decisionplot(model, x, class = "Species", main = "CART")
ypreddt = predict(model, x, type="class")
accdt = mean(ypreddt==iris$Species);accdt
tdt = table(ypreddt, iris$Species);tdt

accuracy = c(acck1, acck5, acck10, accnb, acclda,accrl, acctd)

