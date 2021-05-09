## Classification

#Use the iris data.
#The data contain observations on X2=sepal width and X4=petal width for samples from three species of iris.

#a) Plot the data in the (X2, X4) variable space. Do the observations for the three groups appear to be bivariate normal?

library(MASS)
data("iris")
data = iris[,c(2,4,5)]
head(data)
#plot the data in the (X2,x4) variable space
plot(x = data[data$Species == 'setosa',]$Sepal.Width, y = data[data$Species == 'setosa',]$Petal.Width, main = 'setosa')
plot(x = data[data$Species == 'versicolor',]$Sepal.Width, y = data[data$Species == 'versicolor',]$Petal.Width, main = 'versicolor')
plot(x = data[data$Species == 'virginica',]$Sepal.Width, y = data[data$Species == 'virginica',]$Petal.Width, main = 'virginica')


plot(formula = Sepal.Width ~ Petal.Width, data = data, col = c("red", "green", "blue")[Species])

z = kde2d(x = data[data$Species == 'setosa',]$Sepal.Width,y = data[data$Species == 'setosa',]$Petal.Width)
contour(z,main="setosa")

z = kde2d(x = data[data$Species == 'versicolor',]$Sepal.Width,y = data[data$Species == 'versicolor',]$Petal.Width)
contour(z,main='versicolor')

z = kde2d(x = data[data$Species == 'virginica',]$Sepal.Width,y = data[data$Species == 'virginica',]$Petal.Width)
contour(z,main='virginica')

#이변량 정규분포를 따르는지 확인해 보기 위해 contour 함수를 써보았다.contour와  plot을 통해서는 versicolor, viriginica는 넓게 퍼진 마치 타원같은 형태를 보이지만(이변량 정규분포를 따를 것이다.) setosa는 그렇지 않는 것을 볼 수 있다.0.2- 0.4 부분에 모여 있는데, contour에서는 어느정도 타원의 형태를 보이므로 이변량 정규 분포를 따를 것이라고 생각한다. 실제로 그래프로 정확한 판단은 어려웠다.



# b) Assuming that the populations are bivariate normal. Construct the quadratic discriminate scores with p1=p2=p3=1/3 and use it to classify the new observation x=(3.5, 1.75)’.

library(MASS)
library(klaR)
test = data.frame(cbind(3.5, 1.75))
names(test) = c("Sepal.Width", "Petal.Width")
test
bn.qda = qda(Species ~ .,data=data,prior=c(1/3,1/3,1/3))
bn.qda
qda_score = function(X, data, group, prior){
  X_bar = apply(data[data$Species == group, c(1,2)], MARGIN = 2, FUN = mean)
  X_bar = data.matrix(X_bar)
  S = cov(data[data$Species == group, c(1,2)])
  return (-0.5*( log(det(S)) + t(X - X_bar) %*% solve(S) %*% (X - X_bar)) + log(prior))
}
X = data.matrix(c(3.5, 1.75))
score <- cbind(qda_score(X, data, 1, 1/3), qda_score(X, data, 2, 1/3), qda_score(X, data, 3, 1/3))
score

predict(bn.qda,test)
#versicolor로 판별

#c) Assuming that the covariance matrices are the same for all three bivariate normal populations. Construct the linear discriminate score with p1=p2=p3=1/3 and use it to assign the new observation x=(3.5, 1.75)’.

Sp = (49 *cov(data[data$Species == 1, c(1,2)]) + 49 *cov(data[data$Species == 2, c(1,2)]) + 49 *cov(data[data$Species == 3, c(1,2)])) / 147;

cov.lda = lda(Species ~ .,data=data,prior=c(1/3,1/3,1/3))
cov.lda
lda_score = function(X, data, group, prior){
  X_bar = apply(data[data$Species == group, c(1,2)], MARGIN = 2, FUN = mean)
  X_bar = data.matrix(X_bar)
  return(t(X_bar) %*% solve(Sp) %*% X - 0.5 * t(X_bar) %*% solve(Sp) %*% X_bar + log(prior))
}

score <- cbind(lda_score(X, data, 1, 1/3), lda_score(X, data, 2, 1/3), lda_score(X, data, 3, 1/3))
score

predict(cov.lda,test)
#versicolor로 판별

#d) Compare the result in parts (b) and (c). Explain which approach you prefer.For the parts (b) and (c), build the confusion matrices and compute the misclassification rates.

library(caret)
#confusion matrix lda
lda.test = predict(cov.lda)
table(data$Species,lda.test$class)
misclassification_rate = (4+1)/(150); misclassification_rate # 함수 찾기 어려워서 직접 계산

#confusion matrix qda
qda.test = predict(bn.qda)
table(data$Species,qda.test$class)
misclassification_rate = (3+4)/(150); misclassification_rate


# b),c)결과 모두 versicolor로 판별한다.등분산성을 가정하는 linear discrimination 은 더 엄밀한 결과를 낳을 수 있다고 생각한다.Confusion matrix를 비교해보았을때 qda 의 성능보다 lda성능이 더 좋음을 알려준다. 오분류 비율또한 lda가 더 낮다.