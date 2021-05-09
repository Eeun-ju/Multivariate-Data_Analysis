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

#�̺��� ���Ժ����� �������� Ȯ���� ���� ���� contour �Լ��� �Ẹ�Ҵ�.contour��  plot�� ���ؼ��� versicolor, viriginica�� �а� ���� ��ġ Ÿ������ ���¸� ��������(�̺��� ���Ժ����� ���� ���̴�.) setosa�� �׷��� �ʴ� ���� �� �� �ִ�.0.2- 0.4 �κп� �� �ִµ�, contour������ ������� Ÿ���� ���¸� ���̹Ƿ� �̺��� ���� ������ ���� ���̶�� �����Ѵ�. ������ �׷����� ��Ȯ�� �Ǵ��� �������.



# b) Assuming that the populations are bivariate normal. Construct the quadratic discriminate scores with p1=p2=p3=1/3 and use it to classify the new observation x=(3.5, 1.75)��.

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
#versicolor�� �Ǻ�

#c) Assuming that the covariance matrices are the same for all three bivariate normal populations. Construct the linear discriminate score with p1=p2=p3=1/3 and use it to assign the new observation x=(3.5, 1.75)��.

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
#versicolor�� �Ǻ�

#d) Compare the result in parts (b) and (c). Explain which approach you prefer.For the parts (b) and (c), build the confusion matrices and compute the misclassification rates.

library(caret)
#confusion matrix lda
lda.test = predict(cov.lda)
table(data$Species,lda.test$class)
misclassification_rate = (4+1)/(150); misclassification_rate # �Լ� ã�� ������� ���� ���

#confusion matrix qda
qda.test = predict(bn.qda)
table(data$Species,qda.test$class)
misclassification_rate = (3+4)/(150); misclassification_rate


# b),c)��� ��� versicolor�� �Ǻ��Ѵ�.��л꼺�� �����ϴ� linear discrimination �� �� ������ ����� ���� �� �ִٰ� �����Ѵ�.Confusion matrix�� ���غ������� qda �� ���ɺ��� lda������ �� ������ �˷��ش�. ���з� �������� lda�� �� ����.