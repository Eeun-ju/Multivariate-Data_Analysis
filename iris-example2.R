#Logistic regression

#Use the iris data. We consider only two groups: Iris setosa and versicolor in the iris data.

#Consider the logistic regression model and explain the model.

data("iris")
data = iris[,c(2,4,5)]

data = data[(data$Species == "setosa") | (data$Species == "versicolor"), ];
data$Species = factor(data$Species);data$Species

str(data)

data$Species = as.integer((data$Species == "setosa"))
#setosa�� 1�� versicolor�� 0���� ����
data$Species

str(data)

#glm�� �̿��� ������ƽ ȸ�͸��� ����
DF.glm = glm(as.factor(Species) ~., data = data,family = binomial(link = "logit"))
summary(DF.glm)

# warning�� �߻��Ѵ�. ���� summary�� ���� �ǹ̸� �ؼ��غ��� p ���� ���� sepal ���� 1�� ������ ���δ�. logi ���� �и� 0���� ������ ���ų� ���ڰ� 0���� ������ ���� ��찡 �ƴұ� �ǽɵȴ�. 2���� ������ linear regression �غ������� ����� �Ʒ��� ����.
 
  
LF.glm = glm(as.integer(Species) ~., data = data)
summary(LF.glm)

#linear model ��� ������ p���� ���� �� ���ǹ����� �� �� �ִ�.

#Build the confusion matrices and compute the misclassification rates.

library(caret)
predict(DF.glm, data[, c(1,2)])

y_pred = (predict(DF.glm, data[, c(1,2)], type = 'response') > 0.5)*1 

data$pred = y_pred;

f_pred = as.factor(y_pred)
f_actual = as.factor(data$Species)

confusionMatrix(f_pred,f_actual)

misclassification_rate = (0+0)/(100); misclassification_rate

#Use the result in part (a), classify the new observation x=(3.5, 1.75)��.

test = data.frame(cbind(3.5, 1.75))
names(test) = c("Sepal.Width", "Petal.Width")
test

y_pred = (predict(DF.glm, test, type = 'response') > 0.5)
y_pred

#versicolor�� 0���� �Ǵ�