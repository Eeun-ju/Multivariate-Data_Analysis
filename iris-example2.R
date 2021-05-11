#Logistic regression

#Use the iris data. We consider only two groups: Iris setosa and versicolor in the iris data.

#Consider the logistic regression model and explain the model.

data("iris")
data = iris[,c(2,4,5)]

data = data[(data$Species == "setosa") | (data$Species == "versicolor"), ];
data$Species = factor(data$Species);data$Species

str(data)

data$Species = as.integer((data$Species == "setosa"))
#setosa는 1로 versicolor은 0으로 세팅
data$Species

str(data)

#glm을 이용한 로지스틱 회귀모형 구축
DF.glm = glm(as.factor(Species) ~., data = data,family = binomial(link = "logit"))
summary(DF.glm)

# warning이 발생한다. 모델의 summary를 통해 의미를 해석해보면 p 값을 통해 sepal 값은 1로 오류를 보인다. logi 값의 분모가 0으로 가까이 가거나 부자가 0으로 가까이 가는 경우가 아닐까 의심된다. 2개의 변수를 linear regression 해보았을때 결과는 아래와 같다.
 
  
LF.glm = glm(as.integer(Species) ~., data = data)
summary(LF.glm)

#linear model 결과 변수의 p값을 통해 더 유의미함을 볼 수 있다.

#Build the confusion matrices and compute the misclassification rates.

library(caret)
predict(DF.glm, data[, c(1,2)])

y_pred = (predict(DF.glm, data[, c(1,2)], type = 'response') > 0.5)*1 

data$pred = y_pred;

f_pred = as.factor(y_pred)
f_actual = as.factor(data$Species)

confusionMatrix(f_pred,f_actual)

misclassification_rate = (0+0)/(100); misclassification_rate

#Use the result in part (a), classify the new observation x=(3.5, 1.75)’.

test = data.frame(cbind(3.5, 1.75))
names(test) = c("Sepal.Width", "Petal.Width")
test

y_pred = (predict(DF.glm, test, type = 'response') > 0.5)
y_pred

#versicolor은 0으로 판단
