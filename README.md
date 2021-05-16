# Multivariate-Data_Analysis
다변량자료분석 예제 정리 및 코드 정리  <h5> 데이터 원본 : http://www1.aucegypt.edu/faculty/hadi/RABE5/
  
### [placerating-example](placerating-example.R)
14개의 feature를 갖는 329개의 지역 데이터, PCA를 통해 차원 축소하기  
기존 14개 feature 중 9개의 feature(CaseNum, Long, Lat, Pop, stNum 제외)를 이용하여 PCA 진행. 
  
     place_pca = prcomp(x = log(dat[,1:9])) #PCA 수행을 위해 저장 및 plot 그리기
     screeplot(place_pca,npcs = 9,type = "lines")


       
### [Seatbelts-example](Seatbelts-example.R)

### [iris-example](iris-example.R)

### [Diabetes-example](Diabetes-example.R)
Diabetes data 이용하여 clustering 분석 해보기  
Hierarchical clustering, K-means clustering, Mixture model based clustering을 각각 실행하고 비교한다.

### [iris-example2](iris-example2.R)
iris data 이용하여 logistic regression으로 추정 해보기  
단, 두 그룹만 존재(Setosa,Versicolor)
  
     data = data[(data$Species == "setosa") | (data$Species == "versicolor"), ]



### [sweat-example](sweat-example.R)
행렬화를 이용하여 S(variance-covariance), eigenvalue, eigenvector 



