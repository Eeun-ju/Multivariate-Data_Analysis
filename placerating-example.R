## PCA(Place Rating data)

dat = read.table("C:/Users/user/Desktop/PlaceRating.txt")
head(dat[,1:9])

#skewed 데이터 처리
log_dat = log(dat[,1:9])
#S는 variance-covariance matrix
S = cov(log_dat)
#S의 eigen vector, eigen value 구하기
eigen(S)
#소수점 3째 짜리
round(eigen(S)$value,3)

#PCA -prcomp

#PCA 수행을 위해 저장
place_pca = prcomp(x = log_dat)
summary(place_pca)
#place_pca 출력
print(place_pca)

#몇개의 PC를 선택할 것인가 screeplot 그려보기 - 3개 pc 선택이 적당함(팔꿈치 부분)
screeplot(place_pca,npcs = 9,type = "lines")

#Y1과 X와의 correlation 확인하기

PCscore1 = as.matrix(log_dat)%*%(place_pca$rotation[,1])
cor(log_dat,PCscore1)

#climate와 y1과의 관계 확인하기
k=1
i=1
place_pca$rotation[k,i]*sqrt(eigen(S)$value[i])/sqrt(S[k,k])

#PCscore1,PCscore2 그래프 그리기
PCscore2 = as.matrix(log_dat)%*%(place_pca$rotation[,2])

plot(PCscore1,PCscore2)

#biplot
biplot(place_pca)