## PCA(Place Rating data)

dat = read.table("C:/Users/user/Desktop/PlaceRating.txt")
head(dat[,1:9])

#skewed ������ ó��
log_dat = log(dat[,1:9])
#S�� variance-covariance matrix
S = cov(log_dat)
#S�� eigen vector, eigen value ���ϱ�
eigen(S)
#�Ҽ��� 3° ¥��
round(eigen(S)$value,3)

#PCA -prcomp

#PCA ������ ���� ����
place_pca = prcomp(x = log_dat)
summary(place_pca)
#place_pca ���
print(place_pca)

#��� PC�� ������ ���ΰ� screeplot �׷����� - 3�� pc ������ ������(�Ȳ�ġ �κ�)
screeplot(place_pca,npcs = 9,type = "lines")

#Y1�� X���� correlation Ȯ���ϱ�

PCscore1 = as.matrix(log_dat)%*%(place_pca$rotation[,1])
cor(log_dat,PCscore1)

#climate�� y1���� ���� Ȯ���ϱ�
k=1
i=1
place_pca$rotation[k,i]*sqrt(eigen(S)$value[i])/sqrt(S[k,k])

#PCscore1,PCscore2 �׷��� �׸���
PCscore2 = as.matrix(log_dat)%*%(place_pca$rotation[,2])

plot(PCscore1,PCscore2)

#biplot
biplot(place_pca)