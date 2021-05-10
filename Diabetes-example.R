#clusterting �м�

library(heplots)
library("klaR")
library(mclust)
library(clusterSim)

diabetes.x<-Diabetes[,1:5]
head(diabetes.x)

#1) Hierarchical clustering (K=4, dendrogram �׸���, average method)

diabetes.mean = apply(diabetes.x,2,mean)
diabetes.std = sqrt(apply(diabetes.x,2,var))
diabetes.sx = sweep(diabetes.x,2,diabetes.mean,FUN="-")
diabetes.sx = sweep(diabetes.sx,2,diabetes.std,FUN="/")

hc = hclust(dist(diabetes.sx),method="average")

plot(hc, main = "Average Linkage Cluster Analysis")

cutree(hc, k = 4)

#2) K means clustering (k=4, �� Hierarchical clustering����� initial cluster �� �̿�)

km = kmeans(x=diabetes.sx,centers = 4,nstart=1000)

a = lda(diabetes.sx,km$cluster)

scores = as.matrix(diabetes.sx)%*%a$scaling[,1:2]

partimat(as.factor(km$cluster)~scores,data=diabetes.sx,method="lda")

# initial center���
ic <- initial.Centers(diabetes.sx, 4)

km = kmeans(diabetes.sx,diabetes.sx[initial.Centers(diabetes.sx, 4),],nstart=1000) 
a = lda(diabetes.sx,km$cluster)

scores = as.matrix(diabetes.sx)%*%a$scaling[,1:2]

partimat(as.factor(km$cluster)~scores,data=diabetes.sx,method="lda")

#3) Mixture model based clustering (K=2,3,4 ���� BIC ���Ͽ� clustering ��� �����ϱ�)

mixm2<-Mclust(diabetes.sx,G = 2)
mixm3<-Mclust(diabetes.sx,G = 3)
mixm4<-Mclust(diabetes.sx,G = 4)
summary(mixm2);summary(mixm3);summary(mixm4)

#BIC ������ ���� ���� k=3�� �ùٸ� ������ ������. k = 3�� ��� ����� �����Ѵ�.