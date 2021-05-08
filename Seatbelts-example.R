#Seatbelts �ڷ� ��� �� �ð�ȭ, �ؼ��ϱ�


data <- as.data.frame(Seatbelts)
str(data)
summary(data)

#1969�� 1������ 1984�� 12������ ������
#������ Ȯ��

library(ggplot2)

Seatbelts <- data.frame(Year=floor(time(Seatbelts)),Month=factor(cycle(Seatbelts),labels=month.abb), Seatbelts)

head(Seatbelts)
ggplot(data = Seatbelts, aes(x = Year, y =PetrolPrice))+  geom_point()

#�ű��� feature ���� �ֹ��� ���� PetrolPrice ���� ��ȭ�� �׷����� ��Ÿ�´�.  
#�ð��� �带���� ������ �ö󰥰��̶� ���������� 1976�� ������ ���� ����, 1980����� ���ı��� ������ �ö󰡴°��� Ȯ�� �� �� �ִ�.

#������ ��� �߻� ��Ȳ 
ggplot(data = Seatbelts, aes(x = Year, y =drivers/kms))+  geom_line(color = "#00AFBB", size = 1)

#����Ÿ��� ����ڸ� Ȯ���� ��� �ð��� �带���� ��� �߻� �� ���� �پ���� �� �� �ִ�. �� ������ 1983�⵵���� ������Ʈ �ǹ�ȭ�� ����Ǿ��� �����̶�� �����Ѵ�.  

#�������� 7���� features �����Ͽ� �����Ͽ���. 
#nullmean���� (118.5,1631,828.5,401.5,14987,0.10448,8) confidence region�� ������ Ȯ���Ѵ�.

xbar = apply(Seatbelts[,3:9],2,mean)
xvar = var(Seatbelts[,3:9])

p = nrow(xvar); n = nrow(Seatbelts)
nullmean = c(118.5,1631,828.5,401.5,14987,0.10448,8)
d = xbar-nullmean

t2 = n*t(d)%*%solve(xvar)%*%d;
t2

t2 < p*(n-1)/(n-p)*qf(0.95,p,n-p)
#�����ȿ� ���� ����

#F��跮�� �̿��� hotellingsT2
library(ICSNP)
HotellingsT2(X=Seatbelts[,3:9],mu=nullmean)

#�͹������� �Ⱒ�Ѵ�. ��� �ϳ��� ���������� �Ⱒ�� �Ǿ���. 

#USArrests �ڷ� ���׸�, ü������ �󱼱׸� �׸���, �ؼ��ϱ�
#1973�� �̱� �ֺ� ���� ������ ������ 

#install.packages("tibble")

data1<-USArrests
str(data1)
head(USArrests)

#1. ���׸�
stars(data1,cex=0.5)


#2. ü��������
#install.packages("aplpack")
library(aplpack)
faces(USArrests,face.type = 1, main = "Cheronoff faces: USArrests",cex=0.9)

#�ڷ� �ؼ��ϱ�
#���׸��� ü���������� ���� ����� ������� ������ ���Ҵ�.  
#1)�׷� : mississippi, north carolina, south carolina, georgia, alabama  
#2)�׷� : montana, kentucky  
#3)�׷� : new maxico, michigan, texas  
#4)�׷� : ohio, washington, connecticut  

#�׷캰�� 4�� feature���� ������� Ȯ�� �� �� �ִ�.


#�׷�1 ������  
#1) �׷��� ��κ� �����, �ձٸӸ�,���¾��� ü������ �׷����� �׷����� ���׸��� �翷���� �� ������ �׸��� �׷�����.

rbind(USArrests["Mississippi",], USArrests["North Carolina",],USArrests["South Carolina",], USArrests["Georgia",], USArrests["Alabama",])

#�׷�2 ������  
#2) �׷��� ��κ� ��Ȳ��, ���� ���� ���� ü������ �׷����� �׷����� ���׸��� ���� ���׸��� �׷�����.
rbind(USArrests["Montana",], USArrests["Kentucky",])

# �׷�3 ������  
#3) �׷��� ��κ� ���Ʒ��� ū ���׸��� �׷�����, ������ �󱼿� �����, ū���� ���� ���̴�.
rbind(USArrests["New Mexico",], USArrests["Michigan",], USArrests["Texas",])

#�׷�4 ������  
#4) �׷��� ��κ� ���׸��� �󱼿� ��Ȳ�� �󱼿� �������� ���̸�,���������� ġ��ģ ������� ������ �ִ�.
rbind(USArrests["Ohio",], USArrests["Washington",],USArrests["Connecticut",])