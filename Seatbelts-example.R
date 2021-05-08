#Seatbelts 자료 요약 및 시각화, 해석하기


data <- as.data.frame(Seatbelts)
str(data)
summary(data)

#1969년 1월부터 1984년 12월까지 데이터
#데이터 확인

library(ggplot2)

Seatbelts <- data.frame(Year=floor(time(Seatbelts)),Month=factor(cycle(Seatbelts),labels=month.abb), Seatbelts)

head(Seatbelts)
ggplot(data = Seatbelts, aes(x = Year, y =PetrolPrice))+  geom_point()

#신기한 feature 값인 휘발유 가격 PetrolPrice 값의 변화를 그래프로 나타냈다.  
#시간이 흐를수록 가격이 올라갈것이라 생각했지만 1976년 이전이 가장 높고, 1980년부터 이후까지 가격이 올라가는것을 확인 할 수 있다.

#연도별 사고 발생 상황 
ggplot(data = Seatbelts, aes(x = Year, y =drivers/kms))+  geom_line(color = "#00AFBB", size = 1)

#주행거리당 사상자를 확인한 결과 시간이 흐를수록 사고 발생 그 값이 줄어듬을 알 수 있다. 그 이유는 1983년도부터 안전벨트 의무화가 시행되었기 때문이라고 생각한다.  

#가설검정 7개의 features 고려하여 검정하였다. 
#nullmean값이 (118.5,1631,828.5,401.5,14987,0.10448,8) confidence region에 들어가는지 확인한다.

xbar = apply(Seatbelts[,3:9],2,mean)
xvar = var(Seatbelts[,3:9])

p = nrow(xvar); n = nrow(Seatbelts)
nullmean = c(118.5,1631,828.5,401.5,14987,0.10448,8)
d = xbar-nullmean

t2 = n*t(d)%*%solve(xvar)%*%d;
t2

t2 < p*(n-1)/(n-p)*qf(0.95,p,n-p)
#영역안에 들어가지 못함

#F통계량을 이용한 hotellingsT2
library(ICSNP)
HotellingsT2(X=Seatbelts[,3:9],mu=nullmean)

#귀무가설을 기각한다. 적어도 하나의 변수에서는 기각이 되었다. 

#USArrests 자료 별그림, 체르노프 얼굴그림 그리기, 해석하기
#1973년 미국 주별 강력 범죄율 데이터 

#install.packages("tibble")

data1<-USArrests
str(data1)
head(USArrests)

#1. 별그림
stars(data1,cex=0.5)


#2. 체르노프얼굴
#install.packages("aplpack")
library(aplpack)
faces(USArrests,face.type = 1, main = "Cheronoff faces: USArrests",cex=0.9)

#자료 해석하기
#별그림과 체르노프얼굴을 통해 비슷한 모양으로 나누어 보았다.  
#1)그룹 : mississippi, north carolina, south carolina, georgia, alabama  
#2)그룹 : montana, kentucky  
#3)그룹 : new maxico, michigan, texas  
#4)그룹 : ohio, washington, connecticut  

#그룹별로 4개 feature값이 비슷함을 확인 할 수 있다.


#그룹1 데이터  
#1) 그룹은 대부분 노란얼굴, 둥근머리,웃는얼굴의 체르노프 그래프가 그려지고 별그림은 양옆으로 더 길죽한 그림이 그려진다.

rbind(USArrests["Mississippi",], USArrests["North Carolina",],USArrests["South Carolina",], USArrests["Georgia",], USArrests["Alabama",])

#그룹2 데이터  
#2) 그룹은 대부분 주황얼굴, 웃지 않은 얼굴의 체르노프 그래프가 그려지고 별그림은 작은 별그림이 그려진다.
rbind(USArrests["Montana",], USArrests["Kentucky",])

# 그룹3 데이터  
#3) 그룹은 대부분 위아래로 큰 별그림이 그려지고, 넓적한 얼굴에 노란얼굴, 큰눈을 가진 얼굴이다.
rbind(USArrests["New Mexico",], USArrests["Michigan",], USArrests["Texas",])

#그룹4 데이터  
#4) 그룹은 대부분 조그마한 얼굴에 주황색 얼굴에 웃지않은 얼굴이며,오른쪽으로 치우친 별모양을 가지고 있다.
rbind(USArrests["Ohio",], USArrests["Washington",],USArrests["Connecticut",])