#---已完成的意思是已经复制到R帮助的word文件里面(当然是自己比较陌生且重要的代码)
#-------（已完成）帮助吧友

Path <- 'C:\\Users\\Administrator\\Desktop\\探索R_matlab数据挖掘'
#Path <- 'C:\\Users\\Administrator\\Desktop\\探索R_matlab数据挖掘\\R语言帮人解决作业'
setwd(Path)
getwd()
#帮助贴吧
#-------------联列表显著性检验
M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
dimnames(M) <- list(gender = c("M","F"),
                    party = c("Democrat","Independent", "Republican"))
(Xsq <- chisq.test(M))  # Prints test summary
Xsq$observed   # observed counts (same as M)
Xsq$expected   # expected counts under the null
Xsq$residuals  # Pearson residuals
Xsq$stdres     # standardized residuals

Data1 <- read.csv('研究生教育满意度调查数据.csv')
Matr <- Data1[,2:4] # 提取变量
M1 <- as.matrix(Matr) #转化为矩阵
M2 <- as.table(M1) #转化为连列表
M2 #输出该连列表
Xsq2 <- chisq.test(M2)

#帮助贴吧
library(ggplot2) # load the ggplot2 package
library(lubridate) # load the lubridate package
library(randomForest) # load the randomForest package

set.seed(1) # set a random seed in case of the random numbers' uniformity 

train <- read.csv('../input/train.csv') # read the data from the csv file
test <- read.csv('../input/test.csv')
 
extractFeatures <- function(data) { # define a function whose arguement is data
 ## set feature variables
 features <- c('season','holiday','workday','weather','temp','atemp','humidity','windspeed','hour')
 
 data$hour <- hour(ymd_hms(data$datetime)) # add another variable hour to the data
 return(data[,features]) # return the features variables
}

trainFea <- extractFeatures(train) # extract the features variables from train data
testFea <- extractFeatures(test) # extract the features variables from test
## create a dataframe named submission which contains two elememts,datetime and count.
submission <- data.frame(datetime=test$datetime,count=NA) 

for(i_year in  unique(year(ymd_hms(test$datetime)))){
   for(i_month in unique(month(ymd_hms(test$datetime)))){
        cat('Year:', i_year, '\tMonth:',i_month,'\n')
	testLocs  <-year(ymd_hms(test$datetime)) == i_year & month(ymd_hms(test$datetime)) == i_month
	testSubset <- test[testLocs,]
	trainLocs <- ymd_hms(train$datetime) <= min(ymd_hms(testSubset$datetime))
	rf <- randomForest(extractFeatures(train[trainLocs,]), train[trainLocs,'count'], ntree= 50)
	submission[testLocs, 'count'] <- predict(rf, extractFeatures(testSubset))
	}
}

#-------------------路径设置完毕(支持向量机)
#---帮助贴吧
rm(list=ls() )
require(e1071)  #载入函数包
#---载入数据
Data <- read.csv('result.csv')
Data[,'C'] <- as.factor(Data[,'C'])
#----设置训练集和测试机
set.seed(123)
temp1 <- sample(1:dim(Data)[1], 100) 
TrainData <- Data[temp1,] #随机抽取100个观测值作为训练集
TestData <- Data[-temp1,] #余下的作为测试集
#----开始训练
model <- svm(C ~ ., data = TrainData )#SVM函数有公式形式和数据形式，此为公式形式。
print(model)#输出显示SVM模型结果
summary(model)#汇总
# 用训练样本进行检验（test with train data）
pred <- fitted(model)
# 检验精确度（Check accuracy）:
(ConfuseMatrix<-table(pred,  TrainData$C)) #构造混淆矩阵
prop.table(ConfuseMatrix,2) #按列计算比例，对角线上值为各类的预测精度
# 用测试集进行预测
x <- TestData[,1:8]
TePred <- predict(model, x)
# 检验精确度（Check accuracy）:
(ConfuseMatrix<-table(TePred ,  TestData$C)) #构造混淆矩阵
prop.table(ConfuseMatrix,2) #按列计算比例，对角线上值为各类的预测精度



#-----帮助贴吧
#--用模特卡洛模拟求e,sqrt(2),sqrt(3),ln(2)
MC.ln2 <- function(n){
x <- runif(n,1,2)
y <- runif(n, 0,1)
return(sum(y< 1/x) /n)
}
MC.ln2(1000) # 一次模拟
#多次模拟以后求平均值
ys <- replicate(1000, MC.ln2(1000))
mean(ys)
#----我的方法
N <- 10000
T <- runif(N)
mean(1/(1+T))   #一次模拟 ln2
mean(1/sqrt(2*T)) #一次模拟 sqrt(2)
mean(3/2/sqrt(3*T)) #一次模拟 sqrt(3)
sum(1/ sapply(0:N, factorial))  #一次计算e
mean(N/factorial(N*T))  #一次计算e


# ---（已完结）帮助贴吧
df<-data.frame(
 姓名=c("张三","李四","王五","赵六","丁七"),
 性别=c("女","男","女","男","女"),
年龄=c(14,15,16,14,15),
 身高=c(156,165,157,162,159),
体重=c("42","49","41.5","52","45.5"));df
#----5  题
studata<-read.table("3.7.txt",header=T) #读入数据
 data.frame(studata) #转化为数据框
 attach(studata) #将数据框调入内存
plot(Weight~Height,col="red") #体重对于身高的散点图c
coplot(Weight~Height|Sex,col="blue") #不同性别，体重与身高的散点图
coplot(Weight~Height|Age,col="blue") #不同年龄，体重与身高的散点图
par(mar=c(0,0,0,0))
coplot(Weight~Height|Age+Sex,col="blue") #

#--3题
data_outline<-function(x){
n<-length(x)
m<-mean(x)
v<-var(x)
s<-sd(x)
me<-median(x)
cv<-100*s/m
css<-sum((x-m)^2)
uss<-sum(x^2)
R <-max(x)-min(x)
R1 <-quantile(x,3/4)-quantile(x,1/4)
sm <-s/sqrt(n)
g1 <-n/((n-1)*(n-2))*sum((x-m)^3)/s^3
g2 <-((n*(n+1))/((n-1)*(n-2)*(n-3))*sum((x-m)^4)/s^4-(3*(n-1)^2)/((n-2)*(n-3)))
data.frame(N=n,Mean=m,Var=v,std_dev=s,Median=me,std_mean=sm,CV=cv,CSS=css,USS=uss,R=R,R1=R1,Skewness=g1,Kurtosis=g2,row.names=1)
}
source("data_outline.R") #将程序调入内存
serumdata<-scan("3.1.txt");serumdata #将数据读入向量serumdata，按行读入返回一个向量。
data_outline(serumdata)
hist(serumdata,freq=FALSE,col="purple",border="red",density=3,angle=60,main=paste("the histogram of serumdata"),xlab="age",ylab="frequency")
lines(density(serumdata),col="blue")#密度估计曲线
x<- floor(min(serumdata)): ceiling(max(serumdata))
lines(x,dnorm(x,mean(serumdata),sd(serumdata)),col="green")  #正态分布的概率密度曲线
plot(ecdf(serumdata),verticals=TRUE,do.p=FALSE) #绘制经验分布图
lines(x,pnorm(x,mean(serumdata),sd(serumdata)),col="blue") #正态经验分布
qqnorm(serumdata,col="purple") #绘制QQ图
qqline(serumdata,col="red") #绘制QQ直线
boxplot(serumdata,col="lightblue",notch=T)

#(已完成)帮助贴吧
#------5 题
X <- c(rep(1,length=4),2,2,2,3,3,3,4,4,4,5,6,6,6,7,7,7,8,8,8,9,11,12,12,12)
Y <- c(0.6,1.6,0.5,1.2,2.0,1.3,2.5,2.2,2.4,1.2,3.5,4.1,5.1,5.7,3.4,9.7
     , 8.6,4.0,5.5,10.5,17.5,13.4,4.5,30.4,12.4,13.4,26.2,7.4)
#---(1)
Data <- data.frame(X,Y)
plot(Data)
fit1 <- lm(Y~X, data=Data)
lines(X,fitted(fit1))
#----(2)
summary(fit1)
#-----(3)
win.graph()
op <- par(mfrow=c(2,2))
plot(fit1)
par(op)
#-----(4)
Y1 <- sqrt(Y)
Data1 <- data.frame(X,Y1)
win.graph()
plot(Data1)
fit2 <- lm(Y1~X, data=Data1)
lines(X,fitted(fit2))
summary(fit2)
win.graph()
par(mfrow=c(2,2))
plot(fit2)
#------load data（数据载入）
MaNum<-  c(0,0,1,1,2,2,3,3,4,4,5,5,6,6)  #  Machine Number
Sales <- c(508.1,498.4,568.2,577.3,651.7,657.0,713.4,697.5,755.3,758.9,787.6,792.1,841.4,831.8)
Data  <- data.frame(MaNum, Sales)
#------ linear regression(线性回归)
fit1 <- lm(Sales ~ MaNum, data =Data)
summary(fit1)  # linear regression is good
#------ polynomial regression(多项式回归)
fit2 <- lm(Sales ~ MaNum + I(MaNum^2)) # polynomial regression with order two(二次多项式回归)
summary(fit2) # fit information （查看拟合信息），效果也不错
fit3 <- lm(Sales ~ MaNum + I(MaNum^2)+ I(MaNum^3)) # polynomial regression with order three(三次多项式回归)
summary(fit3) # 3次时，它的高次项变量的显著性检验通不过

#------scatter plot and fit curve
par(mfrow=c(2,2))
plot(Data, main='raw data') # scatter
plot(MaNum,fitted(fit1),  type='l',main='linear regression')
plot(MaNum,fitted(fit2),  type='l', main='polynomial regression with order two')
plot(MaNum,fitted(fit3),  type='l', main='polynomial regression with order three')
#------九题 install and load the MPV library
install.packages('MPV')
library(MPV)
#-------load data
?table.b4 #  make figure of the data  等价于 
# help(table.b4)
#--------(a) multiple regression model
fit91 <- lm(y ~ x3+x6+x8, data= table.b4)
summary(fit91)
#--------(b) estamated error variance
var(resid(fit91))
#--------(c) diagnostic plots
par(mfrow=c(2,2))
plot(fit91)
#--------(d) Re_estimate the model parameters
fit92 <- lm(y~ x3+x6+x8, data=table.b4, weights=1/x6^2)
summary(fit92)
win.graph()
par(mfrow=c(2,2))
plot(fit92)
#--
fit <- list()
for(k in 1:n){
 temp <- paste0('I(MaNum^)', k)
 fit[[k]] <- lm(Sales ~ MaNum + I(MaNum^2))
}


#-----10 题
A <- matrix(1:20, nrow=4, ncol=5);A
B <- matrix(1:20, nrow=4,ncol=5, byrow=T);B
C <- A + B;C
D <- A*B;D
E <- A%*%t(B);E 
F <- A[1:3,1:3];F 
G <- B[,-3];G
#-----11 题
x1 <- c(2,4,3,2,4,7,2,2,5,4)
x2 <- c(5, 6,8,5,10,7,12,12,6,6)
x3 <- c(7,11,6,6,7,9,5,5,10,6,3,10)
#-----箱线图
boxplot(x1,x2,x3, col=1:3)
type <- c(rep(1,length= length(x1)), rep(2,length= length(x2)), rep(3,length= length(x3)))
x<- c(x1, x2,x3)
plot(type,x, col=type)
abline(h=c(mean(x1),mean(x2),mean(x3)), col=1:3)


#（已完成）帮助贴吧
#---------第(1)问
Matrix <- NULL
for(i in 1:5){
Matrix <- cbind(Matrix, runif(10, 0, 10)) # 用于生成一个服从U(0,10)的均匀分布的10维向量
}
print(Matrix) # 输出生成的矩阵
#---------第(2)问
colmean1 <- function(x)  colMeans(x)   #这是写一个计算矩阵列均值的函数的方法1
colmean2 <- function(x) apply(x, 2, mean) #这是方法2
colmean1(Matrix)
colmean2(Matrix)
#---------第(3)问
ExtractEvenRow <- function(x) {
   n <- dim(x)[1]
   Result <- NULL
   for(i in 1:n){
      if(i %%2 ==0){ #行标为偶数，则提取出来
	   Result <- rbind(Result, x[i,])
	   }
   }
   return(Result)
 }
ExtractEvenRow(Matrix)


#-------(已完成)帮助贴吧
m <- 5000  #设m为5000页
n  <- 5  # n为5 
x <- 5   # 假设x=5，这里x可以取成一个向量，同时求多个分布概率。
ppois(x, lambda=n) 
##比如下面求多个x的概率
x <- 1:100
ppois(x, lambda=n) 
 

#--------(已完成)帮助贴吧
#--------配对t检验
n <- 10
x1 <- 79.5
x2 <- 71
sd1 <-  9.124
sd2 <- 9.940
r <- 0.704
T <- (x1-x2)/sqrt((sd1^2+sd2^2-2*r*sd1*sd2) /n) #配对t检验公式
T  # 计算t统计量
(P_value <- 2* (1- pt(abs(T), (2*n-2)))) # 计算检验的p值


#----------------
install.packages('dplyr')
install.packages('tidyr')
library(ggplot2)
library(plyr)
library(dplyr)
library(rvest)
library(tidyr)
n <- 11
anscombe_tidy <- anscombe %>%  # %>%在rvest包中，是管道函数
   mutate(observation = seq_len(n)) %>% #seq_len()是seq的函数变体用法
   gather(key, value, -observation) %>% 
   separate(key, c("variable", "set"), 1, convert = TRUE) %>%
    mutate(set = c("I", "II", "III", "IV")[set]) %>%
	spread(variable, value)



#------------帮夏晓波进行反应面分析
K <- seq(0, 1.5, length=4)
Ca <- seq(0, 0.3, length=4)
Mg <- seq(0, 0.15, length=4)
KK <- c(K,rep(K[3],length=8))
Ca2 <- c(rep(Ca[3], length=4), Ca, rep(Ca[3], length=4))
Mg2 <- c(rep(Mg[3], length=8), Mg)
Mg22 <- Mg2^2 
Sour <- c(3.88,5.9,7.05,4.51,4.6,5.05,7.07,9.41,4.69,2.83,7.4,9.01)
Data <- data.frame(Sour, KK, Ca2, Mg2)
lm.s <- lm(Sour~ KK+Ca2*Mg2+KK:Ca2+I(KK^2)+I(Ca2^2)+ I(Mg2^2), data= Data) #这是二次回归
#lm.s <- lm(Sour~ (KK+Ca2+Mg2+Mg2)^2)
model <- formula(Sour~ 0+KK+ KK%in% Ca2, data=Data) #截距为0，可以自己单独建立公式或者模型，再用lm建模
lm(model)
summary(lm(model))

summary(lm.s) 
anova(lm.s) 
write.table(Data,'xiaxiaobo.txt',  sep=' ', row.names=F, col.names=F) #这是为了和matlab进行交互而利用的数据输出格式
#----------帮助贴吧
请教各位大神，下面的极大似然估计可以估计出三个参数u、k和w。
为了解决新的问题，u不再是一个具体数值，而是一个数列，
optim函数下只能求具体值，就需要对每一步求一个u。总共需要运行262次，
每次返回一个结果，最后形成包含262个u的序列。现在的问题是，
在极大似然函数LL外加循环时，到return这一步就中断了，在循环内则会报错。
请教各位，这个问题该如何解决，谢谢大家了！
#-------------if else的标准用法示意
if(2>=1){
  stop('滚出')
  }else
  {
    stop('get in')
	}
	
LL<- function(x,data1)
{
	 u<- x[1]
	 w<- x[2]
	 k<- x[3]
	 extra.sum<-0
   for (a in 2:262) {
	 inner.sum<-0
	 outer.sum<-0
       for (b in 1:261) {
	 if (data1[b] >= data1[a]) {
		next 
	 } else {
		 inner.sum <- inner.sum + w * exp(w * (data1[b] - data1[a]))
		 extra.sum <- extra.sum + exp(w * data1[b])-exp(w * (data1[b] - 4350))
		 }
 }
 k0_inner.sum <- k * inner.sum
 log_inner.sum <- log(u + k0_inner.sum)
 outer.sum <- outer.sum + log_inner.sum
 k0_extra.sum <- k * extra.sum
 }
 c<- outer.sum-u*4350-k0_extra.sum
 return(-c)
}
x <- c(2,3,4)
data1 <- rnorm(262)
LL(x, data1)
Result <- NULL
for(i in 1: 262){
Data.res <- optim(c(0.036,0.06,0.048) , LL , data1 = data1 )
Result <- rbind(Result,Data.res$par)
}
LL(Data.res$par, data1)
#---------------help2
已知十名运动员100米短跑的成绩，按编号顺序存放在D盘根
目录下的数据文件（score.txt）中。1～10号运动员的成绩
依次为 12.12,11.53,11.45,12.11,12.22,13.10,12.33,13.14,11.98,13.45。
然后从数据文件中读入数据并排出名次，保存到sort.txt中.

score <- read.table('D:\\score.txt') #从指定文件夹文件读入数据，注意路径的字符串的写法
score <- as.vector(t(score))
info <- data.frame('成绩'=sort(score), '名次'=paste0('第',1:length(score),'名'),'运动员编号'=order(score))
write.table(info, 'D:\\sort.txt', row.names=F)
#--------------help3
找出在1～1000中同时满足被7除余5、被5除余3、被3除余2的数。

Num <- 0 
k <-1 
for(n in 1:100000){ 
if(n%%7==5 & n%%5==3 & n%%3==2){  # %%表示求余的运算
  Num[k] <- n
  k <- k+1 
	}
	if(n %%10000 ==0)
	cat('程序运行进度为：',floor(n /1000) ,'%\n')  #利用for循环的循环指标变量来得到程序运行的进度
}
#----------------help4
求大神解答为什么用二次多项式lm()函数拟合的曲线很杂乱？附图
x <- c(1:4,2,3,5, 8:100)
y <- -x^5+ 2*x^4 + 3*x^3 + x + 6
plot(x, ann=F, type='n' )  #这是仅建立坐标轴，无横纵轴的标签，但有坐标刻度
lines(x, y) #这是他的绘图方法
win.graph()
plot(x,y, type='l')  #应该这样绘图才对

#------帮助R朋友
library(MASS)  载入相应的R包，各个包有各个包的相应的功能
#data(iris)  #载入指定的数据集，其实载入包时，数据集已经在内存中了，这句话是多余的
attach(iris)  # 将数据框的元素加到内存中
model <- as.formula('Species~Sepal.Length + Sepal.Width+ Petal.Length + Petal.Width') #将字符串转化为公式
lda(model) # 建立线性判别模型
iris.lda<-lda(Species~Sepal.Length + Sepal.Width+ Petal.Length + Petal.Width)
iris.lda
decreaseDimention <- as.matrix(iris[,1:4])%*%iris.lda[[4]] # 这一步就是降维操作，其实是计算投影后的判别函数值
iris.pred<-predict(iris.lda)$class  # 对新样品进行归类
plot(decreaseDimention, col=as.numeric(Species),pch=as.numeric(Species), main='预测图')
legend('topright',c('类别1','类别2','类别3'),col=1:3,pch=c(1,2,3))
win.graph()
plot(decreaseDimention, col=as.numeric(iris.pred),pch=as.numeric(iris.pred), main='实际图')
legend('topright',c('类别1','类别2','类别3'),col=1:3,pch=c(1,2,3)) #添加图例

#利用混淆矩阵评价判别分析的效果
(ConfuseMatrix<-table(iris.pred,Species)) #该函数建立一个联列表，统计分类情况，正确个数和错误个数。
#对角线上就是正确个数，非对角线上就是误判个数。
prop.table(ConfuseMatrix,2) #2表示按列计算所占的比例

detach(iris) #关闭iris与内存的连接
#精度高，可以开始对新样本进行分类
New.pred<-predict(iris.lda,iris[145:150,1:4]);New.pred$class #predict利用指定的模型进行预测，输出分类结果。
New.pred$x #输出投影空间的坐标

#----------------帮助贴吧（不能对数据框指定元素直接赋值，即使向量长度一样）
my.dat <- list()
My.dat <- data.frame()
Data <- cbind(rnorm(252), runif(252), rpois(252, 4), rbinom(252, 10, prob=0.5))
my.dat$gdp <- Data[,4]
my.dat$Gdp <- Data[,3]
as.data.frame(my.dat) # 所以最好先用列表，再转换成数据框为好。
My.dat$gdp <- Data[,3] # 直接给数据框中的元素赋值会出错

#--------------自己所学
rm(list=ls())
path = 'C:\\Users\\Administrator\\Desktop\\问卷'
setwd(path)  #更改工作目录文件
# cat函数原来不仅可以用来输出信息，还可以用于建立无扩展名的文本文档
cat("file A\n", file="A") #创建一个文件A，文件内容是'file A','\n'表示换行，这是一个很好的习惯
cat("file B\n", file="B")  #创建一个文件B
file.append("A", "B")  #将文件B的内容附到A内容的后面，注意没有空行
append(c(1,3,4),2,after=1) # 用于对向量中指定位置追加数据
file.create("A")  #创建一个文件A, 注意会覆盖原来的文件
file.append("A", rep("B", 10)) #将文件B的内容复制10便，并先后附到文件A内容后
file.show("B")  # 新开R脚本工作窗口显示文件A的内容
file.copy("A", "C") #复制文件A保存为C文件，同一个文件夹
file.show('C')
dir.create("tmp")  #创建名为tmp的文件夹
file.copy(c("A", "B"), "tmp") #将文件夹拷贝到tmp文件夹中
list.files("tmp")  #查看文件夹tmp中的文件名
unlink("tmp", recursive=F) #如果文件夹tmp为空，删除文件夹tmp
unlink("tmp", recursive=TRUE) #删除文件夹tmp，如果其中有文件一并删除
file.remove("A", "B", "C")  #移除三个文件
file.rename() #重命名文件

# 有点作假的感觉，不能乱用
#----------------------批量填写指定格式的“调查问卷.txt”N份-------------

Wenjuan <- list()  # 初始化一个列表，用于放每份问卷的内容
N <- 500  # 设定问卷文件的份数 
#---------问卷复制和读取问卷的内容
for(i in 1:N){
 A <- paste0('调查问卷',i,'.txt') #设置每份复制的问卷的文件名
 file.remove(A) #若路径中已存在同名文件，则移除该文件
 file.append(A, '调查问卷.txt') # 将文件“调查问卷.txt”复制N份
 Wenjuan[[i]] <- readLines(A, encoding ="UTF-8") #读取问卷的内容，放入列表的元素中
}
#--------------------对所有问卷文件进行填写--------------------------#

#-----只有第1题和7为A和B选项，第3题为A，B,C选项，其他为A,B,C,D的选项
for(j in 1:N){ 
for(ii in 1:length(Wenjuan[[j]]) ){ 
 
 if(ii ==3 || ii==27 ){  #对指定行进行特殊处理
 Item17 <- sample(c('A','B'),1) # 从A,B中选取一个
 Wenjuan[[j]][ii]<-sub('（  ）',paste0('（ ',Item17,' ）'), Wenjuan[[j]][ii]) # 利用替换法进行答案填写
 }
 else if(ii == 10){
  Item3 <- sample(c('A','B','C'),1, prob=c(0.1,0.4,0.5))
  Wenjuan[[j]][ii]<-sub('（  ）',paste0('（ ',Item3,' ）'), Wenjuan[[j]][ii])
  }
 else{
  Item <- sample(c('A','B','C','D'),1,prob=c(0.1,0.15,0.5,0.25))
  Wenjuan[[j]][ii]<-sub('（  ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii])
  }
}
#---------问卷内容填写完成后，写入到txt文件中，然后修改txt文件的扩展名为.doc
write.table(Wenjuan[[j]],paste0('问卷',j,'.txt'), row.names=F, col.names=F,quote = F,fileEncoding =' ') #写入到.txt文件中，quote=F表示去掉引号,不进行编码
file.rename(paste0('问卷',j,'.txt'),sub("txt","doc",paste0('问卷',j,'.txt'))) #修改扩展名
}

#---------------------------于是，问卷的填写结束-------------------------#

#----------------------------------R语言进行文件处理的其他技巧------------------#
# 1.建立文件目录表
f1 <- dir() #将目录列表赋值给变量
f1 <- as.data.frame(f1) #将变量转为data.frame以便执行后面的写入操作，此步骤也可与上面一并执行
write.table(f1, file="本文件夹中的所有文件目录.txt", quote=F, col.names=F) #将数据框fl写入文
    #本文件，由于写入时会有引号，所以加参数去掉，列名称被去掉了，
   #或者可以改成想要的列名称，另外如果不想要每一行的序号还
   #可以加入“row.names=F”
# 2.修改文件名
fl_1 <- dir() # 读入后为一个字符串向量
fl_2 <- as.character(fl_1) #这句话没用，仅仅起到了赋值的作用
fl_2 <- gsub("(\\()([0-9]{1})(\\))", "\\100\\2\\3", fl_2) #查找“(1)”等，并改为“(001)”至“(009)”
fl_2 <- gsub("(\\()([0-9]{2})(\\))", "\\10\\2\\3", fl_2) #查找“(10)”等，并改为“(010)”至“(099)”

file.rename(fl_1, fl_2) #函数形式为file.rename(from, to)，from为原始文件名向量，to为新的文件名向量
names <- dir(".")
for (i in 1:length(names))
{file.rename(names[i],sub("dat","asc",names[i]))}
for(i in 1:N){
 A <- paste0('调查问卷',i,'.doc')
 B <- paste0('调查问卷',i,'.txt')
 file.rename(A, B)
}
#-----------------------正则表达式的学习-------------------------------#
#----常用正则表达式的语法
##  strsplit函数默认利用正则表达式进行匹配，下面利用
#   默认参数fixed=F讲解正则表达式（下面的字符都被当做正则表达式）
strsplit('afa gaga sadlss sdg',split='.') #按照任意字符分割字符串,该函数会返回一个列表
strsplit('afa gaga sadlss sdg',split='*') #将每个字符拆分开
strsplit('afa gaga sadlss sdg',split='[a-z]')# split='[a-z]'表示按照a-z中任意一个字母分割数据
strsplit('afa gaga sadlss sdg',split='s{2}')# split='s{2}'表示按照ss分割字符串
strsplit('afa gaga sadlss sdg',split='^a')#匹配文本开始位置的a拆分字符串
strsplit('afa gaga sadlss sdg[dagg]',split='[^afa]') #匹配非方括号中的任意字符分割字符串
strsplit('afa gaga sadlss sdg[dagg]',split='a|s') #从a、s中任选一个进行匹配，分割字符串

strsplit('a.b.c.H.\ngh^G',split='[.^\n]') #查找元字符，从这里也可以看到R把 \n 是当成一个字符来处理的
strsplit('abcd1342', split='[[:alnum:]]')
strsplit('abcd1342@#%^$$@#@^#&%*(*&&^$%', split='[^[:punct:]]') #在非标点字符处断行

#----基本字符串操作
# 1.字符数统计和查找替换
# nchar这个函数简单，统计向量中每个元素的字符个数，注意这个函数和length函数的差别：
# nchar是向量元素的字符个数，而length是向量长度（向量元素的个数）。其他没什么需要说的。
x <- c("Hellow", "World", "!") 
nchar(x)  #[1] 6 5 1
length(x) # 3
DNA <- "AtGCtttACC" 
tolower(DNA)  #将字符串字母全部转化成小写字母
toupper(DNA)  #转化成大写字母
chartr(old="Tt",new= "Uu", DNA) #用新的字符替换对应的旧的字符，实质就是查找替换 
casefold(DNA, upper = FALSE) # upper = FALSE表示全部转化为小写字母
# 2.字符串连接
# paste应该是R中最常用字符串函数了，也是R字符串处理函数里面非常纯的不使用正则表达式的函数
# （因为用不着）。它相当于其他语言的strjoin，但是功能更强大。它把向量连成字串向量，其他类
# 型的数据会转成向量，但不一定是你要的结果：

paste("CK", 1:6, sep="") #等价于
paste0("CK",1:6)
x <- list(a="aaa", b="bbb", c="ccc") 
y <- list(d=1, e=2) 
paste(x, y, sep="-") #较短的向量被循环使用 （循环规则）
z <- list(x,y) 
paste("T", z, sep=":") #z中2个元素分别与T连接 
# 短向量重复使用，列表数据只有一级列表能有好的表现，能不能用看自己需要。
# 会得到什么样的结果是可以预知的，用as.character函数看吧，这又是一个字符串处理函数：
as.character(x) #将列表转化为字符串向量
as.character(z) 
# paste函数还有一个用法，设置collapse参数，连成一个字符串：
paste(x, y, sep="-", collapse='; ') #生成含有一个元素的字符串向量
paste(x, collapse='; ') 

# 3.字符串查询
# grep和grepl函数（默认用正则表达式进行匹配）
#  这两个函数返回向量水平的匹配结果，不涉及匹配字符串的详细位置信息。
#grep(pattern, x, ignore.case = FALSE, perl = FALSE, value = FALSE, 
     fixed = FALSE, useBytes = FALSE, invert = FALSE) 
# grepl(pattern, x, ignore.case = FALSE, perl = FALSE, 
      fixed = FALSE, useBytes = FALSE) 
# 虽然参数看起差不多，但是返回的结果不一样。下来例子列出C:\windows目录下的所有文件，
# 然后用grep和grepl查找exe文件：
 files <- list.files("c:/windows") ##下面查找以名字.exe结尾的文件
 grep("\\.exe$", files)  #返回元素位置构成的整型向量（使用正则表达式进行匹配）
grepl("\\.exe$", files)  #返回逻辑向量（因为.是元字符，所以必须用\\来对它进行转义处理）
# grep仅返回匹配项的下标，而grepl返回所有的查询结果，并用逻辑向量表示有没有找到匹配。
# 两者的结果用于提取数据子集的结果都一样：
files[grep("\\.exe$", files)] 
files[grepl("\\.exe$", files)] 

# 4.字符串的查询和提取

# 函数 regexpr、gregexpr和regexec
text <-c("Hellow, Adam!", "Hi, Adam!", "How are you, Adam.")
# ----提取方法1：
#-------建立一个内容提取函数（针对于regexpr函数的）
getcontent1 <- function(s,weizhi, idx){
substring(s,weizhi[idx],weizhi[idx]+attr(weizhi,'match.length')[idx]-1)#从s字符串中的第g个字符到第(g+匹配的字符长度-1)个字符进行提取
#实质上，第g个字符到第(g+匹配的字符长度-1)个字符就是我们需要的匹配字符串
}
#-------建立完毕
(weizhi<-regexpr("Adam", text) )#对向量中每个元素进行搜索，返回一个整型向量
xinxi<-0
xinxi1 <- NULL
for(i in 1:length(text)){
xinxi[i]<- substring(text[i],weizhi[i],weizhi[i]+attr(weizhi,'match.length')[i]-1) #提取指定字符串的for循环
xinxi1[i] <- getcontent1(text[i], weizhi, i) #用函数getcontent1来获取内容
}
xinxi
xinxi1   #输出提取的内容

#  汉字的正则表达式编码:[\u4E00-\u9FA5]+
# ----------提取方法2：
#-------建立一个内容提取函数（针对于gregexpr函数的）
getcontent2 <- function(s,g){
substring(s,g,g+attr(g,'match.length')-1) #从s字符串中的第g个字符到第(g+匹配的字符长度-1)个字符进行提取
#实质上，第g个字符到第(g+匹配的字符长度-1)个字符就是我们需要的匹配字符串
}
#-------建立完毕
weizhi2<- gregexpr("Adam", text) #返回一个列表
gregexpr("Adam", text)[[2]]#输出该列表中第2个元素
xinxi2 <- NULL
for(i in 1:length(text)){
xinxi2[i]<- getcontent2(text[i], weizhi2[[i]]) #利用获取内容的函数提取指定字符串
}
xinxi2 # 输出提取的内容
#-----------提取方法3：
regexec("Adam", text)  #返回一个列表
regexec("Adam", text)[[1]][1] #输出该列表中第1个元素的第一个子元素
#---------- 提取方法4:
# sub和gsub函数可以使用提取表达式（转义字符+数字）让部分变成全部：
sub(pattern=".*(Adam).*", replacement="\\1", text)
sub(pattern=".*(Adam).*", replacement="\\1", text)  

# 5. 字符串替换
text
sub(pattern="Adam", replacement="world", text) 
# sub和gsub的区别是前者对向量每个元素只做一次替换（不管有几次匹配），
# 而gsub把满足条件的匹配都做替换：
text<-c('Adam! Adam! Adam','Adam!  Adam')
sub(pattern="Adam|Ava", replacement="world", text) 
[1] "world! Adam! Adam" "world!  Adam"
gsub(pattern="Adam|Ava", replacement="world", text) 
[1] "world! world! world" "world!  world"
# sub和gsub函数可以使用提取表达式（转义字符+数字）让部分变成全部：
sub(pattern=".*(Adam).*", replacement="\\1", text) #用Adam字符串替换其他的字符串
gsub(pattern=".*((Ada)m).*", replacement="\\2", text) ##用Ada字符串替换其他的字符串

# 6. 字符串提取
# substr和substring函数通过位置进行字符串拆分或提取，它们本身并不使用正则表达式，
# 但是结合正则表达式函数regexpr、gregexpr或regexec使用可以非常方便地从大量文本
# 中提取所需信息。两者的参数设置基本相同：
substr(x, start, stop) 
substring(text, first, last) 
第 1个参数均为要拆分的字串向量，第2个参数为截取的起始位置向量，第3个参数为截取字串
的终止位置向量。但它们的返回值的长度（个数）有差 别：substr返回的字串个数等于第一个
参数的长度；而substring返回字串个数等于三个参数中最长向量长度，短向量循环使用。先看
#第1参数（要 拆分的字符向量）长度为1例子：
x <- "123456789" 
substr(x, c(2,4), c(4,5,8)) #因为x的向量长度为1，所以substr获得的结果只有1个字串，
                  #即第2和第3个参数向量只用了第一个组合：起始位置2，终止位置4。
substring(x, c(2,4), c(4,5,8)) 
#用regexpr、gregexpr或regexec函数获得位置信息后再进行字符串提取的操作，非常方便
#--------------------------正则表达式篇暂时结束-----------------------#

#-------------------贴吧网页数据抓取-----------#
rm(list=ls())
url1 <- 'http://tieba.baidu.com/p/3668030315?see_lz=1#&qq-pf-to=pcqq.c2c'
require(XML) #载入网页抓取相关包，支持XPath选择器。
URL<-htmlParse(url1,encoding="UTF-8")#把html文件读入r语言中并用UTF-8编码解析成源文件
yp.imfo1<-getNodeSet(URL,'//body//div[@class="post_bubble_middle"]//text()')
#-----------------------别人的爬虫R程序
library(bitops);
library(RCurl);##url的R版##
library(XML);##解析网页用##

##method (1)##直接借助别人写好的函数##

URL = 'http://data.eastmoney.com/bbsj/stock300326/yjbb.html'

if(url.exists(URL)){ #如果该网址存在，则进行操作

      ##read the special table data##
      TableData <- readHTMLTable(URL)

      ##ok##

}

##method (2)##利用网页dom树来查找自己所需要数据

Link ='http://data.eastmoney.com/bbsj/stock000547/yjbb.html'

if(url.exists(Link)){

webpage <- getURL(Link);

webpage2 <- readLines(tc <- textConnection(webpage));close(tc);

pagetree <- htmlTreeParse(webpage2, error = function(...){}, useInternalNodes = TRUE);

###网页的多个表格情况下，用HTML语言限定用什么样的表格###

tablehead <- xpathSApply(pagetree, "//table//th", xmlValue);

###tablehead 是抓取表格的标题，“”里面是是HTML语言###

result <- xpathSApply(pagetree, "//table//td", xmlValue)

##上面是重点，在pagetree上来查找所需要的内容（其实是正则匹配）。

}


##---------问题1 "Wed Jun 03 20:32:24 2015"
#-------read.table如何读取多个txt文件，只会读取单个txt文件，但手头有n个txt文件需要读取，怎么办？
Path <-"E:/学习/R处理文本文件/批量读取txt文档"
setwd(Path)
getwd()  # 获得当前路径
txtFile <- dir()   # 获取当前路径中的所有文件名
TotalData <- list() # 初始化列表
k <- 1
for(File in txtFile){  # 这是R中独有的向量循环语句
    TotalData[[k]] <- readLines(File, encoding ="UTF-8") #这是读取中文纯文本文件，要以UTF-8编码
     TotalData[[k]] <- read.table(File)  #这是批量读取样本数据的方式
    k <- k+1
}

#------------------------已帮助贴吧
#t.test(73, 79.2, )
mu1 <- 73; mu2 <- 79.2
delta <- 17
n <- 20
(T <-  (mu1-mu2)/(delta/sqrt(n)) )#计算t统计量
(pvalue <- 2*pnorm(-abs((mu1-mu2)/(delta/sqrt(n))))) #计算p值


#-------------------帮助贴吧
require(rvest)
URL <-  "www.swu.edu.cn"
getdata <- function(URL){
       nametext <- ""
        pricetext <- ""
       web <- html(URL, encoding="UTF-8")
        nametext <- web%>%html_nodes('h3.tb-main-title') %>%html_text()
        pricetext <- web %>%html_nodes('div.tb-property-cont en.tb-rmb-num') %>%html_text()
        data.frame(name=nametext, price=pricetext)
}
getdata(URL)


