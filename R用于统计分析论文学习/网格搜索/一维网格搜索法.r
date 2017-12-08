###其实网格搜索就是几个for循环，来对参数进行最优搜索，对参数进行估计
#一维的网格搜索法进行求根
f=function(x) x^2-2*x+1 
x<-seq(-2,2,by=0.001)
x1<-x[1];
for(i in 2:length(x))
{
if(f(x[i])<f(x1))
x1<-x[i]
}
f(x1)
x1


#二维网格搜索
f2<-function(x,y) abs(x^2+y^2-8) #定义目标函数
x<-seq(0,3,by=0.001)
y<-seq(0,3,by=0.001)
x1<-x[1]
y1<-y[1]
##进行网格搜索
for(i in 2:length(x)){
  for(j in 2: length(y)){
    if(f2(x[i],y[j])<f2(x1,y1)){
      x1<-x[i]
      y1<-y[j]
}
}
}
c(x1,y1)
f2(x1,y1)