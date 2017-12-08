
rm(list=ls())
dirPath <- 'E:\\学习\\R处理文本文件\\西南大学毕业生问卷调查'
setwd('E:\\学习\\R处理文本文件\\西南大学毕业生问卷调查')
list.files()

#----------------------批量填写指定格式的“调查问卷.txt”N份-------------

# 师范生问卷填写 -----------------------------------------------------------------


# create a new file

newfile <- '师范生调查问卷'
dir.create(newfile)
# set a new directory
newPath <- paste(dirPath,'\\', newfile,sep='')
setwd(newPath)
list.files()
#---------问卷复制和读取问卷的内容
# 指定源文件路径
oriFile <- paste0(dirPath, '\\', '2015届毕业生就业状况调查问卷(正式).txt')

Wenjuan <- list()  # 初始化一个列表，用于放每份问卷的内容
N <- 90 # 设定问卷文件的份数 
for(i in 1:N){
  A <- paste0('2015届毕业生就业状况调查问卷(正式)',i,'.txt') #设置每份复制的问卷的文件名
  file.remove(A) #移除文件
  file.append(A, oriFile) # 将文件“调查问卷.txt”复制N份
  Wenjuan[[i]] <- readLines(A) #读取问卷的内容，放入列表的元素中
}
#--------------------对所有问卷文件进行填写--------------------------#
Wenjuan[[1]]
#-----只有第1题和7为A和B选项，第3题为A，B,C选项，其他为A,B,C,D的选项
for(j in 1:N){ 
  for(ii in 1:length(Wenjuan[[j]]) ){ 
    if(ii ==5){
      Item <- 'B' # 师范生只能从B中选取
      Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) # 利用替换法进行答案填写
    }
     if(ii == 24){ #只能选A
       Item <- 'A' # 师范生只能从B中选取
       Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
     } 
    if(ii == 30){ #只能选A
      Item <- 'A' # 师范生只能从B中选取
      Wenjuan[[j]][ii]<-sub('（  ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
    } 
    if(ii == 36){ 
      Item <- sample(c('A','B','C'), size=1, prob=c(0.6, 0.4-1e-2,1e-2 ))
      Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
    } 
    if(ii == 42){ #只能选A
      Item <- 'A' 
      Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
    } 
    if(ii == 48){ 
      Item <- sample(c('A','B','C'), size=1, prob=c(0.6, 0.4-1e-2,1e-2 ))
      Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
    } 
    if(ii == 54){ 
      Item <- sample(c('A','B','C'), size=1, prob=c(0.6, 0.4-1e-2,1e-2 ))
      Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
    } 
    if(ii == 60){ 
      Item <- sample(c('B','C','D'), size=1, prob=c(0.6, 0.4-1e-2,1e-2 ))
      Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
    } 
    if(ii == 71){ 
      Item <- sample(c('A','B','C'), size=1, prob=c(0.8, 0.2-1e-2,1e-2 ))
      Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
    } 
    if(ii == 77){ 
      Item1 <- sample(c('A','B'), size=1, prob=c(1-1e-2,1e-2 ))
      Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item1,' ）'), Wenjuan[[j]][ii]) 
    } 
    if(ii == 85 ){ 
      if(Item1=='B'){
      Item <- sample(c('A','C','D'), size=1, prob=c(1/3, 1/3, 1/3))
      Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
      }
    } 
    if(ii == 94){ 
      Item <- sample(c('A','B'), size=1, prob=c(1-1e-2,1e-2 ))
      Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
    } 
    if(ii == 99){ 
      Item <- sample(c('B','C','D','E','F','H','G'), size=1, prob=c(1-6e-2,1e-2, 1e-2,1e-2,1e-2,1e-2,1e-2))
      Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
    } 
    if(ii == 109){ 
      Item <- sample(c('A','B','C'), size=1, prob=c(0.8, 0.2-1e-2,1e-2 ))
      Wenjuan[[j]][ii]<-sub('（  ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
    } 
    if(ii == 114){ 
      Item <- sample(c('ABC','ABCE'), size=1, prob=c(0.8,0.2))
      Wenjuan[[j]][ii]<-sub('（多选）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
    } 
    if(ii == 121){ 
      Item <- sample(c('A','B','C'), size=1, prob=c(0.8, 0.2-1e-2,1e-2 ))
      Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
    } 
    if(ii == 127){ 
      Item <- sample(c('A','B','C'), size=1, prob=c(0.8, 0.2-1e-2,1e-2 ))
      Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
    } 
    if(ii == 133){ 
      Item <- sample(c('A','B'), size=1, prob=c(0.8, 0.2))
      Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
    } 
    if(ii == 139){ 
      Item <- sample(c('A','B'), size=1, prob=c(0.8, 0.2))
      Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
    } 
    if(ii == 145){ 
      Item <- 'C'
      Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
    } 
    if(ii == 160){ 
      Item <- sample(c('A','B'), size=1, prob=c(0.9, 0.1))
      Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
    } 
    
  }
  #---------问卷内容填写完成后，写入到txt文件中，然后修改txt文件的扩展名为.doc
  write.table(Wenjuan[[j]],paste0('问卷',j,'.txt'), row.names=F, col.names=F,quote = F) #写入到.txt文件中，quote=F表示去掉引号
  file.rename(paste0('问卷',j,'.txt'),sub("txt","doc",paste0('问卷',j,'.txt'))) #修改扩展名
}
#-----师范生的问卷的填写结束-------------------------#

#----------------------- 非师范生问卷填写 ---------------------------------------------
rm(list=ls())

# create a new file
newfile <- '非师范生调查问卷'
dir.create(newfile)

# set a new directory
newPath <- paste(dirPath,'\\', newfile,sep='')
setwd(newPath)
list.files()
#---------问卷复制和读取问卷的内容
# 指定源文件路径
oriFile <- paste0(dirPath, '\\', '2015届毕业生就业状况调查问卷(正式).txt')

Wenjuan <- list()  # 初始化一个列表，用于放每份问卷的内容
N <- 20 # 设定问卷文件的份数 
for(i in 1:N){
  A <- paste0('2015届毕业生就业状况调查问卷(正式)',i,'.txt') #设置每份复制的问卷的文件名
  file.remove(A) #移除文件
  file.append(A, oriFile) # 将文件“调查问卷.txt”复制N份
  Wenjuan[[i]] <- readLines(A) #读取问卷的内容，放入列表的元素中
}

#--------------------对所有问卷文件进行填写--------------------------#
Wenjuan[[1]]
#-----只有第1题和7为A和B选项，第3题为A，B,C选项，其他为A,B,C,D的选项
for(j in 1:N){ 
  for(ii in 1:length(Wenjuan[[j]]) ){ 
    if(ii ==5){
      Item3 <- sample(c('A','C','D','H'), size=1, prob=c(0.2,0.7-5*1e-2,0.1,5*1e-2)) # 师范生只能从B中选取
      Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item3,' ）'), Wenjuan[[j]][ii]) # 利用替换法进行答案填写
    }
    if(ii ==14){
      if(Item3=='H'){
      Item <- sample(c('A','B','E'), size=1, prob=c(0.2,0.6,0.2)) 
      Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) # 利用替换法进行答案填写
      }
      }
    if(ii == 24){ 
      Item <- sample(c('B','C'), size=1, prob=c(0.7,0.3))
      Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
    } 
    if(ii == 30){ #只能选A
      Item <- sample(c('B','C'), size=1, prob=c(0.64,0.36))
      Wenjuan[[j]][ii]<-sub('（  ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
    } 
    if(ii == 36){ 
      Item <- sample(c('A','B','C'), size=1, prob=c(0.6, 0.4-1e-2,1e-2 ))
      Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
    } 
    if(ii == 42){ #只能选A
      Item <- sample(c('A','B','C'), size=1, prob=c(0.6, 0.4-1e-2,1e-2 ))
      Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
    } 
    if(ii == 48){ 
      Item <- sample(c('A','B','C'), size=1, prob=c(0.6, 0.4-1e-2,1e-2 ))
      Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
    } 
    if(ii == 54){ 
      Item <- sample(c('A','B','C'), size=1, prob=c(0.6, 0.4-1e-2,1e-2 ))
      Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
    } 
    if(ii == 60){ 
      Item <- sample(c('B','C','D'), size=1, prob=c(0.6, 0.4-1e-2,1e-2 ))
      Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
    } 
    if(ii == 71){ 
      Item <- sample(c('A','B','C'), size=1, prob=c(0.8, 0.2-1e-2,1e-2 ))
      Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
    } 
    if(ii == 77){ 
      Item1 <- sample(c('A','B'), size=1, prob=c(1-1e-2,1e-2 ))
      Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item1,' ）'), Wenjuan[[j]][ii]) 
    } 
    if(ii == 85 ){ 
      if(Item1=='B'){
        Item <- sample(c('A','C','D'), size=1, prob=c(1/3, 1/3, 1/3))
        Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
      }
    } 
    if(ii == 94){ 
      Item <- sample(c('A','B'), size=1, prob=c(1-1e-2,1e-2 ))
      Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
    } 
    if(ii == 99){ 
      Item <- sample(c('B','C','D','E','F','H','G'), size=1, prob=c(1-6e-2,1e-2, 1e-2,1e-2,1e-2,1e-2,1e-2))
      Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
    } 
    if(ii == 109){ 
      Item <- sample(c('A','B','C'), size=1, prob=c(0.8, 0.2-1e-2,1e-2 ))
      Wenjuan[[j]][ii]<-sub('（  ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
    } 
    if(ii == 114){ 
      Item <- sample(c('ABC','ABCE'), size=1, prob=c(0.8,0.2))
      Wenjuan[[j]][ii]<-sub('（多选）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
    } 
    if(ii == 121){ 
      Item <- sample(c('A','B','C'), size=1, prob=c(0.8, 0.2-1e-2,1e-2 ))
      Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
    } 
    if(ii == 127){ 
      Item <- sample(c('A','B','C'), size=1, prob=c(0.8, 0.2-1e-2,1e-2 ))
      Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
    } 
    if(ii == 133){ 
      Item <- sample(c('A','B'), size=1, prob=c(0.8, 0.2))
      Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
    } 
    if(ii == 139){ 
      Item <- sample(c('A','B'), size=1, prob=c(0.8, 0.2))
      Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
    } 
    if(ii == 145){ 
      Item <- 'C'
      Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
    } 
    if(ii == 160){ 
      Item <- sample(c('A','B'), size=1, prob=c(0.9, 0.1))
      Wenjuan[[j]][ii]<-sub('（   ）',paste0('（ ',Item,' ）'), Wenjuan[[j]][ii]) 
    } 
    
  }
  #---------问卷内容填写完成后，写入到txt文件中，然后修改txt文件的扩展名为.doc
  write.table(Wenjuan[[j]],paste0('问卷',j,'.txt'), row.names=F, col.names=F,quote = F) #写入到.txt文件中，quote=F表示去掉引号
  file.rename(paste0('问卷',j,'.txt'),sub("txt","doc",paste0('问卷',j,'.txt'))) #修改扩展名
}