setwd('J:\\论文方面\\常用模拟代码')
# My sendmail function ----------------------------------------------------
mySendmail <- function(Path, ...){
  ## This R function send a email with attachments to my 163 mailbox.
  # INPUT Arguments:
  # Path: a character string denotes file's direction. 0 indicates the work direction.
  # ...: any number of files' names in the Path direction.
  attach.name <- list(...)
  if(Path == 0) Path <- getwd()
  require(mailR)  
  sender = 'weidliu321@163.com' # 授权码：ldw438369d
  recipients <- 'weidliu321@163.com'
  title = "程序运行情况汇报"  
  body = "我是R机器猫，现在向主人汇报程序运行已结束，运行
  结果以附件形式发送给您！"   # 邮件正文部分
  n <- length(attach.name)
  attchments <- sapply(1:n, function(i) paste(Path, attach.name[[i]], sep='/'))
  send.mail(  
    from = sender,  
    to = recipients,  
    subject = title,  
    body =body,  
    encoding = "utf-8",  
    html = TRUE,  
    smtp = list(  
      host.name = "smtp.163.com",   
      port = 465,  
      user.name = sender,  
      passwd = 'ldw438369d',
      ssl = TRUE  
    ),  
    authenticate = TRUE,  
    send = TRUE,
    attach.files = attchments # add attachment
  )  
}
# examples
mySendmail(0, 'all.Rdata', "R语言实现自动发送邮件.txt", "multivariate.R")



