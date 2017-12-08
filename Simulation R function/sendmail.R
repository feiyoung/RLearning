install.packages('sendmailR')
library(sendmailR)
from <- sprintf("<sendmailR@\\%s>", Sys.info()[4])
to <- "<1029725413@qq.com>"
subject <- "Hello from R"
body <- list("It works!", mime_part(iris))
sendmail(from, to, subject, body,
         control=list(smtpServer="ASPMX.L.GOOGLE.COM"))
sendmail(from, to, subject, msg = msg,control=list(smtpServer="smtp-gw1.wal-mart.com"),headers=list("Content-Type"="text/html; charset=UTF-8; format=flowed"))

library(mailR)  
#body??????  
body = "this is a test"  
#?????????  
#recipients <- "215020208038@2015.swufe.edu.cn" 
recipients <-"1029725413@qq.com"
#????????? 
#sender = 'weiliu321@yahoo.com'
sender = "1029725413@qq.com"   # 授权码登陆第三方:leggapxdhsckbech
#sender = 'weidliu321@163.com' # 授权码：ldw428369d
#??????  
title = "this is title"  
#??????????????????code??????body???html?????????body=html???????????????????????????????????????html????????????  
send.mail(  
   from = sender,  
    to = recipients,  
   subject = title,  
    body ='html',  
   encoding = "utf-8",  
    html = TRUE,  
    smtp = list(  
        host.name = "smtp.qq.com",   
        port = 465,  
        user.name = sender,  
        #passwd = "ldw438369", 
        passwd = 'leggapxdhsckbech',
        ssl = TRUE  
     ),  
    authenticate = TRUE,  
    send = TRUE  
  )  

library(mailR)
sender = 'weidliu321@163.com'
recipients <- c("weidliu321@163.com")
send.mail(from = sender,
          to = recipients,
          subject = "Subject of the email",
          body = "Body of the email",
          smtp = list(host.name = "smtp.163.com", port = 465, 
                      user.name = sender,            
                      passwd = "ldw438369", ssl = TRUE),
          authenticate = TRUE,
          send = TRUE)
