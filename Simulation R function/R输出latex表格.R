install.packages('xtable')
library(xtable)
## Load example dataset
data(tli)
mydf <- tli[1:20, ] # my data.frame to latex table
## Demonstrate data.frame
tli.table <- xtable(mydf, align=rep(" ", ncol(mydf) + 1)) # generate a space between each cell.
print(tli.table) # default is the three-line table.
xtable(mydf, auto=T)

## example 2
## Demonstrate data.frame with different digits in cells
mydf <- tli[1:20, ] # my data.frame to latex table
tli.table <- xtable(mydf)
display(tli.table)[c(2,6)] <- "f" # specify the column of 2 and 6 display as float form, others
                                 # are default as character form display
tbdigits <- function(nr, nc, digit=2){
  if(length(digit)!=nc & length(digit)!=1)
    stop('the length of digit must be 1 or equal to nc!')
  tb.mat <- matrix(digit, nr,nc)
  if(length(digit) == 1)
    digit <- rep(digit, nc)
  tb.mat <- matrix(digit, nr, nc, byrow = T)
  return(tb.mat)
}

tli.digits <- tbdigits(20,6, digit=c(0,0,0,0,0,3)) 
digits(tli.table) <- tli.digits # specify digits for each column
print(tli.table, include.rownames=T, include.colnames=T) # default include rownames and colnames


a <- matrix(rnorm(25), 5 ,5)
row.names(a) <- paste0('d=', 1:5)
colnames(a) <- paste0('q=', 1:5)
x <- xtable(a,auto=T) # We repeat empty string 6 times
display(x)[2:6] <- 'f'
digits(x) <- tbdigits(5,6, digit = 2)
x # print with a table environment with three-line table
print(x, hline.after=NULL, include.rownames=T, include.colnames=T) # drop hline


#
## Demonstration of longtable support.
## Remember to insert \usepackage{longtable} on your LaTeX preamble
x <- matrix(rnorm(1000), ncol = 10)
x.big <- xtable(x, label = 'tabbig',
                caption = 'Example of longtable spanning several pages')
print(x.big, tabular.environment = 'longtable', floating = FALSE)

#
## Demonstration of sidewaystable support.
## Remember to insert \usepackage{rotating} on your LaTeX preamble
x <- matrix(rnorm(10*5), 10, 5)
x.small <- xtable(x, label = 'tabsmall', caption = 'regular table env')
print(x.small)  # default, no longtable
print(x.small, floating.environment = 'sidewaystable')


library(tables)
mytable <- tabular( Heading(Flowers)*(Species ) ~ (n=1) + Format(digits=2)*
           (Sepal.Length + Sepal.Width)*(mean + sd), data=iris )
mytable
latex(mytable)
#
st <- data.frame(matrix(rnorm(2*5), 2,5),what = factor(c('y1','y2')))
row.names(st) <- paste0('y',1:2)
tabular(Heading(y)*what ~ Heading(st)*(X1+X2+X3+X4+X5)*Heading()*(identity), data=st)
#
Sex <- factor(sample(c("Male", "Female"), 100, rep=TRUE))
Status <- factor(sample(c("low", "medium", "high"), 100, rep=TRUE))
z <- rnorm(100)+5
fmt <- function(x) {
  s <- format(x, digits=2)
  even <- ((1:length(s)) %% 2) == 0
  s[even] <- sprintf("(%s)", s[even])
  s
}
tab <- tabular( Heading()*z*Sex*Heading(st)*(mean+sd)
                ~ Status )
latex(tab)

st <- data.frame(matrix(rnorm(2*5),2,5), station='T1', row.names = NULL, check.names=F)
st
mytable <- tabular(Heading()*(c('1','2'))~station*('1'+'2'+'3'+'4'+'5'),data=st)
mytable  