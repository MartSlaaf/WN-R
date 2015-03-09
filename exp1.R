import <- function(filename) source(paste(paste(getwd(),filename, sep='')))
#import('/new_UIR.R')
import('/main_script.R')
import('/education.R')

df = read.csv2('exp1.csv')
N = 3000
a = list(NULL)
for (i in 1:N)
{
  a[[i]] = matrix(df$'a'[[i]], nrow=1, ncol=1)
}

b = list(NULL)
for (i in 1:N)
{
  b[[i]] = matrix(df$b[[i]], nrow=1, ncol=1)
}
mses = 0
for(i in 1:1)
{
  nuka = initialization(1, 35, 1, c(0.03, 0.5))
  #nuka = initialization(1, 35, 1, list(c(0.03, 0.5)))
  
  nuka = train_for_number_of_epochs(a, b, nuka, 600)
  c = list(NULL)
  optuka = nuka[[1]][[which.min(nuka[[2]])]][[1]]
  print(min(nuka[[2]]))
  print(which.min(nuka[[2]]))
  print(optuka)
  
  testinp = list()
  for (i in 1:80)
  {
    testinp[[i]] = matrix((i-40)/10, nrow=1, ncol=1)
  }
  testoutp = list()
  for (i in 1:80)
  {
    testoutp[[i]] = matrix(forward(optuka, testinp[[i]]), nrow=1, ncol=1)
  }
  realoutp = list()
  for (i in 1:80)
  {
    realoutp[[i]] = matrix(sin(testinp[[i]]/3)*cos(2*testinp[[i]]), nrow=1, ncol=1)
  }
  mses = mses + MSE(testinp, realoutp, list(NULL, optuka))
}
print(mses)
plot(1,type='n',xlim=c(-4,4),ylim=c(-1,1),xlab='x', ylab='y')
lines(testinp,testoutp,type='l', col='red')
lines(testinp,realoutp,type='l', col='green')