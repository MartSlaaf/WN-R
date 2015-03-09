import <- function(filename) source(paste(paste(getwd(),filename, sep='')))
import('/new_UIR.R')
#import('/main_script.R')
import('/education.R')

df = read.csv2('exp3.csv')
N = 1500
a = list(NULL)
for (i in 1:N)
{
  a[[i]] = matrix(c(df$'a'[[i]],df$'b'[[i]],df$'c'[[i]],df$'d'[[i]],df$'e'[[i]],df$'f'[[i]],df$'g'[[i]],df$'h'[[i]],df$'i'[[i]],df$'j'[[i]]), nrow=1, ncol=10)
}


b = list(NULL)
for (i in 1:N)
{
  b[[i]] = matrix(df$'a'[[i+1]], nrow=1, ncol=1)
}
mses = 0
for(i in 1:1)
{
  nuka = initialization(10, 19, 1)
  #nuka = initialization(2, 19, 1)
  nuka = train_for_number_of_epochs(a, b, nuka, 500)
  c = list(NULL)
  optuka = nuka[[1]][[which.min(nuka[[2]])]][[1]]
  print(min(nuka[[2]]))
  testinp = list()
  testoutp = list()
  realoutp = list()
  inpmat = matrix(c(df$'a'[[1501]],df$'b'[[1501]],df$'c'[[1501]],df$'d'[[1501]],df$'e'[[1501]],df$'f'[[1501]],df$'g'[[1501]],df$'h'[[1501]],df$'i'[[1501]],df$'j'[[1501]]), nrow=1, ncol=10)
  for (i in 1501:2001)
  {
    counter = i - 1500
    testinp[[counter]]= inpmat
    testoutp[[counter]] = matrix(forward(optuka, inpmat), nrow=1, ncol=1)
    for (q in 1:9)
    {
      inpmat[[q]] = inpmat[[q+1]]
    }
    inpmat[[10]] = testoutp[[counter]]
    realoutp[[counter]] = matrix(df$'a'[[i+1]], nrow=1, ncol=1)
  }
  print(testinp)
  mses = mses + MSE(testinp, realoutp, list(NULL, optuka))
}
print(mses)
plot(1,type='n',xlim=c(0,50),ylim=c(0,1),xlab='x', ylab='y')
lines(1:501,testoutp,type='l', col='red')
lines(1:501,realoutp,type='l', col='green')
