import <- function(filename) source(paste(paste(getwd(),filename, sep='')))
import('/new_UIR.R')
#import('/main_script.R')
import('/education.R')

df = read.csv2('exp3.csv')
N = 1500
a = list(NULL)
for (i in 1:N)
{
  #a[[i]] = matrix(c(df$'a'[[i]],df$'b'[[i]],df$'c'[[i]],df$'d'[[i]],df$'e'[[i]],df$'f'[[i]],df$'g'[[i]],df$'h'[[i]],df$'i'[[i]],df$'j'[[i]]), nrow=1, ncol=10)
  a[[i]] = matrix(c(df$'f'[[i]],df$'g'[[i]],df$'h'[[i]],df$'i'[[i]],df$'j'[[i]]), nrow=1, ncol=5)
}


b = list(NULL)
for (i in 1:N)
{
  b[[i]] = matrix(df$'j'[[i+1]], nrow=1, ncol=1)
}
mses = list(0,0,0,0,0)
global_mses=list()
for(i in 1:1)
{
  nuka = initialization(5, 30, 1)
  nuka = train_for_number_of_epochs(a, b, nuka, 35)
  c = list(NULL)
  optuka = nuka[[1]][[which.min(nuka[[2]])]][[1]]
  print(min(nuka[[2]]))
  testinp_1 = list()
  testoutp_1 = list()
  realoutp = list()
  #inpmat = matrix(c(df$'a'[[1501]],df$'b'[[1501]],df$'c'[[1501]],df$'d'[[1501]],df$'e'[[1501]],df$'f'[[1501]],df$'g'[[1501]],df$'h'[[1501]],df$'i'[[1501]],df$'j'[[1501]]), nrow=1, ncol=10)
  inpmat = matrix(c(df$'f'[[1501]],df$'g'[[1501]],df$'h'[[1501]],df$'i'[[1501]],df$'j'[[1501]]), nrow=1, ncol=5)
  for (i in 1501:2001)
  {
    counter = i - 1500
    testinp_1[[counter]]= inpmat
    testoutp_1[[counter]] = matrix(forward(optuka, inpmat), nrow=1, ncol=1)
    #inpmat = matrix(c(df$'a'[[i+1]],df$'b'[[i+1]],df$'c'[[i+1]],df$'d'[[i+1]],df$'e'[[i+1]],df$'f'[[i+1]],df$'g'[[i+1]],df$'h'[[i+1]],df$'i'[[i+1]],df$'j'[[i+1]]), nrow=1, ncol=10)
    inpmat = matrix(c(df$'f'[[i+1]],df$'g'[[i+1]],df$'h'[[i+1]],df$'i'[[i+1]],df$'j'[[i+1]]), nrow=1, ncol=5)
    realoutp[[counter]] = matrix(df$'j'[[i+1]], nrow=1, ncol=1)
  }
  mses[[1]] = mses[[1]] + MSE(testinp_1, realoutp, list(NULL, optuka))
  testinp_2 = list()
  testoutp_2 = list()
  #inpmat = matrix(c(df$'a'[[1501]],df$'b'[[1501]],df$'c'[[1501]],df$'d'[[1501]],df$'e'[[1501]],df$'f'[[1501]],df$'g'[[1501]],df$'h'[[1501]],df$'i'[[1501]],df$'j'[[1501]]), nrow=1, ncol=10)
  inpmat = matrix(c(df$'f'[[1501]],df$'g'[[1501]],df$'h'[[1501]],df$'i'[[1501]],df$'j'[[1501]]), nrow=1, ncol=5)
  for (i in 1501:2001)
  {
    counter = i - 1500
    testinp_2[[counter]]= inpmat
    testoutp_2[[counter]] = matrix(forward(optuka, inpmat), nrow=1, ncol=1)
    #inpmat = matrix(c(df$'a'[[i+1]],df$'b'[[i+1]],df$'c'[[i+1]],df$'d'[[i+1]],df$'e'[[i+1]],df$'f'[[i+1]],df$'g'[[i+1]],df$'h'[[i+1]],df$'i'[[i+1]],df$'j'[[i+1]]), nrow=1, ncol=10)
    inpmat = matrix(c(df$'f'[[i+1]],df$'g'[[i+1]],df$'h'[[i+1]],df$'i'[[i+1]],df$'j'[[i+1]]), nrow=1, ncol=5)
    inpmat[[5]] = testoutp_2[[counter]]
  }
  mses[[2]] = mses[[2]] + MSE(testinp_2, realoutp, list(NULL, optuka))
  testinp_3 = list()
  testoutp_3 = list()
  #inpmat = matrix(c(df$'a'[[1501]],df$'b'[[1501]],df$'c'[[1501]],df$'d'[[1501]],df$'e'[[1501]],df$'f'[[1501]],df$'g'[[1501]],df$'h'[[1501]],df$'i'[[1501]],df$'j'[[1501]]), nrow=1, ncol=10)
  inpmat = matrix(c(df$'f'[[1501]],df$'g'[[1501]],df$'h'[[1501]],df$'i'[[1501]],df$'j'[[1501]]), nrow=1, ncol=5)
  for (i in 1501:2001)
  {
    counter = i - 1500
    testinp_3[[counter]]= inpmat
    testoutp_3[[counter]] = matrix(forward(optuka, inpmat), nrow=1, ncol=1)
    inpmat_tmp_9 = inpmat[[5]]
    #inpmat = matrix(c(df$'a'[[i+1]],df$'b'[[i+1]],df$'c'[[i+1]],df$'d'[[i+1]],df$'e'[[i+1]],df$'f'[[i+1]],df$'g'[[i+1]],df$'h'[[i+1]],df$'i'[[i+1]],df$'j'[[i+1]]), nrow=1, ncol=10)
    inpmat = matrix(c(df$'f'[[i+1]],df$'g'[[i+1]],df$'h'[[i+1]],df$'i'[[i+1]],df$'j'[[i+1]]), nrow=1, ncol=5)
    inpmat[[4]] = inpmat_tmp_9
    inpmat[[5]] = testoutp_3[[counter]]
  }
  mses[[3]] = mses[[3]] + MSE(testinp_3, realoutp, list(NULL, optuka))
}
print(mses)
plot(1,type='n',xlim=c(0,50),ylim=c(0.4,0.6),xlab='x', ylab='y')
lines(1:497,testoutp_1[c(5:501)],type='p', col='blue', pch=18)
lines(1:497,testoutp_2[c(5:501)],type='p', col='black', pch=19)
lines(1:497,testoutp_3[c(5:501)],type='p', col='magenta', pch=20)
lines(1:497,realoutp[c(1:497)],type='l', col='green')
