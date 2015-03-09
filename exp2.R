import <- function(filename) source(paste(paste(getwd(),filename, sep='')))
import('/new_UIR.R')
#import('/main_script.R')
import('/education.R')

df = read.csv2('exp2.csv')
N = 3000
a = list(NULL)
for (i in 1:N)
{
  a[[i]] = matrix(c(df$'a'[[i]],df$'q'[[i]]), nrow=1, ncol=2)
}


b = list(NULL)
for (i in 1:N)
{
  b[[i]] = matrix(df$b[[i]], nrow=1, ncol=1)
}
mses = 0
for(i in 1:10)
{
  nuka = initialization(2, 45, 1, list(c(0.04, 0.09), c(0.2, 0.4)))
  #nuka = initialization(2, 35, 1, c(0.04, 0.4))
  
  nuka = train_for_number_of_epochs(a, b, nuka, 60)
  c = list(NULL)
  optuka = nuka[[1]][[which.min(nuka[[2]])]][[1]]
  print(min(nuka[[2]]))
  testinp = list()
  cortex = 1
  for (k in 1:16)
  {
    for (l in 1:16)
    {
      testinp[[cortex]] = matrix(c((k-8)/2.0,(l-8)/2.0), nrow=1, ncol=2)
      cortex = cortex + 1
    }
  }
  testoutp = list()
  for (i in 1:256)
  {
    testoutp[[i]] = matrix(forward(optuka, testinp[[i]]), nrow=1, ncol=1)
  }
  realoutp = list()
  for (i in 1:256)
  {
    realoutp[[i]] = matrix(sin(testinp[[i]][[1]]/3)*cos(2*testinp[[i]][[2]]), nrow=1, ncol=1)
  }
  mses = mses + MSE(testinp, realoutp, list(NULL, optuka))
}
print(mses)
ix = c()
iy = c()
o = c()
cortex = 1
for (cone in 1:16)
{
  for (icone in 1:16)
  {
    ix = c(ix, (cone-8)/2.0)
    iy = c(iy, (icone-8)/2.0)
    o = c(o, testoutp[[cortex]])
    cortex = cortex + 1
  }
}
ndf = data.frame(ix, iy, o)
write.table(ndf, 'result2.csv', append = FALSE, quote = TRUE, sep = ";",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = FALSE, qmethod = c("escape", "double"))
