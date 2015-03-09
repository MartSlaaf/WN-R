import <- function(filename) source(paste(paste(getwd(),filename, sep='')))
import('/new_UIR.R')
import('/education.R')


s = initialization(2,5,1)
inp = matrix(runif(2, -4.0, 4.0), nrow=1, ncol=2)
N = 3000
a = list(NULL)
for (i in 1:N)
{
  a[[i]] = matrix(runif(1, -4.0, 4.0), nrow=1, ncol=2)
}

b = list(NULL)
for (i in 1:N)
{
  b[[i]] = matrix(sin(a[[i]][[1]]/3)*cos(2*a[[i]][[2]]), nrow=1, ncol=1)
}

nuka = train_for_number_of_epochs(a, b, s, 30)
print(nuka)
