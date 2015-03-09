mhat = function(t, s, tau)
{
  current = ((t-tau)/s)^2
  (1 - current) * exp(-current / 2)
}

morlet = function(t,s,tau)
{
  x = ((t-tau)/s)^2
  exp((-x^2)/2) * cos(5*x)
}

mhat_derivative = function(t, s, tau)
{
  
  current = ((t-tau)/s)^2
  ((current - 3) * exp(-current / 2)) / 2
}

morlet_derivative = function(t,s,tau)
{
  x = ((t-tau)/s)^2
  -5*exp((-x^2)/2)*sin(5*x)-x*exp((-x^2)/2)*cos(5*x)
}

morlet_dilations = function(f)
{
  return(0.8458 / f + 0.0005407)
}

mhat_dilations = function(f)
{
  return(abs(0.2282 / f - 0.001325))
}