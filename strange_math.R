multiplier = function(x)
{
  t=1
  for (i in x){t=t*i}
  return(t)
}

summer = function(x)
{
  t=0
  for (i in x){t=t+i}
  return(t)
}

replace_mutipl = function(from, to)
{
  result = matrix(0, nrow(from), ncol(from))
  tmp = result
  for (i in 1:nrow(from))
  {
    tmp = to
    tmp[i,] = from[i,]
    result[i,] = apply(tmp,2,multiplier)
  }
  return(result)
}

make_mu = function(mu, inp)
{
  new_mu = t(apply(mu, 1, summer))
  result = matrix(0, inp, ncol(new_mu))
  for (i in 1:inp)
  {
    result[i,] = new_mu[1,]
  }
  return(result)
}

make_uu = function(u, hid)
{
  result = matrix(0, ncol(u), hid)
  for (i in 1:hid)
  {
    result[,i] = u[1,]
  }
  return(result)
}

one_education_step = function(previous_pos, derivative, current_pos)
{
  ret_pos = list()
  for(name in names(previous_pos))
  {
    ret_pos[[name]] = current_pos[[name]] + derivative[[name]] * etta + (current_pos[[name]] - previous_pos[[name]]) * etta
  }
  ret_pos
}