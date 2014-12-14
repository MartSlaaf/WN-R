etta = 0.0001

main_condition = list(NULL)

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


forward = function(state, input)
{
  mother_function(input %*% state[[1]], state[[4]], state[[3]]) %*% state[[2]] + state[[5]]
}

one_education_step = function(previous_pos, derivative, current_pos)
{
  ret_pos = list(NULL)
  ret_pos[[1]] = current_pos[[1]] + derivative[[1]] * etta + (current_pos[[1]] - previous_pos[[1]]) * etta
  ret_pos[[2]] = current_pos[[2]] + derivative[[2]] * etta + (current_pos[[2]] - previous_pos[[2]]) * etta
  ret_pos[[3]] = current_pos[[3]] + derivative[[3]] * etta + (current_pos[[3]] - previous_pos[[3]]) * etta
  ret_pos[[4]] = current_pos[[4]] + derivative[[4]] * etta + (current_pos[[4]] - previous_pos[[4]]) * etta
  ret_pos[[5]] = current_pos[[5]] + derivative[[5]] * etta + (current_pos[[5]] - previous_pos[[5]]) * etta
  ret_pos
}

backward = function(input, sample_output, real_output,  net_state)
{
  state = net_state[[2]]
  previous_state = net_state[[1]]
  derivatives = list(NULL)
  Z = mother_function(input %*% state[[1]] , state[[4]], state[[3]])
  dZ = morlet_derivative(input %*% state[[1]] , state[[4]], state[[3]])
  Err = real_output - sample_output
  
  derivatives[[5]] = Err
  derivatives[[2]] = t(Z) %*% Err
  derivatives[[1]] = t(input) %*% (t(state[[2]] %*% t(Err)) * dZ / state[[4]])
  derivatives[[3]] = -1 * (Err %*% t(state[[2]]) * (dZ / state[[4]]))
  derivatives[[4]] = -1 * dZ * ((input %*% state[[1]]) - state[[3]])/(state[[4]] * state[[4]])

  newstate = one_education_step(previous_state, derivatives, state)
  main_condition[[1]] = state
  main_condition[[2]] = newstate
  main_condition
}

initialization = function(input, hidden, output)
{
  state = list(NULL)
  state[[1]] = matrix(runif(input * hidden, -1.0, 1.0), nrow=input, ncol=hidden)
  state[[2]] = matrix(runif(hidden * output, -1.0, 1.0), nrow=hidden, ncol=output)
  state[[3]] = matrix(runif(hidden, -4.0, 4.0), nrow=1, ncol=hidden)
  state[[4]] = matrix(runif(hidden, 0.0001, 1.0), nrow=1, ncol=hidden)
  state[[5]] = matrix(runif(output, -1.0, 1.0), nrow=1, ncol=output)
  circumstate = state
  list(circumstate, state)
}

plot_the_thing = function(a, nuka)
{
  c = list(NULL)
  for (i in 1:length(a))
  {
    c[[i]] = matrix(forward(nuka[[2]], a[[i]]), nrow=1, ncol=1)
  }
  
  plot(a,c,'p')
}

train_one_epoch = function(input, target, network_state)
{
  for (i in 1:length(input))
  {
    out_cur = forward(network_state[[2]], input[[i]])
    network_state = backward(input[[i]], out_cur, target[[i]], network_state)
  }
  #plot_the_thing(input, network_state)
  network_state
}

MSE = function(input, target, network_state)
{
  global_error = 0.0
  for (i in 1:length(input))
  {
    global_error = global_error + abs(forward(network_state[[2]], input[[i]]) - target[[i]])
  }
  return (global_error / length(input))
}

train_for_number_of_epochs = function(input, target, network_state, count)
{
  states = c(NULL)
  deviants = c(NULL)
  for (i in 1:count)
  {
    network_state = train_one_epoch(input, target, network_state)
    deviant = MSE(input, target, network_state)
    states[[i]] = network_state
    deviants[[i]] = deviant
  }
  list(states,deviants)
}


mother_function = mhat
mother_Derivative = mhat_derivative



N = 3000
a = list(NULL)
for (i in 1:N)
{
  a[[i]] = matrix(runif(1, -4.0, 4.0), nrow=1, ncol=1)
}

b = list(NULL)
for (i in 1:N)
{
  b[[i]] = matrix(sin(a[[i]]/3)*cos(2*a[[i]]), nrow=1, ncol=1)
}

nuka = initialization(1, 19, 1)


nuka = train_for_number_of_epochs(a, b, nuka, 300)
print(nuka[[2]])
print(nuka[[2]][[which.min(nuka[[2]])]])
c = list(NULL)
optuka = nuka[[1]][[which.min(nuka[[2]])]][[1]]
print(optuka)
for (i in 1:N)
{
  c[[i]] = matrix(forward(optuka, a[[i]]), nrow=1, ncol=1)
}
plot(1,type='n',xlim=c(-4,4),ylim=c(-1,1),xlab='x', ylab='y')
lines(a,c,type='p', col='red')
lines(a,b,type='p', col='green')


