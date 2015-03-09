
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
  network_state
}

MSE = function(input, target, network_state)
{
  global_error = 0.0
  for (i in 1:length(input))
  {
    global_error = global_error + abs(0.5 * (forward(network_state[[2]], input[[i]]) - target[[i]])^2)
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
