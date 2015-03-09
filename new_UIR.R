import <- function(filename) source(paste(paste(getwd(),filename, sep='')))
import('/wavelets.R')
import('/strange_math.R')

mother_function = mhat
mother_derivative = mhat_derivative
dilations = mhat_dilations
forward = function(state, input)
{
  apply(mother_function(diag(c(input), ncol=ncol(input)) %*% state[['Omega']], state[['T']], state[['Lambda']]),2,multiplier) %*% state[['Mu']] + state[['Hi']]
}

backward = function(input, sample_output, real_output,  net_state)
{
  state = net_state[[2]]
  previous_state = net_state[[1]]
  derivatives = list()
  current_coefficient = diag(c(input), ncol=ncol(input)) %*% state[['Omega']]
  Z = mother_function(current_coefficient, state[['T']], state[['Lambda']])
  dZ = mother_derivative(current_coefficient, state[['T']], state[['Lambda']])
  Err = real_output - sample_output
  md = replace_mutipl(dZ, Z)
  qm = apply(Z,2,multiplier)
  mm = make_mu(state[['Mu']] %*% t(Err), nrow(state$'Omega'))
  uu = make_uu(input, ncol(state$'Omega'))
  
  derivatives$'Omega' = md * mm * uu / state$'Lambda'
  derivatives$'T' =  1 * md * mm * state$'T' / state$'Lambda'
  derivatives$'Lambda' = md * mm * (current_coefficient - state$'T') / state$'Lambda'
  derivatives$'Mu' = qm %*% Err
  derivatives$'Hi' = Err
  
  new_state = one_education_step(previous_state, derivatives, state)
  list(state, new_state)
}

initialization = function(input, hidden, output, freq='N')
{
  state = list()
  state[['Omega']] = matrix(runif(input * hidden, -1.0, 1.0), nrow=input, ncol=hidden)
  state[['Mu']] = matrix(runif(hidden * output, 0.0, 1.0), nrow=hidden, ncol=output)
  state[['T']] = matrix(runif(hidden * input, 0.0, 6.0), nrow=input, ncol=hidden)
  
  if (missing(freq))
  {
    state$'Lambda' = matrix(runif(hidden * input, 0.001, 1.0), nrow=input, ncol=hidden)
  }
  else
  {
    state$'Lambda' = matrix(0, nrow=input, ncol=hidden)
    for (i in 1:input)
    {
      state$'Lambda'[i,] = matrix(runif(hidden, dilations(freq[[i]][2]), dilations(freq[[i]][1])), nrow=1, ncol=hidden)
    }
  }
  
  state[['Hi']] = matrix(runif(output, -1.0, 1.0), nrow=1, ncol=output)
  circumstate = state
  list(circumstate, state)
}
