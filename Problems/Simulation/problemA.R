Simulate <- function(Alpha, Beta, n) {

  m_sum = 0
  sum = 0
  I_sum = 0
  Xn = 1
  for(i in 1:n)
  {
    #using the conditional probability
    last_xn = Xn
    total = 0
    repeat
    {
      if( Alpha[Xn] > 0.0 && runif(1) <= Alpha[Xn]/(1-total))
      {
        Xn = Xn + 1
        I_sum = I_sum + 1
        break
      }
      else
      {
        total = total + Alpha[Xn]
      }

      if(Beta[Xn] > 0.0 && runif(1) <= Beta[Xn]/(1-total))
      {
        Xn = Xn - 1
        I_sum = I_sum - 1
        break
      }
      else
      {
        total = total + Beta[Xn]
      }

    }

    m_sum = m_sum + Xn*last_xn
    sum = sum + Xn
  }
  avg = sum / n
  i_avg = I_sum / n
  n_1 = avg + i_avg
  m_sum = m_sum / n

  return(m_sum - (avg*n_1))
}
CalculateCov <- function(Pis, Alpha, Beta, n)
{
  two = 0
  for(i in 1:n)
  {
    two = two + i*(i+(Alpha[i] - Beta[i]))*Pis[i]
  }

  exn = 0
  for(i in 1:n)
  {
    exn = exn + i*Pis[i]
  }

  exni = 0
  for(i in 1:n)
  {
    exni = exni + (i + (Alpha[i]-Beta[i]))*Pis[i]
  }

  return(two - (exn*exni))
}

RunSimulation <- function()
{
  pijdef = matrix(rep(0,16), nrow = 4)
  pijdef
  pijdef[1,2] = 1
  pijdef[2,3] = 0.5
  pijdef[2,1] = 0.5
  pijdef[3,2] = 0.5
  pijdef[2,3] = 0.5
  pijdef[3,4] = 0.5
  pijdef[4,3] = 1
  pijdef
  c = mc(pijdef)
  stn(c)
  pis = stn(c)
  Alphas = c(1,0.5,0.5,0)
  Betas = c(0,0.5,0.5,1)
  simulation_size = 1000000

  math_value = CalculateCov(pis, Alphas, Betas, length(pis))
  sim_value = Simulate(Alphas, Betas, simulation_size)

  print("Mathematical Value")
  print(math_value)
  print("Simulation Value")
  print(sim_value)
  print("Difference")
  print(abs(math_value-sim_value))
}



