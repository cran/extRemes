gpd.ret<- function(z, p)
  ## returns values based on the gpd mle parameters and lambda.
  ## z is a gpd model object
  ## rate = proportion of exceedances
  ## 365 assumes daily sampling
  ## p = return period in years
  { thres <- z$thres
    sig <- z$mle[1]
    xi <- z$mle[2]
    rate<- z$rate
    npy <- z$npy
    thres +(sig/xi)*((p*npy*rate)^xi-1)
    }
