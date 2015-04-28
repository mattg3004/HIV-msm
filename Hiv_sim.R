library(msm)

##' Function idea - generate data using a Markov model, where there are two states and death. A person can only transition forwards i.e. state 1 to 2 is ok, but state 2 to 1 not allowed. There is a measurable variable, with both states having a different Normal distribution for this variable. 
##' @param N - number of people
##' @param num.data.points - number of data points to generate for each person
##' @param Mean.1 - mean of the measurable variable for state 1. Mean.2 is similar.
##' @param SD.1 - standard deviation for measurable variable when in state 1. SD.2 is similar.
##' @param trans.1.to.2 - probability of transitioning from state 1 to state 2 from one time point to another. trans.1.to.Death and trans.2.to.Death are similar.
##' @param init.proportion.in.1 - proportion of people who begin in state 1


generate.hidden.markov.model.data.2.states.plus.death <- function(N, num.data.points, Mean.1, SD.1, Mean.2, SD.2, trans.1.to.2, trans.1.to.Death, trans.2.to.Death, init.proportion.in.1){
  
  data = matrix(0, N * num.data.points, 4)
  count = 1
  for(i in 1 : N){
    if(i < round((init.proportion.in.1 * N))){
      data[count, 2] = 1
    } else {data[count, 2] = 2}
    
    for(j in 1 : num.data.points){
      data[count, 1] = i
      
      if(j == 1){
        state = data[count, 2]
        if(state == 1){
          data[count, 3] = max(0, rnorm(1, mean = Mean.1, sd = SD.1))
        } else{data[count, 3] = max(0, rnorm(1, Mean.2, sd = SD.2))}
      } else{
        state = data[count - 1, 2]
        if(state == 1){
          k = runif(1)
          if(k < trans.1.to.2){
            data[count, 2] = 2
            data[count, 3] = max(0, rnorm(1, Mean.2, sd = SD.2))  
          } else if((k > trans.1.to.2) & (k < trans.1.to.2 + trans.1.to.Death)){
            data[count, 2] = 3
            data[count, 3] = -9
          } else{
            data[count, 2] = 1
            data[count, 3] = max(0, rnorm(1, mean = Mean.1, sd = SD.1))  
          }
        } else if(state == 2){
          k = runif(1)
          if(k < trans.2.to.Death){
            data[count, 2] = 3
            data[count, 3] = -9
          } else{
            data[count, 2] = 2
            data[count, 3] = max(0, rnorm(1, Mean.2, sd = SD.2))  
          }
        } else{
          data[count, 2] = 3
          data[count, 3] = -9
        }
        
      }
      data[count, 4] = j
      count = count + 1
    }
  }
  data = data.frame(data)
  colnames(data) = c("ID", "State", "X", "time")
  return(data)
}


results = generate.hidden.markov.model.data.2.states.plus.death(N = 1000, num.data.points = 5, Mean.1 = 700, SD.1 = 200, Mean.2 = 400, SD.2 = 200, trans.1.to.2 = 0.3, trans.1.to.Death = 0.2, trans.2.to.Death = 0.3, init.proportion.in.1 = 1)

q12 = 0.3
q13 = 0.2
q23 = 0.3
Q = rbind(c(-(q12 + q13), q12, q13), 
          c(0, -q23, q23), 
          c(0, 0, 0))

Q.emp<-crudeinits.msm(State~time, ID, data=results, qmatrix=Q)

hiv.hmm = msm(X ~ time, subject=ID, data = results,
              qmatrix = Q,
              hmodel = list (hmmNorm(mean=700, sd = 250), hmmNorm(mean=400, sd=200),
                             hmmIdent(-9)) )


     
plot(hiv.hmm, legend.pos=c(3.5, 1)) #survival plots (survival=not entering a final absorbing state)
pmatrix.msm(hiv.hmm) #transition probability matricies

sojourn.msm(hiv.hmm) #mean sojourn times
pnext.msm(hiv.hmm) #probability that each state is next (don't understand this one)
totlos.msm(hiv.hmm) #total length of stay
hazard.msm(hiv.hmm) #if we include covariates

