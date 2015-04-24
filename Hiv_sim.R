library(msm)

##' Function idea - generate data using a Markov model, where there are two states and death. A person can only transition forwards i.e. state A to B is ok, but state B to A not allowed. There is a measurable variable, with both states having a different Normal distribution for this variable. 
##' @param N - number of people
##' @param num.data.points - number of data points to generate for each person
##' @param Mean.1 - mean of the measurable variable for state A. Mean.2 is similar.
##' @param SD.1 - standard deviation for measurable variable when in state A. SD.2 is similar.
##' @param trans.A.to.B - probability of transitioning from state A to state B from one time point to another. trans.A.to.Death and trans.B.to.Death are similar.
##' @param init.proportion.in.A - proportion of people who begin in state A


generate.hidden.markov.model.data.2.states.plus.death <- function(N, num.data.points, Mean.1, SD.1, Mean.2, SD.2, trans.A.to.B, trans.A.to.Death, trans.B.to.Death, init.proportion.in.A){
  
  data = matrix(0, N * num.data.points, 4)
  count = 1
  for(i in 1 : N){
    if(i < round((init.proportion.in.A * N))){
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
          if(k < trans.A.to.B){
            data[count, 2] = 2
            data[count, 3] = max(0, rnorm(1, Mean.2, sd = SD.2))  
          } else if((k > trans.A.to.B) & (k < trans.A.to.B + trans.A.to.Death)){
            data[count, 2] = 3
            data[count, 3] = -9
          } else{
            data[count, 2] = 1
            data[count, 3] = max(0, rnorm(1, mean = Mean.1, sd = SD.1))  
          }
        } else if(state == 2){
          k = runif(1)
          if(k < trans.B.to.Death){
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


results = generate.hidden.markov.model.data.2.states.plus.death(N = 5000, num.data.points = 10, Mean.1 = 700, SD.1 = 200, Mean.2 = 400, SD.2 = 200, trans.A.to.B = 0.2, trans.A.to.Death = 0.1, trans.B.to.Death = 0.1, init.proportion.in.A = 0.7)


Q = rbind(c(0, 0.3, 0.1), 
          c(0, 0, 0.2), 
          c(0, 0, 0))

a = msm(X ~ time, subject=ID, data = results,
     qmatrix = Q,
     hmodel = list (hmmNorm(mean=800, sd=150), hmmNorm(mean=450, sd=200),
                    hmmIdent(-9)) )
print(a)
