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
          } else{
            data[count, 2] = 1
            data[count, 3] = max(0, rnorm(1, mean = Mean.1, sd = SD.1))  
          }
        } else if(state == 2){
          k = runif(1)
          if(k < trans.B.to.Death){
            data[count, 2] = 3
          } else{
            data[count, 2] = 2
            data[count, 3] = max(0, rnorm(1, Mean.2, sd = SD.2))  
          }
        } else{
          data[count, 2] = 3
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


results = generate.hidden.markov.model.data.2.states.plus.death(N = 100, num.data.points = 5, Mean.1 = 700, SD.1 = 200, Mean.2 = 400, SD.2 = 200, trans.A.to.B = 0.3, trans.A.to.Death = 0.1, trans.B.to.Death = 0.2, init.proportion.in.A = 0.8)

results$X[which(results$X==0)]<- -9 #Kate set final state to this; doesn't appear to be necessary


statetable.msm(State, ID, data=results)

Q<-rbind(c(-.25, .25,  0),
         c(0,   -.25,  .25),
         c(0,     0,   1))

Q.emp<-crudeinits.msm(State~time, ID, data=results, qmatrix=Q)

statelist=list(hmmNorm(mean=mean(results$X[results$State==1]), sd=sd(results$X[results$State==1])),
               hmmNorm(mean=mean(results$X[results$State==2]), sd=sd(results$X[results$State==2])),
               hmmIdent(-9))
hiv.hmm<-msm(X~time, subject=ID, data=results, qmatrix=Q.emp, hmodel=statelist)

plot(hiv.hmm, legend.pos=c(3.5, 1)) #survival plots (survival=not entering a final absorbing state)
pmatrix.msm(hiv.hmm) #transition probability matricies

sojourn.msm(hiv.hmm) #mean sojourn times
pnext.msm(hiv.hmm) #probability that each state is next (don't understand this one)
totlos.msm(hiv.hmm) #total length of stay
hazard.msm(hiv.hmm) #if we include covariates

