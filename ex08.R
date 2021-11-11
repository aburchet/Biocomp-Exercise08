# Alice Burchett
# Exercise 08

rm(list=ls())# this clears the environment

# Question 1. PLot game score over time

data<-read.table(file="UWvMSU_1-22-13.txt", header=TRUE)

cumu_score<-function(x){
  # x should be a data.frame with scores in "score" col
  # returns a list of cumulative scores
  
  result<-numeric(dim(x)[1]) # Initialize output vector
  for (i in 1:length(result)){
    result[i]<-sum(x$score[1:i])
  } # End i loop
  return(result)
}

# separate into 2 new data.frames by team:
uw<-data[data$team=="UW",]
msu<-data[data$team=="MSU",]

# get the cumulative score for each team
uw_cumu<-cumu_score(uw)
msu_cumu<-cumu_score(msu)

# plot comulative score over time for each team
plot(uw$time, uw_cumu,type='l')
lines(msu$time,msu_cumu)


# Question 2: Number guessing game

number<-sample(100,1) # picks a random number between 1 and 100

for(i in 1:10){
  guess<-readline(prompt="Guess: ") # prompts and stores response
  if(guess>number){
    print("Lower")
  }else if(guess<number){
    print("Higher")
  }else{
    print("Correct!")
    break # this exits loop if guess is correct
  } # end if/else sequence
} # end i loop





