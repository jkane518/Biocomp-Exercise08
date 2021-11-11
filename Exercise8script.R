#############Exercise 8###########
#John Kane

############Q1##################
#Make a plot that depicts the cumulative score for two teams over time
#Upload the data. File is tab delimited. 
results<-read.table("UWvMSU_1-22-13.txt", header = TRUE, sep = "\t")

#Make a matrix. We want to have 3 columns: 1 for time, 1 for UW score, and 1 for MSU score
#We want 51 rows. Our original "results" file has 50 rows, but we want to start from 0
#so we need an extra row
scores<-matrix(0,nrow=51,ncol=3)
colnames(scores)<-c("Time", "UW Score","MSU Score")


#We want a for loop to run through each row of "results"
#We need an if else statement to separate when UW scores from when MSU scores
for (i in 1:nrow(results)){
  if(results$team[i]=="UW"){
    #Since we want a cumulative score, we are adding the number of points in 
    #this row to the sum of the previous row (which is sum of previous rows)
    scores[(i+1),2]<-scores[i,2]+results[i,3]
    #This line keeps the MSU score from the previous line the same
    scores[(i+1),3]<-scores[i,3]
    #Adds the time to column 1
    scores[i+1,1]<-results[i,1]
  }else{
    scores[(i+1),3]<-scores[i,3]+results[i,3]
    scores[(i+1),2]<-scores[i,2]
    scores[i+1,1]<-results[i,1]
  }
}#end i loop

#plot data with line graph
#Plot the scores from UW in red
plot(scores[,1],scores[,2], col="firebrick1", type="l",
     xlab = "Time", ylab = "Score", ylim = c(0,50),)
legend('bottomright',c('UW','MSU'),lwd=1,col=c('firebrick1','forestgreen'),box.lty=0)
#Add the second line with scores from MSU in green
lines(scores[,1],scores[,3], col="forestgreen")



#########Q2 guess my number##########
#Use sample function to generate random number. We want to select 1 number between 1:100
randNumber<-sample(100,1)

#we want a for loop between 1:10 because we get up to 10 guesses
#There are 3 options: They guess correctly, they are too high, or too low
for (i in 1:10){
  guess<-readline(prompt="Guess a number 1-100: ")
  if(guess==randNumber){
    print("Correct!")
  }else if(guess>randNumber){
    print("Too High")
  }else if(guess<randNumber){
    print("Too Low")
  }
}

#This function will automatically exit after 10 attempts are reached.
