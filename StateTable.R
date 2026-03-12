state.table <- function(input.state){
  #Define state for output
  state <- input.state
  
  #Calculate the statewide dem and rep perc
  dem.perc <- round(100*(sum(pres$biden.votes[pres$state==input.state])/sum(pres$total.votes[pres$state==input.state])),1)
  rep.perc <- round(100*(sum(pres$trump.votes[pres$state==input.state])/sum(pres$total.votes[pres$state==input.state])),1)
  
  #Use and if/else statement to define the winner based on the relative size of dem and rep percent, which we just defined
  #No states are ties
  if(dem.perc>rep.perc){
    winner = "Biden"
  } else { 
    winner = "Trump"
  }
  
  #Define total votes
  total.votes <- sum(pres$total.votes[pres$state==input.state])
  
  #Make output dataframe
  out <- cbind.data.frame(state, winner, dem.perc, rep.perc, total.votes)
  
  names(out) <- c("State", "Winner", "Democratic Percent", "Republican Percent", "Total Votes")
  #Output is that table
  return(out)
}
