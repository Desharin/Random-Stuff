ids = as.character(read.csv("Day2_Puzzle1_input.csv", header = F, sep = "\n")[,1])
hastwos = searchFor(ids, 2)
hasthrees = searchFor(ids, 3)
sum(hastwos) * sum(hasthrees)
  
#Functions 
searchFor = function(input, n) {
  split_input = strsplit(input, split = "") # Split strings into individual characters
  contains = rep(F, length(split_input)) # Vector with whether each string contains n duplicates of a character
  for(i in 1:length(split_input)) { # For every string
    for(j in unique(split_input[[i]])) { # For every unique character
      if(sum(split_input[[i]] == j) == n) { # If this character occurs n times
        contains[i] = T # This string has n of a character
      }
    }
  }
  return(contains) # Return vector full of T/F
}
