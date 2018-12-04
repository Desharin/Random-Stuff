ids = as.character(read.csv("Day2_Puzzle1_input.csv", header = F, sep = "\n")[,1])
differences = adist(ids) # Table of differences between all strings
targetBoxes = which(differences == 1, arr.ind = T) # Index of strings between which one character is different
equalChars = strsplit(ids[targetBoxes[1]], split = "")[[1]] == strsplit(ids[targetBoxes[2]], split = "")[[1]] # Whether each character between these two strings is the same
paste(strsplit(ids[targetBoxes[1]], split = "")[[1]][equalChars], collapse = "") # Combine the same characters into a single string
# pazvmqbftrbeosiecxlghkwud
