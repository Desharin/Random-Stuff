f_changes = read.csv("Day1_Puzzle1_input.csv", header = F, sep = "\n")[,1]
f = 0
f_values = c(0)
completed = F
while(!completed) {
  for(i in f_changes) {
    f = f + i
    if(f %in% f_values) {
      print(f) #66105
      completed = T
      break
    }
    f_values = append(f_values, f)
  }
}