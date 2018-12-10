polymer = read.csv("Day5_Puzzle1_input.csv", header = F, sep = "", stringsAsFactors = F)

oldLength = nchar(polymer)
newLength = 0

while(!newLength == oldLength) {
  oldLength = newLength
  polymer = gsub("(.)(?!\\1)(?i:\\1)", "", polymer, perl = T)
  newLength = nchar(polymer)
}

nchar(polymer) # 9202