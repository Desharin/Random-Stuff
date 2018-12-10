polymer_raw = read.csv("Day5_Puzzle1_input.csv", header = F, sep = "", stringsAsFactors = F)

letters_removed = data.frame(letters, length = 0, stringsAsFactors = F)

for(i in 1:length(letters)) {
  polymer = gsub(letters[i], "", polymer_raw)
  polymer = gsub(LETTERS[i], "", polymer)
  
  oldLength = nchar(polymer)
  newLength = 0
  
  while(!newLength == oldLength) {
    oldLength = newLength
    polymer = gsub("(.)(?!\\1)(?i:\\1)", "", polymer, perl = T)
    newLength = nchar(polymer)
  }
  
  letters_removed$length[letters_removed$letters == letters[i]] = nchar(polymer)
}

letters_removed$length[letters_removed$length == min(letters_removed$length)] # 6394