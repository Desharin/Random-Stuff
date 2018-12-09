claims_raw = as.character(read.csv("Day3_Puzzle1_input.csv", header = F, sep = "\n")[,1])
claims = data.frame(matrix(NA, nrow = length(claims_raw), ncol = 0))
regex_result = t(data.frame(regmatches(claims_raw, regexec("#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)", claims_raw, perl = T))))
claims$ID = as.numeric(regex_result[,2])
claims$from_left = as.numeric(regex_result[,3])
claims$from_top = as.numeric(regex_result[,4])
claims$width = as.numeric(regex_result[,5])
claims$heigth = as.numeric(regex_result[,6])

coordinates = data.frame(matrix(NA, ncol = 3, nrow = 0))
colnames(coordinates) = c("ID", "x", "y")

for(i in 1:nrow(claims)) {
  coordinates = rbind(
    coordinates, 
    data.frame(
      ID = claims$ID[i],
      x = rep((claims$from_left[i] + 1):(claims$from_left[i] + claims$width[i]), 
              times = claims$heigth[i]), 
      y = rep((claims$from_top[i] + 1):(claims$from_top[i] + claims$heigth[i]),
              each = claims$width[i])
    )
  )
}

coordinates = cbind(coordinates, duplicated(coordinates[,2:3], fromLast = T) | duplicated(coordinates[,2:3]))
colnames(coordinates) = c("ID", "x", "y", "duplicated")
claims$ID[!claims$ID %in% unique(coordinates$ID[coordinates$duplicated == T])] # 1019
