records_raw = as.character(read.csv("Day4_Puzzle1_input.csv", header = F, sep = "\n")[,1])
records_raw = records_raw[order(records_raw)]
regex_result = t(data.frame(regmatches(records_raw, regexec("[[]([0-9]+)-([0-9]+)-([0-9]+) ([0-9]+):([0-9]+)] (.*)", records_raw, perl = T))))
records = data.frame(matrix(NA, nrow = length(records_raw), ncol = 0))
records$year = as.numeric(regex_result[,2])
records$month = as.numeric(regex_result[,3])
records$day = as.numeric(regex_result[,4])
records$hour = as.numeric(regex_result[,5])
records$minute = as.numeric(regex_result[,6])
records$string = as.character(regex_result[,7])
records$guard = 0

guard_regex = regmatches(records$string, regexec("([0-9]+)", records$string, perl = T))

for(i in 1:length(guard_regex)) {
  if(identical(guard_regex[[i]], character(0))) {
    records$guard[i] = records$guard[i-1]
    next
  }
  records$guard[i] = as.numeric(guard_regex[[i]][1])
}

guards = data.frame(matrix(NA, ncol = 3, nrow = 0))

for(i in unique(records$guard)) {
  sleepingtimes = c()
  waketime = records$minute[records$guard == i & grepl("wake", records$string)]
  sleeptime = records$minute[records$guard == i & grepl("asleep", records$string)]
  if(identical(sleeptime, numeric(0))) {
    next
  }
  for(j in 1:length(sleeptime)) {
    sleepingtimes = c(sleepingtimes, sleeptime[j]:(waketime[j]-1))
  }
  freqTable = data.frame(sort(table(sleepingtimes), decreasing = T))
  
  guards = rbind(guards, c(i, as.numeric(as.character(freqTable[1,1])), as.numeric(as.character(freqTable[1,2]))))
}

colnames(guards) = c("ID", "sleepingtimes", "Freq")
guards$mostAsleep = guards$sleepingtimes * guards$Freq

guards$ID[guards$mostAsleep == max(guards$mostAsleep)] * guards$sleepingtimes[guards$mostAsleep == max(guards$mostAsleep)] # 80711



