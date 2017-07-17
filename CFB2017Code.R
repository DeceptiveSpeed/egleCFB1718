install.packages("XML")
library(XML)
options(max.print = 99999)
## 
names <- c("Air Force", "Akron", "Alabama", "Alabama-Birmingham", "Appalachian State", "Arizona", "Arizona State", "Arkansas", "Arkansas State", "Army", "Auburn", "Ball State", "Baylor", "Boise State", "Boston College", "Bowling Green", "Buffalo", "Brigham Young", "California", "Coastal Carolina", "Fresno State", "UCLA", "Central Florida", "Central Michigan", "Charlotte", "Cincinnati", "Clemson", "Colorado", "Colorado State", "Connecticut", "Duke", "Eastern Michigan", "East Carolina", "Florida International", "Florida", "Florida Atlantic", "Florida State", "Georgia", "Georgia Southern", "Georgia State", "Georgia Tech", "Hawaii", "Houston", "Idaho", "Illinois", "Indiana", "Iowa", "Iowa State", "Kansas", "Kansas State", "Kent State", "Kentucky", "Louisiana State", "Louisiana Tech", "Louisiana-Lafayette", "Louisiana-Monroe", "Louisville", "Marshall", "Maryland", "Massachusetts", "Memphis", "Miami FL", "Miami OH", "Michigan", "Michigan State", "Middle Tennessee", "Minnesota", "Mississippi", "Mississippi State", "Missouri", "Navy", "Nebraska", "Nevada", "Nevada-Las Vegas", "New Mexico", "New Mexico State", "North Carolina", "North Carolina State", "North Texas", "Northern Illinois", "Northwestern", "Notre Dame", "Ohio", "Ohio State", "Oklahoma", "Oklahoma State", "Old Dominion", "Oregon", "Oregon State", "Penn State", "Pittsburgh", "Purdue", "Rice", "Rutgers", "San Diego State", "San Jose State", "South Alabama", "South Carolina", "South Florida", "Southern California", "Southern Methodist", "Stanford", "Syracuse", "Texas Christian", "Temple", "Tennessee", "Texas", "Texas A&M", "Texas State", "Texas Tech", "Texas-El Paso", "Texas-San Antonio", "Toledo", "Troy", "Tulane", "Tulsa", "Utah", "Utah State", "Vanderbilt", "Virginia", "Virginia Tech", "Wake Forest", "Washington", "Washington State", "West Virginia", "Western Kentucky", "Western Michigan", "Wisconsin", "Wyoming")
rating <- c(rep(1000, 129))
fbs <- data.frame(names, rating, sort.list(rating, decreasing = TRUE))
print(fbs)
#
score <- readHTMLTable("http://www.sports-reference.com/cfb/years/2015-schedule.html", as.data.frame = FALSE)[[1]]
score <- data.frame(score, stringsAsFactors = FALSE)
# score gets the full schedule & scores for the season from SportsReference
print(score[[6]])
#
print(score[5, 6])

print(fbs[6, 1])

containsNum <- function (name) {
  if (grepl(1, name) || grepl(2, name) || grepl(3, name) || grepl(4, name) || grepl(5, name) || grepl(6, name) || grepl(7, name) || grepl(8, name) || grepl(9, name) || grepl(0, name)) {
    return(TRUE)
  }
  return(FALSE)
}

i <- 1
s <- 1
repeat {
  repeat {
    searchTeam <- fbs[s, 1]
    teamName <- score[i, 6]
    searchTeam <- factor(searchTeam)
    print(paste(teamName, " ", searchTeam))
    if(containsNum(teamName)) {
      teamName <- substring(teamName, 6, nchar(teamName))
    }
    else {
      print(teamName)
    }
    #if(grepl(searchTeam, teamName) && (nchar(searchTeam) == nchar(teamName) || nchar(searchTeam) == nchar(teamName))) {
    #  print(paste("TRUE  ", searchTeam))
    #}
    #else {
    #  print("FALSE")
    #}
    if(i > nrow(fbs)) {
      break
    }
    s = s+1
  }
  if(i > 100) {
    break
  }
  i = i+1
}
