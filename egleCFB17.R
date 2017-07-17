newGame <- function(teamRating, oppRating, teamScore, oppScore, home) {
	# scenario where 'team' is underdog and wins - completed 5/19/17
	if (teamRating < oppRating & teamScore > oppScore) {
		line <- round((teamRating / oppRating) * 40, digits = 0)
		line <- 80 - line
		pointDif <- teamScore - oppScore
		if (home == TRUE) {	
			teamGain <- line + round((pointDif / 2), digits = 0)
		}
		else {
			teamGain <- line + round((pointDif / 2), digits = 0) + 5
		}
	}
	# scenario where 'team' is favored and loses - completed 5/19/17
	if (oppRating < teamRating & oppScore > teamScore) {
		line <- round((oppRating / teamRating) * 40, digits = 0)
		line <- 80 - line
		line <- line * -1
		pointDif <- teamScore - oppScore
		if (home == TRUE) {
			teamGain <- line + round((pointDif / 2), digits = 0) - 5
		}
		else {
			teamGain <- line + round((pointDif / 2), digits = 0)
		}
	}
	# scenario where 'team' is favored and wins - completed 5/20/17
	if (oppRating < teamRating & oppScore < teamScore) {
		line <- round((oppRating / teamRating) * 40, digits = 0)
		pointDif <- teamScore - oppScore
		if (home == TRUE) {
			teamGain <- line + round((pointDif / 2), digits = 0)
		}
		else {
			teamGain <- line + round((pointDif / 2), digits = 0) + 5
		}
	}
	# scenario where 'team' is underdog and loses - completed 5/20/17
	if (teamRating < oppRating & teamScore < oppRating) {
		line <- 0 - round((teamRating / oppRating) * 40, digits = 0)
		pointDif <- teamScore - oppScore
		teamGain <- line + round((pointDif / 2), digits = 0)
	}
	# scenario where 'team' is equal with opponent and wins - completed 5/20/17
	if (teamRating == oppRating & teamScore > oppScore)
		line <- round((teamRating / oppRating) * 40, digits = 0)
		pointDif <- teamScore - oppScore
		if (home == TRUE) {
			teamGain = line + round((pointDif / 2), digits = 0)
		}
		else {
			teamGain = line + round((pointDif / 2), digits = 0) + 5
		}
	}
	# scenario where 'team' is equal with opponent and loses - completed 5/20/17 - hasn't been working
	if (teamRating == oppRating & teamScore < oppScore)
		line <- round((teamRating / oppRating) * 40, digits = 0) * -1
		pointDif <- teamScore - oppScore
		teamGain <- line + round((pointDif / 2), digits = 0) + 5
	}
	return(teamGain)
}
# tests to make sure that this function works
UWRoad <- newGame(824, 1204, 30, 28, FALSE)
FWRoad <- newGame(1172, 899, 45, 31, FALSE)
ULRoad <- newGame(988, 1182, 10, 23, FALSE)
FLRoad <- newGame(1310, 1286, 24, 38, FALSE)
UWHome <- newGame(766, 937, 66, 10, TRUE)
FWHome <- newGame(883, 792, 30, 0, TRUE)
ULHome <- newGame(872, 1107, 7, 34, TRUE)
FLHome <- newGame(1311, 1041, 3, 35, TRUE)
EWHome <- newGame(1000, 1000, 16, 14, TRUE)
ELRoad <- newGame(1000, 1000, 13, 48, FALSE)
ELHome <- newGame(1000, 1000, 13, 19, TRUE)
EWRoad <- newGame(1000, 1000, 33, 24, FALSE)
print(UWRoad) # no error but wrong output
print(FWRoad)
print(ULRoad) # no error but wrong output
print(FLRoad) # no error but wrong output
print(UWHome) # no error but wrong output
print(FWHome)
print(ULHome)
print(FLHome)
print(EWHome)
print(ELRoad) # error
print(ELHome) # error
print(EWRoad)