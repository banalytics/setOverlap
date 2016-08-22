pageviews <- as.data.frame(matrix(ncol = 7, nrow = 100000))
#Need to figure out,  whether we will have a column for no experience or whether this always gets calculated
names(pageviews) <- c("experienceOne", "experienceTwo", "noExperience", "adblockOn", "adblockOff", "sourceSocial", "sourceOther")
#Experiences will be in a 1:1:8 ratio for experience1:experience2:noexperience
pageviews[, "experienceOne"] <- FALSE
pageviews[sample(rownames(pageviews), size = round(nrow(pageviews) / 10)), "experienceOne"] <- TRUE
pageviews[, "experienceTwo"] <- FALSE
pageviews[sample(rownames(pageviews[pageviews[, "experienceOne"] == FALSE, ]), size = round(nrow(pageviews) / 10)), "experienceTwo"] <- TRUE
pageviews[, "noExperience"] <- FALSE
pageviews[pageviews[, "experienceOne"] == FALSE & pageviews[, "experienceTwo"] == FALSE, "noExperience"] <- TRUE
#We expect 20 % of users to have adblock,  let's assume experienceOne is for adblock users on a partilcular content type
#This means 10 % of users will have experienceOne == TRUE while the remaining 10 % adblockers (on all users) will be randomly distributed
pageviews[, "adblockOn"] <- FALSE
pageviews[pageviews[, "experienceOne"] == TRUE, "adblockOn"] <- TRUE
pageviews[sample(rownames(pageviews[pageviews[,"experienceOne"] == FALSE, ]), size = round(nrow(pageviews) / 10)), "adblockOn"] <- TRUE
pageviews[, "adblockOff"] <- FALSE
pageviews[pageviews[, "adblockOn"] == FALSE, "adblockOff"] <- TRUE
#We randomly assign 20 % of traffic as social and rest as other
pageviews[, "sourceSocial"] <- FALSE
pageviews[sample(rownames(pageviews), size = round(nrow(pageviews) / 5)), "sourceSocial"] <- TRUE
pageviews[, "sourceOther"] <- FALSE
pageviews[pageviews[, "sourceSocial"] == FALSE, "sourceOther"] <- TRUE
