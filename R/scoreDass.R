#' Score DASS Scale
#'
#' I can be used to score a week's worth of DASS data that has been downloaded
#' from Qualtrics and either append it to a master.csv file or create a master.csv
#' file. If your working directory has a 'master' DASS file in it (e.g., masterCCTDass.csv)
#' where you are combining data from courses of the same class type but from different
#' locations, I will append this week to it and nest the data accordingly. If your working
#' directory does not have a masterCOURSENAMEDass.csv file in it, I will create that file
#' (to the name of your choosing) to start you off collecting data. I assume that there are
#' two character rows to remove below the header (i.e., rows 1 & 2). I assume that the
#' participant ID column is labeled 'ID' and that the DASS columns begin with 'DASS_.' I assume
#' that there are 16 columns to remove from the front of the Qualtrics CSV file.
#' Dependencies: dplyr and tidyr.
#'
#' @param currWk A .csv file for the current week to score that you downloaded from Qualtrics.
#' Use quotes.
#'
#' @param weekNum Of class character denoting the current week that you are going to score.
#' Use quotes.
#'
#' @param courseName The course name formatted as follows:
#' locationBeginningAndEndingMonthofCourseYear, where the location is a 3 letter
#' abbreviation and the beginning and ending months are shortened to 3 letters if
#' longer than 5 letters and do not include follow-up months. Use quotes. See example.
#'
#' @param masterFile A .csv file that includes computed DASS scores from all courses
#' of a specific type. If this is the first course of a specific class type that you
#' are scoring, I will create the file for you as long as you input a file name in
#' quotes with a .csv extension. Use quotes.
#'
#' @examples scoreDass(currWk = "SacCCTWeek5A.csv", weekNum = "5A",
#' courseName = "sacAprilJune2016", masterFile = "masterCCTDass.csv")
#'
#' @export

scoreDass <- function(currWk, weekNum, courseName, masterFile) {

        ##read in current week file
        currentWk <- read.csv(currWk,
                              header=T,
                              na.strings = c("", " ", "NA", "."),
                              stringsAsFactors=FALSE)

        ##drop first two rows and cols 1 thru 16
        currentWk <- currentWk[-c(1, 2), -c(1:16)]

        ##convert char to numeric, will convert char to na
        currentWk <- as.data.frame(lapply(currentWk, as.numeric))

        ## COMPUTE DASS ##
        #compute dass depression
        currentWk <- currentWk %>%
                mutate(dass_depression = DASS_3 + DASS_5 + DASS_10 + DASS_13 +
                               DASS_16 + DASS_17 + DASS_21)

        #compute dass anxiety
        currentWk <- currentWk %>%
                mutate(dass_anxiety = DASS_2 + DASS_4 + DASS_7 + DASS_9 +
                               DASS_15 + DASS_19 + DASS_20)

        #compute dass stress
        currentWk <- currentWk %>%
                mutate(dass_stress = DASS_1 + DASS_6 + DASS_8 + DASS_11 +
                               DASS_12 + DASS_14 + DASS_18)

        ##select computed vars
        currentWk <- select(currentWk, matches("ID"), matches("dass_depression"),
                            matches("dass_anxiety"), matches("dass_stress"))

        ##add week number NOTE: change week number to current week
        currentWk <- cbind(weekNumber = rep(weekNum, length(currentWk$ID)),
                           currentWk)

        ##add course name NOTE: change if needed
        currentWk <- cbind(course = rep(courseName,
                                        length(currentWk$ID)),
                           currentWk)

        ##convert to long format
        currentWk <- gather(currentWk, subscale, value, 4:6)

        ##convert weekNum to factor
        currentWk$weekNumber <- factor(currentWk$weekNumber)

        if (file.exists(masterFile)) {
                ##read in master csv to join
                masterDass <- read.csv(masterFile,
                                       header=T,
                                       na.strings = c("", " ", "NA", "."))

                ##join current to master
                joinCurrentToMaster <- rbind(masterDass, currentWk)

                ##order subscale levels and convert to factor
                joinCurrentToMaster$subscale <- factor(joinCurrentToMaster$subscale,
                                                       levels = c("dass_depression",
                                                                  "dass_anxiety",
                                                                  "dass_stress"))
                ##sort by course then subscale
                joinCurrentToMaster <- joinCurrentToMaster[order(joinCurrentToMaster$course,
                                                                 joinCurrentToMaster$ID,
                                                                 joinCurrentToMaster$weekNumber,
                                                                 joinCurrentToMaster$subscale), ]
                ##write to csv
                write.csv(joinCurrentToMaster, masterFile, row.names = F)

        } else {
                ##order subscale levels and convert to factor
                currentWk$subscale <- factor(currentWk$subscale,
                                             levels = c("dass_depression",
                                                        "dass_anxiety",
                                                        "dass_stress"))
                ##sort by id then subscale
                currentWk <- currentWk[order(currentWk$course,
                                             currentWk$ID,
                                             currentWk$weekNumber,
                                             currentWk$subscale), ]
                ##write to csv
                write.csv(currentWk, masterFile, row.names = F)
        }
}
