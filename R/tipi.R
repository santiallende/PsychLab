#' Score Ten-Item Personality Inventory (TIPI)
#'
#' This function can be used to score TIPI data that has been downloaded
#' from Qualtrics (manually or via the API - e.g., QualtRics package) and either append it to a <master.csv>
#' file or create a <master.csv> for you. A <master.csv> file is a file in long format that
#' contains one or more timepoints of scored TIPI data (e.g., weeks 1-n). If manually
#' downloaded from Qualtrics, it will remove rows 1 and 2. It assumes that the
#' participant ID column is labeled 'ID' and that the TIPI columns begin with 'TIPI_' (e.g., TIPI_1)
#' It assumes that there are 16 columns to remove from the front of the Qualtrics .csv file
#' or dataframe (unless dates parameter is specified).
#'
#' @param tPoint A .csv file or API dataframe with a timepoint's worth of TIPI data to score. Use quotes.
#'
#' @param tPointNum Of class character denoting the current timepoint to score. This will add
#' that character to a 'timepoint column.' Use quotes.
#'
#' @param masterFile A .csv file that includes computed TIPI scores from all scored timepoints.
#' If this is the first timepoint to score, it will create the master file to the name of
#' your choosing (e.g., masterTIPI.csv). Use quotes and a .csv extension.
#'
#' @param dates Defaults to FALSE. Set to TRUE if you'd like the start and end
#' dates/times of survey completion for each participant (in 2 separate columns).
#'
#' @param QualtRics Defaults to FALSE. Set to TRUE if you accessed the data through
#' the QualtRics R package (Qualtrics API). This will read an object of class dataframe
#' and will not remove the first two rows of the dataframe.
#'
#' @param duplicates Defaults to NULL. This parameter will correct for when participants manage
#' to take the Qualtrics survey more than once (e.g., a participant took the baseline survey twice).
#' This code chunk will choose the survey with the lowest number of missing values, if there
#' is a tie in the number of missing values (e.g., both surveys have 1 missing value), it
#' will choose the survey with the earliest timestamp. NOTE: if you manually
#' download the .csv files from Qualtrics and open them in excel prior to scoring them,
#' excel will change the date format and this code chunk will not execute.
#' So, either do not open the files in excel after downloading them from qualtrics or
#' open them in excel and do the following... highlight all dates in the file,
#' right click > format cells > custom, and in the "Type" text field insert:
#' yyyy-mm-dd hh:mm:ss. This will convert the dates to the appropriate format. This is
#' not an issue if you access the data through the API via QualtRics.
#'
#' @param group This will add a column called groups and fill in the rows with
#' the name of the group (i.e., treatment group) that you are scoring. Use quotes.
#'
#' @examples scoreTIPI(
#' tPoint = "PreCourse_Survey.csv",
#' tPointNum = "4",
#' masterFile = "masterTIPI.csv",
#' dates = FALSE,
#' QualtRics = FALSE,
#' duplicates = FALSE,
#' group = "Tx_1A")
#'
#' @export
#' @importFrom dplyr select rowwise mutate left_join
#' @importFrom tidyr gather
#' @importFrom lubridate parse_date_time
#'
scoreTIPI <- function(tPoint, tPointNum, masterFile, dates = FALSE, QualtRics = FALSE, duplicates = FALSE, group = NULL) {

        ##if not from API read in csv current week file
        if (QualtRics == FALSE) {
                currentWk <- readCsv(tPoint)
        } else {
                currentWk <- tPoint
        }

        ##drop first two rows if not from API
        if (QualtRics == FALSE) {
                currentWk <- currentWk[-c(1, 2), ]
        }

        ##extract dates if true
        if (dates == TRUE) {
                qDates <- select(currentWk, ID, startDate = StartDate, endDate = EndDate)
        }

        ##converts API atomic ID var to character
        currentWk[, "ID"] <- as.character(currentWk[, "ID"])

        ##names of TIPI columns
        tipi <- c("TIPI_1", "TIPI_2", "TIPI_3", "TIPI_4",
                 "TIPI_5", "TIPI_6", "TIPI_7", "TIPI_8",
                 "TIPI_9", "TIPI_10")

        ##if duplicates T then do below and assign the df to currentWk #make this work after people open csv with excel - excel changes date format from
        #2016-01-05 02:58:02 to 8/2/2016  9:14:00 PM
        if (duplicates == TRUE) {
                currentWk$EndDate <- parse_date_time(currentWk$EndDate, orders = c("ymd HMS"))
                currentWk$naCount <- rowSums(is.na(currentWk[, tipi]))
                currentWk <- currentWk %>%
                        group_by(ID) %>% #group rows by id
                        arrange(., naCount, EndDate) %>% #sort by total NA in ascending, if tie then by date
                        filter(row_number() == 1) %>% #choose index 0/1 in that list
                        ungroup()
                currentWk <- select(currentWk, -c(EndDate, naCount))
        }

        ## convert only mcs cols to numeric to keep id as character, not num
        currentWk[, tipi] <- lapply(currentWk[, tipi], as.numeric)

        ##score TIPI 10-item
        ##Extraversion: 1, 6R; Agreeableness: 2R, 7; Conscientiousness; 3,
        ##8R; Emotional Stability: 4R, 9; Openness to Experiences: 5, 10R.

        #reverse: reserved, critical, disorganized, anxious, conventional)
        reverse <- c("TIPI_6", "TIPI_2", "TIPI_8", "TIPI_4", "TIPI_10")
        currentWk[, reverse] <- 8-currentWk[, reverse]

        #score extraversion
        currentWk <- currentWk %>%
                mutate(extraversion = (TIPI_1 + TIPI_6)/2)

        #score agreeableness
        currentWk <- currentWk %>%
                mutate(agreeableness = (TIPI_2 + TIPI_7)/2)

        #score conscientiousness
        currentWk <- currentWk %>%
                mutate(conscientiousness = (TIPI_3 + TIPI_8)/2)

        #score conscientiousness
        currentWk <- currentWk %>%
                mutate(emotionalStability = (TIPI_4 + TIPI_9)/2)

        #score conscientiousness
        currentWk <- currentWk %>%
                mutate(openToExperience = (TIPI_5 + TIPI_10)/2)

        #select computed vars
        currentWk <- select(currentWk, ID, extraversion, agreeableness,
                            conscientiousness, emotionalStability, openToExperience)

        #mcs subscales for addDates function
        subscales <- c("extraversion", "agreeableness",
                       "conscientiousness", "emotionalStability",
                       "openToExperience")

        #add list of dates==f and t for
        datesF <- c("ID", "timepoint", "extraversion", "agreeableness",
                    "conscientiousness", "emotionalStability",
                    "openToExperience")

        datesT <- c("ID", "timepoint", "startDate", "endDate",
                    "extraversion", "agreeableness",
                    "conscientiousness", "emotionalStability",
                    "openToExperience")

        if (dates == TRUE) {

                ##add week number NOTE: change week number to current week
                currentWk <- cbind(timepoint = rep(tPointNum, length(currentWk$ID)),
                                   currentWk)

                ##add date
                currentWk <- left_join(currentWk, qDates, by = "ID")

                ## reorder cols so ID, timepoint, date
                currentWk <- currentWk[c(datesT)]

                ##convert to long format
                currentWk <- gather(currentWk, subscale, value, 5:9)

                ##add group column as first col
                if (!is.null(group)) {
                        currentWk <- cbind(groups = rep(group, length(currentWk$ID)),
                                           currentWk)
                }

                if (file.exists(masterFile)) {
                        ##read in master csv to join
                        masterMcs <- readCsv(masterFile)

                        ##join current to master
                        joinCurrentToMaster <- rbind(masterMcs, currentWk)

                        ##order subscale levels and convert to factor
                        joinCurrentToMaster$subscale <- factor(joinCurrentToMaster$subscale,
                                                               levels = subscales)

                        ##sort by ID then subscale
                        joinCurrentToMaster <- joinCurrentToMaster[order(joinCurrentToMaster$ID,
                                                                         joinCurrentToMaster$timepoint,
                                                                         joinCurrentToMaster$subcale), ]

                        ##write to csv
                        write.csv(joinCurrentToMaster, masterFile, row.names = F)

                } else {
                        ##order subscale levels and convert to factor
                        currentWk$subscale <- factor(currentWk$subscale,
                                                     levels = subscales)
                        ##sort by ID then subscale
                        currentWk <- currentWk[order(currentWk$ID,
                                                     currentWk$timepoint,
                                                     currentWk$subscale), ]

                        ##write to csv
                        write.csv(currentWk, masterFile, row.names = F)
                }

        } else {

                ##add week number NOTE: change week number to current week
                currentWk <- cbind(timepoint = rep(tPointNum, length(currentWk$ID)),
                                   currentWk)

                ## reorder cols so ID, timepoint
                currentWk <- currentWk[c(datesF)]

                ##convert to long format
                currentWk <- gather(currentWk, subscale, value, 3:7)

                ##add group column as first col
                if (!is.null(group)) {
                        currentWk <- cbind(groups = rep(group, length(currentWk$ID)),
                                           currentWk)
                }

                if (file.exists(masterFile)) {
                        ##read in master csv to join
                        masterMcs <- readCsv(masterFile)

                        ##join current to master
                        joinCurrentToMaster <- rbind(masterMcs, currentWk)

                        ##order subscale levels and convert to factor
                        joinCurrentToMaster$subscale <- factor(joinCurrentToMaster$subscale,
                                                               levels = subscales)

                        ##sort by ID then subscale
                        joinCurrentToMaster <- joinCurrentToMaster[order(joinCurrentToMaster$ID,
                                                                         joinCurrentToMaster$timepoint,
                                                                         joinCurrentToMaster$subcale), ]

                        ##write to csv
                        write.csv(joinCurrentToMaster, masterFile, row.names = F)

                } else {
                        ##order subscale levels and convert to factor
                        currentWk$subscale <- factor(currentWk$subscale,
                                                     levels = subscales)
                        ##sort by ID then subscale
                        currentWk <- currentWk[order(currentWk$ID,
                                                     currentWk$timepoint,
                                                     currentWk$subscale), ]

                        ##write to csv
                        write.csv(currentWk, masterFile, row.names = F)
                }

        }
}
































