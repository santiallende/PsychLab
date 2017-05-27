#' Score Multi-dimensional Compassion Scale Scale
#'
#' This function can be used to score MCS data that has been downloaded
#' from Qualtrics (manually or via the API) and either append it to a <master.csv>
#' file or create a <master.csv> for you. A <master.csv> file is a file in long format that
#' contains multiple timepoints of scored MCS data (e.g., weeks 1-n). If manually
#' downloaded from Qualtrics, it will remove rows 1 and 2. It assumes that the
#' participant ID column is labeled 'ID' and that the MCS columns begin with 'MCS_.'
#' It assumes that there are 16 columns to remove from the front of the Qualtrics CSV file.
#'
#' @param currWk A .csv file with a timepoint worth of MCS data to score. Use quotes.
#'
#' @param weekNum Of class character denoting the current week to score. This will add ###################weekNum should be timepoint or something because not all people will collect on a weekly basis.
#' that character to a 'timepoint column' Use quotes.
#'
#' @param masterFile A .csv file that includes computed MCS scores from all scored timepoints.
#' If this is the first timepoint to score, it will create the master file to the name of
#' your choosing (e.g., masterMCS.csv). Use quotes and a .csv extension.
#'
#' @param dates Defaults to FALSE. Set to TRUE If you'd like the start and end
#' dates/times of survey completion for each participant(in 2 separate columns).
#'
#' @param QualtRics Defaults to FALSE. Set to TRUE if you accessed the data through
#' the QualtRics R package (Qualtrics API). This will read an object of class dataframe
#' and will not remove the first two rows of the dataframe.
#'
#' @examples scoreMcs(currWk = "SacCCTWeek4.csv", weekNum = 4, courseName =
#' "sacAprilJune2016", masterFile = "masterCCTMcs.csv", dates = FALSE, QualtRics = FALSE)
#'
#' @export
#' @importFrom dplyr select rowwise mutate left_join
#' @importFrom tidyr gather

scoreMcs <- function(currWk, weekNum, masterFile, dates = FALSE, QualtRics = FALSE) {

        #if not from API read in csv current week file
        if (QualtRics == FALSE) {
                currentWk <- readCsv(currWk)
        } else {
                currentWk <- currWk
        }

        ##drop first two rows if not from API
        if (QualtRics == FALSE) {
                currentWk <- currentWk[-c(1, 2), ]
        }

        if (dates == TRUE) {
                qDates <- select(currentWk, ID, startDate = StartDate, endDate = EndDate)
        }

        #converts API atomic ID var to character
        currentWk[, "ID"] <- as.character(currentWk[, "ID"])

        ## convert only mcs cols to numeric to keep id as character, not num
        mcs <- c("MCS_1", "MCS_2", "MCS_3", "MCS_4",
                 "MCS_5", "MCS_6", "MCS_7", "MCS_8",
                 "MCS_9", "MCS_10", "MCS_11", "MCS_12",
                 "MCS_13", "MCS_14", "MCS_15", "MCS_16")

        currentWk[, mcs] <- lapply(currentWk[, mcs], as.numeric)

        ##compute mcs 16
        #compute mcs_cognitive_mean
        currentWk <-  currentWk %>%
                rowwise() %>%
                mutate(mcs_cog_mean = (MCS_2 + MCS_5 + MCS_9 + MCS_13) / 4)

        #compute mcs_affective mean
        currentWk <- currentWk %>%
                rowwise() %>%
                mutate(mcs_aff_mean = (MCS_1 + MCS_6 + MCS_8 + MCS_11) / 4)

        #compute mcs_intentional mean
        currentWk <- currentWk %>%
                rowwise() %>%
                mutate(mcs_int_mean = (MCS_4 + MCS_10 + MCS_12 + MCS_15) / 4)

        #compute mcs_motivational_mean
        currentWk <- currentWk %>%
                rowwise() %>%
                mutate(mcs_mot_mean = (MCS_3 + MCS_7 + MCS_14 + MCS_16) / 4)

        #compute mcs total mean
        currentWk <- currentWk %>%
                rowwise() %>%
                mutate(mean_mcs = (MCS_1 + MCS_2 + MCS_3 + MCS_4 + MCS_5 +
                                           MCS_6 + MCS_7 + MCS_8 + MCS_9 +
                                           MCS_10 + MCS_11 + MCS_12 + MCS_13 +
                                           MCS_14 + MCS_15 + MCS_16) / 16)

        #select computed vars
        currentWk <- select(currentWk, ID, mcs_cog_mean,
                       mcs_aff_mean, mcs_int_mean,
                       mcs_mot_mean, mean_mcs)

        #mcs subscales for addDates function
        subscales <- c("mean_mcs", "mcs_cog_mean",
                          "mcs_aff_mean", "mcs_int_mean",
                          "mcs_mot_mean")

        #add list of dates==f and t for

        if (dates == TRUE) {

                ##add week number NOTE: change week number to current week
                currentWk <- cbind(timepoint = rep(weekNum, length(currentWk$ID)),
                                   currentWk)

                ##add date
                currentWk <- left_join(currentWk, qDates, by = "ID")

                ## reorder cols so ID, timepoint, date
                currentWk <- currentWk[c("ID", "timepoint", "startDate", "endDate",
                               "mcs_cog_mean", "mcs_aff_mean", "mcs_int_mean",
                               "mcs_mot_mean", "mean_mcs")]

                ##convert to long format
                currentWk <- gather(currentWk, subscale, value, 5:9)

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
                currentWk <- cbind(timepoint = rep(weekNum, length(currentWk$ID)),
                                   currentWk)

                ## reorder cols so ID, timepoint
                currentWk <- currentWk[c("ID", "timepoint", "mcs_cog_mean",
                               "mcs_aff_mean", "mcs_int_mean",
                               "mcs_mot_mean", "mean_mcs")]

                ##convert to long format
                currentWk <- gather(currentWk, subscale, value, 3:7)

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
                                                                         joinCurrentToMaster$subcale), ] #add start and end?

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
































