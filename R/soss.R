#' Read and recode Michigan State of the State (SOSS) data
#'
#' @param waves vector: a numeric vector containing the SOSS waves to include (currently available: 79, 82, 84, 85, 86)
#' @param extra.vars vector: a character vector containing the names of variables to be retained from the raw data
#' @param progress boolean: display a progress bar
#'
#' @details
#' The \href{http://ippsr.msu.edu/survey-research/state-state-survey-soss}{State of the State Survey} (SOSS) is
#'    regularly collected by the Institute for Public Policy and Social Research (IPPSR) at Michigan State
#'    University (MSU). Each wave is collected from a sample of 1000 adults in the US state of Michigan, and
#'    includes sampling weights to obtain a sample that is representative of the state's population with respect
#'    to age, gender, race, and education. The `soss()` function reads the raw data from IPPSR's website, extracts
#'    and recodes selected variables useful for studying childfree adults and other family statuses, then returns a
#'    single data frame. Questions necessary for identifying childfree adults were asked in five waves, which each
#'    include unique questions that may be of interest:
#'    * \href{http://ippsr.msu.edu/survey-research/state-state-survey-soss/soss-data/soss-79b-spring-2020}{Wave 79} (May 2020) - Neighborhoods, Health care, COVID, Personality
#'    * \href{http://ippsr.msu.edu/survey-research/state-state-survey-soss/soss-data/soss-82-fall-2021}{Wave 82} (September 2021) - Trust in government, Critical Race Theory
#'    * \href{http://ippsr.msu.edu/survey-research/state-state-survey-soss/soss-data/soss-84-spring-2022}{Wave 84} (April 2022) - Trust in scientists, Autonomous vehicles, Morality
#'    * \href{http://ippsr.msu.edu/survey-research/state-state-survey-soss/soss-data/soss-85-fall-2022}{Wave 85} (September 2022) - Reproductive rights, Race equity
#'    * \href{http://ippsr.msu.edu/survey-research/state-state-survey-soss/soss-data/soss-86-winter-2022}{Wave 86} (December 2022) - Education, Infrastructure
#'
#' **Weights**
#'
#' The \href{https://cran.r-project.org/web/packages/survey/index.html}{`survey`} package can be used to incorporate sampling weights
#'    and obtain population-representative estimates by wave. After using `soss()` to obtain data for a given wave (see example below), use
#'    `dat <- svydesign(data = dat, ids = ~1, weights = ~weight)` to incorporate information about the survey design.
#'
#' **Known issues**
#'   * Wave 79 did not include a "do not know" option for selected questions. Therefore, it is not possible to identify
#'     "undecided" or "ambivalent non-parent" respondents. This may lead other family status categories to be inflated.
#'   * Wave 82 originally included a 500 person oversample of parents. These respondents are omitted if `wave == 82`.
#'
#' @return A data frame containing variables described in the codebook available using \code{vignette("codebooks")}
#'
#' @export
#'
#' @examples
#' data <- soss(waves = 84, extra.vars = c("neal1"))
soss <- function(waves, extra.vars = NULL, progress = TRUE) {

  if (!all(waves %in%c(79,82,84,85,86))) {stop("Only the following SOSS waves are available: 79, 82, 84, 85, 86")}  #Check for valid waves
  waves <- sort(waves)  #Put waves in order

  if (!is.null(extra.vars)) {extra.vars <- tolower(extra.vars)}  #Make requested extra variables lowercase

  if (progress) {message("Processing SOSS data files -")}
  if (progress) {pb <- utils::txtProgressBar(min = 0, max = length(waves), initial = 0, style = 3)} #Initialize progress bar
  wave.num <- 1

  #Loop over each supplied data file
  for (wave in waves) {

    #Increment progress bar
    if (progress) {utils::setTxtProgressBar(pb,wave.num)}

    #Import raw data
    if (wave==79) {dat <- rio::import("http://ippsr.msu.edu/sites/default/files/soss79b.sav")}
    if (wave==82) {dat <- rio::import("https://ippsr.msu.edu/sites/default/files/soss/soss82.sav")}
    if (wave==84) {dat <- rio::import("http://ippsr.msu.edu/sites/default/files/SOSS%2084%20WEIGHTED%20DATASET_5.4.22.sav")}
    if (wave==85) {dat <- rio::import("http://ippsr.msu.edu/sites/default/files/SOSS%2085_WEIGHTED_OUTPUT.sav")}
    if (wave==86) {dat <- rio::import("http://ippsr.msu.edu/sites/default/files/SOSS86_weighted_OUTPUT.sav")}
    colnames(dat) <- tolower(colnames(dat))  #Make all variables lowercase

    #### Family Status ####
    #Do you have children?
    if (wave==79) {
      dat$havekid <- NA
      dat$havekid[which(dat$neal01==1)] <- 1  #Yes
      dat$havekid[which(dat$neal01==2)] <- 2  #No (don't know was not an option)
    }
    if (wave==82) {
      dat$havekid <- NA
      dat$havekid[which(dat$neal1==1)] <- 1  #Yes
      dat$havekid[which(dat$neal1==2)] <- 2  #No
      dat$havekid[which(dat$neal1==6)] <- 3  #Don't know
    }
    if (wave==84 | wave==85) {
      dat$havekid <- NA
      dat$havekid[which(dat$neal1==1)] <- 1  #Yes
      dat$havekid[which(dat$neal1==2)] <- 2  #No
      dat$havekid[which(dat$neal1==7)] <- 3  #Don't know
    }
    if (wave==86) {
      dat$havekid <- NA
      dat$havekid[which(dat$neal1==1)] <- 1  #Yes
      dat$havekid[which(dat$neal1==2)] <- 2  #No
      dat$havekid[which(dat$neal1==3)] <- 3  #Don't know
    }
    dat$havekid <- factor(dat$havekid, levels = c(1,2,3), labels = c("Yes", "No", "DK"))

    #Are you planning to have children?
    if (wave==79) {
      dat$plankid <- NA
      dat$plankid[which(dat$neal02==1)] <- 1  #Yes
      dat$plankid[which(dat$neal02==2)] <- 2  #No (don't know was not an option)
    }
    if (wave==82) {
      dat$plankid <- NA
      dat$plankid[which(dat$neal2==1)] <- 1  #Yes
      dat$plankid[which(dat$neal2==2)] <- 2  #No
      dat$plankid[which(dat$neal2==6)] <- 3  #Don't know
    }
    if (wave==84 | wave==85) {
      dat$plankid <- NA
      dat$plankid[which(dat$neal2==1)] <- 1  #Yes
      dat$plankid[which(dat$neal2==2)] <- 2  #No
      dat$plankid[which(dat$neal2==7)] <- 3  #Don't know
    }
    if (wave==86) {
      dat$plankid <- NA
      dat$plankid[which(dat$neal2==1)] <- 1  #Yes
      dat$plankid[which(dat$neal2==2)] <- 2  #No
      dat$plankid[which(dat$neal2==3)] <- 3  #Don't know
    }
    dat$plankid <- factor(dat$plankid, levels = c(1,2,3), labels = c("Yes", "No", "DK"))

    #Do you wish you could have had children?
    if (wave==79) {
      dat$wishkid <- NA
      dat$wishkid[which(dat$neal03==1)] <- 1  #Yes
      dat$wishkid[which(dat$neal03==2)] <- 2  #No (don't know was not an option)
    }
    if (wave==82) {
      dat$wishkid <- NA
      dat$wishkid[which(dat$neal3==1)] <- 1  #Yes
      dat$wishkid[which(dat$neal3==2)] <- 2  #No
      dat$wishkid[which(dat$neal3==6)] <- 3  #Don't know
    }
    if (wave==84 | wave==85) {
      dat$wishkid <- NA
      dat$wishkid[which(dat$neal3==1)] <- 1  #Yes
      dat$wishkid[which(dat$neal3==2)] <- 2  #No
      dat$wishkid[which(dat$neal3==7)] <- 3  #Don't know
    }
    if (wave==86) {
      dat$wishkid <- NA
      dat$wishkid[which(dat$neal3==1)] <- 1  #Yes
      dat$wishkid[which(dat$neal3==2)] <- 2  #No
      dat$wishkid[which(dat$neal3==3)] <- 3  #Don't know
    }
    dat$wishkid <- factor(dat$wishkid, levels = c(1,2,3), labels = c("Yes", "No", "DK"))

    #Family status
    dat$famstat <- NA
    dat$famstat[which(dat$havekid=="Yes")] <- 1  #Parent - Unclassified
    dat$famstat[which(dat$havekid=="No" & dat$plankid=="Yes")] <- 6  #Not yet parent
    dat$famstat[which(dat$havekid=="No" & dat$plankid=="DK")] <- 11  #Undecided
    dat$famstat[which(dat$havekid=="No" & dat$plankid=="No" & dat$wishkid=="Yes")] <- 7  #Childless - Unclassified
    dat$famstat[which(dat$havekid=="No" & dat$plankid=="No" & dat$wishkid=="DK")] <- 10  #Ambivalent non-parent
    dat$famstat[which(dat$havekid=="No" & dat$plankid=="No" & dat$wishkid=="No")] <- 12  #Childfree
    dat$famstat <- factor(dat$famstat, levels = c(1:12),
                          labels = c("Parent - Unclassified", "Parent - Fulfilled", "Parent - Unfulfilled", "Parent - Reluctant", "Parent - Ambivalent",
                                     "Not yet parent", "Childless - Unclassified", "Childless - Social", "Childless - Biological", "Ambivalent non-parent", "Undecided", "Childfree"))

    #Childfree (want)
    dat$cf_want <- NA
    dat$cf_want[which(dat$famstat=="Childfree")] <- 1
    dat$cf_want[which(dat$famstat!="Childfree")] <- 0

    #### Demographics ####
    #Sex
    dat$sex <- NA
    dat$sex[which(dat$cd1==2)] <- 1  #Female
    dat$sex[which(dat$cd1==1)] <- 2  #Male
    dat$sex[which(dat$cd1==3)] <- 3  #Intersex
    dat$sex <- factor(dat$sex, levels = c(1,2,3), labels = c("Female", "Male", "Other"))

    #Sexual orientation
    dat$lgbt <- NA
    if (wave==84) {
      dat$lgbt[which(dat$neal11==2)] <- 0  #Not LGBT
      dat$lgbt[which(dat$neal11==1)] <- 1  #LGBT
    }

    #Race
    dat$race <- NA
    if (wave==79 | wave==82) {
      dat$race[which(dat$cd4_m_1==1 & dat$cd4_m_2==2 & dat$cd4_m_3==2 & dat$cd4_m_4==2 & dat$cd4_m_5==2 & dat$cd4_m_6==2)] <- 1  #White
      dat$race[which(dat$cd4_m_1==2 & dat$cd4_m_2==1 & dat$cd4_m_3==2 & dat$cd4_m_4==2 & dat$cd4_m_5==2 & dat$cd4_m_6==2)] <- 2  #Black
      dat$race[which(dat$cd4_m_1==2 & dat$cd4_m_2==2 & dat$cd4_m_3==1 & dat$cd4_m_4==2 & dat$cd4_m_5==2 & dat$cd4_m_6==2)] <- 3  #Hawaiian
      dat$race[which(dat$cd4_m_1==2 & dat$cd4_m_2==2 & dat$cd4_m_3==2 & dat$cd4_m_4==1 & dat$cd4_m_5==2 & dat$cd4_m_6==2)] <- 4  #Asian
      dat$race[which(dat$cd4_m_1==2 & dat$cd4_m_2==2 & dat$cd4_m_3==2 & dat$cd4_m_4==2 & dat$cd4_m_5==1 & dat$cd4_m_6==2)] <- 5  #American Indian
      dat$race[which(dat$cd4_m_1==2 & dat$cd4_m_2==2 & dat$cd4_m_3==2 & dat$cd4_m_4==2 & dat$cd4_m_5==2 & dat$cd4_m_6==1)] <- 6  #Other
      dat$race[which(rowSums(dat[,c("cd4_m_1", "cd4_m_2", "cd4_m_3", "cd4_m_4", "cd4_m_5", "cd4_m_6")]==1)>1)] <- 7  #Multi-racial
    }
    if (wave==84 | wave==85 | wave==86) {
      dat$race[which(dat$cd4_1==1 & dat$cd4_2==2 & dat$cd4_3==2 & dat$cd4_4==2 & dat$cd4_5==2 & dat$cd4_6==2)] <- 1  #White
      dat$race[which(dat$cd4_1==2 & dat$cd4_2==1 & dat$cd4_3==2 & dat$cd4_4==2 & dat$cd4_5==2 & dat$cd4_6==2)] <- 2  #Black
      dat$race[which(dat$cd4_1==2 & dat$cd4_2==2 & dat$cd4_3==1 & dat$cd4_4==2 & dat$cd4_5==2 & dat$cd4_6==2)] <- 3  #Hawaiian
      dat$race[which(dat$cd4_1==2 & dat$cd4_2==2 & dat$cd4_3==2 & dat$cd4_4==1 & dat$cd4_5==2 & dat$cd4_6==2)] <- 4  #Asian
      dat$race[which(dat$cd4_1==2 & dat$cd4_2==2 & dat$cd4_3==2 & dat$cd4_4==2 & dat$cd4_5==1 & dat$cd4_6==2)] <- 5  #American Indian
      dat$race[which(dat$cd4_1==2 & dat$cd4_2==2 & dat$cd4_3==2 & dat$cd4_4==2 & dat$cd4_5==2 & dat$cd4_6==1)] <- 6  #Other
      dat$race[which(rowSums(dat[,c("cd4_1", "cd4_2", "cd4_3", "cd4_4", "cd4_5", "cd4_6")]==1)>1)] <- 7  #Multi-racial
    }
    dat$race <- factor(dat$race, levels = c(1:7), labels = c("White", "Black", "Hawaiian", "Asian", "American Indian", "Other", "Multi-racial"))

    #Hispanic
    dat$hispanic <- NA
    dat$hispanic[which(dat$cd5a==5)] <- 0  #Not hispanic
    dat$hispanic[which(dat$cd5a==1)] <- 1  #Hispanic

    #Age in years
    if (wave==79) {dat$age <- 2020 - dat$cd2}
    if (wave==82) {dat$age <- 2021 - dat$cd2}
    if (wave==84) {dat$age <- 2022 - dat$cd2}
    if (wave==85) {dat$age <- 2022 - dat$cd2}
    if (wave==86) {dat$age <- 2022 - dat$cd2}

    #Education in years
    dat$education <- NA
    dat$education[which(dat$cd3==0)] <- 1  #No education
    dat$education[which(dat$cd3>=1 & dat$cd3<=11)] <- 2  #No high school
    dat$education[which(dat$cd3==12)] <- 3  #High school graduate
    dat$education[which(dat$cd3==13 | dat$cd3==14 | dat$cd3==15 | dat$cd3==20)] <- 4  #Some college
    dat$education[which(dat$cd3==16)] <- 5  #College graduate
    dat$education[which(dat$cd3==17)] <- 6  #Some post-grad
    dat$education[which(dat$cd3==18)] <- 7  #Graduate degree
    dat$education <- factor(dat$education,
                            levels = c(1:7),
                            labels = c("No education", "Did not graduate high school", "High School graduate",
                                       "Some college", "College graduate", "Some post-graduate", "Graduate degree"),
                            ordered = TRUE)

    #Partnership status
    dat$partnered <- NA
    dat$partnered[which(dat$cd8==6)] <- 1  #Single, never married
    dat$partnered[which(dat$cd8==1 | dat$cd8==5)] <- 2  #Currently partnered
    dat$partnered[which(dat$cd8==2 | dat$cd8==3 | dat$cd8==4)] <- 3  #Formerly partnered
    dat$partnered <- factor(dat$partnered, levels = c(1,2,3), labels = c("Never", "Currently", "Formerly"))

    #Residence
    dat$residence <- dat$x1
    dat$residence[which(dat$residence==5 | dat$residence==8)] <- NA
    dat$residence <- factor(dat$residence, levels = c(1,2,3,4), labels = c("Rural", "Town", "Suburb", "Urban"), ordered = TRUE)

    #Employed
    dat$employed <- NA
    dat$employed[which(dat$cd15==6 | dat$cd15==7 | dat$cd15==8 | dat$cd15==9 | dat$cd15==10)] <- 0  #Not employed
    dat$employed[which(dat$cd15==1 | dat$cd15==2 | dat$cd15==3 | dat$cd15==4 | dat$cd15==5 | dat$cd15==11)] <- 1  #Employed

    #In school
    dat$inschool <- 0  #Not in school
    dat$inschool[which(dat$cd15==3 | dat$cd15==8)] <- 1  #In school
    dat$inschool[which(dat$cd15==98)] <- NA

    #### Attitude ####
    #Ideology
    dat$ideology[which(dat$ideology==8 | dat$ideology==98 | dat$ideology==99)] <- NA  #8 = Don't know, 98 = Skipped, 99 = Not asked
    dat$ideology <- factor(dat$ideology, levels = c(1:7), labels = c("Very conservative", "Somewhat conservative", "Closer to the conservative side",
                                                                     "In the middle",
                                                                     "Closer to the liberal side", "Somewhat liberal", "Very liberal"), ordered = TRUE)

    #Religion
    dat$religion <- dat$cd6
    dat$religion[which(dat$religion==8)] <- NA  #8 = Skipped
    dat$religion <- factor(dat$religion, levels = c(0:5), labels = c("None", "Catholic / Orthodox", "Muslim", "Jewish", "Protestant / Christian", "Other"))

    #### Design ####
    #Identifier
    dat$id <- as.character(dat$caseid)

    #Country
    dat$country <- "United States"

    #Sampling weight
    if (wave==82) {dat <- dat[which(!is.na(dat$weight)),]}  #In wave 82, only use base sample; drop parent oversample
    dat$weight <- dat$weight

    #Wave
    dat$wave <- wave

    #Year of data collection
    if (wave==79) {dat$year <- 2020}
    if (wave==82) {dat$year <- 2021}
    if (wave==84) {dat$year <- 2022}
    if (wave==85) {dat$year <- 2022}
    if (wave==86) {dat$year <- 2022}

    #Month of data collection
    if (wave==79) {dat$month <- 5}
    if (wave==82) {dat$month <- 9}
    if (wave==84) {dat$month <- 4}
    if (wave==85) {dat$month <- 9}
    if (wave==86) {dat$month <- 12}
    dat$month <- factor(dat$month, levels = c(1:12), labels = c("January", "February", "March", "April", "May", "June",
                                                                "July", "August", "September", "October", "November", "December"),
                        ordered = TRUE)

    #Source file
    if (wave==79) {dat$file <- "soss79b.sav"}
    if (wave==82) {dat$file <- "soss82.sav"}
    if (wave==84) {dat$file <- "SOSS%2084%20WEIGHTED%20DATASET_5.4.22.sav"}
    if (wave==85) {dat$file <- "SOSS%2085_WEIGHTED_OUTPUT.sav"}
    if (wave==86) {dat$file <- "SOSS86_weighted_OUTPUT.sav"}

    #Source survey
    dat$survey <- "SOSS"

    #### Clean up ####
    #Check for extra variables; if not present, add them
    if (!is.null(extra.vars)) {
      for (var in extra.vars) {if (!(var %in% colnames(dat))) {  #For each extra variable that is not present:
        dat$v8675309 <- NA   #(1) Add a temporary missing variable called `v8675309`
        colnames(dat)[length(colnames(dat))] <- var  #(2) Rename this new missing variable with the correct name
        }
      }
    }

    #Reduce data
    if (!is.null(extra.vars)) {
      dat <- dat[,c("cf_want", "famstat",  #Family status
                    "sex", "lgbt", "race", "hispanic", "age", "education", "partnered", "residence", "employed", "inschool",  #Demographics
                    "ideology", "religion",  #Attitude
                    "id", "country", "weight", "file", "survey", "wave", "year", "month",  #Design
                    extra.vars)]
    } else {
      dat <- dat[,c("cf_want", "famstat",  #Family status
                    "sex", "lgbt", "race", "hispanic", "age", "education", "partnered", "residence", "employed", "inschool",  #Demographics
                    "ideology", "religion",  #Attitude
                    "id", "country", "weight", "file", "survey", "wave", "year", "month")]  #Design
    }

    #Start data file, or append to existing data file
    if (wave==min(waves)) {data <- dat} else {data <- rbind(data, dat)}
    wave.num <- wave.num + 1

  }

  #Finalize
  if (progress) {close(pb)}  #Close progress bar
  class(data) <- c("data.frame", "childfree")
  return(data)  #Export data
}
