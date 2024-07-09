#' Read and recode National Survey of Family Growth (NSFG) data
#'
#' @param years vector: a numeric vector containing the starting year of NSFG waves to include (2002, 2006, 2011, 2013, 2015, 2017)
#' @param progress boolean: display a progress bar
#'
#' @details
#' The U.S. Centers for Disease Control \href{https://www.cdc.gov/nchs/nsfg/index.htm}{National Survey of Family Growth} (NSFG)
#'    regularly collects fertility and other health information from a population-representative sample of adults in the
#'    United States. Between 1973 and 2002, the NSFG was conducted periodically. Starting in 2002, the NSFG transitioned to
#'    continuous data collection, releasing data in three-year waves (e.g., the 2013-2015, 2015-2017). The `nsfg()` function reads
#'    the raw data from CDC's website, extracts and recodes selected variables useful for studying childfree adults and other family
#'    statuses, then returns a single data frame.
#'
#' **Weights**
#'
#' The \href{https://cran.r-project.org/web/packages/survey/index.html}{`survey`} package can be used to incorporate sampling weights
#'    and obtain population-representative estimates by wave. After using `nsfg()` to obtain data for a given wave (see example below), use
#'    `dat <- svydesign(data = dat, ids = ~cluster, strata = ~stratum, weights = ~weight, nest = TRUE)` to incorporate information about
#'    the survey design.
#'
#' **Known issues**
#'   * Starting in 2006, "hispanic" was a response option for race, however "hispanic" is not a racial category, but an ethnicity.
#'     When a respondent chose this option, their actual race is unknown.
#'
#' @return A data frame containing variables described in the codebook available using \code{vignette("codebooks")}
#'
#' @export
#'
#' @examples
#' \donttest{data <- nsfg(years = 2017)}
nsfg <- function(years, progress = TRUE) {

  if (!all(years %in%c(2002, 2006, 2011, 2013, 2015, 2017))) {stop("Only the following NSFG years are available: 2002, 2006, 2011, 2013, 2015, 2017")}  #Check for valid years
  years <- sort(years)  #Put years in order

  if (progress) {message("Processing NSFG data files -")}
  if (progress) {pb <- utils::txtProgressBar(min = 0, max = length(years) * 2, initial = 0, style = 3)} #Initialize progress bar
  year.num <- 1

  #### FEMALE RESPONDENT LOOP ####
  for (year in years) {

    #Increment progress bar
    if (progress) {utils::setTxtProgressBar(pb,year.num)}

    #Import raw data
    if (year==2002) {raw <- readLines("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2002FemResp.dat")}
    if (year==2006) {raw <- readLines("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2006_2010_FemResp.dat")}
    if (year==2011) {raw <- readLines("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2011_2013_FemRespData.dat")}
    if (year==2013) {raw <- readLines("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2013_2015_FemRespData.dat")}
    if (year==2015) {raw <- readLines("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2015_2017_FemRespData.dat")}
    if (year==2017) {raw <- readLines("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2017_2019_FemRespData.dat")}

    #Initialize dataframe with id variable
    if (year==2002) {dat <- data.frame(id = as.character(substring(raw,1,12)))}
    if (year==2006 | year==2011 | year==2013 | year==2015 | year==2017) {dat <- data.frame(id = as.character(substring(raw,1,5)))}

    #### Family Status ####
    #Source variables
    if (year==2002) {
      dat$hasbabes <- as.numeric(substring(raw,79,79)) #Any live births: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$everadpt <- as.numeric(substring(raw,306,306)) #Adoption experience: 1 = Yes, 3 = Trying, 5 = No
      dat$seekadpt <- as.numeric(substring(raw,307,307))  #Are you seeking to adopt: 1 = Yes, 5 = No, 9 = Don't know
      dat$rwant <- as.numeric(substring(raw,3512,3512)) #Wants a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$rstrstat <- as.numeric(substring(raw,1463,1463)) #Respondent's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$pstrstat <- as.numeric(substring(raw,1464,1464)) #Partner's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$intend <- as.numeric(substring(raw,3522,3522)) #Not partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$jintend <- as.numeric(substring(raw,3515,3515)) #Partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
    }
    if (year==2006) {
      dat$hasbabes <- as.numeric(substring(raw,118,118)) #Any live births: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$everadpt <- as.numeric(substring(raw,696,696)) #Adoption experience: 1 = Yes, 3 = Trying, 5 = No
      dat$seekadpt <- as.numeric(substring(raw,697,697))  #Are you seeking to adopt: 1 = Yes, 5 = No, 9 = Don't know
      dat$rwant <- as.numeric(substring(raw,4539,4539)) #Wants a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$rstrstat <- as.numeric(substring(raw,1902,1902)) #Respondent's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$pstrstat <- as.numeric(substring(raw,1903,1903)) #Partner's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$intend <- as.numeric(substring(raw,4550,4550)) #Not partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$jintend <- as.numeric(substring(raw,4542,4542)) #Partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
    }
    if (year==2011) {
      dat$hasbabes <- as.numeric(substring(raw,123,123)) #Any live births: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$everadpt <- as.numeric(substring(raw,623,623)) #Adoption experience: 1 = Yes, 3 = Trying, 5 = No
      dat$seekadpt <- as.numeric(substring(raw,624,624))  #Are you seeking to adopt: 1 = Yes, 5 = No, 9 = Don't know
      dat$rwant <- as.numeric(substring(raw,3282,3282)) #Wants a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$rstrstat <- as.numeric(substring(raw,1754,1754)) #Respondent's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$pstrstat <- as.numeric(substring(raw,1755,1755)) #Partner's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$intend <- as.numeric(substring(raw,3293,3293)) #Not partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$jintend <- as.numeric(substring(raw,3285,3285)) #Partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
    }
    if (year==2013) {
      dat$hasbabes <- as.numeric(substring(raw,118,118)) #Any live births: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$everadpt <- as.numeric(substring(raw,551,551)) #Adoption experience: 1 = Yes, 3 = Trying, 5 = No
      dat$seekadpt <- as.numeric(substring(raw,552,552))  #Are you seeking to adopt: 1 = Yes, 5 = No, 9 = Don't know
      dat$rwant <- as.numeric(substring(raw,3420,3420)) #Wants a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$rstrstat <- as.numeric(substring(raw,1683,1683)) #Respondent's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$pstrstat <- as.numeric(substring(raw,1684,1684)) #Partner's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$intend <- as.numeric(substring(raw,3252,3252)) #Not partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$jintend <- as.numeric(substring(raw,3243,3243)) #Partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      }
    if (year==2015) {
      dat$hasbabes <- as.numeric(substring(raw,96,96)) #Any live births: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$everadpt <- as.numeric(substring(raw,400,400)) #Adoption experience: 1 = Yes, 3 = Trying, 5 = No
      dat$seekadpt <- as.numeric(substring(raw,401,401))  #Are you seeking to adopt: 1 = Yes, 5 = No, 9 = Don't know
      dat$rwant <- as.numeric(substring(raw,2785,2785)) #Wants a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$rstrstat <- as.numeric(substring(raw,1237,1237)) #Respondent's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$pstrstat <- as.numeric(substring(raw,1238,1238)) #Partner's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$intend <- as.numeric(substring(raw,2796,2796)) #Not partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$jintend <- as.numeric(substring(raw,2788,2788)) #Partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      }
    if (year==2017) {
      dat$hasbabes <- as.numeric(substring(raw,89,89)) #Any live births: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$everadpt <- as.numeric(substring(raw,213,213)) #Adoption experience: 1 = Yes, 3 = Trying, 5 = No
      dat$seekadpt <- as.numeric(substring(raw,214,214))  #Are you seeking to adopt: 1 = Yes, 5 = No, 9 = Don't know
      dat$rwant <- as.numeric(substring(raw,2410,2410)) #Wants a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$rstrstat <- as.numeric(substring(raw,836,836)) #Respondent's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$pstrstat <- as.numeric(substring(raw,837,837)) #Partner's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$intend <- as.numeric(substring(raw,2421,2421)) #Not partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$jintend <- as.numeric(substring(raw,2413,2413)) #Partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
    }

    #Constructed variables
    dat$anykids <- NA  #Does the respondent have biological or adopted children?
    dat$anykids[which(dat$hasbabes==5 & (dat$everadpt!=1 | is.na(dat$everadpt)))] <- 0  #No
    dat$anykids[which(dat$hasbabes==1 | dat$everadpt==1)] <- 1  #Yes

    dat$planadpt <- NA  #Is the respondent trying (currently), or seeking (plans in the future), to adopt?
    dat$planadpt[which((dat$everadpt==5 | is.na(dat$everadpt)) & (dat$seekadpt==5 | is.na(dat$seekadpt)))] <- 0  #No (Have not adopted & don't plan to)
    dat$planadpt[which(dat$everadpt==1 | dat$seekadpt==5)] <- 0  #No (Have adopted, but don't plan to again)
    dat$planadpt[which(dat$everadpt==3 | dat$seekadpt==1)] <- 1  #Yes (May adopt in the future)
    dat$planadpt[which(dat$seekadpt==9 & dat$everadpt!=3)] <- 9  #Don't know (Not currently trying to adopt, don't know about future)

    dat$wantbio <- NA  #Does the respondent want a(nother) biological child?
    dat$wantbio[which(dat$rwant==5)] <- 0  #No
    dat$wantbio[which(dat$rwant==1)] <- 1  #Yes
    dat$wantbio[which(dat$rwant==9)] <- 9  #Don't know

    #Childfree (want)
    dat$cf_want <- NA
    dat$cf_want[which(dat$anykids==0 & dat$wantbio==0 & dat$planadpt==0)] <- 1  #Childfree
    dat$cf_want[which(dat$anykids==1 | dat$wantbio==1 | dat$wantbio==9 | dat$planadpt==1 | dat$planadpt==9)] <- 0  #Not childfree

    #Childfree (expect) - Unknown because intention question only asked of single respondents if they wanted children

    #Family status
    dat$famstat <- NA
    dat$famstat[which(dat$anykids==1)] <- 1  #Parent - Unclassified
    #Parent - Fulfilled: Unknown because parents who do not want another child could also be reluctant
    dat$famstat[which(dat$anykids==1 & (dat$wantbio==1 | dat$planadpt==1))] <- 3 #Parent - Unfulfilled
    #Parent - Reluctant: Unknown because parents who do not want another child could also be fulfilled
    dat$famstat[which(dat$anykids==1 & (dat$wantbio==9 | dat$planadpt==9))] <- 5 #Parent - Ambivalent

    dat$famstat[which(dat$anykids==0 & (dat$wantbio==1 | dat$planadpt==1))] <- 6  #Not yet parent

    #Childless - Unclassified: Not used because all can be classified

    dat$famstat[which(dat$anykids==0 & dat$planadpt==0 & dat$wantbio==1 & dat$intend==5)] <- 8  #Childless - Social: Single respondent who wanted, but do not intend, to have children
    dat$famstat[which(dat$anykids==0 & dat$planadpt==0 & dat$wantbio==1 & dat$jintend==5)] <- 8  #Childless - Social: Partnered respondent who wanted, but do not intend, to have children

    dat$famstat[which(dat$anykids==0 & dat$planadpt==0 & dat$wantbio==1 & (dat$rstrstat==1 | dat$rstrstat==2))] <- 9  #Childless - Biological: Respondent who wanted, but is sterile
    dat$famstat[which(dat$anykids==0 & dat$planadpt==0 & dat$wantbio==1 & (dat$pstrstat==1 | dat$pstrstat==2))] <- 9  #Childless - Biological: Respondent who wanted, but who's partner is sterile

    dat$famstat[which(dat$anykids==0 & (dat$wantbio==9 | dat$planadpt==9))] <- 11 #Undecided

    dat$famstat[which(dat$anykids==0 & dat$intend==5 & (dat$wantbio==9 | dat$planadpt==9))] <- 10  #Ambivalent non-parent: Single respondent who does not intend, but does not know if wanted
    dat$famstat[which(dat$anykids==0 & dat$jintend==5 & (dat$wantbio==9 | dat$planadpt==9))] <- 10  #Ambivalent non-parent: Partnered respondent who does not intend, but does not know if wanted

    dat$famstat[which(dat$anykids==0 & dat$wantbio==0 & dat$planadpt==0)] <- 12 #Childfree

    dat$famstat <- factor(dat$famstat, levels = c(1:12),
                          labels = c("Parent - Unclassified", "Parent - Fulfilled", "Parent - Unfulfilled", "Parent - Reluctant", "Parent - Ambivalent",
                                     "Not yet parent", "Childless - Unclassified", "Childless - Social", "Childless - Biological", "Ambivalent non-parent", "Undecided", "Childfree"))

    #### Demographics ####
    #Sex
    dat$sex <- 1
    dat$sex <- factor(dat$sex, levels = c(1,2,3), labels = c("Female", "Male", "Other"))

    #Sexual orientation
    dat$lgbt <- NA
    if (year==2002 | year==2006) {dat$orient <- NA}
    if (year==2011) {dat$orient <- as.numeric(substring(raw,3704,3704))}  #1 straight, 2 gay, 3 bi, 7 not asked, 8 refused, 9 don't know
    if (year==2013) {dat$orient <- as.numeric(substring(raw,3654,3654))}  #1 straight, 2 gay, 3 bi, 7 not asked, 8 refused, 9 don't know
    if (year==2015) {
      dat$a <- as.numeric(substring(raw,3184,3184))  #1 straight, 2 gay, 3 bi, 7 not asked, 8 refused, 9 don't know
      dat$a[is.na(dat$a)] <- 0  #Code missing as zero
      dat$b <- as.numeric(substring(raw,3185,3185))  #1 gay, 2 straight, 3 bi, 4 something else, 7 not asked, 8 refused, 9 don't know
      dat$b[which(dat$b==1 | dat$b==2)] <- 3 - dat$b[which(dat$b==1 | dat$b==2)]  #Reverse so 1 straight, 2 gay
      dat$b[is.na(dat$b)] <- 0  #Code missing as zero
      dat$orient <- dat$a + dat$b  #Combine versions a and b
    }
    if (year==2017) {
      dat$a <- as.numeric(substring(raw,2779,2779))  #1 straight, 2 gay, 3 bi, 7 not asked, 8 refused, 9 don't know
      dat$a[is.na(dat$a)] <- 0  #Code missing as zero
      dat$b <- as.numeric(substring(raw,2780,2780))  #1 gay, 2 straight, 3 bi, 4 something else, 7 not asked, 8 refused, 9 don't know
      dat$b[which(dat$b==1 | dat$b==2)] <- 3 - dat$b[which(dat$b==1 | dat$b==2)]  #Reverse so 1 straight, 2 gay
      dat$b[is.na(dat$b)] <- 0  #Code missing as zero
      dat$orient <- dat$a + dat$b  #Combine versions a and b
    }
    dat$lgbt <- factor(dat$orient, levels = c(1,2,3,4), labels = c("Straight", "Gay/Lesbian", "Bisexual", "Something else"))
    
    #Race
    if (year==2002) {
      dat$race <- as.numeric(substring(raw,17,17))
      dat$race <- factor(dat$race, levels = c(5,4,3,2,1,99,98), labels = c("White", "Black", "Hawaiian", "Asian", "American Indian", "Other", "Multi-racial"))
    }
    if (year==2006 | year==2011 | year==2013) {  #Coding assumes that 1/2/3 mean the same here as in 2002, despite not being labeled in the provided Stata .do code
      dat$race <- as.numeric(substring(raw,10,10))
      dat$race <- factor(dat$race, levels = c(5,4,3,2,1,96,99), labels = c("White", "Black", "Hawaiian", "Asian", "American Indian", "Other", "Multi-racial"))
    }
    if (year==2015 | year==2017) {
      dat$race <- as.numeric(substring(raw,10,10))
      dat$race <- factor(dat$race, levels = c(3,2,99,98,97,1,99), labels = c("White", "Black", "Hawaiian", "Asian", "American Indian", "Other", "Multi-racial"))
    }

    #Hispanic
    if (year==2002) {dat$hispanic <- as.numeric(substring(raw,16,16))}
    if (year==2006 | year==2011 | year==2013 | year==2015 | year==2017) {dat$hispanic <- as.numeric(substring(raw,9,9))}
    dat$hispanic[which(dat$hispanic==7)] <- NA  #Not asked
    dat$hispanic[which(dat$hispanic==8)] <- NA  #Refused
    dat$hispanic[which(dat$hispanic==9)] <- NA  #Don't know
    dat$hispanic[which(dat$hispanic==5)] <- 0  #Not hispanic
    dat$hispanic[which(dat$hispanic==1)] <- 1  #Hispanic

    #Age in years
    if (year==2002) {dat$age <- as.numeric(substring(raw,20,21))}
    if (year==2006 | year==2011 | year==2013 | year==2015 | year==2017) {dat$age <- as.numeric(substring(raw,13,14))}
    dat$age[which(dat$age==98)] <- NA  #Refused
    dat$age[which(dat$age==99)] <- NA  #Don't know

    #Education in years
    if (year==2006) {dat$higrade <- as.numeric(substring(raw,39,40))}
    if (year==2002 | year==2011) {dat$higrade <- as.numeric(substring(raw,43,44))}
    if (year==2013) {dat$higrade <- as.numeric(substring(raw,40,41))}
    if (year==2015 | year==2017) {dat$higrade <- as.numeric(substring(raw,36,37))}
    dat$higrade[which(dat$higrade==98)] <- NA  #Refused
    dat$higrade[which(dat$higrade==99)] <- NA  #Don't know

    if (year==2002) {dat$dipged <- as.numeric(substring(raw,47,47))}
    if (year==2011) {dat$dipged <- as.numeric(substring(raw,46,46))}
    if (year==2006 | year==2013) {dat$dipged <- as.numeric(substring(raw,43,43))}
    if (year==2015 | year==2017) {dat$dipged <- as.numeric(substring(raw,39,39))}
    dat$dipged[which(dat$dipged==8)] <- NA  #Refused
    dat$dipged[which(dat$dipged==9)] <- NA  #Don't know

    if (year==2011) {dat$havedeg <- as.numeric(substring(raw,71,71))}
    if (year==2006 | year==2013) {dat$havedeg <- as.numeric(substring(raw,68,68))}
    if (year==2002 | year==2015 | year==2017) {dat$havedeg <- as.numeric(substring(raw,52,52))}
    dat$havedeg[which(dat$havedeg==7)] <- NA  #Not asked
    dat$havedeg[which(dat$havedeg==8)] <- NA  #Refused
    dat$havedeg[which(dat$havedeg==9)] <- NA  #Don't know

    if (year==2011) {dat$degrees <- as.numeric(substring(raw,72,72))}
    if (year==2006 | year==2013) {dat$degrees <- as.numeric(substring(raw,69,69))}
    if (year==2002 | year==2015 | year==2017) {dat$degrees <- as.numeric(substring(raw,53,53))}
    dat$degrees[which(dat$degrees==8)] <- NA  #Refused
    dat$degrees[which(dat$degrees==9)] <- NA  #Don't know

    dat$education <- NA
    dat$education[which(dat$higrade<=12)] <- 2  #Did not finish high school (yet)
    dat$education[which(dat$dipged==1 | dat$dipged==2 | dat$dipged==3)] <- 3  #High school graduate
    dat$education[which(dat$higrade>12 & (dat$havedeg==5 | dat$degrees==1))] <- 4  #Some college (no degree or associates degree)
    dat$education[which(dat$degrees==2)] <- 5  #College degree
    dat$education[which(dat$degrees==3 | dat$degrees==4 | dat$degrees==5)] <- 7  #Graduate degree
    dat$education <- factor(dat$education,
                            levels = c(1:7),
                            labels = c("No education", "Did not graduate high school", "High School graduate",
                                       "Some college", "College graduate", "Some post-graduate", "Graduate degree"),
                            ordered = TRUE)

    #Partnership status
    if (year==2006 | year==2011 | year==2013) {dat$marstat <- as.numeric(substring(raw,21,21))}
    if (year==2002 | year==2015 | year==2017) {dat$marstat <- as.numeric(substring(raw,28,28))}
    dat$partnered <- NA
    dat$partnered[which(dat$marstat==6)] <- 1  #Single, never married
    dat$partnered[which(dat$marstat==1 | dat$marstat==2)] <- 2  #Currently partnered
    dat$partnered[which(dat$marstat==3 | dat$marstat==4 | dat$marstat==5)] <- 3  #Formerly partnered
    dat$partnered <- factor(dat$partnered, levels = c(1,2,3), labels = c("Never", "Currently", "Formerly"))

    #Residence
    if (year==2002) {dat$metro <- as.numeric(substring(raw,4821,4821))}
    if (year==2006) {dat$metro <- as.numeric(substring(raw,6116,6116))}
    if (year==2011) {dat$metro <- as.numeric(substring(raw,4890,4890))}
    if (year==2013) {dat$metro <- as.numeric(substring(raw,5016,5016))}
    if (year==2015) {dat$metro <- as.numeric(substring(raw,4454,4454))}
    if (year==2017) {dat$metro <- as.numeric(substring(raw,3772,3772))}
    dat$residence <- NA
    dat$residence[which(dat$metro==1)] <- 4  #Principal city of MSA = Urban
    dat$residence[which(dat$metro==2)] <- 3  #Other part of MSA = Suburb
    dat$residence[which(dat$metro==3)] <- 1  #Not in MSA = Rural
    dat$residence <- factor(dat$residence, levels = c(1,2,3,4), labels = c("Rural", "Town", "Suburb", "Urban"), ordered = TRUE)

    #Employed
    if (year==2002) {dat$rwrkst <- as.numeric(substring(raw,3674,3674))}
    if (year==2006) {dat$rwrkst <- as.numeric(substring(raw,4758,4758))}
    if (year==2011) {dat$rwrkst <- as.numeric(substring(raw,3511,3511))}
    if (year==2013) {dat$rwrkst <- as.numeric(substring(raw,3461,3461))}
    if (year==2015) {dat$rwrkst <- as.numeric(substring(raw,3008,3008))}
    if (year==2017) {dat$rwrkst <- as.numeric(substring(raw,2663,2663))}
    dat$employed <- NA
    dat$employed[which(dat$rwrkst==1)] <- 1  #Employed
    dat$employed[which(dat$rwrkst==5)] <- 0  #Not employed

    #In school
    if (year==2006) {dat$goschol <- as.numeric(substring(raw,37,37))}
    if (year==2002 | year==2011) {dat$goschol <- as.numeric(substring(raw,41,41))}
    if (year==2013) {dat$goschol <- as.numeric(substring(raw,38,38))}
    if (year==2015 | year==2017) {dat$goschol <- as.numeric(substring(raw,34,34))}
    dat$inschool <- NA
    dat$inschool[which(dat$goschol==1)] <- 1  #In school
    dat$inschool[which(dat$goschol==5)] <- 0  #Not in school

    #### Attitude ####
    #Religion
    if (year==2002) {dat$relcurr <- as.numeric(substring(raw,3653,3654))}
    if (year==2006) {dat$relcurr <- as.numeric(substring(raw,4728,4729))}
    if (year==2011) {dat$relcurr <- as.numeric(substring(raw,3493,3494))}
    if (year==2013) {dat$relcurr <- as.numeric(substring(raw,3444,3445))}
    if (year==2015) {dat$relcurr <- as.numeric(substring(raw,2990,2991))}
    if (year==2017) {dat$relcurr <- as.numeric(substring(raw,2644,2645))}
    dat$religion <- NA
    dat$religion[which(dat$relcurr==1)] <- 1  #None
    dat$religion[which(dat$relcurr==2)] <- 2  #Catholic
    dat$religion[which(dat$relcurr==3)] <- 5  #Baptist/Southern Baptist ==> Protestant
    dat$religion[which(dat$relcurr==4)] <- 5  #Methodist, Lutheran, Presbyterian, Episcopal ==> Protestant
    dat$religion[which(dat$relcurr==5)] <- 5  #Fundamentalist Protestant ==> Protestant
    dat$religion[which(dat$relcurr==6)] <- 5  #Other Protestant denomination ==> Protestant
    dat$religion[which(dat$relcurr==7)] <- 5  #Protestant - No specific denomination ==> Protestant
    dat$religion[which(dat$relcurr==8)] <- 6  #Other
    dat$religion <- factor(dat$religion, levels = c(1:6), labels = c("None", "Catholic / Orthodox", "Muslim", "Jewish", "Protestant / Christian", "Other"))

    #Bother (If it turns out that you do not have any children, how much would it bother you?)
    if (year==2002) {dat$bother <- as.numeric(substring(raw,3723,3723))}
    if (year==2006) {dat$bother <- as.numeric(substring(raw,4806,4806))}
    if (year==2011) {dat$bother <- as.numeric(substring(raw,3537,3537))}
    if (year==2013) {dat$bother <- as.numeric(substring(raw,3486,3486))}
    if (year==2015) {dat$bother <- as.numeric(substring(raw,3027,3027))}
    if (year==2017) {dat$bother <- as.numeric(substring(raw,2682,2682))}
    dat$bother <- factor(dat$bother, levels = c(4,3,2,1), labels = c("Not at all", "A little", "Some", "A great deal"), ordered = TRUE)

    #### Design ####
    #Identifier - This step is performed above, when initializing the data frame

    #Country
    dat$country <- "United States"

    #Sampling weight
    if (year==2002) {dat$weight <- as.numeric(substring(raw,4873,4891))}
    if (year==2006) {dat$weight <- as.numeric(substring(raw,6150,6168))}
    if (year==2011) {dat$weight <- as.numeric(substring(raw,4906,4922))}
    if (year==2013) {dat$weight <- as.numeric(substring(raw,5032,5048))}
    if (year==2015) {dat$weight <- as.numeric(substring(raw,4470,4486))}
    if (year==2017) {dat$weight <- as.numeric(substring(raw,3787,3803))}

    #Cluster
    if (year==2002) {dat$cluster <- as.numeric(substring(raw,4891,4891))}
    if (year==2006) {dat$cluster <- as.numeric(substring(raw,6222,6222))}
    if (year==2011) {dat$cluster <- as.numeric(substring(raw,4922,4922))}
    if (year==2013) {dat$cluster <- as.numeric(substring(raw,5048,5048))}
    if (year==2015) {dat$cluster <- as.numeric(substring(raw,4486,4486))}
    if (year==2017) {dat$cluster <- as.numeric(substring(raw,3803,3803))}

    #Stratum
    if (year==2002) {dat$stratum <- as.numeric(substring(raw,4892,4893))}
    if (year==2006) {dat$stratum <- as.numeric(substring(raw,6223,6225))}
    if (year==2011) {dat$stratum <- as.numeric(substring(raw,4923,4923))}
    if (year==2013) {dat$stratum <- as.numeric(substring(raw,5049,5051))}
    if (year==2015) {dat$stratum <- as.numeric(substring(raw,4487,4489))}
    if (year==2017) {dat$stratum <- as.numeric(substring(raw,3804,3806))}

    #Wave
    if (year==2002) {dat$wave <- "2002"}
    if (year==2006) {dat$wave <- "2006-2010"}
    if (year==2011) {dat$wave <- "2011-2013"}
    if (year==2013) {dat$wave <- "2013-2015"}
    if (year==2015) {dat$wave <- "2015-2017"}
    if (year==2017) {dat$wave <- "2017-2019"}

    #Year of data collection
    if (year==2002) {dat$cmintvw <- as.numeric(substring(raw,4894,4897))}
    if (year==2006) {dat$cmintvw <- as.numeric(substring(raw,6226,6229))}
    if (year==2011) {dat$cmintvw <- as.numeric(substring(raw,4926,4929))}
    if (year==2013) {dat$cmintvw <- as.numeric(substring(raw,5052,5055))}
    if (year==2015) {dat$cmintvw <- as.numeric(substring(raw,4490,4493))}
    if (year==2017) {dat$cmintvw <- as.numeric(substring(raw,3807,3810))}
    dat$year <- 1900+floor((dat$cmintvw-1)/12)

    #Month of data collection
    dat$month <- dat$cmintvw - (12 * (dat$year - 1900))
    dat$month <- factor(dat$month, levels = c(1:12), labels = c("January", "February", "March", "April", "May", "June",
                                                                "July", "August", "September", "October", "November", "December"),
                        ordered = TRUE)

    #Source file
    if (year==2002) {dat$file <- "2002FemResp.dat"}
    if (year==2006) {dat$file <- "2006_2010_FemRespData.dat"}
    if (year==2011) {dat$file <- "2011_2013_FemRespData.dat"}
    if (year==2013) {dat$file <- "2013_2015_FemRespData.dat"}
    if (year==2015) {dat$file <- "2015_2017_FemRespData.dat"}
    if (year==2017) {dat$file <- "2017_2019_FemRespData.dat"}

    #Source survey
    dat$survey <- "NSFG"

    #### Clean up ####
    #Reduce data
    dat <- dat[,c("cf_want", "famstat",  #Family status
                  "sex", "lgbt", "race", "hispanic", "age", "education", "partnered", "residence", "employed", "inschool",  #Demographics
                  "religion", "bother",  #Attitude
                  "id", "country", "weight", "cluster", "stratum", "file", "survey", "wave", "year", "month")]  #Design

    #Start data file, or append to existing data file
    if (year==min(years)) {data <- dat} else {data <- rbind(data, dat)}
    year.num <- year.num + 1
  }

  #### MALE RESPONDENT LOOP ####
  for (year in years) {

    #Increment progress bar
    if (progress) {utils::setTxtProgressBar(pb,year.num)}

    #Import raw data
    if (year==2002) {raw <- readLines("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2002Male.dat")}
    if (year==2006) {raw <- readLines("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2006_2010_Male.dat")}
    if (year==2011) {raw <- readLines("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2011_2013_MaleData.dat")}
    if (year==2013) {raw <- readLines("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2013_2015_MaleData.dat")}
    if (year==2015) {raw <- readLines("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2015_2017_MaleData.dat")}
    if (year==2017) {raw <- readLines("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NSFG/2017_2019_MaleData.dat")}

    #Initialize dataframe with id variable
    if (year==2002) {dat <- data.frame(id = as.character(substring(raw,1,12)))}
    if (year==2006 | year==2011 | year==2013 | year==2015 | year==2017) {dat <- data.frame(id = as.character(substring(raw,1,5)))}

    #### Family Status ####
    #Source variables
    if (year==2002) {
      dat$anykids <- as.numeric(substring(raw,2136,2136)) #Any biological or adopted children: 0 = No, 1 = Yes
      dat$rwant <- as.numeric(substring(raw,2414,2414)) #Wants a(nother) baby: 1 = Yes, 5 = No, 8 = Refused, 9 = Don't know
      dat$rstrstat <- as.numeric(substring(raw,114,114)) #Respondent's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$pstrstat <- as.numeric(substring(raw,220,220)) #Partner's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$intend <- as.numeric(substring(raw,2424,2424)) #Not partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$jintend <- as.numeric(substring(raw,2416,2416)) #Partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
    }
    if (year==2006) {
      dat$anykids <- as.numeric(substring(raw,3375,3375)) #Any biological or adopted children: 0 = No, 1 = Yes
      dat$rwant <- as.numeric(substring(raw,3745,3745)) #Wants a(nother) baby: 1 = Yes, 5 = No, 8 = Refused, 9 = Don't know
      dat$rstrstat <- as.numeric(substring(raw,163,163)) #Respondent's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$pstrstat <- as.numeric(substring(raw,314,314)) #Partner's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$intend <- as.numeric(substring(raw,3755,3755)) #Not partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$jintend <- as.numeric(substring(raw,3747,3747)) #Partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
    }
    if (year==2011) {
      dat$anykids <- as.numeric(substring(raw,3370,3370)) #Any biological or adopted children: 0 = No, 1 = Yes
      dat$rwant <- as.numeric(substring(raw,3750,3750)) #Wants a(nother) baby: 1 = Yes, 5 = No, 8 = Refused, 9 = Don't know
      dat$rstrstat <- as.numeric(substring(raw,179,179)) #Respondent's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$pstrstat <- as.numeric(substring(raw,328,328)) #Partner's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$intend <- as.numeric(substring(raw,3760,3760)) #Not partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$jintend <- as.numeric(substring(raw,3752,3752)) #Partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
    }
    if (year==2013) {
      dat$anykids <- as.numeric(substring(raw,3189,3189)) #Any biological or adopted children: 0 = No, 1 = Yes
      dat$rwant <- as.numeric(substring(raw,3592,3592)) #Wants a(nother) baby: 1 = Yes, 5 = No, 8 = Refused, 9 = Don't know
      dat$rstrstat <- as.numeric(substring(raw,180,180)) #Respondent's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$pstrstat <- as.numeric(substring(raw,327,327)) #Partner's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$intend <- as.numeric(substring(raw,3602,3602)) #Not partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$jintend <- as.numeric(substring(raw,3594,3594)) #Partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
    }
    if (year==2015) {
      dat$anykids <- as.numeric(substring(raw,2902,2902)) #Any biological or adopted children: 0 = No, 1 = Yes
      dat$rwant <- as.numeric(substring(raw,3316,3316)) #Wants a(nother) baby: 1 = Yes, 5 = No, 8 = Refused, 9 = Don't know
      dat$rstrstat <- as.numeric(substring(raw,152,152)) #Respondent's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$pstrstat <- as.numeric(substring(raw,280,280)) #Partner's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$intend <- as.numeric(substring(raw,3326,3326)) #Not partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$jintend <- as.numeric(substring(raw,3318,3318)) #Partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
    }
    if (year==2017) {
      dat$anykids <- as.numeric(substring(raw,2859,2859)) #Any biological or adopted children: 0 = No, 1 = Yes
      dat$rwant <- as.numeric(substring(raw,3276,3276)) #Wants a(nother) baby: 1 = Yes, 5 = No, 8 = Refused, 9 = Don't know
      dat$rstrstat <- as.numeric(substring(raw,167,167)) #Respondent's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$pstrstat <- as.numeric(substring(raw,290,290)) #Partner's sterility status: 0 = Not sterile, 1 = Surgically, 2 = Nonsurgically, 8 = Refused, 9 = Don't know
      dat$intend <- as.numeric(substring(raw,3286,3286)) #Not partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
      dat$jintend <- as.numeric(substring(raw,3278,3278)) #Partnered & fertile, Intends to have a(nother) baby: 1 = Yes, 5 = No, 7 = Not asked, 8 = Refused, 9 = Don't know
    }

    #Childfree (want)
    dat$cf_want <- NA
    dat$cf_want[which(dat$anykids==0 & dat$rwant==5)] <- 1  #Childfree
    dat$cf_want[which(dat$anykids==1 | dat$rwant==1 | dat$rwant==9)] <- 0  #Not childfree

    #Childfree (expect) - Unknown because intention question only asked of single respondents if they wanted children

    #Family status
    dat$famstat <- NA
    dat$famstat[which(dat$anykids==1)] <- 1  #Parent - Unclassified
    #Parent - Fulfilled: Unknown because parents who do not want another child could also be reluctant
    dat$famstat[which(dat$anykids==1 & dat$rwant==1)] <- 3 #Parent - Unfulfilled
    #Parent - Reluctant: Unknown because parents who do not want another child could also be fulfilled
    dat$famstat[which(dat$anykids==1 & dat$rwant==9)] <- 5 #Parent - Ambivalent

    dat$famstat[which(dat$anykids==0 & dat$rwant==1)] <- 6  #Not yet parent

    #Childless - Unclassified: Not used because all can be classified

    dat$famstat[which(dat$anykids==0 & dat$rwant==1 & dat$intend==5)] <- 8  #Childless - Social: Single respondent who wanted, but do not intend, to have children
    dat$famstat[which(dat$anykids==0 & dat$rwant==1 & dat$jintend==5)] <- 8  #Childless - Social: Partnered respondent who wanted, but do not intend, to have children

    dat$famstat[which(dat$anykids==0 & dat$rwant==1 & (dat$rstrstat==1 | dat$rstrstat==2))] <- 9  #Childless - Biological: Respondent who wanted, but is sterile
    dat$famstat[which(dat$anykids==0 & dat$rwant==1 & (dat$pstrstat==1 | dat$pstrstat==2))] <- 9  #Childless - Biological: Respondent who wanted, but who's partner is sterile

    dat$famstat[which(dat$anykids==0 & dat$rwant==9)] <- 11 #Undecided

    dat$famstat[which(dat$anykids==0 & dat$intend==5 & dat$rwant==9)] <- 10  #Ambivalent non-parent: Single respondent who does not intend, but does not know if wanted
    dat$famstat[which(dat$anykids==0 & dat$jintend==5 & dat$rwant==9)] <- 10  #Ambivalent non-parent: Partnered respondent who does not intend, but does not know if wanted

    dat$famstat[which(dat$anykids==0 & dat$rwant==5)] <- 12 #Childfree

    dat$famstat <- factor(dat$famstat, levels = c(1:12),
                          labels = c("Parent - Unclassified", "Parent - Fulfilled", "Parent - Unfulfilled", "Parent - Reluctant", "Parent - Ambivalent",
                                     "Not yet parent", "Childless - Unclassified", "Childless - Social", "Childless - Biological", "Ambivalent non-parent", "Undecided", "Childfree"))

    #### Demographics ####
    #Sex
    dat$sex <- 2
    dat$sex <- factor(dat$sex, levels = c(1,2,3), labels = c("Female", "Male", "Other"))

    #Sexual orientation
    dat$lgbt <- NA
    if (year==2002 | year==2006) {dat$orient <- NA}
    if (year==2011) {dat$orient <- as.numeric(substring(raw,4175,4175))}  #1 straight, 2 gay, 3 bi, 7 not asked, 8 refused, 9 don't know
    if (year==2013) {dat$orient <- as.numeric(substring(raw,4020,4020))}  #1 straight, 2 gay, 3 bi, 7 not asked, 8 refused, 9 don't know
    if (year==2015) {
      dat$a <- as.numeric(substring(raw,3737,3737))  #1 straight, 2 gay, 3 bi, 7 not asked, 8 refused, 9 don't know
      dat$a[is.na(dat$a)] <- 0  #Code missing as zero
      dat$b <- as.numeric(substring(raw,3738,3738))  #1 gay, 2 straight, 3 bi, 4 something else, 7 not asked, 8 refused, 9 don't know
      dat$b[which(dat$b==1 | dat$b==2)] <- 3 - dat$b[which(dat$b==1 | dat$b==2)]  #Reverse so 1 straight, 2 gay
      dat$b[is.na(dat$b)] <- 0  #Code missing as zero
      dat$orient <- dat$a + dat$b  #Combine versions a and b
    }
    if (year==2017) {
      dat$a <- as.numeric(substring(raw,3648,3648))  #1 straight, 2 gay, 3 bi, 7 not asked, 8 refused, 9 don't know
      dat$a[is.na(dat$a)] <- 0  #Code missing as zero
      dat$b <- as.numeric(substring(raw,3649,3649))  #1 gay, 2 straight, 3 bi, 4 something else, 7 not asked, 8 refused, 9 don't know
      dat$b[which(dat$b==1 | dat$b==2)] <- 3 - dat$b[which(dat$b==1 | dat$b==2)]  #Reverse so 1 straight, 2 gay
      dat$b[is.na(dat$b)] <- 0  #Code missing as zero
      dat$orient <- dat$a + dat$b  #Combine versions a and b
    }
    dat$lgbt <- factor(dat$orient, levels = c(1,2,3,4), labels = c("Straight", "Gay/Lesbian", "Bisexual", "Something else"))

    #Race
    if (year==2002) {
      dat$race <- as.numeric(substring(raw,17,17))
      dat$race <- factor(dat$race, levels = c(5,4,3,2,1,99,98), labels = c("White", "Black", "Hawaiian", "Asian", "American Indian", "Other", "Multi-racial"))
    }
    if (year==2006 | year==2011 | year==2013) {  #Coding assumes that 1/2/3 mean the same here as in 2002, despite not being labeled in the provided Stata .do code
      dat$race <- as.numeric(substring(raw,10,10))
      dat$race <- factor(dat$race, levels = c(5,4,3,2,1,96,99), labels = c("White", "Black", "Hawaiian", "Asian", "American Indian", "Other", "Multi-racial"))
    }
    if (year==2015 | year==2017) {  #Coding assumes values match female respondents (e.g., 3 = White); the provided Stata .do file seems incorrect
      dat$race <- as.numeric(substring(raw,10,10))
      dat$race <- factor(dat$race, levels = c(3,2,99,98,97,1,99), labels = c("White", "Black", "Hawaiian", "Asian", "American Indian", "Other", "Multi-racial"))
    }

    #Hispanic
    if (year==2002) {dat$hispanic <- as.numeric(substring(raw,16,16))}
    if (year==2006 | year==2011 | year==2013 | year==2015 | year==2017) {dat$hispanic <- as.numeric(substring(raw,9,9))}
    dat$hispanic[which(dat$hispanic==7)] <- NA  #Not asked
    dat$hispanic[which(dat$hispanic==8)] <- NA  #Refused
    dat$hispanic[which(dat$hispanic==9)] <- NA  #Don't know
    dat$hispanic[which(dat$hispanic==5)] <- 0  #Not hispanic
    dat$hispanic[which(dat$hispanic==1)] <- 1  #Hispanic

    #Age in years
    if (year==2002) {dat$age <- as.numeric(substring(raw,20,21))}
    if (year==2006) {dat$age <- as.numeric(substring(raw,14,15))}
    if (year==2011 | year==2013 | year==2015 | year==2017) {dat$age <- as.numeric(substring(raw,13,14))}
    dat$age[which(dat$age==98)] <- NA  #Refused
    dat$age[which(dat$age==99)] <- NA  #Don't know

    #Education in years
    if (year==2006) {dat$higrade <- as.numeric(substring(raw,36,37))}
    if (year==2002 | year==2011) {dat$higrade <- as.numeric(substring(raw,38,39))}
    if (year==2013) {dat$higrade <- as.numeric(substring(raw,35,36))}
    if (year==2015 | year==2017) {dat$higrade <- as.numeric(substring(raw,31,32))}
    dat$higrade[which(dat$higrade==98)] <- NA  #Refused
    dat$higrade[which(dat$higrade==99)] <- NA  #Don't know

    if (year==2002) {dat$dipged <- as.numeric(substring(raw,42,42))}
    if (year==2011) {dat$dipged <- as.numeric(substring(raw,41,41))}
    if (year==2006) {dat$dipged <- as.numeric(substring(raw,40,40))}
    if (year==2013) {dat$dipged <- as.numeric(substring(raw,38,38))}
    if (year==2015 | year==2017) {dat$dipged <- as.numeric(substring(raw,34,34))}
    dat$dipged[which(dat$dipged==8)] <- NA  #Refused
    dat$dipged[which(dat$dipged==9)] <- NA  #Don't know

    if (year==2011) {dat$havedeg <- as.numeric(substring(raw,66,66))}
    if (year==2013) {dat$havedeg <- as.numeric(substring(raw,63,63))}
    if (year==2006) {dat$havedeg <- as.numeric(substring(raw,65,65))}
    if (year==2002 | year==2015 | year==2017) {dat$havedeg <- as.numeric(substring(raw,47,47))}
    dat$havedeg[which(dat$havedeg==7)] <- NA  #Not asked
    dat$havedeg[which(dat$havedeg==8)] <- NA  #Refused
    dat$havedeg[which(dat$havedeg==9)] <- NA  #Don't know

    if (year==2011) {dat$degrees <- as.numeric(substring(raw,67,67))}
    if (year==2013) {dat$degrees <- as.numeric(substring(raw,64,64))}
    if (year==2006) {dat$degrees <- as.numeric(substring(raw,66,66))}
    if (year==2002 | year==2015 | year==2017) {dat$degrees <- as.numeric(substring(raw,48,48))}
    dat$degrees[which(dat$degrees==8)] <- NA  #Refused
    dat$degrees[which(dat$degrees==9)] <- NA  #Don't know

    dat$education <- NA
    dat$education[which(dat$higrade<=12)] <- 2  #Did not finish high school (yet)
    dat$education[which(dat$dipged==1 | dat$dipged==2 | dat$dipged==3)] <- 3  #High school graduate
    dat$education[which(dat$higrade>12 & (dat$havedeg==5 | dat$degrees==1))] <- 4  #Some college (no degree or associates degree)
    dat$education[which(dat$degrees==2)] <- 5  #College degree
    dat$education[which(dat$degrees==3 | dat$degrees==4 | dat$degrees==5)] <- 7  #Graduate degree
    dat$education <- factor(dat$education,
                            levels = c(1:7),
                            labels = c("No education", "Did not graduate high school", "High School graduate",
                                       "Some college", "College graduate", "Some post-graduate", "Graduate degree"),
                            ordered = TRUE)

    #Partnership status
    if (year==2002) {dat$marstat <- as.numeric(substring(raw,26,26))}
    if (year==2006) {dat$marstat <- as.numeric(substring(raw,22,22))}
    if (year==2011 | year==2013) {dat$marstat <- as.numeric(substring(raw,21,21))}
    if (year==2015 | year==2017) {dat$marstat <- as.numeric(substring(raw,23,23))}
    dat$partnered <- NA
    dat$partnered[which(dat$marstat==6)] <- 1  #Single, never married
    dat$partnered[which(dat$marstat==1 | dat$marstat==2)] <- 2  #Currently partnered
    dat$partnered[which(dat$marstat==3 | dat$marstat==4 | dat$marstat==5)] <- 3  #Formerly partnered
    dat$partnered <- factor(dat$partnered, levels = c(1,2,3), labels = c("Never", "Currently", "Formerly"))

    #Residence
    if (year==2002) {dat$metro <- as.numeric(substring(raw,2876,2876))}
    if (year==2006) {dat$metro <- as.numeric(substring(raw,4413,4413))}
    if (year==2011) {dat$metro <- as.numeric(substring(raw,4578,4578))}
    if (year==2013) {dat$metro <- as.numeric(substring(raw,4421,4421))}
    if (year==2015) {dat$metro <- as.numeric(substring(raw,4111,4111))}
    if (year==2017) {dat$metro <- as.numeric(substring(raw,4030,4030))}
    dat$residence <- NA
    dat$residence[which(dat$metro==1)] <- 4  #Principal city of MSA = Urban
    dat$residence[which(dat$metro==2)] <- 3  #Other part of MSA = Suburb
    dat$residence[which(dat$metro==3)] <- 1  #Not in MSA = Rural
    dat$residence <- factor(dat$residence, levels = c(1,2,3,4), labels = c("Rural", "Town", "Suburb", "Urban"), ordered = TRUE)

    #Employed
    if (year==2002) {dat$rwrkst <- as.numeric(substring(raw,2565,2565))}
    if (year==2006) {dat$rwrkst <- as.numeric(substring(raw,3924,3924))}
    if (year==2011) {dat$rwrkst <- as.numeric(substring(raw,3944,3944))}
    if (year==2013) {dat$rwrkst <- as.numeric(substring(raw,3787,3787))}
    if (year==2015) {dat$rwrkst <- as.numeric(substring(raw,3514,3514))}
    if (year==2017) {dat$rwrkst <- as.numeric(substring(raw,3488,3488))}
    dat$employed <- NA
    dat$employed[which(dat$rwrkst==1)] <- 1  #Employed
    dat$employed[which(dat$rwrkst==5)] <- 0  #Not employed

    #In school
    if (year==2002 | year==2011) {dat$goschol <- as.numeric(substring(raw,36,36))}
    if (year==2006) {dat$goschol <- as.numeric(substring(raw,34,34))}
    if (year==2013) {dat$goschol <- as.numeric(substring(raw,33,33))}
    if (year==2015 | year==2017) {dat$goschol <- as.numeric(substring(raw,29,29))}
    dat$inschool <- NA
    dat$inschool[which(dat$goschol==1)] <- 1  #In school
    dat$inschool[which(dat$goschol==5)] <- 0  #Not in school

    #### Attitude ####
    #Religion
    if (year==2002) {dat$relcurr <- as.numeric(substring(raw,2541,2542))}
    if (year==2006) {dat$relcurr <- as.numeric(substring(raw,3891,3892))}
    if (year==2011) {dat$relcurr <- as.numeric(substring(raw,3923,3924))}
    if (year==2013) {dat$relcurr <- as.numeric(substring(raw,3767,3768))}
    if (year==2015) {dat$relcurr <- as.numeric(substring(raw,3495,3496))}
    if (year==2017) {dat$relcurr <- as.numeric(substring(raw,3470,3471))}
    dat$religion <- NA
    dat$religion[which(dat$relcurr==1)] <- 1  #None
    dat$religion[which(dat$relcurr==2)] <- 2  #Catholic
    dat$religion[which(dat$relcurr==3)] <- 5  #Baptist/Southern Baptist ==> Protestant
    dat$religion[which(dat$relcurr==4)] <- 5  #Methodist, Lutheran, Presbyterian, Episcopal ==> Protestant
    dat$religion[which(dat$relcurr==5)] <- 5  #Fundamentalist Protestant ==> Protestant
    dat$religion[which(dat$relcurr==6)] <- 5  #Other Protestant denomination ==> Protestant
    dat$religion[which(dat$relcurr==7)] <- 5  #Protestant - No specific denomination ==> Protestant
    dat$religion[which(dat$relcurr==8)] <- 6  #Other
    dat$religion <- factor(dat$religion, levels = c(1:6), labels = c("None", "Catholic / Orthodox", "Muslim", "Jewish", "Protestant / Christian", "Other"))

    #Bother (If it turns out that you do not have any children, how much would it bother you?)
    if (year==2002) {dat$bother <- as.numeric(substring(raw,2596,2596))}
    if (year==2006) {dat$bother <- as.numeric(substring(raw,3956,3956))}
    if (year==2011) {dat$bother <- as.numeric(substring(raw,3969,3969))}
    if (year==2013) {dat$bother <- as.numeric(substring(raw,3813,3813))}
    if (year==2015) {dat$bother <- as.numeric(substring(raw,3533,3533))}
    if (year==2017) {dat$bother <- as.numeric(substring(raw,3507,3507))}
    dat$bother <- factor(dat$bother, levels = c(4,3,2,1), labels = c("Not at all", "A little", "Some", "A great deal"), ordered = TRUE)

    #### Design ####
    #Identifier - This step is performed above, when initializing the data frame

    #Country
    dat$country <- "United States"

    #Sampling weight
    if (year==2002) {dat$weight <- as.numeric(substring(raw,2927,2944))}
    if (year==2006) {dat$weight <- as.numeric(substring(raw,4446,4463))}
    if (year==2011) {dat$weight <- as.numeric(substring(raw,4593,4608))}
    if (year==2013) {dat$weight <- as.numeric(substring(raw,4436,4451))}
    if (year==2015) {dat$weight <- as.numeric(substring(raw,4126,4141))}
    if (year==2017) {dat$weight <- as.numeric(substring(raw,4044,4059))}

    #Cluster
    if (year==2002) {dat$cluster <- as.numeric(substring(raw,2945,2945))}
    if (year==2006) {dat$cluster <- as.numeric(substring(raw,4518,4518))}
    if (year==2011) {dat$cluster <- as.numeric(substring(raw,4609,4609))}
    if (year==2013) {dat$cluster <- as.numeric(substring(raw,4452,4452))}
    if (year==2015) {dat$cluster <- as.numeric(substring(raw,4142,4142))}
    if (year==2017) {dat$cluster <- as.numeric(substring(raw,4060,4060))}

    #Stratum
    if (year==2002) {dat$stratum <- as.numeric(substring(raw,2946,2947))}
    if (year==2006) {dat$stratum <- as.numeric(substring(raw,4519,4521))}
    if (year==2011) {dat$stratum <- as.numeric(substring(raw,4610,4612))}
    if (year==2013) {dat$stratum <- as.numeric(substring(raw,4453,4455))}
    if (year==2015) {dat$stratum <- as.numeric(substring(raw,4143,4145))}
    if (year==2017) {dat$stratum <- as.numeric(substring(raw,4061,4063))}

    #Wave
    if (year==2002) {dat$wave <- "2002"}
    if (year==2006) {dat$wave <- "2006-2010"}
    if (year==2011) {dat$wave <- "2011-2013"}
    if (year==2013) {dat$wave <- "2013-2015"}
    if (year==2015) {dat$wave <- "2015-2017"}
    if (year==2017) {dat$wave <- "2017-2019"}

    #Year of data collection
    if (year==2002) {dat$cmintvw <- as.numeric(substring(raw,2948,2951))}
    if (year==2006) {dat$cmintvw <- as.numeric(substring(raw,4522,4525))}
    if (year==2011) {dat$cmintvw <- as.numeric(substring(raw,4613,4616))}
    if (year==2013) {dat$cmintvw <- as.numeric(substring(raw,4456,4459))}
    if (year==2015) {dat$cmintvw <- as.numeric(substring(raw,4146,4149))}
    if (year==2017) {dat$cmintvw <- as.numeric(substring(raw,4064,4067))}
    dat$year <- 1900+floor((dat$cmintvw-1)/12)

    #Month of data collection
    dat$month <- dat$cmintvw - (12 * (dat$year - 1900))
    dat$month <- factor(dat$month, levels = c(1:12), labels = c("January", "February", "March", "April", "May", "June",
                                                                "July", "August", "September", "October", "November", "December"),
                        ordered = TRUE)

    #Source file
    if (year==2002) {dat$file <- "2002Male.dat"}
    if (year==2006) {dat$file <- "2006_2010_Male.dat"}
    if (year==2011) {dat$file <- "2011_2013_MaleData.dat"}
    if (year==2013) {dat$file <- "2013_2015_MaleData.dat"}
    if (year==2015) {dat$file <- "2015_2017_MaleData.dat"}
    if (year==2017) {dat$file <- "2017_2019_MaleData.dat"}

    #Source survey
    dat$survey <- "NSFG"

    #### Clean up ####
    #Reduce data
    dat <- dat[,c("cf_want", "famstat",  #Family status
                  "sex", "lgbt", "race", "hispanic", "age", "education", "partnered", "residence", "employed", "inschool",  #Demographics
                  "religion", "bother", #Attitude
                  "id", "country", "weight", "cluster", "stratum", "file", "survey", "wave", "year", "month")]  #Design

    #Append to existing data file from female respondent loop
    data <- rbind(data, dat)
    year.num <- year.num + 1
  }

  #Finalize
  if (progress) {close(pb)}  #Close progress bar
  class(data) <- c("data.frame", "childfree")
  return(data)  #Export data
}
