#' Read and recode UN Demographic and Health Surveys (DHS) individual data
#'
#' @param files vector: a character vector containing the paths for one or more Individual Recode DHS data files (see details)
#' @param extra.vars vector: a character vector containing the names of variables to be retained from the raw data
#' @param progress boolean: display a progress bar
#'
#' @details
#' The \href{https://www.dhsprogram.com/}{Demographic and Health Surveys} (DHS) program regularly collects
#'    health data from population-representative samples in many countries using standardized surveys since 1984. The
#'    "individual recode" data files contain women's responses, while the "men recode" files contain men's responses. These
#'    files are available in SPSS, SAS, and Stata formats from \href{https://www.dhsprogram.com/}{https://www.dhsprogram.com/},
#'    however access requires a \href{https://dhsprogram.com/data/Access-Instructions.cfm}{free application}. The `dhs()` function
#'    reads one or more of these files, extracts and recodes selected variables useful for studying childfree adults and other
#'    family statuses, then returns a single data frame.
#'
#' Although access to DHS data requires an application, the DHS program provides \href{https://dhsprogram.com/data/Download-Model-Datasets.cfm}{model datasets}
#'    for practice. The example provided below uses the model data file "ZZIR62FL.SAV", which contains fictitious women's data,
#'    but has the same structure as real DHS data files. The example can be run without prior application for data access.
#'
#' **Known issues**
#'   * The SPSS-formatted files containing data from Gabon Recode 4 (GAIR41FL.SAV, GAMR41FL.SAV) and Turkey Recode 4 (TRIR41FL.SAV, TRMR41FL.SAV)
#'     contain encoding errors. Use the SAS-formatted files (GAIR41FL.SAS7BDAT, GAMR41FL.SAS7BDAT, TRIR41FL.SAS7BDAT, TRMR41FL.SAS7BDAT) instead.
#'   * In some cases, DHS makes available individual recode data files for specific regions. For example, women's data from individual states
#'     in India from 1999 are contained in files named XXIR42FL.SAV, where the "XX" is a two-letter state code. The `dhs()` function has only
#'     been tested using whole-country files, and may not perform as expected for regional files.
#'   * Variables containing women's responses in the individual recode files begin with `v`, while variables containing men's responses in the
#'     men recode files begin with `mv`. When applying `dhs()` to both female and male data, these are automatically harmonized. However, if
#'     extra variables are requested using the `extra.vars` option, be sure to specify both names (e.g. `extra.vars = c("v201", "mv201")`).
#'
#' @return A data frame containing variables described in the codebook available using \code{vignette("codebooks")}
#'
#' @export
#'
#' @examples
#' \donttest{data <- dhs(files = c("ZZIR62FL.SAV"), extra.vars = c("v201"))}
dhs <- function(files, extra.vars = NULL, progress = TRUE) {

  if (length(files) > 1 & "ZZIR62FL.SAV" %in% files) {stop("Model data (file ZZIR62FL.SAV) should not be combined with files containing real data.")}

  if (!is.null(extra.vars)) {extra.vars <- tolower(extra.vars)}  #Make requested extra variables lowercase

  if (progress) {message("Processing DHS data files -")}
  if (progress) {pb <- utils::txtProgressBar(min = 0, max = length(files), initial = 0, style = 3)} #Initialize progress bar

  #Loop over each supplied data file
  for (file in 1:length(files)) {

    #Increment progress bar
    if (progress) {utils::setTxtProgressBar(pb,file)}

    #Import raw data
    if (files[file]=="ZZIR62FL.SAV") {  #Model file from https://dhsprogram.com/data/Download-Model-Datasets.cfm
      temp <- tempfile()
      utils::download.file(url = "https://osf.io/download/hk23e", destfile = temp)
      dat <- rio::import(temp, format = "sav")
    } else {dat <- rio::import(files[file])}
    colnames(dat) <- tolower(colnames(dat))  #Make all variables lowercase

    #Check type of file
    female <- NULL
    if (colnames(dat)[1]=="caseid" | colnames(dat)[1]=="case$id") {female <- TRUE}
    if (colnames(dat)[1]=="mcaseid" | colnames(dat)[1]=="mcase$id") {female <- FALSE}
    if (is.null(female)) {stop(paste0(files[file], " does not appear to be an individual or men DHS recode file."))}

    #### Family Status ####
    #Number of children
    if (female) {dat$numkid <- dat$v201} else {dat$numkid <- dat$mv201}

    #Want children
    if (female) {dat$want <- dat$v602} else {dat$want <- dat$mv602}
    dat$want[which(dat$want==0)] <- NA  #Zero doesn't seem to be a valid value
    dat$want[which(dat$want==4)] <- 5  #Combine 4-Sterilized with 5-Infecund
    dat$want[which(dat$want==7)] <- 5  #Combine 7-Man Infecund with 5-Infecund
    dat$want[which(dat$want>=6)] <- NA  #Various labels, none about wants
    dat$want <- factor(dat$want, levels = c(1,2,3,5), labels = c("Have (another)", "Undecided", "No (more)", "Infecund"))

    #Ideal number of children
    if (female) {dat$ideal <- dat$v613} else {dat$ideal <- dat$mv613}
    dat$ideal[which(dat$ideal==98)] <- -1  #Special code for "Don't Know"
    dat$ideal[which(dat$ideal>30)] <- NA  #Treat all values above 30 as missing (includes some undocumented country-specific special codes)

    #Childfree (want)
    dat$cf_want <- NA
    dat$cf_want[which(!is.na(dat$numkid) & dat$numkid==0 &
                      !is.na(dat$want) & dat$want=="No (more)")] <- 1  #Childfree if (a) have no children and (b) want no children
    dat$cf_want[which(!is.na(dat$numkid) & dat$numkid>0)] <- 0  #Not childfree if have children
    dat$cf_want[which(!is.na(dat$want) & (dat$want=="Have (another)" | dat$want=="Undecided"))] <- 0  #Not childfree if want or may want children

    #Childfree (ideal)
    dat$cf_ideal <- NA
    dat$cf_ideal[which(!is.na(dat$numkid) & dat$numkid==0 &
                       !is.na(dat$ideal) & dat$ideal==0)] <- 1  #Childfree if (a) have no children and (b) zero children is ideal
    dat$cf_ideal[which(!is.na(dat$numkid) & dat$numkid>0)] <- 0  #Not childfree if have children
    dat$cf_ideal[which(!is.na(dat$ideal) & (dat$ideal==-1 | dat$ideal>0))] <- 0  #Not childfree if it is ideal to have some number of children, or the ideal number of children is unknown

    #Family status
    dat$famstat <- NA
    dat$famstat[which(!is.na(dat$numkid) & dat$numkid>0)] <- 1  #Parent - Unclassified (known number of children greater than zero)
    dat$famstat[which(!is.na(dat$numkid) & dat$numkid>0 &
                      !is.na(dat$ideal) & dat$numkid==dat$ideal)] <- 2  #Parent - Fulfilled (has ideal number of children)
    dat$famstat[which(!is.na(dat$numkid) & dat$numkid>0 &
                      !is.na(dat$ideal) & dat$numkid<dat$ideal)] <- 3  #Parent - Unfulfilled (has less than ideal number of children)
    dat$famstat[which(!is.na(dat$numkid) & dat$numkid>0 &
                      !is.na(dat$ideal) & dat$numkid>dat$ideal & dat$ideal!=-1)] <- 4  #Parent - Reluctant (has more than ideal number of children)
    dat$famstat[which(!is.na(dat$numkid) & dat$numkid>0 &
                      !is.na(dat$ideal) & dat$ideal==-1)] <- 5  #Parent - Ambivalent (unsure how many children is ideal)

    #This may include a small number of "childless - unclassified/social" (respondent wants children, but does not intend to have them for non-medical/social reasons)
    dat$famstat[which(!is.na(dat$numkid) & dat$numkid==0 &
                      !is.na(dat$want) & dat$want=="Have (another)")] <- 6  #Not yet parent (wants child(ren), regardless of how many is ideal)

    dat$famstat[which(!is.na(dat$numkid) & dat$numkid==0 &
                      !is.na(dat$want) & dat$want=="Infecund" &
                      !is.na(dat$ideal) & dat$ideal>0)] <- 9  #Childless - Biological (cannot have children, but a specific number would have been ideal)

    dat$famstat[which(!is.na(dat$numkid) & dat$numkid==0 &
                      !is.na(dat$want) & dat$want=="Infecund" &
                      !is.na(dat$ideal) & dat$ideal==-1)] <- 10  #Ambivalent non-parent (cannot have children, ideal number is unknown)

    dat$famstat[which(!is.na(dat$numkid) & dat$numkid==0 &
                        !is.na(dat$want) & dat$want=="Undecided")] <- 11  #Undecided (unsure if want children)

    dat$famstat[which(!is.na(dat$numkid) & dat$numkid==0 &
                      !is.na(dat$want) & dat$want=="No (more)")] <- 12  #Childfree (do not want children)
    dat$famstat[which(!is.na(dat$numkid) & dat$numkid==0 &
                      !is.na(dat$ideal) & dat$ideal==0)] <- 12  #Childfree (zero children is ideal)

    dat$famstat[which(!is.na(dat$numkid) & dat$numkid==0 &
                      !is.na(dat$ideal) & dat$ideal==0 &
                      !is.na(dat$want) & dat$want!="No (more)")] <- 11  #Undecided, ideal and want responses are inconsistent
    dat$famstat[which(!is.na(dat$numkid) & dat$numkid==0 &
                      !is.na(dat$ideal) & dat$ideal>0 &
                      !is.na(dat$want) & dat$want=="No (more)")] <- 11  #Undecided, ideal and want responses are inconsistent

    dat$famstat <- factor(dat$famstat, levels = c(1:12),
                          labels = c("Parent - Unclassified", "Parent - Fulfilled", "Parent - Unfulfilled", "Parent - Reluctant", "Parent - Ambivalent",
                                     "Not yet parent", "Childless - Unclassified", "Childless - Social", "Childless - Biological", "Ambivalent non-parent", "Undecided", "Childfree"))

    #### Demographics ####
    #Sex
    if (female) {dat$sex <- 1} else {dat$sex <- 2}
    dat$sex <- factor(dat$sex, levels = c(1,2,3), labels = c("Female", "Male", "Other"))

    #Age in years
    if (female) {dat$age <- dat$v012} else {dat$age <- dat$mv012}

    #Education in years
    if (female) {dat$education <- dat$v133} else {dat$education <- dat$mv133}
    dat$education[dat$education>=40] <- NA

    #Partnership status
    if (female) {dat$partnered <- dat$v502 + 1} else {dat$partnered <- dat$mv502 + 1}
    dat$partnered <- factor(dat$partnered, levels = c(1,2,3), labels = c("Never", "Currently", "Formerly"))

    #Residence
    if (female) {
      dat$residence <- dat$v102
      if (dat$v000[1]=="MX" & dat$v007[1]==87) {  #Mexico Wave 1 used a different coding
        dat$residence[which(dat$residence<4)] <- 2  #Code as rural (1) Less than 2500, (2) 2500-19999, and (3) 20000+
        dat$residence[which(dat$residence==4)] <- 1  #Code as urban (4) Areas Metropolitanas
      }
    }
    if (!female) {
      dat$residence <- dat$mv102
      if (dat$mv000[1]=="MX" & dat$mv007[1]==87) {  #Mexico Wave 1 used a different coding
        dat$residence[which(dat$residence<4)] <- 2  #Code as rural (1) Less than 2500, (2) 2500-19999, and (3) 20000+
        dat$residence[which(dat$residence==4)] <- 1  #Code as urban (4) Areas Metropolitanas
      }
    }
    dat$residence <- factor(dat$residence, levels = c(2,98,99,1), labels = c("Rural", "Town", "Suburb", "Urban"), ordered = TRUE)  #98 and 99 are dummy values; "Town" and "Suburb" categories are not used by DHS

    #Employed
    if (female) {dat$employed <- dat$v714} else {dat$employed <- dat$mv714}

    #### Attitude ####
    #Religion
    dat$religion <- NA

    if (female) {x <- as.data.frame(attr(dat$v130, "labels"))} else {x <- as.data.frame(attr(dat$mv130, "labels"))}  #Get file-specific dictionary (o = old value, l = label, n = new value)
    if (nrow(x) > 0) {  #If there are labeled values for religions...
      x$label <- rownames(x)
      colnames(x) <- c("o", "l")
      x$n <- NA

      for (i in 1:nrow(x)) {  #For each old label, identify new value
        if (x$l[i] %in% c("Agnostic", "Atheist", "DK", "Don t know", "Don't know", "No religion", "No Religion", "No religion (Sem religiao)",
                          "No religion/atheists", "No religion/none", "None", "NONE", "Not religion", "Not Religious", "Not religious",
                          "Sans", "Sem religio")) {x$n[i] <- 1}  #None

        if (x$l[i] %in% c("Catholic", "Catholic (Cat\U00A2lica)", "Catholic/greek cath.", "Catholicism", "Catholique", "Catolica romana",
                          "Christian Catholic", "Christian Orthodox", "Orthodox", "Roman Catholic", "Roman catholic",
                          "Roman Catholic church")) {x$n[i] <- 2}  #Catholic/Orthodox

        if (x$l[i] %in% c("Bektashi", "Islam", "Islamic", "Islamic (Mu\U2021ulman)", "Moslem", "Mulsim", "Muslem", "Muslim",
                          "muslim", "Muslim/Islam", "Muslin", "Muslman", "Muslum", "Musulman", "Musulmane")) {x$n[i] <- 3}  #Muslim

        if (x$l[i] %in% c("Jew or Isreaeli", "Jewish", "Judaica ou israelita", "Judaism", "Zion", "Zionist")) {x$n[i] <- 4}  #Jewish

        if (x$l[i] %in% c("\"Celestes\"", "7th Day adventist", "Adventist", "Adventist/Jehova", "Adventiste", "Adventiste/Jehova",
                          "African instituted churches", "Aglipay", "Anglican", "Anglican Church", "Apostolic sect", "Apostolic Sect",
                          "Arm,e du Salut", "Assembly of god", "Assembly of God", "Aventist", "Baptist", "Born Again Christian (other recode)",
                          "Born-again/Jehovah's Witness/SDA", "Budu", "CCAP", "Celestes", "Celestes (Celestial Church of Christ)", "Charismatic",
                          "Chistiane", "Christan", "Christian", "christian", "Christian Protestant", "Christian/protestant", "Christianity",
                          "Christrian", "Eglise de r\U00E9veil", "Eglise du 7e jour", "Elcin", "Evangelic", "Evangelica (Crente)", "Evangelical",
                          "Evangelical / Protestant", "Evangelical Alliance", "Evangelical churches", "Evangelical Lutheran", "Evangelical presbyterian",
                          "Evangelical/pentecostal", "Evangelist", "FJKM/FLM/Anglikana", "Iglesia ni Cristo", "Iglesia ni kristo", "Iglesia Ni Kristo",
                          "Jahovai", "JEHOVAH witness", "Jehovah witness", "Jehovah Witness", "Jehovah's Witness", "Jehovah's Witness (other recode)",
                          "Jehovah's witnesses", "Jeova witness", "Kibanguist", "Kimbanguist", "Kimbanguiste", "Lesotho Evangelical church",
                          "Method., Advent., ..", "Methodist", "Methodist/Baptist", "New apostolic", "Other Christian", "Other christian",
                          "other Christian", "Other Christian (not otherwise categorisable)", "Other Christian Church", "Other Christian religion",
                          "Other christians", "Other Christians", "Other chritians", "Other protestant", "Other Protestant", "Other Protestants",
                          "Pentecostal", "Pentecostal/Born Again/Evangelical", "Pentecostal/Charismatic", "Pentecostal/charismatic", "Pentecotist",
                          "Presbyterian", "Prostestant", "Protest /Oth Cristian", "Protestant", "Protestant (ex. evangelical, baptist, jehovah witness)",
                          "Protestant (Protestante)", "Protestant / Evangelic", "Protestant /Christian", "Protestant methodist", "Protestant Methodist",
                          "Protestant presbyterian, methodist", "Protestant, methodist, adventist, witness of Jesus", "Protestant/ methodist/adventist/Jehova witness",
                          "Protestant/ other Christian", "Protestant/ Other Christian", "Protestant/Anglican", "Protestant/FLM", "Protestant/other Christian",
                          "Protestant/other christian", "Protestanta", "Protestante", "Protestantism", "Protestants", "Rastafarian", "Salvation Army",
                          "Salvation army", "SDA", "Seventh Day Advent", "Seventh day advent.", "Seventh Day Advent./Baptist", "Seventh Day Advent/ Baptist",
                          "Seventh Day Adventist", "Seventh Day Adventist (other recode)", "Seventh Day Adventist / Baptist", "Seventh Day Adventist/Baptist",
                          "Seventh-day adventist", "Trad. prosestant", "Tradit. protestant", "United Church", "Universal")) {x$n[i] <- 5}  #Protestant

        if (x$l[i] %in% c("Aucune", "Autre", "Autres", "Baha'i", "Bahai", "Confucian", "Espirita Kardecista", "Espiritista kardecis",
                          "Jain", "Mammon", "Mana", "New Religions (Eglises Rebeillees)", "Non-Christian", "Only god", "Oriental religions",
                          "Other", "other", "Other (Outra)", "Other non-Christian", "Other religion", "Other religions", "Others", "Outras",
                          "Parsi / Zoroastrian", "Parsi/Zoroastrian", "Religioes orientais", "Revival church", "Sect", "Sikh", "Spiritual",
                          "Spiritual kardecista", "Spiritualist", "Zephirin/Matsouaniste/Ngunza", "Zephirrin/Matsouanist/Ngunza",
                          "Zoroastian/Parsi", "Animalist", "Animist", "Animiste", "Cao Dai", "Doni-Polo", "Donyi polo", "Espirita Afro-Bras.",
                          "Espiritista afro-bra", "Indigenous spirituality", "Kirat", "Mayan", "Nature worship", "Other traditional",
                          "Religion traditionelle", "Sanamahi", "Taditional", "Tradition/animist", "Traditional", "Traditional (Vodoun)",
                          "Traditional / animist", "Traditional Mayan", "Traditional religion", "Traditional Religion", "Traditional/animist",
                          "Traditional/Animist", "Traditional/spiritualist", "Traditionalist", "Traditionelle", "Traditionists", "Traditionnal",
                          "Traditionnal/animist", "Umbanda /Candomble", "Vaudou", "Vaudousant", "Vodoun")) {x$n[i] <- 6}  #Other

        if(x$l[i] %in% c("Buddhism", "Buddhist", "Buddhist / Neo-Buddhist", "Buddhist/Neo Buddhist", "Buddhist/Neo-Buddhist",
                         "Budhist", "Hoa Hao")) {x$n[i] <- 7}  #Buddhist

        if (x$l[i] %in% c("Hindu", "Hinduism")) {x$n[i] <- 8}  #Hindu

        if (female) {dat$religion[which(dat$v130==x$o[i])] <- x$n[i]} else {dat$religion[which(dat$mv130==x$o[i])] <- x$n[i]}  #Insert new value into recoded religion variable
      }
    }

    dat$religion <- factor(dat$religion, levels = c(1:8), labels = c("None", "Catholic / Orthodox", "Muslim", "Jewish",
                                                                     "Protestant / Christian", "Other", "Buddhist", "Hindu"))

    #### Design ####
    #Identifier (non-standard variable name in Egypt 1988-89)
    if (female) {if (dat$v000[1]=="EG" & (dat$v007[1]==88 | dat$v007[1]==89)) {dat$id <- dat$`case$id`} else {dat$id <- dat$caseid}}
    if (!female) {if (dat$mv000[1]=="EG" & (dat$mv007[1]==88 | dat$mv007[1]==89)) {dat$id <- dat$`mcase$id`} else {dat$id <- dat$mcaseid}}
    dat$id <- as.character(dat$id)

    #Country
    country.codes <- c("AF", "AL", "AO", "AM", "AZ", "BD", "BJ", "BO", "BT", "BR", "BF", "BU", "KH", "CM", "CV", "CF", "TD", "CO", "KM", "CG",
                       "CD", "CI", "DR", "EC", "EG", "ES", "EK", "ER", "ET", "GA", "GM", "GH", "GU", "GN", "GY", "HT", "HN", "IA", "ID", "JO",
                       "KK", "KE", "KY", "LA", "LS", "LB", "MD", "MW", "MV", "ML", "MR", "MX", "MB", "MA", "MZ", "MM", "NM", "NP", "NC", "NI",
                       "NG", "OS", "PK", "PY", "PE", "PH", "RW", "WS", "ST", "SN", "SL", "ZA", "LK", "SD", "SZ", "TJ", "TZ", "TH", "TL", "TG",
                       "TT", "TN", "TR", "TM", "UG", "UA", "UZ", "VN", "YE", "ZM", "ZW", "PG", "ZZ")
    country.names <- c("Afghanistan", "Albania", "Angola", "Armenia", "Azerbaijan", "Bangladesh", "Benin", "Bolivia", "Botswana", "Brazil", "Burkina Faso", "Burundi", "Cambodia", "Cameroon", "Cape Verde", "Central African Republic", "Chad", "Columbia", "Comoros", "Congo",
                       "Congo Democratic Republic", "Cote d'Ivoire", "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guatamala", "Guinea", "Guyana", "Haiti", "Honduras", "India", "Indonesia", "Jordan",
                       "Kazakhstan", "Kenya", "Kyrgyz Republic", "Lao People's Democratic Republic", "Lesotho", "Liberia", "Madagascar", "Malawi", "Maldives", "Mali", "Mauritania", "Mexico", "Moldova", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nepal", "Nicaragua", "Niger",
                       "Nigeria", "Nigeria (Ondo State)", "Pakistan", "Paraguay", "Peru", "Philippines", "Rwanda", "Samoa", "Sao Tome and Principe", "Senegal", "Sierra Leone", "South Africa", "Sri Lanka", "Sudan", "Swaziland", "Tajikstan", "Tanzania", "Thailand", "Timor-Leste", "Togo",
                       "Trinidad and Tobago", "Tunisia", "Turkey", "Turkministan", "Uganda", "Ukraine", "Uzbekistan", "Vietnam", "Yemen", "Zambia", "Zimbabwe", "Papua New Guinea", "Fake Country")
    if (female) {dat$country <- country.names[match(substr(dat$v000,1,2), country.codes)]} else {dat$country <- country.names[match(substr(dat$mv000,1,2), country.codes)]}

    #Sampling weight
    if (female) {dat$weight <- dat$v005/1000000} else {dat$weight <- dat$mv005/1000000}

    #Wave (called "Recode" in the DHS)
    if (female) {
      dat$wave <- as.numeric(substr(dat$v000,3,3))
      dat$wave[which(dat$country=="Vietnam" & dat$v007==97)] <- 3  #Recode was labeled as "T" for Vietnam 1997
      dat$wave[which(dat$country=="Vietnam" & dat$v007==2)] <- 4  #Recode was labeled as "T" for Vietnam 2002
    }
    if (!female) {
      dat$wave <- as.numeric(substr(dat$mv000,3,3))
      dat$wave[which(dat$country=="Vietnam" & dat$mv007==97)] <- 3  #Recode was labeled as "T" for Vietnam 1997
      dat$wave[which(dat$country=="Vietnam" & dat$mv007==2)] <- 4  #Recode was labeled as "T" for Vietnam 2002
    }
    dat$wave[which(is.na(dat$wave))] <- 1  #In recode 1, v000 only contained the country code

    #Year and month of data collection
    if (female) {dat$cmc <- dat$v008} else {dat$cmc <- dat$mv008}  #Century month code
    if (dat$country[1]=="Ethiopia") {dat$cmc <- dat$cmc + 92}  #Correction for Ethiopian calendar (https://dhsprogram.com/data/Guide-to-DHS-Statistics/Organization_of_DHS_Data.htm)
    if (dat$country[1]=="Nepal" & dat$wave[1]!=3) {dat$cmc <- dat$cmc - 681}  #Correction for Nepali calendar (https://dhsprogram.com/data/Guide-to-DHS-Statistics/Organization_of_DHS_Data.htm)
    if (dat$country[1]=="Nepal" & dat$wave[1]==3) {dat$cmc <- dat$cmc + 519}
    if (dat$country[1]=="Afghanistan") {dat$cmc <- dat$cmc + 255}  #Correction for Afghani calendar (https://dhsprogram.com/data/Guide-to-DHS-Statistics/Organization_of_DHS_Data.htm)
    dat$year <- 1900+floor((dat$cmc-1)/12)
    dat$month <- dat$cmc - (12 * (dat$year - 1900))
    dat$month <- factor(dat$month, levels = c(1:12), labels = c("January", "February", "March", "April", "May", "June",
                                                                "July", "August", "September", "October", "November", "December"),
                        ordered = TRUE)

    #Source file
    dat$file <- files[file]

    #Source survey
    dat$survey <- "DHS"

    #### Clean up ####
    #Check for extra variables; if not present, add them
    for (var in extra.vars) {if (!(var %in% colnames(dat))) {  #For each extra variable that is not present:
      dat$v8675309 <- NA   #(1) Add a temporary missing variable called `v8675309`
      colnames(dat)[length(colnames(dat))] <- var  #(2) Rename this new missing variable with the correct name
      }
    }

    #Reduce data
    if (!is.null(extra.vars)) {
      dat <- dat[,c("cf_want", "cf_ideal", "famstat",  #Family status
                    "sex", "age", "education", "partnered", "residence", "employed",  #Demographics
                    "religion",  #Attitude
                    "id", "country", "weight", "file", "survey", "wave", "year", "month",  #Design
                    extra.vars)]
    } else {
      dat <- dat[,c("cf_want", "cf_ideal", "famstat",  #Family status
                    "sex", "age", "education", "partnered", "residence", "employed",  #Demographics
                    "religion",  #Attitude
                    "id", "country", "weight", "file", "survey", "wave", "year", "month")]  #Design
    }

    #Start data file, or append to existing data file
    if (file==1) {data <- dat} else {data <- rbind(data, dat)}

  }

  #Finalize
  if (progress) {close(pb)}  #Close progress bar
  class(data) <- c("data.frame", "childfree")
  return(data)  #Export data
}
