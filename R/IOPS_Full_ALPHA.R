
#requireNamespace("roxygen2")
requireNamespace("Rcpp")
requireNamespace("dplyr")
requireNamespace("tidyr")
requireNamespace("readxl")
requireNamespace("economiccomplexity")
requireNamespace("usethis")
requireNamespace("Matrix")
requireNamespace("tibble")

library("Rcpp")
library("tidyr")
library("dplyr")
library("readxl")
library("economiccomplexity")
library("usethis")
library("roxygen2")

#' IOPS
#'
#' @description  Takes user inputted trade data, any acceptable ISO country code and industrial value chain mapping to calculate various metrics (Economic- and Product complexity metrics, distance metrics, opportunity gain, and inequality metrics) of a given country in order to facilitate better decision making regarding industrial policymaking.
#'
#' @import dplyr
#' @import Rcpp
#' @import tidyr
#' @import readxl
#' @import usethis
#' @import economiccomplexity
#' @import roxygen2
#' @importFrom stats aggregate
#' @importFrom utils write.csv
#' @importFrom utils read.csv
#'
#' @param CountryCode (Type: character/integer) Any accepted ISO country code could be used, e.g. \code{"United Kingdom"}, \code{"GBR"}, \code{"GB"}, \code{828} would all be accepted if the United Kingdom is the desired country.
#' @param tradeData (Type: csv) Accepts any CEPII BACI trade data. NOTE: tradeData and GVCMapping must be from the same "H" Family, e.g. both are from  H3, etc., in order for the program to work correctly.
#' @param ComplexMethod (Type: character) Methods used to calculate complexity measures. Can be any one of these methods: \code{"fitness"}, \code{"reflections"} or \code{"eigenvalues"}. Defaults to "eigenvalues".
#' @param iterCompl (Type: integer) The number of iterations that the chosen complexity measure must use. Defaults to \code{iterCompl = 20}.
#' @param GVCMapping (Type: csv) The desired value chain to be analysed. With Columns "Tiers", "Activity", and "HSCode". NOTE: tradeData and GVCMapping must be from the same "H" Family, e.g. both are from  H3, etc., in order for the program to work correctly.
#'
#' @return A list that constains ECI, PCI, Opportunity_Gain, distance, density, M_absolute, M_binary, Tier_Results, Product_Category_Results, Product_Results, respectively.
#' @export


IOPS <- function(CountryCode ,tradeData , ComplexMethod = "eigenvalues", iterCompl = 20, GVCMapping){

  #tradeData2018 <- read.csv("H5/BACI_HS17_Y2018_V202001.csv", header = TRUE)

  CountryDataPath <- system.file("extdata", "country_codes_V202201.csv", package = "iopspackage")

  AllCountryCodes <- read.csv(CountryDataPath)

  countryChosen <- AllCountryCodes %>%
    filter(country_code == CountryCode | country_name_abbreviation == CountryCode | country_name_full == CountryCode | iso_2digit_alpha == CountryCode| iso_3digit_alpha == CountryCode)

  #Determine country's 3-digit country code
  chosenCountry <- as.character(countryChosen["iso_3digit_alpha"])
  #----------------------------- Import Trade Data -----------------------------
  #Determine country's 3-digit country code

  message(paste0("Selected country: ", chosenCountry))
  #+++++++++++++++++++++++++++

  # read in trade data
  tradeData2018 <- tradeData
  #colnames(tradeData2018) <- c("year", "location_code", "hs_product_code", "export_value", "import_value", "export_RCA", "import_RCA")
  #tradeData_Export <- as.data.frame(matrix(rep(0, times = nrow(tradeData2018)*4), nrow = nrow(tradeData), ncol = 4))
  tradeData2018 <- tradeData2018[c("t","v","i","k")]
  colnames(tradeData2018) <- c("year","export_value","country_code","hs_product_code")

  #+++++++++++ Merge data sets (for compatibility) ++++++++++++++

  ISO_Country <-AllCountryCodes[,c(1, 5)]
  colnames(ISO_Country)[2] <- "location_code"

  common_col_names <- intersect(names(tradeData2018), names(ISO_Country))

  MergedTradeData <- merge(tradeData2018, ISO_Country, by = common_col_names)

  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  tradeData2018 <- MergedTradeData[,c("year","export_value","location_code","hs_product_code")]

  message("Trade data succesfully imported")

  #======read in industry value chain mapping======
  #GVCMapping <- read.csv("H5/Auto Value Chain - updated for H5 - v2.csv", header=FALSE)
  colnames(GVCMapping) <- c("tierNumber", "GVCactivityNumber", "HScode")
  #GVCMapping: || tierNumber | GVCactivityNumber | HScode ||

  message("Value chain succesfully imported")

  #_______________________________________________________________________________________________

  expdata <- tradeData2018[ ,c(2, 3, 4)]
  #expdata <- tradeData2018[ ,c(4, 2, 3)]
  wide_expdata <- expdata %>% pivot_wider(names_from = hs_product_code, values_from = export_value, values_fn = sum, names_sort = TRUE, values_fill = 0)  # transform the data so that all data for one country is contained in a single row

  #----Get Mbin, Mabs, complexities, CSRCA-----

  # create matrix of RCA values (Mabs) and binary RCA matrix (Mbin)
  Mabs <- balassa_index(expdata, discrete = FALSE, country = "location_code", product = "hs_product_code", value = "export_value")
  Mbin <- balassa_index(expdata, country = "location_code", product = "hs_product_code", value = "export_value")

  M_binary <- as.matrix(Mbin)
  M_absolute <- as.matrix(Mabs)

  message("M-matrices calculated")

  Products <- unique(expdata$hs_product_code)   # 1219 product codes, already sorted
  Countries <- rownames(Mabs)   # 235 countries, alphabetically ordered

  # create a matrix containing the RCA data for the country being analysed

  # chosen country subset: wide_expdata[which(wide_expdata$location_code == chosenCountry), -1]
  CS_RCAmat <- cbind.data.frame(colnames(wide_expdata[which(wide_expdata$location_code == chosenCountry), -1]), as.numeric(wide_expdata[which(wide_expdata$location_code == chosenCountry), -1]), as.numeric(colSums(wide_expdata[ ,-1])), as.numeric(Mabs[chosenCountry, ]))
  colnames(CS_RCAmat) <- c("hs_product_code", paste(chosenCountry, "export_of_product", sep = "_"), "World_export_of_product", paste(chosenCountry, "RCA", sep = "_"))
  # || hs_product_code | ZAF_export_of_product | World_export_of_product | ZAF_RCA ||

  message("RCA calculated")

  #+++++++++++++++++++++++++++COMPLEXITY MEASURES++++++++++++++++++++++++++++++#

  #User selects one of three methods to calculate complexities,
  if (ComplexMethod == "eigenvalues"){

    message(paste0("Starting calculation of complexity measures using the '", ComplexMethod, "' method" ))

    # use binary M matrix to calculate complexity using EIGENVALUES, number of iterations must be provided
    #NOTE: This specific method takes a significantly long time to compute
    Compl_Meth <- complexity_measures(Mbin, method = "eigenvalues")

    Method <- "eigv" #method descriptor for Excel/CSV file

  } else if(ComplexMethod == "reflections") {

    message(paste0("Starting calculation of complexity measures using the '", ComplexMethod, "' method" ))

    # use binary M matrix to calculate complexity using METHOD OF REFLECTIONS, number of iterations must be provided
    Compl_Meth <- complexity_measures(Mbin, method = "reflections", iterations = iterCompl)

    Method <- "refl"

  } else {

    message(paste0("Starting calculation of complexity measures using the '", ComplexMethod, "' method" ))

    # use binary M matrix to calculate complexity using FITNESS COMPLEXITY, number of iterations must be provided
    Compl_Meth <- complexity_measures(Mbin, method = "fitness", iterations = iterCompl)

    Method <- "fitn"
  }

  #determine complexities with input method
  ECI <- Compl_Meth$complexity_index_country
  PCI <- Compl_Meth$complexity_index_product

  #-----Get proximity, opp gain, opp val, distance, density-----

  # calculate the proximity between products
  proxes <- proximity(Mbin)

  Proximities <- proxes$proximity_product
  Similarities <- proxes$proximity_country

  # compute the opportunity value
  opporVal <- complexity_outlook(Mbin, Proximities, PCI)
  cntryOpporVal <- opporVal$complexity_outlook_index
  opporGain <- opporVal$complexity_outlook_gain
  Opportunity_Gain <- as.matrix(opporGain)

  # compute the densities and distances
  # ______ (1 - Mbin) creates matrix of ones where Mbin has zeroes and vice versa
  distance <- tcrossprod(1 - as.matrix(Mbin), as.matrix(Proximities)/rowSums(as.matrix(Proximities)))
  density <- tcrossprod(as.matrix(Mbin), as.matrix(Proximities)/rowSums(as.matrix(Proximities)))

  message("Complexity measures calculated")

  #-----create GVCfull, GVCacts, GVCtiers-----

  message("Starting IO-PS calculations")

  GVCMapping <- GVCMapping[order(GVCMapping$tierNumber, GVCMapping$GVCactivityNumber, GVCMapping$HScode), ]   # reorder GVCMapping according to tier, then activity, then HScode

  numTiers <- length(unique(GVCMapping$tierNumber))         # = 5
  numActs <- length(unique(GVCMapping$GVCactivityNumber))   # = 49
  numProds <- length(unique(GVCMapping$HScode))             # = 272

  numProdsInAct <- as.matrix(GVCMapping[ ,c(2, 3)] %>% group_by(GVCactivityNumber) %>% tally())[ ,2]
  numActsInTier <- as.matrix(unique(GVCMapping[ ,c(1, 2)]) %>% group_by(tierNumber) %>% tally())[ ,2]
  numProdsInTier <- as.matrix(GVCMapping[ ,c(1, 3)] %>% group_by(tierNumber) %>% tally())[ ,2]


  #-----GVCfull-----
  GVCfull <- as.data.frame(matrix(rep(0, times = numProds*15), nrow = numProds, ncol = 15))   # initialise GVCfull
  colnames(GVCfull) <- c("tierNumber", "GVCactivityNumber", "HScode", "Complexity", "Density", "RCA", "Complexity_ifOpp", "Distance_ifOpp", "OpporGain_ifOpp", "RCA_ifOpp", paste(chosenCountry, "export_of_product", sep = "_"), "World_export_of_product", paste(chosenCountry, "export_of_product_ifOpp", sep = "_"), "World_export_of_product_ifOpp", "productPosition")

  GVCfull[ , c(1, 2, 3)] <- GVCMapping
  GVCfull <- GVCfull[order(GVCfull$HScode), ]   # order according to HS code

  GVCfull[ , 4] <- as.numeric(PCI[which(Products %in% GVCMapping$HScode)])
  GVCfull[ , 5] <- as.numeric(density[chosenCountry, which(Products %in% GVCMapping$HScode)])   # for distances and densities, subset the one row that is specific to the country being analysed
  GVCfull[ , 6] <- CS_RCAmat[which(Products %in% GVCMapping$HScode), 4]
  GVCfull[ , c(11, 12)] <- CS_RCAmat[which(Products %in% GVCMapping$HScode), c(2, 3)]
  GVCfull[ , 15] <- which(Products %in% GVCMapping$HScode)

  GVCfull[which(GVCfull$RCA < 1), c(7, 10, 13, 14)] <- GVCfull[which(GVCfull$RCA < 1), c(4, 6, 11, 12)]

  GVCfull[ , 8] <- as.numeric(distance[chosenCountry, which(Products %in% GVCMapping$HScode)])
  GVCfull[-which(GVCfull$RCA < 1), 8] <- 0

  GVCfull[ , 9] <- as.numeric(opporGain[chosenCountry, which(Products %in% GVCMapping$HScode)])
  GVCfull[-which(GVCfull$RCA < 1), 9] <- 0

  GVCfull <- GVCfull[order(GVCfull$tierNumber, GVCfull$GVCactivityNumber, GVCfull$HScode), ]

  #-----GVCacts-----

  numOppProdsInAct <- rep(0, times = length(numProdsInAct))

  numOppProdsInAct[as.matrix(GVCMapping[which(GVCMapping$HScode %in% GVCfull[which(GVCfull$RCA < 1), 3]), ] %>% group_by(GVCactivityNumber) %>% tally())[ ,1]] <- as.matrix(GVCMapping[which(GVCMapping$HScode %in% GVCfull[which(GVCfull$RCA < 1), 3]), ] %>% group_by(GVCactivityNumber) %>% tally())[ ,2]

  #in case of mismatch between data lengths
  if (length(numProdsInAct)<length(numOppProdsInAct)) {
    stop("Error: data selected for 'tradeData' and 'GVCmapping' isn't compatible with each other. Select data within the same 'H' class, i.e., 'H0', 'H3', or 'H5'")
  }

  GVCacts <- as.data.frame(matrix(rep(0, times = numActs*15), nrow = numActs, ncol = 15))   # initialise GVCacts
  colnames(GVCacts) <- c("tierNumber", "GVCactivityNumber", "AvgComplexity", "AvgDensity", "AvgRCA", "AvgComplexity_ifOpp", "AvgDistance_ifOpp", "AvgOpporGain_ifOpp", "AvgRCA_ifOpp", paste(chosenCountry, "export_of_activity", sep = "_"), "World_export_of_activity", paste(chosenCountry, "export_of_activity_ifOpp", sep = "_"), "World_export_of_activity_ifOpp", "NumberOfProductsInActivity", "NumberOfOppProductsInActivity")

  GVCacts[ , c(1, 2)] <- unique(GVCMapping[ ,c(1, 2)])
  GVCacts[ , c(3:5)] <- aggregate(GVCfull[ , c(4:6)], by = list(activity = GVCfull$GVCactivityNumber), FUN = sum)[, -1]/numProdsInAct   # also works: #aggregate(GVCfull[ , c(4:6)], by = list(activity = GVCfull$GVCactivityNumber), FUN = mean)[ , -1]
  GVCacts[-which(numOppProdsInAct == 0) , c(6:9)] <- aggregate(GVCfull[-which(GVCfull$GVCactivityNumber %in% which(numOppProdsInAct == 0)) , c(7:10)], by = list(activity = GVCfull$GVCactivityNumber[-which(GVCfull$GVCactivityNumber %in% which(numOppProdsInAct == 0))]), FUN = sum)[, -1]/(numOppProdsInAct[-which(numOppProdsInAct == 0)])
  GVCacts[ , c(10:13)] <- aggregate(GVCfull[ , c(11:14)], by = list(activity = GVCfull$GVCactivityNumber), FUN = sum)[ , -1]
  GVCacts[ , 14] <- numProdsInAct
  GVCacts[ , 15] <- numOppProdsInAct


  #-----GVCtiers-----

  numOppProdsInTier <- rep(0, times = length(numActsInTier))
  numOppActsInTier <- rep(0, times = length(numActsInTier))

  numOppProdsInTier[as.matrix(GVCMapping[which(GVCMapping$HScode %in% GVCfull[which(GVCfull$RCA < 1), 3]), ] %>% group_by(tierNumber) %>% tally())[ ,1]] <- as.matrix(GVCMapping[which(GVCMapping$HScode %in% GVCfull[which(GVCfull$RCA < 1), 3]), ] %>% group_by(tierNumber) %>% tally())[ ,2]
  numOppActsInTier[as.matrix(GVCacts[which(GVCacts$GVCactivityNumber %in% GVCacts[which(GVCacts$AvgRCA < 1), 2]), ] %>% group_by(tierNumber) %>% tally())[ ,1]] <- as.matrix(GVCacts[which(GVCacts$GVCactivityNumber %in% GVCacts[which(GVCacts$AvgRCA < 1), 2]), ] %>% group_by(tierNumber) %>% tally())[ ,2]

  GVCtiers <- as.data.frame(matrix(rep(0, times = numTiers*14), nrow = numTiers, ncol = 14))   # initialise GVCtiers
  colnames(GVCtiers) <- c("tierNumber", "AvgComplexity", "AvgDensity", "AvgRCA", "AvgComplexity_ifOpp", "AvgDistance_ifOpp", "AvgOpporGain_ifOpp", "AvgRCA_ifOpp", paste(chosenCountry, "export", sep = "_"), "World_export", paste(chosenCountry, "export_ifOpp", sep = "_"), "World_export_ifOpp", "NumberOfActivitiesInTier", "NumberOfOppActivitiesInTier")

  #in case of no values numOppProdsInTier == 0, the first line then breaks, hence the else statement
  if(sum(GVCfull$tierNumber %in% which(numOppProdsInTier == 0) == TRUE) > 0){
    GVCtiers[-which(numOppProdsInTier == 0) , c(5:8)] <- aggregate(GVCfull[-which(GVCfull$tierNumber %in% which(numOppProdsInTier == 0)) , c(7:10)], by = list(tier = GVCfull$tierNumber[-which(GVCfull$tierNumber %in% which(numOppProdsInTier == 0))]), FUN = sum)[, -1]/(numOppProdsInTier[-which(numOppProdsInTier == 0)])
  } else {
    GVCtiers[, c(5:8)] <- aggregate(GVCfull[ ,c(7:10)], by = list(tier = GVCfull$tierNumber), FUN = mean)[, -1]
    J <-"Else"
  }

  GVCtiers[ , 1] <- unique(GVCMapping$tierNumber)
  GVCtiers[ , c(2:4)] <- aggregate(GVCfull[ , c(4:6)], by = list(tier = GVCfull$tierNumber), FUN = sum)[, -1]/numProdsInTier
  #GVCtiers[-which(numOppProdsInTier == 0) , c(5:8)] <- aggregate(GVCfull[-which(GVCfull$tierNumber %in% which(numOppProdsInTier == 0)) , c(7:10)], by = list(tier = GVCfull$tierNumber[-which(GVCfull$tierNumber %in% which(numOppProdsInTier == 0))]), FUN = sum)[, -1]/(numOppProdsInTier[-which(numOppProdsInTier == 0)])
  GVCtiers[ , c(9:12)] <- aggregate(GVCfull[ , c(11:14)], by = list(tier = GVCfull$tierNumber), FUN = sum)[ , -1]
  GVCtiers[ , 13] <- numActsInTier
  GVCtiers[ , 14] <- numOppActsInTier

#///////////////////////////////////////////////////////////////////////////////

  Product_Results <- GVCfull
  Product_Category_Results <- GVCacts
  Tier_Results <- GVCtiers

  #------------------ Converting back to 'country_codes' ------------------

  #ECI
  ECI_df <- as.data.frame(ECI, check.names=FALSE)
  ECI_df <- cbind(rownames(ECI_df), data.frame(ECI_df, row.names=NULL))
  colnames(ECI_df)[1] <- "location_code"
  ECI_df <- merge.data.frame(ISO_Country, ECI_df, by = intersect(names(ISO_Country), names(ECI_df)))[, c(2,3)]
  ECI_df <- data.frame(ECI_df[order(ECI_df$country_code),],  row.names=1)

  #PCI
  PCI_df <- as.data.frame(PCI)

  #Opportunity Gain
  OG_df <- as.data.frame(Opportunity_Gain)
  OG_df <- cbind(rownames(OG_df), data.frame(OG_df, row.names=NULL))
  colnames(OG_df)[1] <- "location_code"
  OG_df <- merge.data.frame(ISO_Country, OG_df, by = intersect(names(ISO_Country), names(OG_df)))[,c(-1)]
  OG_df <- data.frame(OG_df[order(OG_df$country_code),],  row.names=1)
  names(OG_df) <- sub("^X", "", names(OG_df))

  #distance
  dist_df <- as.data.frame(distance)
  dist_df <- cbind(rownames(dist_df), data.frame(dist_df, row.names=NULL))
  colnames(dist_df)[1] <- "location_code"
  dist_df <- merge.data.frame(ISO_Country, dist_df, by = intersect(names(ISO_Country), names(dist_df)))[,c(-1)]
  dist_df <- data.frame(dist_df[order(dist_df$country_code),],  row.names=1)
  names(dist_df) <- sub("^X", "", names(dist_df))
  dist_df <- as.matrix(dist_df)

  #density
  dens_df <- as.data.frame(density)
  dens_df <- cbind(rownames(dens_df), data.frame(dens_df, row.names=NULL))
  colnames(dens_df)[1] <- "location_code"
  dens_df <- merge.data.frame(ISO_Country, dens_df, by = intersect(names(ISO_Country), names(dens_df)))[,c(-1)]
  dens_df <- data.frame(dens_df[order(dens_df$country_code),],  row.names=1)
  names(dens_df) <- sub("^X", "", names(dens_df))
  dens_df <- as.matrix(dens_df)

  #M-absolute
  Mabs_df <- as.data.frame(M_absolute)
  Mabs_df <- cbind(rownames(Mabs_df), data.frame(Mabs_df, row.names=NULL))
  colnames(Mabs_df)[1] <- "location_code"
  Mabs_df <- merge.data.frame(ISO_Country, Mabs_df, by = intersect(names(ISO_Country), names(Mabs_df)))[,c(-1)]
  Mabs_df <- data.frame(Mabs_df[order(Mabs_df$country_code),],  row.names=1)
  names(Mabs_df) <- sub("^X", "", names(Mabs_df))
  Mabs_df <- as.matrix(Mabs_df)

  #M-binary
  Mbin_df <- as.data.frame(M_binary)
  Mbin_df <- cbind(rownames(Mbin_df), data.frame(Mbin_df, row.names=NULL))
  colnames(Mbin_df)[1] <- "location_code"
  Mbin_df <- merge.data.frame(ISO_Country, Mbin_df, by = intersect(names(ISO_Country), names(Mbin_df)))[,c(-1)]
  Mbin_df <- data.frame(Mbin_df[order(Mbin_df$country_code),],  row.names=1)
  names(Mbin_df) <- sub("^X", "", names(Mbin_df))
  Mbin_df <- as.matrix(Mabs_df)
  #-------------------------------------------------------

  ReturnIOPS <- list(ECI_df, PCI_df, OG_df, dist_df, dens_df, Mabs_df, Mbin_df, Tier_Results, Product_Category_Results, Product_Results)
  names(ReturnIOPS) <- c("ECI", "PCI", "Opportunity_Gain", "distance", "density", "M_absolute", "M_binary", "Tier_Results", "Product_Category_Results", "Product_Results")

  message("--------DONE!--------")
  return(ReturnIOPS)
}
