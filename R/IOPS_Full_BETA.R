
#requireNamespace("roxygen2")
requireNamespace("Rcpp")
requireNamespace("dplyr")
requireNamespace("tidyr")
requireNamespace("readxl")
requireNamespace("economiccomplexity")
requireNamespace("usethis")

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

  CountryDataPath <- system.file("extdata", "country_codes_V202001.csv", package = "iopspackage")

  AllCountryCodes <- read.csv(CountryDataPath)

  #----------------------------- Import Trade Data -----------------------------
  countryChosen <- AllCountryCodes %>%
    filter(country_code == CountryCode | country_name_abbreviation == CountryCode | country_name_full == CountryCode | iso_2digit_alpha == CountryCode| iso_3digit_alpha == CountryCode)

  #Determine country's 3-digit country code
  CC <- as.numeric(countryChosen["country_code"])

  #Look at only export data from BACI
  tradeData_Export <- as.data.frame(matrix(rep(0, times = nrow(tradeData)*4), nrow = nrow(tradeData), ncol = 4))
  tradeData_Export <- tradeData[c("t","v","i","k")]
  colnames(tradeData_Export) <- c("year","export_value","country_code","hs_product_code")

  #Merges TradeData with AllCountryCode's ISO 3-digit code
  TDE <- merge(tradeData_Export, unique(AllCountryCodes)[, c("iso_3digit_alpha","country_code")], by="country_code")

  # transform the data so that all data for one country is contained in a single row
  TDE_w <- TDE %>%
    pivot_wider(names_from = hs_product_code, values_from = export_value, values_fn = sum, names_sort = TRUE, values_fill = 0)
  Wide_TDE <- TDE_w[,-c(2,3)]

  #---------------------------- M Matrix Calculations --------------------------

  #create matrix of RCA values (MAbs) and binary RCA matrix (Mbin)
  Mabs <- balassa_index(TDE, discrete = FALSE, country = "country_code", product = "hs_product_code", value = "export_value")
  Mbin <- balassa_index(TDE, country = "country_code", product = "hs_product_code", value = "export_value")

  M_binary <- as.matrix(Mbin)
  M_absolute <- as.matrix(Mabs)

  Products <- unique(TDE$hs_product_code)   # 5381 products total
  Countries <- rownames(Mabs)   # 221 countries total

  # chosen country subset:
  CS_RCAmat <- cbind.data.frame(colnames(Wide_TDE[which(Wide_TDE == CC), -1]), as.numeric(Wide_TDE[which(Wide_TDE$country_code == CC), -1]), as.numeric(colSums(Wide_TDE[ ,-1])), as.numeric(Mabs[as.character(CC), ]))
  colnames(CS_RCAmat) <- c("hs_product_code", paste(CC, "export_of_product", sep = "_"), "World_export_of_product", paste(CC, "RCA", sep = "_"))

  #+++++++++++++++++++++++++++COMPLEXITY MEASURES++++++++++++++++++++++++++++++#

  #User selects one of three methods to calculate complexities,
  if (ComplexMethod == "eigenvalues"){

    # use binary M matrix to calculate complexity using EIGENVALUES, number of iterations must be provided
    #NOTE: This specific method takes a significantly long time to compute
    Compl_Meth <- complexity_measures(Mbin, method = "eigenvalues", iterations = iterCompl)

    Method <- "eigv" #method descriptor for Excel/CSV file

  } else if(ComplexMethod == "reflections") {

    # use binary M matrix to calculate complexity using METHOD OF REFLECTIONS, number of iterations must be provided
    Compl_Meth <- complexity_measures(Mbin, method = "reflections", iterations = iterCompl)

    Method <- "refl"

  } else {
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

#////////////////////////// Export Complexity Measures /////////////////////////
  # write.csv(ECI, paste0("Complexity_Measures_CSV/ECI_", Method,".csv"))
  # write.csv(PCI, paste0("Complexity_Measures_CSV/PCI_", Method,".csv"))
  # write.csv(as.matrix(opporGain), paste0("Complexity_Measures_CSV/oppGain_", Method,".csv"))
  # write.csv(distance, paste0("Complexity_Measures_CSV/distance_", Method,".csv"))
  # write.csv(density, paste0("Complexity_Measures_CSV/density_", Method,".csv"))
  # write.csv(as.matrix(Mbin),"Complexity_Measures_CSV/Mbin.csv")
  # write.csv(as.matrix(Mabs),"Complexity_Measures_CSV/Mabs.csv")

  # use_data(ECI, overwrite = T)
  # use_data(PCI, overwrite = T)
  # use_data(Opportunity_Gain, overwrite = T)
  # use_data(distance, overwrite = T)
  # use_data(density, overwrite = T)
  # use_data(M_binary, overwrite = T)
  # use_data(M_absolute, overwrite = T)
#///////////////////////////////////////////////////////////////////////////////

  #----------------------------- Global Value Chain ------------------------------

  #++++++++++++++++ read in industry value chain mapping++++++++++++++++++++++++++
  #GVCMapping = read.csv(GVCMap)
  colnames(GVCMapping) <- c("tierNumber", "GVCactivityNumber", "HScode")
  GVCMapping <- GVCMapping[order(GVCMapping$tierNumber, GVCMapping$GVCactivityNumber, GVCMapping$HScode), ]   # reorder GVCMapping according to tier, then activity, then HScode

  numTiers <- length(unique(GVCMapping$tierNumber))         # = 5
  numActs <- length(unique(GVCMapping$GVCactivityNumber))   # = 49
  numProds <- length(unique(GVCMapping$HScode))             # = 272

  numProdsInAct <- as.matrix(GVCMapping[ ,c(2, 3)] %>% group_by(GVCactivityNumber) %>% tally())[ ,2]
  numActsInTier <- as.matrix(unique(GVCMapping[ ,c(1, 2)]) %>% group_by(tierNumber) %>% tally())[ ,2]
  numProdsInTier <- as.matrix(GVCMapping[ ,c(1, 3)] %>% group_by(tierNumber) %>% tally())[ ,2]

  #-----GVCfull-----
  GVCfull <- as.data.frame(matrix(rep(0, times = numProds*16), nrow = numProds, ncol = 16))   # initialise GVCfull
  colnames(GVCfull) <- c("tierNumber", "GVCactivityNumber", "HScode", "Complexity", "Density", "RCA", "Complexity_ifOpp", "Distance_ifOpp", "OpporGain_ifOpp", "RCA_ifOpp", paste(CC, "export_of_product", sep = "_"), "World_export_of_product", paste(CC, "export_of_product_ifOpp", sep = "_"), "World_export_of_product_ifOpp", "productPosition") #include "productPGI" at position 16

  GVCfull[ , c(1, 2, 3)] <- GVCMapping
  GVCfull <- GVCfull[order(GVCfull$HScode), ]   # order according to HS code

  GVCfull[ , 4] <- as.numeric(PCI[which(Products %in% GVCMapping$HScode)])
  GVCfull[ , 5] <- as.numeric(density[as.character(CC), which(Products %in% GVCMapping$HScode)])   # for distances and densities, subset the one row that is specific to the country being analysed
  GVCfull[ , 6] <- CS_RCAmat[which(Products %in% GVCMapping$HScode), 4]
  GVCfull[ , c(11, 12)] <- CS_RCAmat[which(Products %in% GVCMapping$HScode), c(2, 3)]
  GVCfull[ , 15] <- which(Products %in% GVCMapping$HScode)
  #GVCfull[ , 16] <- PGI[GVCfull$productPosition]
  GVCfull[which(GVCfull$RCA < 1), c(7, 10, 13, 14)] <- GVCfull[which(GVCfull$RCA < 1), c(4, 6, 11, 12)]
  GVCfull[ , 8] <- as.numeric(distance[as.character(CC), which(Products %in% GVCMapping$HScode)])
  GVCfull[-which(GVCfull$RCA < 1), 8] <- 0
  GVCfull[ , 9] <- as.numeric(opporGain[as.character(CC), which(Products %in% GVCMapping$HScode)])
  GVCfull[-which(GVCfull$RCA < 1), 9] <- 0

  GVCfull <- GVCfull[order(GVCfull$tierNumber, GVCfull$GVCactivityNumber, GVCfull$HScode), ]
  #colnames(GVCfull)[16] <- "productPGI"

  Product_Results <- GVCfull

#////////////////////////// Export Product Results /////////////////////////////
  # write.xlsx2(GVCfull, file = "Results/Product_Results.xlsx", sheetName = "Sheet1",
  #            col.names = TRUE, row.names = FALSE, append = FALSE)

  #use_data(Product_Results, overwrite = T)
#///////////////////////////////////////////////////////////////////////////////

  #-----GVCacts-----

  numOppProdsInAct <- rep(0, times = length(numProdsInAct))

  numOppProdsInAct[as.matrix(GVCMapping[which(GVCMapping$HScode %in% GVCfull[which(GVCfull$RCA < 1), 3]), ] %>% group_by(GVCactivityNumber) %>% tally())[ ,1]] <- as.matrix(GVCMapping[which(GVCMapping$HScode %in% GVCfull[which(GVCfull$RCA < 1), 3]), ] %>% group_by(GVCactivityNumber) %>% tally())[ ,2]

  #in case of mismatch between data lengths
  if (length(numProdsInAct)<length(numOppProdsInAct)) {
    stop("Error: data selected for 'tradeData' and 'GVCmapping' isn't compatible with each other. Select data within the same 'H' class, i.e., 'H0', 'H3', or 'H5'")
  }

  GVCacts <- as.data.frame(matrix(rep(0, times = numActs*17), nrow = numActs, ncol = 17))   # initialise GVCacts
  colnames(GVCacts) <- c("tierNumber", "GVCactivityNumber", "AvgComplexity", "AvgDensity", "AvgRCA", "AvgComplexity_ifOpp", "AvgDistance_ifOpp", "AvgOpporGain_ifOpp", "AvgRCA_ifOpp", paste(CC, "export_of_activity", sep = "_"), "World_export_of_activity", paste(CC, "export_of_activity_ifOpp", sep = "_"), "World_export_of_activity_ifOpp", "NumberOfProductsInActivity", "NumberOfOppProductsInActivity", "AvgPGI", "AvgPGI_ifOpp")

  GVCacts[ , c(1, 2)] <- unique(GVCMapping[ ,c(1, 2)])
  GVCacts[ , c(3:5)] <- aggregate(GVCfull[ , c(4:6)], by = list(activity = GVCfull$GVCactivityNumber), FUN = sum)[, -1]/numProdsInAct   # also works: #aggregate(GVCfull[ , c(4:6)], by = list(activity = GVCfull$GVCactivityNumber), FUN = mean)[ , -1]
  GVCacts[-which(numOppProdsInAct == 0) , c(6:9)] <- aggregate(GVCfull[-which(GVCfull$GVCactivityNumber %in% which(numOppProdsInAct == 0)) , c(7:10)], by = list(activity = GVCfull$GVCactivityNumber[-which(GVCfull$GVCactivityNumber %in% which(numOppProdsInAct == 0))]), FUN = sum)[, -1]/(numOppProdsInAct[-which(numOppProdsInAct == 0)])
  GVCacts[ , c(10:13)] <- aggregate(GVCfull[ , c(11:14)], by = list(activity = GVCfull$GVCactivityNumber), FUN = sum)[ , -1]
  GVCacts[ , 14] <- numProdsInAct
  GVCacts[ , 15] <- numOppProdsInAct
  GVCacts[ , 16] <- aggregate(GVCfull[ , 16], by = list(activity = GVCfull$GVCactivityNumber), FUN = mean)[, -1]
  GVCacts[-which(numOppProdsInAct == 0), 17] <- aggregate(GVCfull[-which(GVCfull$GVCactivityNumber %in% which(numOppProdsInAct == 0)) , 16], by = list(activity = GVCfull$GVCactivityNumber[-which(GVCfull$GVCactivityNumber %in% which(numOppProdsInAct == 0))]), FUN = sum)[, -1]/(numOppProdsInAct[-which(numOppProdsInAct == 0)])

  Product_Category_Results <- GVCacts

  #/////////////////////// Export Product Category Results /////////////////////
  # write.xlsx2(GVCacts, file = "Results/Product_Category_Results.xlsx", sheetName = "Sheet1",
  #            col.names = TRUE, row.names = FALSE, append = FALSE)

  #use_data(Product_Category_Results, overwrite = T)
  #/////////////////////////////////////////////////////////////////////////////

  # create vector of activity numbers that contain no opportunity products
  #noOppActs <- GVCacts[which(GVCacts$NumberOfOppProductsInActivity == 0), 2]

  #-----GVCtiers-----

  numOppProdsInTier <- rep(0, times = length(numActsInTier))
  numOppActsInTier <- rep(0, times = length(numActsInTier))

  numOppProdsInTier[as.matrix(GVCMapping[which(GVCMapping$HScode %in% GVCfull[which(GVCfull$RCA < 1), 3]), ] %>% group_by(tierNumber) %>% tally())[ ,1]] <- as.matrix(GVCMapping[which(GVCMapping$HScode %in% GVCfull[which(GVCfull$RCA < 1), 3]), ] %>% group_by(tierNumber) %>% tally())[ ,2]
  numOppActsInTier[as.matrix(GVCacts[which(GVCacts$GVCactivityNumber %in% GVCacts[which(GVCacts$AvgRCA < 1), 2]), ] %>% group_by(tierNumber) %>% tally())[ ,1]] <- as.matrix(GVCacts[which(GVCacts$GVCactivityNumber %in% GVCacts[which(GVCacts$AvgRCA < 1), 2]), ] %>% group_by(tierNumber) %>% tally())[ ,2]

  GVCtiers <- as.data.frame(matrix(rep(0, times = numTiers*16), nrow = numTiers, ncol = 16))   # initialise GVCtiers
  colnames(GVCtiers) <- c("tierNumber", "AvgComplexity", "AvgDensity", "AvgRCA", "AvgComplexity_ifOpp", "AvgDistance_ifOpp", "AvgOpporGain_ifOpp", "AvgRCA_ifOpp", paste(CC, "export", sep = "_"), "World_export", paste(CC, "export_ifOpp", sep = "_"), "World_export_ifOpp", "NumberOfActivitiesInTier", "NumberOfOppActivitiesInTier")

  GVCtiers[ , 1] <- unique(GVCMapping$tierNumber)
  GVCtiers[ , c(2:4)] <- aggregate(GVCfull[ , c(4:6)], by = list(tier = GVCfull$tierNumber), FUN = mean)[, -1]

  #in case of no values numOppProdsInTier == 0, the first line then breaks, hence the else statement
  if(sum(GVCfull$tierNumber %in% which(numOppProdsInTier == 0) == TRUE) > 0){
    GVCtiers[-which(numOppProdsInTier == 0) , c(5:8)] <- aggregate(GVCfull[-which(GVCfull$tierNumber %in% which(numOppProdsInTier == 0)) , c(7:10)], by = list(tier = GVCfull$tierNumber[-which(GVCfull$tierNumber %in% which(numOppProdsInTier == 0))]), FUN = sum)[, -1]/(numOppProdsInTier[-which(numOppProdsInTier == 0)])
  } else {
    GVCtiers[, c(5:8)] <- aggregate(GVCfull[ ,c(7:10)], by = list(tier = GVCfull$tierNumber), FUN = mean)[, -1]
    J <-"Else"
  }

  GVCtiers[ , c(9:12)] <- aggregate(GVCfull[ , c(11:14)], by = list(tier = GVCfull$tierNumber), FUN = sum)[ , -1]
  GVCtiers[ , 13] <- numActsInTier
  GVCtiers[ , 14] <- numOppActsInTier
  #GVCtiers[ , 15] <- aggregate(GVCfull[ , 16], by = list(tier = GVCfull$tierNumber), FUN = mean)[ , -1]
  #GVCtiers[-which(numOppProdsInTier == 0), 16] <- aggregate(GVCfull[-which(GVCfull$tierNumber %in% which(numOppProdsInTier == 0)) , 16], by = list(tier = GVCfull$tierNumber[-which(GVCfull$tierNumber %in% which(numOppProdsInTier == 0))]), FUN = sum)[, -1]/(numOppProdsInTier[-which(numOppProdsInTier == 0)])

  Tier_Results <- GVCtiers

#//////////////////////////// Export Tier Results //////////////////////////////
  # write.xlsx2(GVCtiers, file = "Results/Tier_Results.xlsx", sheetName = "Sheet1",
  #            col.names = TRUE, row.names = FALSE, append = FALSE)

  #use_data(Tier_Results, overwrite = T)
#///////////////////////////////////////////////////////////////////////////////
  ReturnIOPS <- list(ECI, PCI, Opportunity_Gain, distance, density, M_absolute, M_binary, Tier_Results, Product_Category_Results, Product_Results)
  names(ReturnIOPS) <- c("ECI", "PCI", "Opportunity_Gain", "distance", "density", "M_absolute", "M_binary", "Tier_Results", "Product_Category_Results", "Product_Results")

  return(ReturnIOPS)
  message("Success!")
}
