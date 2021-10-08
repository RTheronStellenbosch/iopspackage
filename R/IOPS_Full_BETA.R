
#requireNamespace("roxygen2")
requireNamespace("xlsx")
requireNamespace("dplyr")
requireNamespace("tidyr")
requireNamespace("readxl")
requireNamespace("economiccomplexity")

library("xlsx")
library("dplyr")
library("tidyr")
library("readxl")
library("economiccomplexity")

# AllCountryCodes <- read_excel("country_codes_V202001.xlsx")
# tradeData <- read.csv("H0/BACI_HS92_Y2018_V202001.csv", header = TRUE)
# GVCMapping <- read.csv( "H0/Auto_Value_Chain.csv")

#' IOPS
#'
#' @description  Takes user inputted trade data, any acceptable ISO country code and industrial value chain mapping to calculate various metrics (Economic- and Product complexity metrics, distance metrics, opportunity gain, and inequality metrics) of a given country in order to facilitate better decision making regarding industrial policymaking.
#'
#' @import xlsx
#' @import dplyr
#' @import tidyr
#' @import readxl
#' @import economiccomplexity
#' @importFrom stats aggregate
#' @importFrom utils write.csv
#'
#' @param CountryCode (Type: character/integer) Any accepted ISO country code could be used, e.g. "United Kingdom", "GBR", "GB", 828 would all be accepted if the United Kingdom is the desired country.
#' @param tradeData (Type: csv) Accepts any CEPII BACI trade data. Defaults to "H0/BACI_HS92_Y2018_V202001.csv". NOTE: tradeDataInput and GVCMap must be from the same "H" Family, e.g. both are from  H3, etc., in order for the program to work correctly.
#' @param ComplexMethod (Type: character) Methods used to calculate complexity measures. Can be any one of these methods: "fitness", "reflections" or "eigenvalues". Defaults to "eigenvalues".
#' @param iterCompl (Type: integer) The number of iterations that the chosen complexity measure must use. Defaults to 20.
#' @param GVCMapping (Type: csv) The desired value chain to be analysed. With Columns "Tiers", "Activity", and "HSCode". Defaults to "H0/Auto_Value_Chain.csv.csv" NOTE: tradeDataInput and GVCMap must be from the same "H" Family, e.g. both are from  H3, etc., in order for the program to work correctly.
#'
#' @return The package creates 3 xlsx files, namely Tier_Results.xlsx, Product_Category_Results.xlsx, and "Product_Results.xlsx. It also creates a number of csv files containing all the complexity measures for a chosen complexity calculation method, under the 'Complexity_Measures_CSV' folder.
#' @export
#'
IOPS <- function(CountryCode ,tradeData , ComplexMethod = "eigenvalues", iterCompl = 20, GVCMapping){

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

  # compute the densities and distances
  # ______ (1 - Mbin) creates matrix of ones where Mbin has zeroes and vice versa
  distance <- tcrossprod(1 - as.matrix(Mbin), as.matrix(Proximities)/rowSums(as.matrix(Proximities)))
  density <- tcrossprod(as.matrix(Mbin), as.matrix(Proximities)/rowSums(as.matrix(Proximities)))

  #+++++++to convert ISO to country code (temp.)+++++++++

  #++++++++++++++++++++++++++++++++++++++++++++++++++++++

  #return complexity metrics in a file named "ComplexityMeasures.xlsx"
 # write.xlsx2(ECI, file="ComplexityMeasures.xlsx", sheetName = paste0("ECI_", Method,".xlsx"), row.names=T)
 # write.xlsx2(PCI, file="ComplexityMeasures.xlsx", sheetName = paste0("PCI_", Method,".xlsx"), append=TRUE, row.names=T)
 # write.xlsx2(as.matrix(opporGain), file="ComplexityMeasures.xlsx", sheetName = paste0("oppGain_", Method,".xlsx"), append=TRUE, row.names=T)
 # write.xlsx2(distance, file="ComplexityMeasures.xlsx", sheetName = paste0("distance_", Method,"xlsx"), append=TRUE, row.names=T)
 # write.xlsx2(density, file="ComplexityMeasures.xlsx", sheetName = paste0("density_", Method,".xlsx"), append=TRUE, row.names=T)
 # write.xlsx2(as.matrix(Mbin), file="ComplexityMeasures.xlsx", sheetName = "M_binary.xlsx", append=TRUE, row.names=T)
 # write.xlsx2(as.matrix(Mabs), file="ComplexityMeasures.xlsx", sheetName = "M_absolute.xlsx", append=TRUE, row.names=T)

#////////////////////////// Export Complexity Measures /////////////////////////
  write.csv(ECI, paste0("Complexity_Measures_CSV/ECI_", Method,".csv"))
  write.csv(PCI, paste0("Complexity_Measures_CSV/PCI_", Method,".csv"))
  write.csv(as.matrix(opporGain), paste0("Complexity_Measures_CSV/oppGain_", Method,".csv"))
  write.csv(distance,paste0("Complexity_Measures_CSV/distance_", Method,".csv"))
  write.csv(density, paste0("Complexity_Measures_CSV/density_", Method,".csv"))

  write.csv(as.matrix(Mbin),"Complexity_Measures_CSV/Mbin.csv")
  write.csv(as.matrix(Mabs),"Complexity_Measures_CSV/Mabs.csv")
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

#////////////////////////// Export Product Results /////////////////////////////
  write.xlsx2(GVCfull, file = "Results/Product_Results.xlsx", sheetName = "Sheet1",
             col.names = TRUE, row.names = FALSE, append = FALSE)
#///////////////////////////////////////////////////////////////////////////////

  #-----GVCacts-----

  numOppProdsInAct <- rep(0, times = length(numProdsInAct))

  numOppProdsInAct[as.matrix(GVCMapping[which(GVCMapping$HScode %in% GVCfull[which(GVCfull$RCA < 1), 3]), ] %>% group_by(GVCactivityNumber) %>% tally())[ ,1]] <- as.matrix(GVCMapping[which(GVCMapping$HScode %in% GVCfull[which(GVCfull$RCA < 1), 3]), ] %>% group_by(GVCactivityNumber) %>% tally())[ ,2]

  #in case of mismatch between data lengths
  # if (length(numProdsInAct)<length(numOppProdsInAct)) {
  #   Dif <- length(numOppProdsInAct)-length(numProdsInAct)+1
  #   numOppProdsInAct <- numOppProdsInAct[Dif:length(numOppProdsInAct)]
  # }

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

  #/////////////////////// Export Product Category Results /////////////////////
  write.xlsx2(GVCacts, file = "Results/Product_Category_Results.xlsx", sheetName = "Sheet1",
             col.names = TRUE, row.names = FALSE, append = FALSE)
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
  if(GVCfull$tierNumber %in% which(numOppProdsInTier == 0) == TRUE){
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

#//////////////////////////// Export Tier Results //////////////////////////////
  write.xlsx2(GVCtiers, file = "Results/Tier_Results.xlsx", sheetName = "Sheet1",
             col.names = TRUE, row.names = FALSE, append = FALSE)
#///////////////////////////////////////////////////////////////////////////////

  #-----Bilateral GVC proximity matrices-----



  #+++++++++++++++++++++++ BASELINE METRICS (Doesn't Work) +++++++++++++++++++++
#
#   BLmetrics <- rep(0, times = 9)
#   names(BLmetrics) <- c("OpportunityValue", "AvgComplexityRCAproducts_VC","AvgDistanceNonRCAproducts_VC", "AvgComplexityRCAproducts_PS", "AvgDistanceNonRCAproducts_PS", "RCAproductsIn_VC", "SumComplexityRCAproducts_VC", "RCAproductsIn_PS", "SumComplexityRCAproducts_PS")
#
#   BLmetrics[1] <- cntryOpporVal[[as.character(CC)]]
#   BLmetrics[2] <- sum(GVCfull[which(GVCfull$RCA >= 1), 4])/length(GVCfull[which(GVCfull$RCA >= 1), 4])
#   BLmetrics[3] <- sum(GVCfull[which(GVCfull$RCA < 1), 8])/length(GVCfull[which(GVCfull$RCA < 1), 8])
#   BLmetrics[4] <- mean(PCI[which(Mabs[as.character(CC), ] >= 1)])
#   BLmetrics[5] <- mean(distance[as.character(CC), which(Mabs[as.character(CC), ] < 1)])
#   BLmetrics[6] <- numProds - sum(numOppProdsInTier)
#   BLmetrics[7] <- sum(GVCfull[which(GVCfull$RCA >= 1), 4])
#   BLmetrics[8] <- sum(Mabs[as.character(CC), ] >= 1)
#   BLmetrics[9] <- sum(PCI[which(Mabs[as.character(CC), ] >= 1)])
#
#
#   # RCA : || HS code | ZAF RCA ||
#   baseRCAspace <- rep(0, times = length(CS_RCAmat[ , 4]))
#   baseRCAspace[which(CS_RCAmat[ , 4] >= 1)] <- 1.1
#
#   #-----calculate scenario RCA space for 1-----
#
#   #__________Calculate raw contributions for each activity
#
#   GVCRCAscenCont <- matrix(rep(0, times = nrow(GVCacts)*length(Products)), nrow = nrow(GVCacts), ncol = length(Products))
#
#   nonRCAprodsPerAct <- aggregate(GVCfull[which(GVCfull$RCA < 1), 15], by = list(activity = GVCfull$GVCactivityNumber[which(GVCfull$RCA < 1)]), FUN = c)
#
#   #Doesn't work VV
#  # for (i in nonRCAprodsPerAct$activity) {
#     #GVCRCAscenCont[41, unlist(nonRCAprodsPerAct$x[which(nonRCAprodsPerAct$activity == 41)])] <- 1.1
#   #}
#
#   #__________Calculate Scenarios by adding baseline and Scenario contributions
#
#   GVCRCAscen1 <- matrix(rep(0, times = nrow(GVCacts)*length(Products)), nrow = nrow(GVCacts), ncol = length(Products))
#   GVCRCAscen1 <- t(t(GVCRCAscenCont) + baseRCAspace)
#
#
#   #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   #-----Calculate Metrics for RCA scenarios after 1-----
#
#   #__________Calculate Complexity values for Different Activities based on the unexploited items
#
#   sumCompContr <- rep(0, times = nrow(GVCacts)) # = contribution to complexity per activity
#   sumCompContr[aggregate(GVCfull[which(GVCfull$RCA < 1), 4], by = list(activity = GVCfull$GVCactivityNumber[which(GVCfull$RCA < 1)]), FUN = sum)[ , 1]] <- aggregate(GVCfull[which(GVCfull$RCA < 1), 4], by = list(activity = GVCfull$GVCactivityNumber[which(GVCfull$RCA < 1)]), FUN = sum)[ , 2]
#   numInActToBeAdded <- GVCacts$NumberOfOppProductsInActivity
#
#   Comp1 <- (sumCompContr + BLmetrics[9])/(numInActToBeAdded + BLmetrics[8])
#
#   #__________Calculate the distance to different activities for one step
#
#   distReq1 <- aggregate(GVCfull[8], by = list(activity = GVCfull$GVCactivityNumber), FUN = sum)[ ,2]
#
#   #__________Calculate OpporGain for different activities for one step
#
#   oppValScen1 <- rep(0, times = nrow(GVCacts))
#   densScen1 <- tcrossprod((GVCRCAscen1/1.1), as.matrix(Proximity)/(rowSums(as.matrix(Prox))))
#   densScen1forOpps <- densScen1 * (1 - (GVCRCAscen1/1.1))
#   oppValScen1 <- tcrossprod(densScen1forOpps, t(as.numeric(PCI)))
#
#   oppGainScen1 <- oppValScen1 - BLmetrics[1]
#
#   round1metrics <- cbind.data.frame(Comp1, oppGainScen1, distReq1)   # all are 1 x 49 vectors
#   colnames(round1metrics) <- c("AvgCompInPS_RCAforAllActProds", "OppGain_RCAforAllActProds", "RequiredDist")
#
#   #__________Calculate winners after 1
#
#   ScenariosDistances <- seq(1, 71, length = 36)
#
#   BestFrom1Comp <- as.data.frame(matrix(rep(0, times = 6*length(ScenariosDistances)), nrow = length(ScenariosDistances), ncol = 6))
#   BestFrom1Oppor <- as.data.frame(matrix(rep(0, times = 6*length(ScenariosDistances)), nrow = length(ScenariosDistances), ncol = 6))
#   colnames(BestFrom1Comp) <- c("Activity 1", "Activity 2", "Activity 3", "BestCompContr", "OpporGain", "Distance")
#   colnames(BestFrom1Oppor) <- c("Activity 1", "Activity 2", "Activity 3", "CompContr", "BestOpporGain", "Distance")
#
#   # CompInPS = BaseLineMetrics[9] / BaseLineMetrics[8]
#   BestCompContr <- rep((BLmetrics[9] / BLmetrics[8]), times = length(ScenariosDistances))
#   BestOpporGain <- rep(0, times = length(ScenariosDistances))
#
#   for (i in 1:length(ScenariosDistances)) {
#     #_________________________for comp maximization:
#     # returns activity number of max comp contr for distance within scenario i's max distance
#     BestFrom1Comp[i, 1] <- rownames(round1metrics[which(round1metrics$RequiredDist < ScenariosDistances[i]), ])[which.max(round1metrics[(round1metrics$RequiredDist < ScenariosDistances[i]), 1])]
#     BestFrom1Comp[i, 4] <- round1metrics[BestFrom1Comp[i, 1], 1]
#     BestFrom1Comp[i, 5] <- round1metrics[BestFrom1Comp[i, 1], 2]
#     BestFrom1Comp[i, 6] <- round1metrics[BestFrom1Comp[i, 1], 3]
#
#     #_________________________for comp maximization:
#     # returns activity number of max opp gain for distance within scenario i's max distance
#     BestFrom1Oppor[i, 1] <- rownames(round1metrics[which(round1metrics$RequiredDist < ScenariosDistances[i]), ])[which.max(round1metrics[(round1metrics$RequiredDist < ScenariosDistances[i]), 2])]
#     BestFrom1Oppor[i, 4] <- round1metrics[BestFrom1Oppor[i, 1], 1]
#     BestFrom1Oppor[i, 5] <- round1metrics[BestFrom1Oppor[i, 1], 2]
#     BestFrom1Oppor[i, 6] <- round1metrics[BestFrom1Oppor[i, 1], 3]
#
#   }
#
#   BestFrom1Comp[-which(BestFrom1Comp$BestCompContr > BestCompContr), ] <- rep(0, times = 6)
#   BestCompContr <- BestFrom1Comp$BestCompContr
#
#   BestFrom1Oppor[-which(BestFrom1Oppor$BestOpporGain > BestOpporGain), ] <- rep(0, times = 6)
#   BestOpporGain <- BestFrom1Oppor$BestOpporGain
#
#   #++++++++++++++++++++++ Calculate Scenario RCA Space for 2 +++++++++++++++++++++
#
#   GVCRCAscen2 <- array((t(GVCRCAscenCont) + baseRCAspace), dim = c(length(Products), nrow(GVCacts), nrow(GVCacts)))
#
#   for (i in 1:nrow(GVCacts)) {
#     GVCRCAscen2[ , , i] <- GVCRCAscen2[ , , i] + GVCRCAscenCont[i, ]
#   }
#
#   #+++++++++++++++++ Calculate Metrics for RCA scenarios after 2 +++++++++++++++++
#
#   #__________Calculate Complexity values for Different Activities based on the unexploited items
#
#   totSumCompContr <- matrix(sumCompContr, nrow = nrow(GVCacts), ncol = nrow(GVCacts))
#   totSumCompContr <- totSumCompContr + t(totSumCompContr)
#   numActsToAdd <- matrix(GVCacts$NumberOfOppProductsInActivity, nrow = nrow(GVCacts), ncol = nrow(GVCacts))
#   numActsToAdd <- numActsToAdd + t(numActsToAdd)
#
#   Comp2 <- (totSumCompContr + BLmetrics[9]) / (numActsToAdd + BLmetrics[8])
#
#   # now make diagonals a very large negative value!!
#   Comp2 <- Comp2 - diag(nrow(GVCacts))*1000
#
#   #__________Calculate the distance to different activities for 2 steps
#
#   distStep2 <- tcrossprod((1 - (GVCRCAscen1/1.1)), as.matrix(Prox)/rowSums(as.matrix(Prox)))
#   distStep2 <- distStep2*(1 - (GVCRCAscen1/1.1))
#
#   # distReq2 = 49 x 49 of Step 1 (row) x Step 2 (col)
#
#   distReq2 <- matrix(distReq1, nrow = nrow(GVCacts), ncol = nrow(GVCacts))  # populate so that each col = distReq1, then add to that initial distance in the for loop
#
#   for (step2 in 1:nrow(GVCacts)) {  # run through all possible second steps
#
#     distReq2[ , step2] <- distReq2[ , step2] + rowSums(distStep2[ , GVCfull[which(GVCfull$GVCactivityNumber == step2), 15], drop = FALSE])
#
#   }
#
#   #__________Calculate OpporGain for different activities for two steps
#
#   oppValScen2 <- matrix(0, nrow = nrow(GVCacts), ncol = nrow(GVCacts))
#
#   for (step1 in 1:nrow(GVCacts)) {  # run through all possible first steps
#
#     # subset the scenario space for 2 steps to obtain a matrix
#     step2mat <- t(GVCRCAscen2[ , step1, ])
#     Binstep2matVect <- as.vector(step2mat)
#     Binstep2matVect[which(Binstep2matVect > 1)] <- 1
#
#     step2matBin <- matrix(Binstep2matVect, nrow = nrow(GVCacts), ncol = length(Products))
#
#     # this creates a new density matrix, where the density of all 1219 product space products is calculated for each of the 49 possible first-step activities
#     densScen2step1 <- tcrossprod((step2matBin), as.matrix(Prox)/(rowSums(as.matrix(Prox))))
#     densScen2step1forOpps <- densScen2step1 * (1 - (step2matBin))
#     oppValScen2[step1, ] <- tcrossprod(densScen2step1forOpps, t(as.numeric(PCI)))
#
#     step2mat <- NULL
#     Binstep2matVect <- NULL
#     step2matBin <- NULL
#   }

}
