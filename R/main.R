# source("R/ClassGlobal.R");
# source("R/ClassAssert.R");
# source("R/ClassAnalyz.R");
# 
# glbl <- global("biomon")
# 
# gbl.mEnvironment <- read.csv(paste0(gbl.matrices,"mEnvironment.csv"), header = TRUE, row.names = 1)
# gbl.mAttributes <- read.csv(paste0(gbl.matrices,"mAttributes.csv"), header = TRUE, row.names = 1)
# gbl.mSpecies <- read.csv(paste0(gbl.matrices,"mSpecies.csv"), header = TRUE, row.names = 1)
# gbl.mSpace <- read.csv(paste0(gbl.matrices,"mSpace.csv"), header = TRUE, row.names = 1)
# 
# message ("Mean of ", colnames(gbl.mEnvironment[1]), " -> ", mean(unlist(gbl.mEnvironment[1])))
# message ("Mean of ", colnames(gbl.mEnvironment[2]), " -> ", mean(unlist(gbl.mEnvironment[2])))
# message ("Mean of ", colnames(gbl.mEnvironment[3]), " -> ", mean(unlist(gbl.mEnvironment[3])))
# message ("Mean of ", colnames(gbl.mEnvironment[4]), " -> ", mean(unlist(gbl.mEnvironment[4])))
# message ("Mean of ", colnames(gbl.mEnvironment[5]), " -> ", mean(unlist(gbl.mEnvironment[5])))
# message ("Mean of ", colnames(gbl.mEnvironment[6]), " -> ", mean(unlist(gbl.mEnvironment[6])))
# message ("Mean of ", colnames(gbl.mEnvironment[7]), " -> ", mean(unlist(gbl.mEnvironment[7])))
# 
# # source("tests/test.R")