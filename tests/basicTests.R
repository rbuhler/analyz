# message("Starting tests")
# 
# library("biomonCore")
# 
# gobj <- new("Global", "biomon")
# object  <- new("Action")
# 
# vSepDir <- .Platform$file.sep
# vPthMatrices <- paste0(getwd(),vSepDir,"inst", vSepDir, "matrices", vSepDir)
# vPthAnalysis <- paste0(getwd(),vSepDir,"inst", vSepDir, "analysis", vSepDir)
# 
# # gbl.mAnalysis <- paste0(vPthAnalysis, "Sample_test.csv")
# # gbl.mAnalysis <- paste0(vPthAnalysis, "Sample_tStudent.csv")
# gbl.mAnalysis <- paste0(vPthAnalysis, "Sample_Factor_Matrix.csv")
# 
# gbl.mAttributes  <- paste0(vPthMatrices, "Sample_Attributes.csv")
# gbl.mSpecies  <- paste0(vPthMatrices, "Sample_Species.csv")
# 
# x            <- "gbl.mAnalysis"
# vEnvironment <- ".GlobalEnv"
# analysisFl   <- get(x, envir=as.environment(vEnvironment))
# 
# # STEP 1 lodad the analysis steps
# object@anlz <- Analyz.loadSteps(object@anlz, analysisFl)
# # STEP 2 get the number of read coluns
# vCols <- Analyz.getNrColumns(object@anlz)
# # STEP 3 get the numbert of read rows
# vRows <- Analyz.getNrRows(object@anlz)           
# # So far so good?
# if(vCols > 0 && vRows > 0){
#   for(x in 1 : vRows){
#     # STEP 4 load the steps one-by-one
#     object@anlz <- Analyz.setStepItems(object@anlz, x)
#     # STEP 5 get the current command
#     vCommand <- Analyz.getStepCommand(object@anlz)
#     # STEP 6 get the current parameters
#     vParms <- Analyz.getStepParameters(object@anlz)
#     # STEP 7 run the command with the paraeters
#     vResult <- Analyz.runAnalysis(object@anlz, vCommand, (vParms))
#     # STEP 8 store the result
#     Analyz.setResult(object@anlz) <- vResult
#   }
# }else{
#   vResult <- "Ops!"
# }
# # Return the result of the last execution
# return(print(vResult))
