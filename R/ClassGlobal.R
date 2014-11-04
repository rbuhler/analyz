# -- CONSTRUCTOR
global <- function(arg){
  return(new(Class = "Global", arguments = as.list(arg)))
}
#' Class Global
#' 
#' Description.
#' 
#' @param arg A list of arguments to be used in the class initializer.
#' @slot arguments A list of arguments informed in time of creating the object.
#' @slot analysis A character string with the path to the folder were the analysis CSV files are found.
#' @slot matrices A character string with the path to the folder were the data matrices CSV files are found.
#' 
setClass( Class="Global",
          representation( arguments = "list",
                          analysis  = "character",
                          matrices  = "character"),
          # -- INSPECTOR 
          validity=function(object){ 
            if(TRUE){
              return(TRUE)
            }else{
              return(FALSE)
            }
          }
)
# -- INITIALIZER
setMethod( f="initialize",
           signature="Global",
           definition=function(.Object, 
                                arguments, 
                                analysis, 
                                matrices){ 
             #--- INITIALIZER
             sepDir <- .Platform$file.sep
             
             #--- Ensure the DATA folder/directory is there
             dataDir <- paste0(getwd(), sepDir, "data")
             if (!file.exists(dataDir)){
              dir.create(dataDir, mode = "0777")
             }else{ TRUE }

             #--- Check whether gbl.RData exists or not
             rData_dir    <- "gbl.RData"
             tryCatch({load( rData_dir )},
                      warning = function(w){
                        #--- Analysis repository
                        analysis_dir <- paste0(dataDir, sepDir,"analysis", sepDir)
                        if (!file.exists(analysis_dir)){
                          dir.create(analysis_dir, mode = "0777")
                        }else{ TRUE }
                        
                        #--- Matrices repository
                        matrices_dir <- paste0(dataDir, sepDir,"matrices", sepDir)
                        if (!file.exists(matrices_dir)){
                          dir.create(matrices_dir, mode = "0777")
                        }else{ TRUE }
                        
                        gbl.active   <-  "X"
                        gbl.analysis <- analysis_dir
                        gbl.matrices <- matrices_dir
                        
                        save( gbl.active,
                              gbl.analysis,
                              gbl.matrices,
                              file = rData_dir)
                        save.image()
                      })
             
             load( rData_dir )

             #--- Set globals with the defaults
            .Object@analysis <- gbl.analysis
            
            if(length(arguments[1]) == 0){
              .Object@arguments[1] <- "NoContext"
            }else{
              .Object@arguments[1] <- toupper(arguments)
            }
            
            # -- Load variables for environment "BIOMON"
            if(.Object@arguments[1] == "BIOMON" ){
              .Object@matrices  <- gbl.matrices  
            }else{
              .Object@matrices <- " "
            }             
            # -- Class inspection
            validObject(.Object)
            return(.Object) 
           }
)
#' Method Global.getAnalysis 
#' 
#' Description.
#' 
#' @param object    Description.
#' @return analysis Description.
#' @examples
#' Global.getAnalysis()
#' @export
#' 
setGeneric("Global.getAnalysis",
           function(object){standardGeneric("Global.getAnalysis")})
setMethod("Global.getAnalysis",
          "Global",
          function(object){return(object@analysis)}
          )
#' Method Global.getMatrices 
#' 
#' Description.
#' 
#' @param object    Description.
#' @return matrices Description.
#' @examples
#' Global.getMatrices()
#' @export
#' 
setGeneric("Global.getMatrices",
           function(object){standardGeneric("Global.getMatrices")})
setMethod("Global.getMatrices",
          "Global",
          function(object){return(object@matrices)}
          )