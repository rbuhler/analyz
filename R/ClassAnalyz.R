# -- CONSTRUCTOR
analyz <- function(){
  return(new("Analyz"))
}
#' Class Analyz
#' 
#' Class to manage analysis steps described in a CSV file.
#'
#' @slot steps A data frame attribute with the steps loaded from a CSV file.
#' @slot nrLines A numeric attribute wiht the number of steps.
#' @slot nrColumns A numeric attribute with the quantity of items of the largest step definition.
#' @slot stepItems A list attribute with the items of a step.
#' @slot results A list attribute with the execution result of the steps.
#' 
setClass( Class="Analyz",
          representation( steps      = "data.frame",
                          nrLines    = "numeric",
                          nrColumns  = "numeric",
                          stepItems  = "list",
                          results    = "list"),
          validity=function(object){ 
            #--- INSPECTOR
           if(TRUE){
             return(TRUE)
           }else{
             return(FALSE)
           }
          }
)
setMethod( f="initialize",
           signature="Analyz",
           definition=function(.Object, 
                               steps, 
                               nrLines, 
                               nrColumns,
                               stepItems,
                               results){ 
            #--- INITIALIZER
            # -- Set the attibutes with the defaults
            .Object@steps     <- data.frame()
            .Object@nrLines   <- 0
            .Object@nrColumns <- 0
            .Object@stepItems  <- list()
            .Object@results   <- list()
            # -- Class inspection
            validObject(.Object)
            return(.Object) }
)
#--- SETTER
#' Method Analyz.loadSteps
#' 
#' Description.
#'
#' @param object  Description.
#' @param value   Description.
#' @return object Description.
#' @examples
#' Analyz.loadSteps()
#' @export
#' 
setGeneric("Analyz.loadSteps<-",
           function(object, value){standardGeneric("Analyz.loadSteps<-")})
setReplaceMethod( f="Analyz.loadSteps",
                  signature="Analyz",
                  definition=function(object, value){
                    # -- Constants   
                    HEADER  <- FALSE
                    FACTORS <- FALSE
                    ROWNMS  <- 1
                    steps <- NULL
                    # -- BODY
                    path = value
                    tryCatch({
                      steps <- read.csv (file = path, header=HEADER, stringsAsFactors = FACTORS, row.names = ROWNMS)
                      object@nrLines   <- nrow(steps)
                      object@nrColumns <- ncol(steps)
                      
                      object@steps<-steps
                    },
                    error   = function(e) FALSE,
                    warning = function(w) FALSE
                    )
                    return(object)
                  }
)
#' Method Analyz.getNrLines 
#' 
#' Description.
#' 
#' @param object   Description.
#' @return nrLines Description.
#' @examples
#' Analyz.getNrLines()
#' @export
#' 
setGeneric("Analyz.getNrLines",
           function(object){ standardGeneric("Analyz.getNrLines") })
setMethod("Analyz.getNrLines",
          "Analyz",
          function(object){ return(object@nrLines) }
)
#' Method Analyz.getNrColumns
#' 
#' Description.
#' 
#' @param object     Description.
#' @return nrColumns Description.
#' @examples
#' Analyz.getNrColumns()
#' @export
#' 
setGeneric("Analyz.getNrColumns",
           function(object){standardGeneric("Analyz.getNrColumns")})
setMethod("Analyz.getNrColumns",
          "Analyz",
          function(object){ return(object@nrColumns) }
)
#' Method Analyz.getStep 
#' 
#' Description.
#' 
#' @param object    Description.
#' @param index     Description.
#' @return stepList Description.
#' @examples
#' Analyz.getStep()
#' @export
#' 
setGeneric("Analyz.getStep",
           function(object, index){standardGeneric("Analyz.getStep")})
setMethod("Analyz.getStep",
          "Analyz",
          function(object, index){
            
            stepList <- list()
            # -- Check for an empty data.frame
            if(object@nrColumns > 0){ 
              # -- "Walk" trhough a specific data.frame line
              for(i in 1:object@nrColumns){
                
                if (length(stepList) == 0){
                  stepList <- object@steps[index, i]
                }else{
                  stepList <- c(stepList, object@steps[index, i])
                }
              }              
            }else{
              FALSE
            }
            return(as.list(stepList))
          }
)
#' Method Analyz.setStepItems 
#' 
#' Description.
#' 
#' @param object  Description.
#' @param value   Description.
#' @return object Description.
#' @examples
#' Analyz.setStepItems()
#' @export
#' 
setGeneric("Analyz.setStepItems<-",
           function(object, value){standardGeneric("Analyz.setStepItems<-")})
setReplaceMethod( f="Analyz.setStepItems",
                  signature="Analyz",
                  definition=function(object, value){
                    # -- 
                    parms <- list()
                    
                    object@stepItems["title"]      <- value[1]
                    object@stepItems["command"]    <- value[2]
                    # -- From position 3 on the values are paramters
                    for(i in 3:length(value)){
                      # -- If the variable is empty create the first entry
                      if( !is.na(value[i]) & (value[i] != "") ){
                        parms[i-2] <- value[i]
                      }else{
                        i = length(value)
                      }
#                       if(length(object@stepItems) == 0 ){
#                         object@stepItems["parameters"] <- list(c(value[i]))
#                         # -- Otherwise add the new entry to the list  
#                       }else{
#                         object@stepItems["parameters"] <- list(c(object@stepItems["parameters"], value[i]))
#                       }
                    }
                    object@stepItems["parameters"] <- list(unlist(parms))
                    return(object)
                  }
)
#' Method Analyz.getStepItems 
#' 
#' Description.
#' 
#' @param object Description.
#' @return stepItems  Description.
#' @examples
#' Analyz.getStepItems()
#' @export
#' 
setGeneric("Analyz.getStepItems",
           function(object){standardGeneric("Analyz.getStepItems")})
setMethod("Analyz.getStepItems",
          "Analyz",
          function(object){
            stepItems <- list()
            
            stepItems["title"]      <- as.character(object@stepItems["title"])
            stepItems["command"]    <- as.character(object@stepItems["command"])
            #--- It is necessary to split the parameter type from its value
            parm_temp  <- list()
            parm_final <- list()
            count      <- 0
            #--- Parameter type is the first argument
            pType <- ""
            #--- Parameter value is the next one
            pValue <- ""
            
            parm_temp <- unlist(object@stepItems["parameters"])
            for(x in 3:object@nrColumns){
              p <- parm_temp[x-2]
              #--- Skip NA values
              if(!is.na(p)){
                #--- Item is the parameter type
                if(pType == ""){
                  pType  <- p
                  pValue <- ""
                  #--- Item is the paramter value
                }else{
                  #--- Coerce type and add to a list
                  pValue <- p
                  count <- count+1                
                  #--- Coercion needed
                  parm_final[count] <- Analyz.coerceType( object, pValue, pType )
                  pType  <- ""
                }
              }else{
                #--- Skip
              }
            }
            stepItems["parameters"] <- list(unlist(parm_final))
            return(stepItems)
          }
)
#' Method Analyz.runAnalysis 
#' 
#' Description.
#' 
#' @param object     Description.
#' @param command    Description.
#' @param parameters Description.
#' @return result    Description.
#' @examples
#' Analyz.runAnalysis()
#' @export
#' 
setGeneric("Analyz.runAnalysis",
           function(object, command, parameters){standardGeneric("Analyz.runAnalysis")})
setMethod("Analyz.runAnalysis",
          "Analyz",
          function(object, command, parameters){
            result <- c()
            
            result <- do.call(command, parameters)
            
            return( result )
          }
)
#' Method Analyz.setResult
#' 
#' Description.
#' 
#' @param object  Description.
#' @param value   Description.
#' @return object Description.
#' @examples
#' Analyz.setResult()
#' @export
setGeneric("Analyz.setResult<-",
           function(object, value){standardGeneric("Analyz.setResult<-")})
setReplaceMethod( f="Analyz.setResult",
                  signature="Analyz",
                  definition=function(object, value){
                    # -- BODY
                    count <- length(object@results) + 1
                    object@results[count] <- list(value)
                    return(object)
                  }
)
#' Method Analyz.getResult 
#' 
#' Description.
#' 
#' @param object  Description.
#' @param index   Description.
#' @return result Description.
#' @examples
#' Analyz.getResult()
#' @export
setGeneric("Analyz.getResult",
           function(object, index){standardGeneric("Analyz.getResult")})
setMethod("Analyz.getResult",
          "Analyz",
          function(object, index){ return( object@results[[index]] ) }
)
#' Method Analyz.coerceType
#' 
#' Method for variable type coercion.
#' 
#' @param object   Description.
#' @param variable Description.
#' @param type     Description.
#' @return result  Description.
#' @examples
#' Analyz.coerceType()
#' @export
setGeneric("Analyz.coerceType",
           function(object, variable, type){standardGeneric("Analyz.coerceType")})
setMethod("Analyz.coerceType",
          "Analyz",
          function(object, variable, type){
            result   <- FALSE
            # -- Coerce a variable to a defined type
            switch(type,
                     numeric={
                       tryCatch(result <- as.numeric(variable),
                                error = function(e) FALSE,
                                warning = function(w) FALSE)
                     },
                     double={
                       tryCatch(result <- as.double(variable),
                                error = function(e) FALSE,
                                warning = function(w) FALSE)
                     },
                     character={
                       tryCatch(result <- as.character(variable),
                                error = function(e) FALSE,
                                warning = function(w) FALSE)
                     },
                     logical={
                       tryCatch(result <- as.logical(variable),
                           error = function(e) FALSE,
                           warning = function(w) FALSE)
                     },
                     vector={
                       tryCatch(result <- list(c(variable)),
                                error = function(e) FALSE,
                                warning = function(w) FALSE)
                     },
                     list={
                       tryCatch(result <- list(variable),
                                error = function(e) FALSE,
                                warning = function(w) FALSE)
                     },
                     result <- variable
            )
            return( result )
          }
)