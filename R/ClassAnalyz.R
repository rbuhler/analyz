# -- CONSTRUCTOR
analyz <- function(){
  return(new("Analyz")) 
}
#' Class Analyz
#' 
#' Class to manage analysis steps described in a CSV file.
#'
#' @slot steps A data frame attribute with the steps loaded from a CSV file.
#' @slot nrRows A numeric attribute wiht the number of steps.
#' @slot nrColumns A numeric attribute with the quantity of items of the largest step definition.
#' @slot stepItems A list attribute with the items of a step.
#' @slot results A list attribute with the execution result of the steps.
#' 
setClass( Class="Analyz",
          representation( steps      = "data.frame",
                          nrRows     = "numeric",
                          nrColumns  = "numeric",
                          stepItems  = "list",
                          results    = "data.frame"),
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
                               nrRows, 
                               nrColumns,
                               stepItems,
                               results){ 
            #--- INITIALIZER
            # -- Set the attibutes with the defaults
            .Object@steps     <- data.frame()
            .Object@nrRows    <- 0
            .Object@nrColumns <- 0
            .Object@stepItems <- list()
            .Object@results   <- data.frame()
            # -- Class inspection
            validObject(.Object)
            return(.Object) }
)
#--- SETTER
#' Method Analyz.loadSteps<-
#' 
#' Description.
#'
#' @param object  Description.
#' @param value   Description.
#' @return object Description.
#' 
#' @export
#' @docType methods
#' @rdname Analyz.loadSteps-methods
#' 
#' @examples
#' obj <- new("Analyz")
#' v_path <- vector()
#' Analyz.loadSteps(obj) <- v_path
#' @export
#' 
setGeneric("Analyz.loadSteps<-",
           function(object, value){standardGeneric("Analyz.loadSteps<-")})
#' @rdname Analyz.loadSteps-methods
#' @aliases Analyz.loadSteps,Analyz,Analyz-method
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
                    steps <- tryCatch(read.csv (file = path, header=HEADER, stringsAsFactors = FACTORS, row.names = ROWNMS),
                                                error   = function(e) message(path, '\n', e),
                                                warning = function(w) message(path, '\n', w)
                    )
                    if(!is.null(steps)){
                      object@nrRows    <- nrow(steps)
                      object@nrColumns <- ncol(steps)
                      
                      object@steps<-steps                      
                    }
                    return(object)
                  }
)
#' Method Analyz.getNrRows 
#' 
#' Description.
#' 
#' @param object   Description.
#' @return nrRows Description.
#' 
#' @export
#' @docType methods
#' @rdname Analyz.getNrRows-methods
#' 
#' @examples
#' obj <- new("Analyz")
#' v_NrRow <- Analyz.getNrRows(obj)
#' @export
#' 
setGeneric("Analyz.getNrRows",
           function(object){ standardGeneric("Analyz.getNrRows") })
#' @rdname Analyz.getNrRows-methods
#' @aliases Analyz.getNrRows,Analyz,Analyz-method
setMethod("Analyz.getNrRows",
          "Analyz",
          function(object){ return(object@nrRows) }
)
#' Method Analyz.getNrColumns
#' 
#' Description.
#' 
#' @param object     Description.
#' @return nrColumns Description.
#' 
#' @export
#' @docType methods
#' @rdname Analyz.getNrColumns-methods
#' 
#' @examples
#' obj <- new("Analyz")
#' v_nrCol <- Analyz.getNrColumns(obj)
#' @export
#' 
setGeneric("Analyz.getNrColumns",
           function(object){standardGeneric("Analyz.getNrColumns")})
#' @rdname Analyz.getNrColumns-methods
#' @aliases Analyz.getNrColumns,Analyz,Analyz-method
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
#' 
#' @export
#' @docType methods
#' @rdname Analyz.getStep-methods
#' 
#' @examples
#' obj <- new("Analyz")
#' v_step <- Analyz.getStep(obj, 1)
#' @export
#' 
setGeneric("Analyz.getStep",
           function(object, index){standardGeneric("Analyz.getStep")})
#' @rdname Analyz.getStep-methods
#' @aliases Analyz.getStep,Analys,Analyz-method
setMethod("Analyz.getStep",
          "Analyz",
          function(object, index){
            
            stepList <- list()
            # -- Check for an empty data.frame
            if(object@nrColumns > 0){ 
              # -- "Walk" trhough a specific data.frame line
              for(i in 1:object@nrColumns){ 
                
                vStep <- object@steps[index, i]
                if(!is.na(vStep)){
                
                  if (length(stepList) == 0){
                    stepList <- vStep
                  }else{
                    stepList <- c(stepList, vStep)
                  }                  
                }else{ # NA value
                  # Skip
                }
              }              
            }else{
              FALSE
            }
            
            return(as.list(stepList))
          }
)
#' Method Analyz.setStepItems<-
#' 
#' Description.
#' 
#' @param object  Description.
#' @param value   Description.
#' @return object Description.
#' 
#' @export
#' @docType methods
#' @rdname Analyz.setStepItems-methods
#' 
#' @examples
#' obj <- new("Analyz")
#' v_value <- vector()
#' Analyz.setStepItems(obj) <- v_value
#' @export
#' 
setGeneric("Analyz.setStepItems<-",
           function(object, value){standardGeneric("Analyz.setStepItems<-")})
#' @rdname Analyz.setStepItems-methods
#' @aliases Analyz.setStepItems,Analyz,Analyz-method
setReplaceMethod( f="Analyz.setStepItems",
                  signature  = "Analyz",
                  definition = function(object, value){
                    #---
                    object@stepItems["title"]      <- value[1]
                    object@stepItems["command"]    <- value[2]

                    #--- From position 3 on the values are paramters
                    vParms <- vector()
                    for(i in 3:length(value)){
                      #--- If the variable is empty create the first entry
                      if( !is.na(value[i]) & (value[i] != "") ){
                        vParms <- c(vParms, value[i])
                      }else{
                        break
                      }
                    }
                    if(!length(vParms) == 0){
                      object@stepItems["parameters"] <- vParms  
                    }
                    return(object)
                  }
)
#' Method Analyz.getStepItems 
#' 
#' Description.
#' 
#' @param object Description.
#' @return stepItems  Description.
#' 
#' @export
#' @docType methods
#' @rdname Analyz.getStepItems-methods
#' 
#' @examples
#' obj <- new("Analyz")
#' v_steps <- Analyz.getStepItems(obj)
#' @export
#' 
setGeneric("Analyz.getStepItems",
           function(object){standardGeneric("Analyz.getStepItems")})
#' @rdname Analyz.getStepItems-methods
#' @aliases Analyz.getStepItems, Analyz, Analyz-method
setMethod("Analyz.getStepItems",
          "Analyz",
          function(object){
            vStepItems <- list()
            vStepItems["title"]   <- as.character(object@stepItems["title"])
            vStepItems["command"] <- as.character(object@stepItems["command"])            
            #---
            vParamLine   <- object@stepItems["parameters"]
            vParamType  <- vParamValue <- ""
            vParmIndex  <- 0
            #---
            vVector     <- vector()
            vParameters <- list()
            #---
            vIndex      <- 1
            vItem       <- vector()
            for(x in 3:object@nrColumns){
              vItem <- vParamLine[vIndex]
              #--- Skip NA values
              if(!is.null(vItem)){
                #--- Item is the parameter type
                if(vParamType ==  ""){ 
                  vParamType  <- vItem
                  vParamValue <- ""
                #--- Item is the parameter value
                }else{
                  vParamValue <- vItem
                  #--- Get the value from the result list
                  if(vParamType == "@"){
                    #--- Save previous results
                    if(length(vVector) > 0){
                      vParmIndex  <- vParmIndex+1
                      vParameters[vParmIndex] <-list(vVector)
                      vVector <- vector()
                    }
                    #-- Load the result and save
                    vParmIndex  <- vParmIndex+1
                    vParameters[vParmIndex] <- list( c( Analyz.getResult( object, as.numeric( vParamValue ) ) ) )
                  }else{
                    #--- Coercion needed
                    vVector <- c( vVector, Analyz.coerceType( object, vParamValue, vParamType ))
                  }
                  vParamType  <- ""
                }
              }
            }
            #--- Save last result
            if(length(vVector) > 0){
              vParmIndex <- vParmIndex+1
              vParameters[vParmIndex] <- list(vVector)
            }
            for(z in 1:length(vParameters)){
              vStepItems["parameters"] <- list(vParameters[[z]])
            }
            return(vStepItems)
          }
)
#' Method Analyz.getParameters 
#' 
#' Description.
#' 
#' @param object          Description.
#' @return stepParameters Description.
#' 
#' @export
#' @docType methods
#' @rdname Analyz.getParameters-methods
#' 
#' @examples
#' obj <- new("Analyz")
#' v_paramters <- Analyz.getParameters(obj)
#' @export
#' 
setGeneric("Analyz.getParameters",
           function(object){standardGeneric("Analyz.getParameters")})
#' @rdname Analyz.getParameters-methods
#' @aliases Analyz.getParameters,Analyz,Analyz-method
setMethod("Analyz.getParameters",
          "Analyz",
          function(object){
            vParamType  <- vParamValue <- ""
            vVector     <- vector()
            vParamLine  <- unlist(object@stepItems["parameters"])
            vParamIndex <- 0
            vParameters <- list()
            
            vItem       <- vector()
            for(x in 3:object@nrColumns){
              vItem <- vParamLine[x-2]
              #--- Skip NA values
              if(!is.null(vItem)){
                #--- Item is the parameter type
                if(vParamType == ""){ 
                  vParamType  <- vItem
                  vParamValue <- ""
                  #--- Item is the parameter value
                }else{ 
                  vParamValue <- vItem
                  #--- Get the value from the result list
                  if(vParamType == "@"){
                    #--- Save previous results
                    if(length(vVector) > 0){
                      vParmIndex  <- vParmIndex+1
                      vParameters[vParmIndex] <- list(vVector)
                      vVector <- vector()
                    }
                    #-- Load the result and save
                    vParmIndex  <- vParmIndex+1
                    vParameters[vParmIndex] <- list( c( Analyz.getResult( object, as.numeric( vParamValue ) ) ) )
                    message("Parameters ->> ", vParameters[vParmIndex])
                  }else{
                    #--- Coercion needed
                    vVector <- c( vVector, Analyz.coerceType( object, vParamValue, vParamType ))
                  }
                  vParamType  <- ""
                }
              }
            }
            #--- Save last result
            if(length(vVector) > 0){
              vParmIndex <- vParmIndex+1
              vParameters[vParmIndex] <- list(vVector)
            }
            return(vParameters)
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
#' 
#' @export
#' @docType methods
#' @rdname Analyz.runAnalysis-methods
#' 
#' @examples
#' obj <- new("Analyz")
#' Analyz.runAnalysis(obj, 'mean', list(c(1,2,3)))
#' @export
#' 
setGeneric("Analyz.runAnalysis",
           function(object, command, parameters){standardGeneric("Analyz.runAnalysis")})
#' @rdname Analyz.runAnalysis-methods
#' @aliases Analyz.runAnalysis,Analyz,Analyz-method
setMethod("Analyz.runAnalysis",
          "Analyz",
          function(object, command, parameters){
            result <- c()
            result <- do.call(command, parameters)
            return( result )
          }
)
#' Method Analyz.setResult<-
#' 
#' Description.
#' 
#' @param object  Description.
#' @param value   Description.
#' @return object Description.
#' 
#' @export
#' @docType methods
#' @rdname Analyz.setResult-methods
#' 
#' @examples
#' obj <- new("Analyz")
#' v_result <- vector()
#' Analyz.setResult(obj) <- v_result
#' @export
setGeneric("Analyz.setResult<-",
           function(object, value){standardGeneric("Analyz.setResult<-")})
#' @rdname Analyz.setResult-methods
#' @aliases Analyz.setResult,Analyz,Analyz-method
setReplaceMethod( f="Analyz.setResult",
                  signature="Analyz",
                  definition=function(object, value){
                    # -- BODY
                    object@results <- rbind( object@results, value )
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
#' 
#' @export
#' @docType methods
#' @rdname Analyz.getResult-methods
#' 
#' @examples
#' obj <- new("Analyz")
#' v_result <- Analyz.getResult(obj, 1)
#' @export
setGeneric("Analyz.getResult",
           function(object, index){standardGeneric("Analyz.getResult")})
#' @rdname Analyz.getResult-methods
#' @aliases Analyz.getResult,Analyz,Analyz-method
#' 
setMethod("Analyz.getResult",
          "Analyz",
          function(object, index){ return( object@results[[index, 'value']] ) }
)
#' Method Analyz.coerceType
#' 
#' Method for variable type coercion.
#' 
#' @param object   Description.
#' @param variable Description.
#' @param type     Description.
#' @return result  Description.
#' 
#' @export
#' @docType methods
#' @rdname Analyz.coerceType-methods
#' 
#' @examples
#' obj <- new("Analyz")
#' v_numeric <- Analyz.coerceType(obj, '2', 'numeric')
#' 
setGeneric("Analyz.coerceType",
           function(object, variable, type){standardGeneric("Analyz.coerceType")})
#' @rdname Analyz.coerceType-methods
#' @aliases Analyz.coerceType,Analyz,Analyz-method
#' 
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