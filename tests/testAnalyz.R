# -- Test Initilization
test <- assert()
# -- Class/method initialization
analyze <- analyz()

# -- Test coerce Type
method   <- "Analyz.coerceType"
expected <- as.numeric("1")
actual   <- Analyz.coerceType(analyze, "1", "numeric")
Assert.setResult(test) <- Assert.equals(test, method, actual, expected)

expected <- as.double("1.2")
actual   <- Analyz.coerceType(analyze, "1.2", "double")
Assert.setResult(test) <- Assert.equals(test, method, actual, expected)

expected <- as.character("x")
actual   <- Analyz.coerceType(analyze, "x", "character")
Assert.setResult(test) <- Assert.equals(test, method, actual, expected)

expected <- as.logical(1)
actual   <- Analyz.coerceType(analyze, 1, "logical")
Assert.setResult(test) <- Assert.equals(test, method, actual, expected)

actual_tmp <- Analyz.coerceType(analyze, "a,b,c", "vector")
actual   <- typeof(actual_tmp)
expected <- "list"
Assert.setResult(test) <- Assert.equals(test, method, actual, expected)

actual_tmp <- Analyz.coerceType(analyze, "1", "list")
actual   <- typeof(actual_tmp)
expected <- "list"
Assert.setResult(test) <- Assert.equals(test, method, actual, expected)

# -- Force coerce error
actual   <- Analyz.coerceType(analyze, "a", "numeric")
expected <- FALSE
Assert.setResult(test) <- Assert.equals(test, method, actual, expected)

# --- Test the load of an Analysis file
glbl <- global("biomon")
path <- Global.getAnalysis(glbl)

# -- Class/method initialization
analyze <- analyz()
Analyz.loadSteps(analyze) <- paste0(path,"PCA.csv")

method   <- "Analyz.getNrLines"
expected <- 9L
actual   <- Analyz.getNrLines(analyze)
Assert.setResult(test)<-Assert.equals(test, method, actual, expected )

method   <- "Analyz.getNrColumns"
expected <- 14L
actual   <- Analyz.getNrColumns(analyze)
Assert.setResult(test)<-Assert.equals(test, method, actual, expected )

#--- 

method <- "Analyz.getStepItems"
expected <- "Paste"

Analyz.setStepItems(analyze) <- Analyz.getStep(analyze, 1)
items <- Analyz.getStepItems(analyze)
actual <- items[["title"]]
Assert.setResult(test)<-Assert.equals(test, method, actual, expected )

expected <- list(c("gbl.matrices", "E.csv"))

Analyz.setStepItems(analyze) <- Analyz.getStep(analyze, 1)
items <- Analyz.getStepItems(analyze)
actual <- items["parameters"]
Assert.setResult(test)<-Assert.listEquals(test, method, actual, expected )

# -- Testing error handling
analyze_e <- analyz()
Analyz.loadSteps(analyze_e) <- paste0(path,"PCA.cs")

method   <- "Analyz.getNrLines"
expected <- 0
actual   <- Analyz.getNrLines(analyze_e)
Assert.setResult(test)<-Assert.equals(test, method, actual, expected )

method   <- "Analyz.getNrColumns"
expected <- 0
actual   <- Analyz.getNrColumns(analyze_e)
Assert.setResult(test)<-Assert.equals(test, method, actual, expected )

# -- Test getSteps
actual_tmp   <- Analyz.getStep(analyze, 2)
method   <- "Analyz.getStep"
expected <- "File"
actual   <- actual_tmp[[1]]
Assert.setResult(test)<-Assert.equals(test, method, actual, expected )

expected <- "as.character"
actual   <- actual_tmp[[2]]
Assert.setResult(test)<-Assert.equals(test, method, actual, expected )

expected <- "character"
actual   <- actual_tmp[[3]]
Assert.setResult(test)<-Assert.equals(test, method, actual, expected )

expected <- "@"
actual   <- actual_tmp[[4]]
Assert.setResult(test)<-Assert.equals(test, method, actual, expected )

expected <- "1"
actual   <- actual_tmp[[5]]
Assert.setResult(test)<-Assert.equals(test, method, actual, expected )

# -- Test Set and Get Results
method <- "Analyz.getResult"
do_what <- "mean"
do_args <- list(10,20)
expected <- do.call(do_what, do_args)
Analyz.setResult(analyze)<- expected
actual   <- Analyz.getResult(analyze, 1)
Assert.setResult(test)<-Assert.equals(test, method, actual, expected )

do_what <- "median"
do_args <- list(c(10,20,30))
expected <- do.call(do_what, do_args)
Analyz.setResult(analyze)<- expected
actual   <- Analyz.getResult(analyze, 2)
Assert.setResult(test)<-Assert.equals(test, method, actual, expected )

do_what <- "median"
do_args <- list(c(10,20,30))
expected <- 20
Analyz.setResult(analyze)<- do.call(do_what, do_args)
actual   <- Analyz.getResult(analyze, 3)
Assert.setResult(test)<-Assert.equals(test, method, actual, expected )

do_what <- "read.csv"
do_args <- list(paste0(path,"mean.csv"), as.logical(FALSE))
Analyz.setResult(analyze)<- do.call(do_what, do_args)
  actual   <- Analyz.getResult(analyze, 4)
  expected <- read.csv(file=(paste0(path,"mean.csv")), header = FALSE)
Assert.setResult(test)<-Assert.dFrameEquals(test, method, actual, expected )

# -- Test Set and Get a Step
line <- list("MEAN CALCULATION", "mean", "double", "1", "double", "2", "double", "3")
Analyz.setStepItems(analyze)<-line
exec_result <- Analyz.getStepItems(analyze)

method   <- "Analyz.getStepItems"
expected <- "MEAN CALCULATION"
actual   <- as.character(exec_result["title"])
Assert.setResult(test)<-Assert.equals(test, method, actual, expected)

expected <- list(c(as.double(1), as.double(2), as.double(3)))
actual   <- exec_result["parameters"]
Assert.setResult(test)<-Assert.listEquals(test, method, actual, expected)

expected <- "mean"
actual   <- as.character(exec_result["command"])
Assert.setResult(test)<-Assert.equals(test, method, actual, expected)

method <- "Analyz.runAnalysis"
expected <- mean(c(2,4))
act_command <- "mean"
act_paramters <- list(c(2,4))
actual <- Analyz.runAnalysis(analyze, act_command, act_paramters)
Assert.setResult(test)<-Assert.equals(test, method, actual, expected)

expected <- median(c(2,3,4))
act_command <- "median"
act_paramters <- list(c(2,3,4))
actual <- Analyz.runAnalysis(analyze, act_command, act_paramters)
Assert.setResult(test)<-Assert.equals(test, method, actual, expected)
# ---
Assert.summary(test)