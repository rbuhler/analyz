test <- assert()
emptyChar <- " "
# ---
glob <- global("biomon")

method   <- "Global.getAnalysis"
actual   <- Global.getAnalysis(glob)
expected <- emptyChar
Assert.setResult(test) <- Assert.differs(test, method, actual, expected)

method   <- "Global.getMatrices"
actual   <- Global.getMatrices(glob)
expected <- emptyChar
Assert.setResult(test) <- Assert.differs(test, method, actual, expected)

# ---
glob2 <- global("biom")

method   <- "Global.getAnalysis"
actual   <- Global.getAnalysis(glob2)
expected <- emptyChar
Assert.setResult(test) <- Assert.differs(test, method, actual, expected)

method   <- "Global.getMatrices"
actual   <- Global.getMatrices(glob2) 
expected <- emptyChar
Assert.setResult(test) <- Assert.equals(test, method, actual, expected)

# ---
Assert.summary(test)