% Generated by roxygen2 (4.1.0.9001): do not edit by hand
% Please edit documentation in R/ClassAnalyz.R
\docType{methods}
\name{Analyz.getStepParameters}
\alias{Analyz.getStepParameters}
\alias{Analyz.getStepParameters,Analyz,Analyz-method}
\alias{Analyz.getStepParameters,Analyz-method}
\title{Method Analyz.getStepParameters}
\usage{
Analyz.getStepParameters(object)

\S4method{Analyz.getStepParameters}{Analyz}(object)
}
\arguments{
\item{object}{Object instance.}
}
\value{
stepParameters Description of the current step command parameters.
}
\description{
Method for returning the current step command parameters.
}
\examples{
obj <- new("Analyz")
v_Parameters <- Analyz.getStepParameters(obj)
}

