% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/computeLR.R
\name{computeLR}
\alias{computeLR}
\title{Compute Loss Rate}
\usage{
computeLR(
  fi,
  flame_arrival_time = NULL,
  buildings,
  maxDist = 500,
  LRfactor = 4.94,
  LRexp = 0.34,
  NLRfactor = -0.21,
  NLRcost = 1.37
)
}
\arguments{
\item{fi}{Fire Intensity and position of fires}

\item{flame_arrival_time}{Flame arrival time}

\item{buildings}{Buildings' grid}

\item{maxDist}{Maximal distance from fire edge}

\item{LRfactor}{First parameter of LR formula}

\item{LRexp}{Second parameter of LR formula}

\item{NLRfactor}{First parameter of NLR formula}

\item{NLRcost}{Second parameter of NLR formula}
}
\value{
Square of impacted by fire and summarizing table
}
\description{
Compute Loss Rate based on Fire Intensity FLI and Distance from Fire Edge DFE, according to the formula
LR = LRfactor * FLI^(LRexp)
By default, LRfactor = 4.94 and LRexp = 0.34
For indirect exposure, LR is multiplied by the Normalized Loss Rate NLR
NLR = NLRfactor*Ln(DFE) + NLRcost
By default, NLRfacotr = -0.21 and NLRcost = 1.37
If FLI is greater that 6800 kW/m, then LR=100
If fire edge is further than maxDist, then LR=0
}
\examples{
grid <- createBuildingGrid <- function(buildings, 100, "conservative")
}
