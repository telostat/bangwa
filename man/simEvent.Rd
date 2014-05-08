\name{simEvent}
\alias{simEvent}
\title{Simulates a binary event.}
\usage{
simEvent(eventProb, length.out, noSim, nonEvent = -1)
}
\arguments{
  \item{eventProb}{a numeric vector with the probabilities
  of event. Length equals the number of variables.}

  \item{length.out}{the number of rows for each variable}

  \item{noSim}{the number of simulations for each set}

  \item{nonEvent}{the value for the non-events}
}
\value{
simulated binary event
}
\description{
\code{simEvent} is a function that simulates binary events
for a given number of variables and a length of
observations and probabilities of events occuring. It
returns a list with the simulations for each set
}
\examples{
eventProb <- c(0.55, 0.6)
length.out<- 200
noSim     <- 1000
nonEvent  <- -1
myEvents  <- simEvent(eventProb, length.out, noSim, nonEvent)
}

