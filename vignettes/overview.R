## ------------------------------------------------------------------------
## Load the MicData package!
library(MicData)

## ------------------------------------------------------------------------
## Let's get all the model name!
gModel()

## ------------------------------------------------------------------------
## Get the description of TLM49!
gDescription("tlm49")

## ----, results='asis'----------------------------------------------------
## Get the techinical data of KU100!
knitr::kable(gMicData("ku100"))

