# hi Finland

source("load.r")
source("functions.r")
source("parties.r")

# folders

dir.create("data"   , showWarnings = FALSE)
dir.create("photos" , showWarnings = FALSE)
dir.create("plots"  , showWarnings = FALSE)
dir.create("raw"    , showWarnings = FALSE)

# parameters

plot = TRUE
gexf = TRUE
mode = "fruchtermanreingold"
meta = c(
  "cty" = "Finland",
  "lang" = "fi", # Wikipedia language for chamber and constituencies
  "ch" = "Eduskunta",
  "type" = "Unicameral",
  "ipu" = 2111,
  "seats" = 200
)

# build routine

source("data.r")  # scrape bills and sponsors
source("build.r") # assemble the networks
source("comm.r")  # add committees covariates

# have a nice day
