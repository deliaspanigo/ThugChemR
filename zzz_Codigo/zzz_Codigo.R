
library(devtools)

document()

load_all()

Create_All_TC_EjecucionDeAurora()


document()

load_all()

check()

install()

# use_package('stringr')
# use_package('stats')
# use_data_raw()

# use_r("ChemFormule_Oxyde")

# rename_files(old = "Nomenclature_Oxyde",
#              new = "Take_Nomenclature_Oxyde")

renv::init()
