
library(devtools)

document()

load_all()

check()

install()

# use_package( 'usethis')

# use_data_raw()

use_r()

rename_files(old = "DataTC_01_PeriodicTable_eng",
             new = "DataTC_01_PeriodicTable")
