library(reschola)
source("shared.R")

# download all data from GDrive folder set in `shared.R` into `data-input`
reschola::gd_download_folder(gd_url,
                             overwrite = FALSE,
                             files_from_subfolders = TRUE)

# If there is other data you expect to only retrieve once
# (from the web, public databases or APIs, etc.),
# this might be a good place to store the code that does it.

reschola::gd_download_folder(
  irt_models,
  dest_dir = "irt_models",
  overwrite = TRUE,
  files_from_subfolders = T
)
