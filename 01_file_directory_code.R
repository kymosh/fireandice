# set working directory (PC)
setwd("C:/Users/km220416/OneDrive - The University of Montana/thesis/fireandice")

# mac
setwd("/Users/kyliemosher/OneDrive/thesis/fireandice")

# Set source directory
source_dir <- "aso"

# Set destination directory
dest_dir <- "data/raw/ASO/tif"

# List all .tif files recursively that contain "swe" (case insensitive)
tif_files <- list.files(
  path = source_dir,
  pattern = "(?i)swe.*\\.tif$", # regex: contains swe and ends in .tif
  recursive = TRUE,
  full.names = TRUE
)

# Copy files to the destination folder
file.copy(tif_files, dest_dir)

tif_folder <- "C:/Users/km220416/OneDrive - The University of Montana/thesis/fireandice/data/raw/ASO/tif"
aso_folder <- "C:/Users/km220416/OneDrive - The University of Montana/thesis/fireandice/aso"

# List all .tif files recursively in source that contain "swe"
tif_files <- list.files(
  path = source_dir,
  pattern = "(?i)swe.*\\.tif$",
  recursive = TRUE,
  full.names = TRUE
)
tif_basenames <- basename(tif_files)

# List all .tif files in the destination folder (not recursive needed here)
existing_in_dest <- list.files(dest_dir, pattern = "\\.tif$", full.names = FALSE)

# Detect duplicates by name
duplicates <- tif_basenames %in% existing_in_dest

# Delete from source if already in destination
files_to_delete <- tif_files[duplicates]
if (length(files_to_delete) > 0) {
  deleted <- file.remove(files_to_delete)
  cat("Deleted from source:\n")
  print(files_to_delete[deleted])
} else {
  cat("No duplicate files found to delete.\n")
}

# Copy only files not already in destination
files_to_copy <- tif_files[!duplicates]
if (length(files_to_copy) > 0) {
  copied <- file.copy(files_to_copy, dest_dir)
  cat("Copied to destination:\n")
  print(files_to_copy[copied])
} else {
  cat("No new files to copy.\n")
}

# Set source directory
source_dir <- "aso"

# Set destination directory
dest_dir <- "data/raw/ASO/tif"

# List all .tif files recursively that contain "swe" (case insensitive)
tif_files <- list.files(
  path = source_dir,
  pattern = "(?i)swe.*\\.tif$", # regex: contains swe and ends in .tif
  recursive = TRUE,
  full.names = TRUE
)

# Copy files to the destination folder
file.copy(tif_files, dest_dir)

tif_folder <- "C:/Users/km220416/OneDrive - The University of Montana/thesis/fireandice/data/raw/ASO/tif"
aso_folder <- "C:/Users/km220416/OneDrive - The University of Montana/thesis/fireandice/aso"

# List all .tif files recursively in source that contain "swe"
tif_files <- list.files(
  path = source_dir,
  pattern = "(?i)swe.*\\.tif$",
  recursive = TRUE,
  full.names = TRUE
)
tif_basenames <- basename(tif_files)

# List all .tif files in the destination folder (not recursive needed here)
existing_in_dest <- list.files(dest_dir, pattern = "\\.tif$", full.names = FALSE)

# Detect duplicates by name
duplicates <- tif_basenames %in% existing_in_dest

# Delete from source if already in destination
files_to_delete <- tif_files[duplicates]
if (length(files_to_delete) > 0) {
  deleted <- file.remove(files_to_delete)
  cat("Deleted from source:\n")
  print(files_to_delete[deleted])
} else {
  cat("No duplicate files found to delete.\n")
}

# Copy only files not already in destination
files_to_copy <- tif_files[!duplicates]
if (length(files_to_copy) > 0) {
  copied <- file.copy(files_to_copy, dest_dir)
  cat("Copied to destination:\n")
  print(files_to_copy[copied])
} else {
  cat("No new files to copy.\n")
}

##### now for Kaweah data

# Set source directory
source_dir <- "aso/kaweah"


# Set destination directory
dest_dir <- "data/raw/ASO/tif"

# List all .tif files recursively that contain "swe" (case insensitive)
tif_files <- list.files(
  path = source_dir,
  pattern = "(?i)swe.*\\.tif$", # regex: contains swe and ends in .tif
  recursive = TRUE,
  full.names = TRUE
)

# Copy files to the destination folder
file.copy(tif_files, dest_dir)

tif_folder <- "data/raw/ASO/tif"
aso_folder <- "aso/kaweah"

# List all .tif files recursively in source that contain "swe"
tif_files <- list.files(
  path = source_dir,
  pattern = "(?i)swe.*\\.tif$",
  recursive = TRUE,
  full.names = TRUE
)
tif_basenames <- basename(tif_files)

####### now for kern data

# Set source directory
source_dir <- "aso/kern"

# Set destination directory
dest_dir <- "data/raw/ASO/tif"

# List all .tif files recursively that contain "swe" (case insensitive)
tif_files <- list.files(
  path = source_dir,
  pattern = "(?i)swe.*\\.tif$", # regex: contains swe and ends in .tif
  recursive = TRUE,
  full.names = TRUE
)

# Copy files to the destination folder
file.copy(tif_files, dest_dir)

tif_folder <- "data/raw/ASO/tif"
aso_folder <- "aso/kern"

# List all .tif files recursively in source that contain "swe"
tif_files <- list.files(
  path = source_dir,
  pattern = "(?i)swe.*\\.tif$",
  recursive = TRUE,
  full.names = TRUE
)
tif_basenames <- basename(tif_files)


# List all .tif files in the destination folder (not recursive needed here)
existing_in_dest <- list.files(dest_dir, pattern = "\\.tif$", full.names = FALSE)

# Detect duplicates by name
duplicates <- tif_basenames %in% existing_in_dest

# Delete from source if already in destination
files_to_delete <- tif_files[duplicates]
if (length(files_to_delete) > 0) {
  deleted <- file.remove(files_to_delete)
  cat("Deleted from source:\n")
  print(files_to_delete[deleted])
} else {
  cat("No duplicate files found to delete.\n")
}


###### change castle fire perimeter file name
# Set directory path
folder_path <- "C:/Users/km220416/OneDrive - The University of Montana/thesis/fireandice/old/mtbs_castle_fire"

# List all files in the folder
all_files <- list.files(folder_path, full.names = TRUE)

# Define the pattern to match
pattern <- "ca3616111845220200819_20200804_20210807_burn_bndy"

# Filter matching files
matching_files <- all_files[grepl(pattern, basename(all_files))]

# Rename the files
# Set directory path
folder_path <- "C:/Users/km220416/OneDrive - The University of Montana/thesis/fireandice/old/mtbs_castle_fire"

# List all files in the folder
all_files <- list.files(folder_path, full.names = TRUE)

# Define the pattern to match
pattern <- "ca3616111845220200819_20200804_20210807_burn_bndy"
pattern.mask <- "ca3616111845220200819_20200804_20210807_mask"

# Filter matching files
matching_files <- all_files[grepl(pattern, basename(all_files))]
matching_files_mask <- all_files[grepl(pattern.mask, basename(all_files))]

# Check if any files matched
if (length(matching_files) == 0) {
  stop("No files matching the pattern were found.")
}

if (length(matching_files_mask) == 0) {
  stop("No files matching the pattern were found.")
}

# Rename the files
for (old_file in matching_files) {
  ext <- tools::file_ext(old_file)
  new_file <- file.path(folder_path, paste0("castle_fire_perimeter.", ext))
  success <- file.rename(from = old_file, to = new_file)
  if (success) {
    cat("Renamed:", basename(old_file), "→", basename(new_file), "\n")
  } else {
    cat("Failed to rename:", basename(old_file), "\n")
  }
}                     

# Rename the files - mask
for (old_file in matching_files_mask) {
  ext <- tools::file_ext(old_file)
  new_file <- file.path(folder_path, paste0("castle_fire_perimeter_mask.", ext))
  success <- file.rename(from = old_file, to = new_file)
  if (success) {
    cat("Renamed:", basename(old_file), "→", basename(new_file), "\n")
  } else {
    cat("Failed to rename:", basename(old_file), "\n")
  }
}   

#change file names to be more clear that it is for the castle fire
# Set the directory
folder_path <- "C:/Users/km220416/OneDrive - The University of Montana/thesis/fireandice/old/mtbs_castle_fire"

# List all files in the folder
all_files <- list.files(folder_path, full.names = TRUE)

# Filter files that contain the string you want to replace
target_files <- all_files[grepl("ca3616111845220200819", basename(all_files))]

# Rename them by replacing the text in the filename
for (old_file in target_files) {
  new_filename <- gsub("ca3616111845220200819", "castle", basename(old_file))
  new_file <- file.path(folder_path, new_filename)
  success <- file.rename(from = old_file, to = new_file)
  if (success) {
    cat("Renamed:", basename(old_file), "→", new_filename, "\n")
  } else {
    cat("Failed to rename:", basename(old_file), "\n")
  }
}

################# 7/17/25 remove "mosaic" from processed file names
# Define the directory
tif.dir <- here('data', 'processed', 'processed', 'tif')

# List all .tif files that contain "_Mosaic"
tif.files <- list.files(tif.dir, pattern = '_Mosaic.*\\.tif$', full.names = TRUE)

# Loop over and rename each file
for (old.name in tif.files) {
  # Create new name by removing "_Mosaic"
  new.name <- sub('_Mosaic', '', old.name)
  
  # Rename the file
  file.rename(old.name, new.name)
}
