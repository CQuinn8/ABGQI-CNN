
infile = commandArgs(trailingOnly=TRUE)[1]
#infile = "/published_repo/envs/R_requirements.txt"

# check to make sure specified file exists
if (!file.exists(infile)) {
  stop("Invalid filepath specified...")
}

# get list of packages
packages = readLines(file(infile))
print(sprintf("Installing: %s", paste0(packages, collapse='\n')))

suppressWarnings(install.packages(packages, character.only=TRUE))

print("Done!")