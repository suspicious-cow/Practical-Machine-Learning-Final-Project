# set a seed in case we use any random items
set.seed(1337)


# Set the names of the packages and libraries you want to install
required_libraries <- c("ggplot2")

# Install missing packages and load all required libraries
for (lib in required_libraries) {
  if (!requireNamespace(lib, quietly = TRUE)) {
    install.packages(lib)
  }
  library(lib, character.only = TRUE)
}