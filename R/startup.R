.onAttach <- function(libname, pkgname) {
  packageStartupMessage(cli::style_bold(paste("Welcome to associatoR version", getNamespaceVersion("associatoR"))))
  packageStartupMessage("For more info about the package visit https://samuelae.github.io/associatoR")
}
