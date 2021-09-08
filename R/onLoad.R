.onAttach <-
function(libname, pkgname) {
    packageStartupMessage("\nlitteR version: ", utils::packageVersion("litteR"))
    packageStartupMessage("Type create_litter_project() to create a new project.")
    packageStartupMessage("Type vignette(\"litteR-manual\") to open the user manual.")
    packageStartupMessage("Open the data file and check the presence of essential column names")
    packageStartupMessage("(see manual, Section 4.2).")
    packageStartupMessage("Open the settings file and check the settings.")
    packageStartupMessage("Type litter() to start litteR.")
}
