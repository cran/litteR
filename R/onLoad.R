.onAttach <-
function(libname, pkgname) {
    packageStartupMessage("\nlitteR version: ", utils::packageVersion("litteR"))
    packageStartupMessage(
        "Copyright 2018-",
        format(Sys.Date(), "%Y"),
        " by Rijkswaterstaat (RWS), the Netherlands."
    )
    packageStartupMessage("Type citation(\"litteR\") ",
                          "on how to cite litteR in publications.")
    packageStartupMessage("Use function 'create_litter_project()' to create a new project.")
    packageStartupMessage("For the tutorial, type: vignette(\"litter-manual\").")
    packageStartupMessage("In case of errors, please consult the troubleshooting section in this tutorial.")
}