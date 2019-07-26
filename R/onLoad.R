.onAttach <-
function(libname, pkgname) {
    packageStartupMessage("\nlitteR version: ", utils::packageVersion("litteR"))
    packageStartupMessage(
        "Copyright 2018-",
        format(Sys.Date(), "%Y"),
        " by Rijkswaterstaat, the Netherlands (RWS)."
    )
    packageStartupMessage("Type citation(\"litteR\") ",
                          "on how to cite litteR in publications.")
    packageStartupMessage("For the tutorial, type: vignette(\"litter-manual\")")
}