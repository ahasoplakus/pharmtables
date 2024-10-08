# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

# golem default's to these options
pkgload::load_all(helpers = FALSE, attach_testthat = FALSE) # export_all = FALSE # ac removed: if false, IDEAFilter fails
options("golem.app.prod" = TRUE)
options(shiny.sanitize.errors = FALSE)
# options(shiny.autoload.r=FALSE) # needed for turning off options
# rsconnect::writeManifest() # Needed for continuous deployment

# Launch the app
run_app() # add parameters here (if any)
