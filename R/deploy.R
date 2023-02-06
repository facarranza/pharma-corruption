
# readRenviron("~/.Renviron")

print(Sys.getenv("SHINYAPPS_ACCOUNT"))
print(Sys.getenv("SHINYAPPS_TOKEN"))
rsconnect::setAccountInfo(name = Sys.getenv("SHINYAPPS_ACCOUNT"),
                          token = Sys.getenv("SHINYAPPS_TOKEN"),
                          secret = Sys.getenv("SHINYAPPS_SECRET"))
rsconnect::deployApp(
  appDir = "R/",
  appName = "OrcaOperativos",
  appTitle = "OrcaOperativos",
  logLevel = "verbose",
  forceUpdate= TRUE
  # # exclude hidden files and renv directory (if present)
  # appFiles = setdiff(list.files(), "renv")
)
