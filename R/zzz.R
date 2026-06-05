# This function is for displaying a custom message,
# when the package/library is loaded into the enviroment by a user.
# The syntax below is specificly required by R to interpret the function for the
# custom startup message.DO NOT MODIFY the name and the arguments of the functionbelow. You can
# however customize and modify the message block to your liking.

.onAttach <- function(libname, pkgname) {
  # Create a dynamic greeting based on the time of day
  hr <- as.POSIXlt(Sys.time())$hour
  greeting <- if (hr < 12) "Good morning!" else if (hr < 17) "Good afternoon!" else "Good evening!"

  # Build a stylized message block
  msg <- paste0(
    "\n========================================\n",
    " 🚀 ", greeting, " Custom environment ready.\n",
    " Package: ", pkgname, " | Version: ", utils::packageVersion(pkgname), "\n",
    "========================================\n"
  )

  packageStartupMessage(msg)
}
