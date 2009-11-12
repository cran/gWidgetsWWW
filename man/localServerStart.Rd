\name{localServerStart}
\alias{localServerStart}
\alias{localServerStop}
\alias{localServerRestart}
\alias{localServerOpen}
\alias{gWidgetsWWWIsLocal}
\alias{makeSessionID}

\title{ Function to interact with local web server }
\description{
	The \pkg{gWidgetsWWW} package allows \code{gWidgets} scripts to be run through a web browser. For wider deployment, the package can be run through the \pkg{RApache} package. To develop scripts and to run scripts standalone, the local server may be used.
	\cr

	The local web server accompanying gWidgetsWWW is controlled through 4 functions to start, stop, restart and open a file. The local web server is a Tcl script, modified from the \pkg{Rpad} package, that runs through the \pkg{tcltk} package. It provides access to \pkg{gWidgetsWWW} files through the 127.0.0.1 IP only.

}
\usage{
localServerStart(file = "", port = 8079, package = NULL)

localServerStop()

localServerRestart()

localServerOpen(file, package)

gWidgetsWWWIsLocal()
}

\arguments{
  \item{file}{Name of file to open. If no \code{package} is specified, this is a file relative to the directory returned by \code{getwd}. If the \code{package} argument is given, then the file is found from the call \code{system.file(file, package=package)}.}
  \item{port}{Port to run on. URLs are of the form 127.0.0.1:port. }
  \item{package}{ If specified, files are found reltive to this package through \code{system.file(file,package=package)}.}
}
\details{

	If no file is specified to \code{localServerStart} then a directory listing of all \code{R} files in the current working directory (\code{getwd}) is given.
	\cr

	Files can be of many types depending on the url: If the URL does not start with \code{gWidgetsWWWrun} or \code{gWidgetsWWWRunFromPackage} then the file is served as is. These can be html files, or svg files, png files, ...
	\cr

	If the url is of the type: \code{http://127.0.0.1/gWidgetsWWWrun/filename.R} then it is assumed the file is a \pkg{gWidgetsWWW} script and it will be so run. 
	\cr

        If the url is of the type: \code{http://127.0.0.1/gWidgetsWWWRunFromPackage/filename.R?package=packagename} then the file is found from \code{system.file(filename.R, package=packagename)} and then run as a \pkg{gWidgetsWWW} script.
	\cr

	The local server borrows that from the \pkg{Rpad} package. It is
        a modified Tcl script and runs under the \pkg{tcltk}
        package. This script handles escaping of backslashed slightly
        differently than the Apache server, so there may be issues
        arising. To check to see if the script is being executed locally
        use \code{gWidgetsWWWIsLocal}.
        \cr

	There is no way to send information to the browser from the R console. The browser must initiate the request.
	\cr

	The local server logs error messages in the console.
	\cr

	The local server will only respond to requests that originate
	from the localhost (127.0.0.1). This is a security measure.

}
\value{
	For \code{localServerStart} and \code{localServerOpen} a new browser page is opened.
}
\references{ The \pkg{Rpad} modifications were made by Tom Short. The original Tcl code is from the tclhttpd project \url{http://www.tcl.tk/software/tclhttpd/ }}
\author{ John Verzani }
\seealso{ \pkg{Rpad} }
\examples{
	\dontrun{
	localServerStart()	# opens browser with files in working directory
	localServerOpen(file="Examples/ex-ggooglemaps.R", package="gWidgetsWWW")
	}
}

\keyword{ misc }

