# Mini HTTPD server that can serve Rpad requests.
# Based on bin/mini/mini1.1.tcl in tclhttpd3.5.1 (http://www.tcl.tk/software/tclhttpd/)
# modified by Tom Short, tshort@eprisolutions.com
## 8/09 Modififications by John Verzani to work with gWidgetsWWW
#
#
# See "TCLHTTPD.license.terms" for copyright and license information.

# Simple Sample httpd/1.[01] server
# Stephen Uhler (c) 1996-1997 Sun Microsystems

# Httpd is a global array containing the global server state
#  root:        the root of the document directory
#  port:        The port this server is serving
#  listen:      the main listening socket id
#  accepts:     a count of accepted connections so far
#  maxtime:     The max time (msec) allowed to complete an http request
#  maxused:     The max # of requests for a socket

# HTTP/1.[01] error codes (the ones we use)

array set HttpdErrors {
    204 {No Content}
    400 {Bad Request}
    404 {Not Found}
    408 {Request Timeout}
    411 {Length Required}
    419 {Expectation Failed}
    503 {Service Unavailable}
    504 {Service Temporarily Unavailable}
    }

array set Httpd {
    bufsize     32768
    maxtime     600000
    maxused     25
}

# Start the server by listening for connections on the desired port.

proc Httpd_Server {root {port 80} {default index.html}} {
    global Httpd

    catch {close Httpd(port)}   ;# it might already be running
    array set Httpd [list root $root default $default port $port]
    array set Httpd [list accepts 0 requests 0 errors 0]
    set Httpd(listen) [socket -server HttpdAccept $port]
    return $Httpd(port)
}

# Accept a new connection from the server and set up a handler
# to read the request from the client.

proc HttpdAccept {sock ipaddr {port {}}} {
    if { $ipaddr == "127.0.0.1" } { # try to ensure some minor security, local requests only
	global Httpd
	upvar #0 Httpd$sock data
	
	incr Httpd(accepts)
	HttpdReset $sock $Httpd(maxused)
	Httpd_Log $sock Connect $ipaddr $port
    }
}
# Initialize or reset the socket state

proc HttpdReset {sock left} {
    global Httpd
    upvar #0 Httpd$sock data

    array set data [list state start linemode 1 version 0 left $left]
    set data(cancel) [after $Httpd(maxtime) [list HttpdTimeout $sock]]
    fconfigure $sock -blocking 0 -buffersize $Httpd(bufsize) \
        -translation {auto crlf}
    fileevent $sock readable [list HttpdRead $sock]
}

# Read data from a client request
# 1) read the request line
# 2) read the mime headers
# 3) read the additional data (if post && content-length not satisfied)

proc HttpdRead {sock} {
    global Httpd
    upvar #0 Httpd$sock data

    # Use line mode to read the request and the mime headers

    if {$data(linemode)} {
        set readCount [gets $sock line]
        set state [string compare $readCount 0],$data(state)
        switch -glob -- $state {
            1,start {
                if {[regexp {(HEAD|POST|GET) ([^?]+)\??([^ ]*) HTTP/1.([01])} $line \
                        x data(proto) data(url) data(query) data(version)]} {
                    set data(state) mime
                    incr Httpd(requests)
                    Httpd_Log $sock Request $data(left) $line
                } else {
                    HttpdError $sock 400 $line
                }
            }
            0,start {
                Httpd_Log $sock Warning "Initial blank line fetching request"
            }
            1,mime {
                if {[regexp {([^:]+):[  ]*(.*)}  $line {} key value]} {
                    set key [string tolower $key]
                    set data(key) $key
                    if {[info exists data(mime,$key)]} {
                        append data(mime,$key) ", $value"
                    } else {
                        set data(mime,$key) $value
                    }
                } elseif {[regexp {^[   ]+(.+)} $line {} value] && \
                        [info exists data(key)]} {
                    append data(mime,$data($key)) " " $value
                } else {
                    HttpdError $sock 400 $line
                }
            }
            0,mime {
                if {$data(proto) == "POST" && \
                        [info exists data(mime,content-length)]} {
                    set data(linemode) 0
                    set data(count) $data(mime,content-length)
                    if {$data(version) && [info exists data(mime,expect]} {
                        if {$data(mime,expect) == "100-continue"} {
                            puts $sock "100 Continue HTTP/1.1\n"
                            flush $sock
                        } else {
                            HttpdError $sock 419 $data(mime,expect)
                        }
                    }
                    fconfigure $sock -translation {binary crlf}
                } elseif {$data(proto) != "POST"}  {
                    HttpdRespond $sock
                } else {
                    HttpdError $sock 411 "Confusing mime headers"
                }
            }
            -1,* {
                if {[eof $sock]} {
                    Httpd_Log $sock Error "Broken connection fetching request"
                    HttpdSockDone $sock 1
                } else {
                    puts stderr "Partial read, retrying"
                }
            }
            default {
                HttpdError $sock 404 "Invalid http state: $state,[eof $sock]"
            }
        }

    # Use counted mode to get the post data

    } elseif {![eof $sock]} {
        Httpd_Log $sock Debug "postdata"
        append data(postdata) [read $sock $data(count)]
        Httpd_Log $sock Debug $data(postdata)
        set data(count) [expr {$data(mime,content-length) - \
                [string length $data(postdata)]}]
        if {$data(count) == 0} {
            HttpdRespond $sock
        }
    } else {
        Httpd_Log $sock Error "Broken connection reading POST data"
        HttpdSockDone $sock 1
    }
}

# Done with the socket, either close it, or set up for next fetch
#  sock:  The socket I'm done with
#  close: If true, close the socket, otherwise set up for reuse

proc HttpdSockDone {sock close} {
    global Httpd
    upvar #0 Httpd$sock data
    after cancel $data(cancel)
    set left [incr data(left) -1]
    unset data
    if {$close} {
        close $sock
    } else {
        HttpdReset $sock $left
    }
    return ""
}

# A timeout happened

proc HttpdTimeout {sock} {
    global Httpd
    upvar #0 Httpd$sock data
    HttpdError $sock 408
}

##########
# Snippets/Sidebar in a Can/The Magic Notebook is (cX) 2001-3 by Jonathan
# Hayward, released under the Artistic License, and comes with no warranty.
# Please visit my homepage at http://JonathansCorner.com to see what else I've
# created--not just software.#Cosmetically adapted from Brent B. Welch, _Practical Programming in Tcl and
#Tk_
proc URLDecode {url} {
    regsub -all {%C2%A0} $url { } url
    regsub -all {\+} $url { } url
    regsub -all {%([[:xdigit:]]{2})} $url \
            {[format %c 0x\1]} url
    return [subst $url]
}
########## end Snippets/Sidebar


# Handle file system queries.  This is a place holder for a more
# generic dispatch mechanism.

proc HttpdRespond {sock} {
    global Httpd HttpdUrlCache
    upvar #0 Httpd$sock data

    set dontcache true 
    regsub  {(^[/]{1,})} $data(url) {} mypath; # strip leading /

    if {[file isdirectory $mypath]} { append mypath $Httpd(default) }

    # If we can't find it in R's current directory, use the root directory
    if {![file exists $mypath]} {
      regsub {(^http://[^/]+)?} $data(url) {} url
      set mypath [HttpdUrl2File $Httpd(root) $url]
      regsub -all "\\.UP\\./" $mypath "../" mypath
      set dontcache false
    }

    if {[file isdirectory $mypath]} { append mypath $Httpd(default) }

    if {[string length $mypath] == 0} {
        HttpdError $sock 400
    } 

#    Httpd_Log $sock Respond "$data(url)"

    ## Here we process the request
    ## run -- run the script
    ## AJAX -- type is assign or runhandler
    ## else serve the file
    if {[regexp {.*gWidgetsWWWRunFromPackage[/]{1,}(.*)} $data(url) match fileName]} {
	## split up post data for AJAX calls
        if {[info exists data(postdata)]} {
            foreach {name value} [split $data(postdata) &=] {
                set user([URLDecode $name]) [URLDecode $value]
            } 
        } elseif {[info exists data(query)]} {
            foreach {name value} [split $data(query) &=] {
                set user([URLDecode $name]) [URLDecode $value]
            } 
        }
	## user(package) has package name
	## filename has filename
	## R script responsible for header information
	R_eval "gWidgetsWWW:::RpadRunFromPackage('$fileName','$user(package)')"
	puts $sock "HTTP/1.$data(version) 200 Data follows"
	if {[string length $RpadTclResults] > 0} {
	    puts -nonewline $sock [subst -novariables -nocommands $RpadTclResults]
	}
	flush $sock
        HttpdSockDone $sock 1 
        return
    }

    if {[regexp {.*gWidgetsWWWrun[/]{1,}(.*)} $data(url) match fileName]} {
	R_eval "gWidgetsWWW:::RpadSourceScript('$fileName')"

	## we run the script -- write header, basic HTML
	## process script through R?
# 	puts $sock "HTTP/1.$data(version) 200 Data follows"
#         puts $sock "Content-Type: text/html"
# 	puts $sock ""
# 	puts $sock "<!DOCTYPE html PUBLIC '-//W3C//DTD XHTML 1.0 Strict//EN' 'http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd'>"
# 	puts $sock "<html xmlns='http://www.w3.org/1999/xhtml' xmlns:v='urn:schemas-microsoft-com:vml'>"
# 	puts $sock "<head>"
# 	puts $sock "<meta http-equiv='Content-Type' content='text/html; charset=UTF-8'>"
# ## JV why are these two commented out?
# 	puts $sock "</head>"
# 	puts $sock "<body>"
# ## jv put in body
# 	puts $sock "<div id='loading'>"
# 	puts $sock "<div class='loading-indicator'><img src='shared/extjs/images/extanim32.gif' width='32' height='32' style='margin-right:8px;float:left;vertical-align:top;'/>gWidgetsWWW<br /><span id='loading-msg'>Loading styles and images...</span></div>"
# 	puts $sock "</div>"
# 	puts $sock "<span id='loading-msg'></span></div>"
# 	puts $sock "<script type='text/javascript'>document.getElementById('loading-msg').innerHTML = 'Loading Core API...';</script>"
# 	puts $sock "<script type='text/javascript' src='/ext/adapter/ext/ext-base.js'></script>"
# 	puts $sock "<script type='text/javascript'>document.getElementById('loading-msg').innerHTML = 'Loading UI Components...';</script>"
# 	puts $sock
# 	puts $sock "<script type='text/javascript' src='/ext/ext-all.js'></script>"
# 	puts $sock "<script type='text/javascript'>document.getElementById('loading-msg').innerHTML = 'Loading gWidgetsWWW...';</script>",              
# 	puts $sock "<script type='text/javascript' src='/gWidgetsWWW.js'></script>"
# 	puts $sock "<script type='text/javascript'>Ext.onReady(function(){Ext.get('loading').remove();});</script>"
# # 	puts $sock "<!-- Call in Ext and its style sheet -->"
# # 	puts $sock "<script type='text/javascript' src='/ext/adapter/ext/ext-base.js'></script>"
# # 	puts $sock "<script type='text/javascript' src='/ext/ext-all.js'></script>"
# # 	puts $sock "<link rel='stylesheet' type='text/css' href='/ext/resources/css/ext-all.css'>"
# # 	puts $sock "<script type='text/javascript' src='/gWidgetsWWW.js'></script>"
	if {[string length $RpadTclResults] > 0} {
	    puts -nonewline $sock [subst -novariables -nocommands $RpadTclResults]
#	    puts $sock "</script>"
	}
#	puts $sock "</body>"
#	puts $sock "</html>"

	flush $sock
        HttpdSockDone $sock 1 
        return
    }


    if {[regexp .*gWidgetsWWWAJAX $data(url)]} {
	## split up post data for AJAX calls
        if {[info exists data(postdata)]} {
            foreach {name value} [split $data(postdata) &=] {
                set user([URLDecode $name]) [URLDecode $value]
            } 
        } elseif {[info exists data(query)]} {
            foreach {name value} [split $data(query) &=] {
                set user([URLDecode $name]) [URLDecode $value]
            } 
        }
        # default return result:
        set result "testtesttest"
        if {$user(type) == "assign"} {
            set result "assign value"
	    ## put command here
	    R_eval "gWidgetsWWW:::RpadAssignValue('$user(variable)', '$user(value)', '$user(sessionID)')"
	    ## signal we are done via return code of 200 or 419 in RpadTclResults variable
	    puts $sock "HTTP/1.$data(version) $RpadTclResults Data follows"
	    puts $sock "Content-Type: text/xml"
	    puts $sock "<?xml version='1.0' encoding='ISO-8859-1'?>"
	    puts $sock "<responseText></responseText>"
	    puts $sock "<readyState>4</readyState>"
	    flush $sock
	    HttpdSockDone $sock 1 
	    return
        }
        if {$user(type) == "runHandler"} {
            set result "assign value"
	    ## put command here
	    R_eval "gWidgetsWWW:::RpadRunHandler('$user(id)', '$user(context)', '$user(sessionID)')"
            puts $sock "HTTP/1.$data(version) 200 Data follows"
            puts $sock "Content-Type: text/javascript"
            puts $sock "Content-Length: [string length [subst -novariables -nocommands $RpadTclResults]]"
            ## Should also close socket if recvd connection close header
            set close [expr {$data(left) == 0}]
	    #            set close 1
            if {$close} {
                puts $sock "Connection: close"
            } elseif {$data(version) == 0 && [info exists data(mime,connection)]} {
                if {$data(mime,connection) == "Keep-Alive"} {
                    set close 0
                    puts $sock "Connection: Keep-Alive"
                }
            }
	    # was orignially lf
            # fconfigure $sock  -translation lf
	    ## tried binary cr auto crlf, all fail!
	    fconfigure $sock  -translation lf 
            puts $sock ""
            puts -nonewline  $sock [subst -novariables -nocommands $RpadTclResults]
            flush $sock
	    HttpdSockDone $sock 1  
            return
        }

	if {$user(type) == "fileupload"} {
	    ## not yet implemented
        }
    }

    if {![file readable $mypath]} {
        HttpdError $sock 404 $mypath
    } else {
        puts $sock "HTTP/1.$data(version) 200 Data follows"
        puts $sock "Date: [HttpdDate [clock seconds]]"
        puts $sock "Last-Modified: [HttpdDate [file mtime $mypath]]"
        puts $sock "Content-Type: [HttpdContentType $mypath]"
        puts $sock "Content-Length: [file size $mypath]"

        ## Should also close socket if recvd connection close header
        set close [expr {$data(left) == 0}]

        if {$close} {
            puts $sock "Connection: close"
        } elseif {$data(version) == 0 && [info exists data(mime,connection)]} {
            if {$data(mime,connection) == "Keep-Alive"} {
                set close 0
                puts $sock "Connection: Keep-Alive"
            }
        }
        puts $sock ""
        flush $sock

        if {$data(proto) != "HEAD"} {
            set in [open $mypath]
            fconfigure $sock -translation binary
            fconfigure $in -translation binary
            fcopy $in $sock -command [list HttpdCopyDone $in $sock $close]
        } else {
            HttpdSockDone $sock $close
        }
    }
}

# Callback when file is done being output to client
# in:  The fd for the file being copied
# sock: The client socket
# close: close the socket if true
# bytes: The # of bytes copied
# error:  The error message (if any)

proc HttpdCopyDone {in sock close bytes {error {}}} {
    global Httpd
    upvar #0 Httpd$sock data
    close $in
    Httpd_Log $sock Done $bytes bytes
    HttpdSockDone $sock $close
}

# convert the file suffix into a mime type
# add your own types as needed

array set HttpdMimeType {
    {}          text/plain
    .txt        text/plain
    .htm        text/html
    .html       text/html
    .gif        image/gif
    .jpg        image/jpeg
    .png        image/png
    .svg        image/svg+xml
    .xbm        image/x-xbitmap
    .css        text/css
    .js     application/x-javascript
    .htc    text/x-component
    .xml    text/xml
    .pdf    application/pdf                
    .eps    application/postscript
    .ps     application/postscript                
    .Rpad   text/html
}

proc HttpdContentType {path} {
    global HttpdMimeType

    set type text/plain
    catch {set type $HttpdMimeType([file extension $path])}
    return $type
}

# Generic error response.

set HttpdErrorFormat {
    <title>Error: %1$s</title>
    Got the error: <b>%2$s</b><br>
    while trying to obtain <b>%3$s</b>
}

# Respond with an error reply
# sock:  The socket handle to the client
# code:  The httpd error code
# args:  Additional information for error logging

proc HttpdError {sock code args} {
    upvar #0 Httpd$sock data
    global Httpd HttpdErrors HttpdErrorFormat

    append data(url) ""
    incr Httpd(errors)
    set message [format $HttpdErrorFormat $code $HttpdErrors($code) $data(url)]
    append head "HTTP/1.$data(version) $code $HttpdErrors($code)"  \n
    append head "Date: [HttpdDate [clock seconds]]"  \n
    append head "Connection: close"  \n
    append head "Content-Length: [string length $message]"  \n

    # Because there is an error condition, the socket may be "dead"

    catch {
        fconfigure $sock  -translation crlf
        puts -nonewline $sock $head\n$message
        flush $sock
    } reason
    HttpdSockDone $sock 1
    Httpd_Log $sock Error $code $HttpdErrors($code) $args $reason
}

# Generate a date string in HTTP format.

proc HttpdDate {seconds} {
    return [clock format $seconds -format {%a, %d %b %Y %T %Z}]
}

# Log an Httpd transaction.
# This should be replaced as needed.

proc Httpd_Log {sock args} {
#    puts stderr "LOG: $sock $args"
}

# Convert a url into a pathname. (UNIX version only)
# This is probably not right, and belongs somewhere else.
# - Remove leading http://... if any
# - Collapse all /./ and /../ constructs
# - expand %xx sequences -> disallow "/"'s  and "."'s due to expansions

proc HttpdUrl2File {root url} {
    regsub -all {//+} $url / url                ;# collapse multiple /'s
    while {[regsub -all {/\./} $url / url]} {}  ;# collapse /./
    while {[regsub -all {/\.\.(/|$)} $url /\x81\\1 url]} {} ;# mark /../
    while {[regsub "/\[^/\x81]+/\x81/" $url / url]} {} ;# collapse /../
    if {![regexp "\x81|%2\[eEfF]" $url]} {      ;# invalid /../, / or . ?
        return $root[HttpdCgiMap $url]
    } else {
        return ""
    }
}

# Decode url-encoded strings.

proc HttpdCgiMap {data} {
    regsub -all {([][$\\])} $data {\\\1} data
    regsub -all {%([0-9a-fA-F][0-9a-fA-F])} $data  {[format %c 0x\1]} data
    return [subst $data]
}

proc bgerror {msg} {
    global errorInfo
    puts stderr "bgerror: $msg\n$errorInfo"
}

#set port 3456
#Httpd_Server /tmp $port index.html
#puts stderr "Starting Tcl httpd SSL server on [info hostname] port $port"
#vwait forever          ;# start the Tcl event loop
