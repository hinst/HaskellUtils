module MyTrace where

import Debug.Trace

traceEnabled = True

trace :: String -> x -> x
trace text x = 
	if 
		traceEnabled
	then
		Debug.Trace.trace text x
	else
		x
 
