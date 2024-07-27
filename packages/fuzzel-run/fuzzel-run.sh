#!/bin/bash

prog=$(echo "$PATH" \
	    | sed 's/\:/\n/g' \
	    | xargs -I{} \
		    find -L {} -maxdepth 1 -mindepth 1 \
		    -type f -executable \
		    -printf '%P\n' 2>/dev/null \
 	    | sort -u | sed 's/^\..*//g; /^$/d' | fuzzel -d)

# echo $prog
setsid -f "$prog" >/dev/null 2>&1
