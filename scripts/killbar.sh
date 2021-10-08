#!/usr/bin/bash
 
# look for running bar.sh and kill it
pid=$(pstree -lp | grep -- -bar.sh)
pid=${pid##*bar.sh\(}
pid=${pid%%\)*}
kill $pid
