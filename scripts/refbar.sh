#!/usr/bin/bash
 
# look for sleep running inside bar.sh and kill it
pid=$(pstree -lp | grep -- -bar.sh)
pid=${pid##*bar.sh\(*sleep\(}
pid=${pid%\)}
kill $pid
