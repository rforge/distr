#!/bin/bash
#
#readout svn-log-file
svn log -v -r $2:$3 --limit $4 $1 >$5  
exit
