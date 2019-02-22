#!/bin/bash - 
#===============================================================================
#
#          FILE: run.sh
# 
#         USAGE: ./run.sh 
# 
#   DESCRIPTION: 
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Yingsheng Huang (), huangys@ihep.ac.cn
#  ORGANIZATION: IHEP
#       CREATED: 23/02/19 02:18
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error

sed -e "s/{n1,n2,n3,n4}={[0-9],[0-9],[0-9],[0-9]}/{n1,n2,n3,n4}={$1,$2,$3,$4}/g" Script.wls
./Script.wls

