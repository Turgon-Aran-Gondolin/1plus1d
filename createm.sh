#!/bin/bash - 
#===============================================================================
#
#          FILE: createm.sh
# 
#         USAGE: ./createm.sh 
# 
#   DESCRIPTION: 
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Yingsheng Huang (), huangys@ihep.ac.cn
#  ORGANIZATION: IHEP
#       CREATED: 02/22/19 17:41
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error
sed '1d' Script.wls > Script.m

