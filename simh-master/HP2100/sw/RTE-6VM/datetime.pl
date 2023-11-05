################################################################################
#
# Print the time and date in the form of an RTE system command "TM"
# Prints ":SYTM,YYYY,DDD,HH,MM,SS
# where YYYY is the 4 digit year
#        DDD is the 3 digit julian day
#         HH is the 2 digit hour
#         MM is the 2 digit minute
#         SS is the 2 digit second
# 
# Ken Cornetet 2008/07/02
# ken@cornetet.org
#
################################################################################

use strict;
use POSIX;

my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime(time());

print POSIX::strftime(":SYTM,%Y,%j,%H,%M,%S\n", $sec, $min, $hour, $mday, $mon, $year, -1, -1, -1);

exit 0;
