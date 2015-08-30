#
# Regular cron jobs for the ebill package
#
0 4	* * *	root	[ -x /usr/sbin/ebill-maintenance ] && /usr/sbin/ebill-maintenance
