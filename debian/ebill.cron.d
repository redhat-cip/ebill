#
# Regular cron jobs for the ebill package
#
@hourly [ -x /usr/sbin/ebill-maintenance ] && /usr/sbin/ebill-maintenance
