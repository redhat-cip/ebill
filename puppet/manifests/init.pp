# == Class: ebill
#
# === Parameters:
#
# [*monitoring*]
#   IP address of pro-active monitoring server
#
# [*billing*]
#   IP adress of the ebill server
#
class ebill(
  $monitoring = '127.0.0.1',
  $billing    = '127.0.0.1',
) {
  include ::apt

  apt::source { 'ebill':
    location       => 'http://46.231.128.88/ebill/',
    repos          => '/',
    release        => ' ',
    allow_unsigned => true,
  }

  Apt::Source['ebill'] -> Exec['apt_update'] -> Package<||>

  package { ['ebill','ebill-monglue','couchdb']:
    ensure  => installed,
    require => Apt::Source['ebill'],
  }

  file { '/var/lib/ebill/.hosts.erlang' :
    ensure  => file,
    owner   => 'ebill',
    group   => 'ebill',
    mode    => '0644',
    content => template('billing/hosts.erlang.erb'),
    require => Package['ebill'],
  }

  file { '/var/lib/ebill/ebill.conf':
    ensure  => file,
    source  => 'puppet:///modules/billing/ebill.conf',
    mode    => '0644',
    owner   => 'ebill',
    require => Package['ebill'],
  }

  exec { 'Install erlang':
    command   => '/var/lib/ebill/utils/install-erlang.sh',
    cwd       => '/var/lib/ebill',
    user      => 'ebill',
    # erlang compilation (take a while)
    timeout   => 0,
    require   => [Package['ebill'],Package['ebill-monglue']],
    unless    => '/usr/bin/test -d /var/lib/ebill/.kerl',
    logoutput => on_failure,
  }

  exec { 'Launch monglue register':
    command   => '/usr/sbin/ebill-monglue-register',
    cwd       => '/var/lib/ebill',
    user      => 'ebill',
    require   => Exec['Launch eBill'],
    unless    => '/bin/ps aux|/bin/grep -q [m]onglue-register.rb',
    logoutput => on_failure,
  }

  exec { 'Launch monglue control':
    command   => "/usr/sbin/ebill-monglue-control ${monitoring} ${billing} 600 &",
    cwd       => '/var/lib/ebill',
    user      => 'ebill',
    require   => Exec['Launch eBill'],
    unless    => '/bin/ps aux|/bin/grep -q [e]bill-monglue-control',
    logoutput => on_failure,
  }

  exec { 'Launch eBill':
    command   => '/usr/sbin/ebill',
    cwd       => '/var/lib/ebill',
    user      => 'ebill',
    # eBill must compile all deps on the first launch.
    timeout   => 0,
    require   => Exec['Install erlang'],
    unless    => '/bin/ps aux|/bin/grep -q [e]bill_storage',
    logoutput => on_failure,
  }
}
