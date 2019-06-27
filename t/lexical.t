#!env perl
use strict;
use warnings FATAL => 'all';
use Data::Section 0.200006 -setup;
use Test::More;
use Test::More::UTF8;
use Test::Trap;

use Log::Any qw/$log/;
use Log::Log4perl qw/:easy/;
use Log::Any::Adapter;
use Log::Any::Adapter::Log4perl;  # Just to make sure dzil catches it

#
# Init log
#
our $defaultLog4perlConf = '
log4perl.rootLogger              = DEBUG, Screen
log4perl.appender.Screen         = Log::Log4perl::Appender::Screen
log4perl.appender.Screen.stderr  = 0
log4perl.appender.Screen.layout  = PatternLayout
log4perl.appender.Screen.layout.ConversionPattern = %d %-5p %6P %m{chomp}%n
';
Log::Log4perl::init(\$defaultLog4perlConf);
Log::Any::Adapter->set('Log4perl');

BEGIN { require_ok('MarpaX::ESLIF::ECMA334') };

my $ecma334 = MarpaX::ESLIF::ECMA334->new();
isa_ok($ecma334, 'MarpaX::ESLIF::ECMA334');

diag("###########################################################");
diag("Inline data");
diag("###########################################################");
foreach (__PACKAGE__->section_data_names) {
    my $want_ok = ($_ =~ /^ok/);
    my $want_ko = ($_ =~ /^ko/);
    #
    # Just in case __DATA__ sections would not start with ok or ko -;
    #
    next unless $want_ok || $want_ko;
    #
    # Test data
    #
    my $input = __PACKAGE__->section_data($_);
    do_test($want_ok, $_, $$input);
}

sub do_test {
    my ($want_ok, $name, $input, $encoding) = @_;

    my @r = $ecma334->lexicalParse($input, $encoding);

    ok($want_ok ? scalar(@r) : !scalar(@r), $name);
}

#
# Done
#
done_testing();

__DATA__
__[ ok / single line comment ]__
// A Hello World! program in C#.
using System;
namespace HelloWorld
{
}
__[ ok / delimited comment 1 ]__
/* A Hello World! program in C#. ****x//*/
using System;
namespace HelloWorld
{
}
__[ ok / delimited comment 2 ]__
/* A Hello World! program in C
   #. ****x
   //*/
using System;
namespace HelloWorld
{
}
