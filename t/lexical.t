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

binmode STDOUT, ":utf8";

#
# Init log
#
our $defaultLog4perlConf = '
log4perl.rootLogger              = INFO, Screen
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
foreach (sort { int((split(' ', $a))[0]) <=> int((split(' ', $b))[0]) } __PACKAGE__->section_data_names) {
    my $want_ok = ($_ =~ /^[0-9]+\s*ok/);
    my $want_ko = ($_ =~ /^[0-9]+\s*ko/);
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

    my @r = $ecma334->parse(input => $input, encoding => $encoding, definitions => { TRUE => $MarpaX::ESLIF::true,  FALSE => $MarpaX::ESLIF::false });
    if ($want_ok && @r) {
        use Data::Scan::Printer;
        dspp($r[0])
    }

    ok($want_ok ? scalar(@r) : !scalar(@r), $name);
}

#
# Done
#
done_testing();

__DATA__
__[ 001 ok / general ]__
#if TEST // comment
/* comment */
  notvalid
#endif
// A Hello World! program in C#.
using System;
{
    string swedishumlaut = "Å"; // U+00C5
    string angstromsign = "Å"; // U+212B
    string money = "€"; // U+20AC
    string sameThing = "\u20AC";
}
__[002 ok / example ]__
using System;
using Netica;
namespace NeticaDemo
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Welcome to Netica API for C# !");
            Netica.ApplicationClass app = new Netica.ApplicationClass();
            app.Visible = true;
            string net_file_name = AppDomain.CurrentDomain.BaseDirectory + "..\\..\\..\\ChestClinic.dne";

            Streamer file = app.NewStream(net_file_name, null);
            BNet net = app.ReadBNet(file, "");
            net.Compile();
            BNode TB = net.Nodes.get_Item("Tuberculosis");
            double bel = TB.GetBelief("present");
            Console.WriteLine("The probability of tuberculosis is " + bel.ToString("G4"));
 
            BNode XRay = net.Nodes.get_Item("XRay");
            XRay.EnterFinding("abnormal");
            bel = TB.GetBelief("present");
            Console.WriteLine("Given an abnormal X-Ray, the probability of tuberculosis is " + bel.ToString("G4"));
 
            net.Nodes.get_Item("VisitAsia").EnterFinding("visit");
            bel = TB.GetBelief("present");
            Console.WriteLine("Given abnormal X-Ray and visit to Asia, the probability of TB is " + 
                              bel.ToString("G4"));
 
            net.Nodes.get_Item("Cancer").EnterFinding("present");
            bel = TB.GetBelief("present");
            Console.WriteLine("Given abnormal X-Ray, Asia visit, and lung cancer, the probability of TB is" + bel.ToString("G4"));
 
            net.Delete();
            if (!app.UserControl) app.Quit();

            Console.WriteLine("Press <enter> to quit.");
            Console.ReadLine();
        }
    }
}
__[ 005 ok / general ]__
#if TEST

#endif
