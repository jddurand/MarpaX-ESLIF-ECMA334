#!env perl
use strict;
use warnings FATAL => 'all';
use Data::Dumper;
use Data::Section 0.200006 -setup;
use Safe::Isa;
use Test::More;
use Test::More::UTF8;
use Test::Trap;
use Try::Tiny;

use Log::Log4perl qw/:easy/;
use Log::Any::Adapter;
use Log::Any qw/$log/;

binmode STDOUT, ":utf8";


BEGIN {
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
    require_ok('MarpaX::ESLIF::ECMA334::Lexical')
};

my $lexicalParser = MarpaX::ESLIF::ECMA334::Lexical->new;
isa_ok($lexicalParser, 'MarpaX::ESLIF::ECMA334::Lexical');

diag("###########################################################");
diag("Inline data");
diag("###########################################################");
foreach (sort { int((split(' ', $a))[0]) <=> int((split(' ', $b))[0]) } __PACKAGE__->section_data_names) {
    # next unless $_ =~ /022 /;
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

    my %options = (input => $input, encoding => $encoding);

    my $status;
    try {
        my $result = MarpaX::ESLIF::ECMA334::Lexical->new->parse(%options);
        diag Dumper($result);
        $status = 1;
    } catch {
        if ($_->$_isa('MarpaX::ESLIF::ECMA334::Lexical::Exception')) {
            my ($error, $file, $line, $_line, $column, $expected) = ($_->error // 'undef',
                                                                     $_->file // 'undef',
                                                                     $_->line // 'undef',
                                                                     $_->_line // 'undef',
                                                                     $_->column // 'undef',
                                                                     $_->expected // 'undef');
            diag ref($_) . " exception:";
            diag "  Error: " . $_->error if defined($_->error);
            diag "  File: " . $_->file if defined($_->file);
            diag "  Line: " . $_->line . " (pp compliant)" if defined($_->line);
            diag "  Line: " . $_->_line . " (raw)" if defined($_->_line);
            diag "  Column: " . $_->column if defined($_->column);
            diag "  Expected: " . Dumper($_->expected) if defined($_->expected);
        } else {
            diag $_
        }
        $status = 0;
    };

    ok($want_ok ? $status : !$status, $name);
}

#
# Done
#
done_testing();

__DATA__
__[ 001 ok / empty input ]__
__[ 002 ok / string and character literals ]__
// A Hello World! program in C#.
using System;
class Test {
    string string_literal_swedishumlaut = "Å"; // U+00C5
    string string_literal_angstromsign = "Å"; // U+212B
    string string_literal_money = "€"; // U+20AC
    string string_literal_simple_escape_sequences = "\'\"\\\0\a\b\f\n\r\t\v";
    string string_literal_sameThing = "\u20AC";
    char character_literal_single_quote = '\'';
    char character_literal_double_quote = '\"';
    char character_literal_backslash = '\\';
    char character_literal_null = '\0';
    char character_literal_alert = '\a';
    char character_literal_backspace = '\b';
    char character_literal_form_feed = '\f';
    char character_literal_new_line = '\n';
    char character_literal_carriage_return = '\r';
    char character_literal_horizontal_tab = '\t';
    char character_literal_vertical_tab = '\v';
}
__[ 003 ok / example ]__
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
__[ 004 ok / pre-processing ]__
#define A
#undef B
class C
{
#if A
void F() {}
#else
void G() {}
#endif
#if B
void H() {}
#else
void I() {}
#endif
}
__[ 005 ok / pre-processing ]_
#define Enterprise
#if Professional || Enterprise
#define Advanced
#endif
namespace Megacorp.Data
{
#if Advanced
class PivotTable {...} // ... is not valid from syntactic grammar point of view, but is ok from lexical point of view
#endif
}
__[ 006 ko / pre-processing: #define after any token ]_
#line 1 "My File"
#define A
namespace N
{
#define B
#if B
class Class1 {}
#endif
}
__[ 007 ok / pre-processing: two #define ]__
#define A
#define A
__[ 008 ok / pre-processing: two #undef ]__
#define A
#undef A
#undef A
__[ 009 ok / pre-preprocessing: nested conditional sections ]__
#define Debug
#undef Trace
// Debugging on
// Tracing off
class PurchaseTransaction
{
  void Commit() {
#if Debug
    CheckConsistency();
  #if Trace
    WriteToLog(this.ToString());
  #endif
#endif
  CommitHelper();
  }
}
__[ 010 ok / pre-processing: invalid comment but inside a skipped section ]__
#define Debug
// Debugging on
class PurchaseTransaction
{
  void Commit() {
#if Debug
    CheckConsistency();
#else
    /* Do something else
#endif
  }
}
__[ 011 ok / pre-processing in muti-line element ]__
public class Hello
{
  public static void Main() {
    System.Console.WriteLine(@"hello,
#if Debug
      world
#else
      Nebraska
#endif
    ");
  }
}
__[ 012 ok / pre-processing special case ]__
#if X
/*
#else
/* */ class Q { }
#endif
__[ 013 ok / verbatim identifier ]__
class @class
{
	public static void @static(bool @bool) {
		if (@bool)
			System.Console.WriteLine("true"); // true
		else
			System.Console.WriteLine("false"); // false
		}
}

class Class1
{
	static void M() {
		cl\u0061ss.st\u0061tic(true); /* =========> Discarded <======== */
	}
}
__[ 014 ko / Missing #endif ]__
class test
{
#if X
}
__[ 015 ko / Missing #endif ]__
class test
{
#if X
#elif Y
}
__[ 016 ko / Missing #endif ]__
class test
{
#if X
#elif Y
#else
}
__[ 017 ok / Balanced #if/#endif ]__
class test
{
#if X
#elif Y
  string Y = "";
#else
  string Z = "";
#endif
}
__[ 018 ko / #error empty message ]__
class test
{
#error
}
__[ 019 ko / #error not empty message ]__
class test
{
#error "PP Error Message"
}
__[ 020 ok / #warning empty message ]__
class test
{
#warning
}
__[ 021 ok / #warning not empty message ]__
class test
{
#warning "PP Warning Message"
/* This is a comment */
}
__[ 022 ok / #pragma text ]__
#pragma
#pragma With Text No Space After
#pragma With Text With Space After   
#pragma      
 # pragma
  #  pragma With Text No Space After
   #    pragma Using MEDIUM MATHEMATICAL SPACE
     #    pragma      Using OGHAM SPACE MARK
__[ 023 ok / null parse ]__
__[ 024 ok / grammar ambiguity from syntactic point of view, not from lexical point of view ]__
class test
{
  F(G<A, B>(7));
}
__[ 025 ok / right-shift is not productive in lexical grammar ]__
class test
{
  unsigned short short13 = short11 >> 10;
}
__[ 026 ok / right-shift is not productive in lexical grammar ]__
class test
{
  unsigned short short13 = short11 > > 10; /* =========> Discarded <======== */
}
