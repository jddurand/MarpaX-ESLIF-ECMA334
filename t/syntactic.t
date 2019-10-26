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
log4perl.rootLogger              = TRACE, Screen
log4perl.appender.Screen         = Log::Log4perl::Appender::Screen
log4perl.appender.Screen.stderr  = 0
log4perl.appender.Screen.layout  = PatternLayout
log4perl.appender.Screen.layout.ConversionPattern = %d %-5p %6P %m{chomp}%n
        ';
    Log::Log4perl::init(\$defaultLog4perlConf);
    Log::Any::Adapter->set('Log4perl');
    require_ok('MarpaX::ESLIF::ECMA334')
};

my $ecma334 = MarpaX::ESLIF::ECMA334->new;
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

    my %options = (input => $input, encoding => $encoding);

    my $ast;
    try {
        $ast = $ecma334->parse(%options);
    } catch {
        $ast = undef;
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
    } finally {
        use Data::Scan::Printer;
        local %Data::Scan::Printer::Option = (with_ansicolor => 0);

        # diag Dumper($ast) if defined($ast)
        dspp($ast) if defined($ast)
    };

    ok($want_ok ? defined($ast) : !defined($ast), $name);
}

#
# Done
#
done_testing();

__DATA__
__[ 001 ok / unicode characters ]__
// A Hello World! program in C#.
using System;
{
    string swedishumlaut = "Å"; // U+00C5
    string angstromsign = "Å"; // U+212B
    string money = "€"; // U+20AC
    string sameThing = "\u20AC";
}
__[002 ok / example ]__
namespace NeticaDemo
{
    class Program
    {
        static void Main()
        {
            Console.WriteLine("Welcome to Netica API for C# !");
        }
    }
}
__[ 003 ok / pre-processing ]__
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
__[004 ok / pre-processing ]_
#define Enterprise
#if Professional || Enterprise
#define Advanced
#endif
namespace Megacorp.Data
{
#if Advanced
class PivotTable {...}
#endif
}
__[005 ko / pre-processing: #define after any token ]_
#line 1 "My File"
#define A
namespace N
{
#define B
#if B
class Class1 {}
#endif
}
__[006 ok / pre-processing: two #define ]__
#define A
#define A
__[007 ok / pre-processing: two #undef ]__
#define A
#undef A
#undef A
__[007 ok / pre-preprocessing: nested conditional sections ]__
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
__[008 ok / pre-processing: invalid comment but inside a skipped section ]__
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
__[009 ok / pre-processing in muti-line element ]__
class Hello
{
  static void Main() {
    System.Console.WriteLine(@"hello,
#if Debug
      world
#else
      Nebraska
#endif
    ");
  }
}
__[010 ok / pre-processing special case ]__
#if X
/*
#else
/* */ class Q { }
#endif
__[011 ok / verbatim identifier ]__
class Class1 {
  cl\u0061ss.st\u0061tic(true);
}
__[012 ko / Missing #endif ]__
class test
{
#if X
}
__[013 ko / Missing #endif ]__
class test
{
#if X
#elif Y
}
__[014 ko / Missing #endif ]__
class test
{
#if X
#elif Y
#else
}
__[015 ok / Balanced #if/#endif ]__
class test
{
#if X
#elif Y
  string Y = "";
#else
  string Z = "";
#endif
}
__[016 ko / #error empty message ]__
class test
{
#error
}
__[017 ko / #error not empty message ]__
class test
{
#error "PP Error Message"
}
__[018 ok / #warning empty message ]__
class test
{
#warning
}
__[019 ok / #warning not empty message ]__
class test
{
#warning "PP Warning Message"
/* This is a comment */
}
__[020 ok / #pragma text ]__
#pragma
#pragma With Text No Space After
#pragma With Text With Space After   
#pragma      
 # pragma
  #  pragma With Text No Space After
   #    pragma Using MEDIUM MATHEMATICAL SPACE
     #    pragma      Using OGHAM SPACE MARK
