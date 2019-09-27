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

binmode STDOUT, ":utf8";


BEGIN {
 require_ok('MarpaX::ESLIF::ECMA334')
};

my $lexicalParser = MarpaX::ESLIF::ECMA334::Lexical->new;
isa_ok($lexicalParser, 'MarpaX::ESLIF::ECMA334::Lexical');

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

    my $lexicalAst;
    try {
        $lexicalAst = MarpaX::ESLIF::ECMA334::Lexical->new->parse(%options);
    } catch {
        if ($_->$_isa('MarpaX::ESLIF::ECMA334::Lexical::Exception')) {
            my ($error, $file, $line, $_line, $column, $expected) = ($_->error // 'undef',
                                                                     $_->file // 'undef',
                                                                     $_->line // 'undef',
                                                                     $_->_line // 'undef',
                                                                     $_->column // 'undef',
                                                                     $_->expected // 'undef');
            diag "$error at file \"$file\", line $line, column $column, expected: " . Dumper($expected)
        } else {
            diag $_
        }
    } finally {
        # diag Dumper($lexicalAst) if defined($lexicalAst)
    };

    ok($want_ok ? defined($lexicalAst) : !defined($lexicalAst), $name);
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
class @class
{
  public static void @static(bool @bool) {
    if (@bool)
      System.Console.WriteLine("true");
    else
      System.Console.WriteLine("false");
  }
}
class Class1
{
  static void M() {
    cl\u0061ss.st\u0061tic(true);
  }
}
__[012 ko / single character is below 0xFFFF ]__
string X = '🍄';
__[013 ko / single regular string literal character is below 0xFFFF ]__
string X = "🍄";
__[014 ok / string literals ]__
string a = "Happy birthday, Joel";
string b = @"Happy birthday, Joel"; // Happy birthday, Joel
// Happy birthday, Joel
string c = "hello \t world";
string d = @"hello \t world"; // hello
world
// hello \t world
string e = "Joe said \"Hello\" to me";
string f = @"Joe said ""Hello"" to me"; // Joe said "Hello" to me
// Joe said "Hello" to me
string g = "\\\\server\\share\\file.txt"; // \\server\share\file.txt
string h = @"\\server\share\file.txt";
// \\server\share\file.txt
string i = "one\r\ntwo\r\nthree";
string j = @"one
two
three";
__[015 ok / pp line and pragma mixed with tokens ]__
#line 999
#line 1 "text.cs"
"String at line 1 in text.cs";
#pragma
"String at line 3 in text.cs";
#pragma Pragma Text
"String at line 5 in text.cs after #pragma";
#line 1 "textagain.cs"
"String again at line 1 in textagain.cs";
