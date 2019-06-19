=for html <a href="https://travis-ci.org/jddurand/MarpaX-ESLIF-ECMA334"><img src="https://travis-ci.org/jddurand/MarpaX-ESLIF-ECMA334.svg?branch=master" alt="Travis CI build status" height="18"></a> <a href="https://badge.fury.io/gh/jddurand%2FMarpaX-ESLIF-ECMA334"><img src="https://badge.fury.io/gh/jddurand%2FMarpaX-ESLIF-ECMA334.svg" alt="GitHub version" height="18"></a> <a href="https://dev.perl.org/licenses/" rel="nofollow noreferrer"><img src="https://img.shields.io/badge/license-Perl%205-blue.svg" alt="License Perl5" height="18">

=cut

use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::ECMA334;

# ABSTRACT: C# as per Standard ECMA-334 5th Edition

# VERSION

# AUTHORITY

=head1 DESCRIPTION

This module parses C# language as per Standard ECMA-334 5th Edition.

=head1 SYNOPSIS

    use MarpaX::ESLIF::ECMA334;

    my $ecma334 = MarpaX::ESLIF::ECMA334->new();
    my $input   = "public interface Test { bool MyTest(); }"
    my $ast     = $ecma334->ast($input);

=cut

use Carp qw/croak/;
use MarpaX::ESLIF 3.0.12;
use Log::Any qw/$log/;

BEGIN {
    # FOR TEST
    use Log::Log4perl qw/:easy/;
    use Log::Any::Adapter;
    use Log::Any::Adapter::Log4perl;  # Just to make sure dzil catches it
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
}

my $_BNF    = do { local $/; <DATA> };

sub new {
    my ($pkg, %options) = @_;

    my $bnf = $_BNF;

    return bless {
        grammar => MarpaX::ESLIF::Grammar->new(MarpaX::ESLIF->new($log), $bnf),
        %options
    }, $pkg
}

1;

__DATA__
#
# 7.3 Lexical analysis
# ====================
#

#
# 7.3.1 General
# -------------

<input>              ::= <input section opt>
<input section opt>  ::=
<input section opt>  ::= <input section>

<input section>      ::= <input section part>+

<input section part> ::= <input elements opt> <new line>
                       | <pp directive>
<input elements opt> ::=
<input elements opt> ::= <input elements>

<input elements>     ::= <input element>+

<input element>      ::= <whitespace>
                       | <comment>
                       | <token>
#
# 7.3.2 Line terminators
# ----------------------

<new line> ::= /[\x{000D}\x{000A}\x{000D}\x{000A}\x{085}\x{2028}\x{2029}]/u

#
# 7.3.3 Comments
# --------------

<comment>                    ::= <single line comment>
                               | <delimited comment>
<single line comment>        ::= '//' <input characters opt>
<input characters>           ::= <input character>+
<input character>            ::= /[^\x{000D}\x{000A}\x{000D}\x{000A}\x{085}\x{2028}\x{2029}]/u
<delimited comment>          ::= '/*' <delimited comment text opt> <asterisks> '*/'
<delimited comment text opt> ::=
<delimited comment text opt> ::= <delimited comment text>
<delimited comment text>     ::= <delimited comment section>+
<delimited comment section>  ::= '/'
                               | <asterisks opt> <not slash or asterisk>
<asterisks opt>              ::=
<asterisks opt>              ::= <asterisks>
<asterisks>                  ::= /\*+/
<not slash or asterisk>      ::= /[^\/*]/

#
# 7.3.4 White space
# -----------------
<whitespace>           ::= <whitespace character>+
<whitespace character> ::= /[\p{Zs}\x{0009}\x{000B}\x{000C}]/

#
# 7.4 Tokens
# ==========

#
# 7.4.1 General
# -------------

<token> ::= <identifier>
          | <keyword>
          | <integer literal>
          | <real literal>
          | <character literal>
          | <string literal>
          | <operator or punctuator>

#
# 7.4.2 Unicode character escape sequence
# ---------------------------------------

<unicode escape sequence> ::= '\\u' <hex digit> <hex digit> <hex digit> <hex digit>
                            | '\\U' <hex digit> <hex digit> <hex digit> <hex digit> <hex digit> <hex digit> <hex digit> <hex digit>

#
# 7.4.3 Identifiers
# -----------------

<identifier>                                     ::= <available identifier>
                                                   | '@' <identifier or keyword>
<available identifier>                           ::= <An identifier or keyword that is not a keyword>
<An identifier or keyword that is not a keyword> ::= <IDENTIFIER OR KEYWORD> - <KEYWORD>
<identifier or keyword>                          ::= <IDENTIFIER OR KEYWORD>

<IDENTIFIER OR KEYWORD>                            ~ <IDENTIFIER START CHARACTER> <IDENTIFIER PART CHARACTERS>
                                                   | <IDENTIFIER START CHARACTER>
<IDENTIFIER START CHARACTER>                       ~ <LETTER CHARACTER>
                                                   | <UNDERSCORE CHARACTER>
<UNDERSCORE CHARACTER>                             ~ /\x{005F}/
                                                   | <A unicode escape sequence representing the character 005F>
<IDENTIFIER PART CHARACTERS>                       ~ <IDENTIFIER PART CHARACTER>+
<IDENTIFIER PART CHARACTER>                        ~ <LETTER CHARACTER>
                                                   | <DECIMAL DIGIT CHARACTER>
                                                   | <CONNECTING CHARACTER>
                                                   | <COMBINING CHARACTER>
                                                   | <FORMATTING CHARACTER>
<LETTER CHARACTER>                                 ~ /[\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}\p{Nl}]/
                                                   | <A unicode escape sequence representing a character of classes Lu Ll Lt Lm Lo or Nl>
<COMBINING CHARACTER>                              ~ /[\p{Mn}\p{Mc}]/
                                                   | <A unicode escape sequence representing a character of classes Mn or Mc>
<DECIMAL DIGIT CHARACTER>                          ~ /[\p{Nd}]/
                                                   | <A unicode escape sequence representing a character of the class Nd>
<CONNECTING CHARACTER>                             ~ /[\p{Pc}]/
                                                   | <A unicode escape sequence representing a character of the class Pc>
<FOMATTING CHARACTER>                              ~ /[\p{Cf}]/
                                                   | <A unicode escape sequence representing a character of the class Cf>

#
# 7.4.4 Keywords
# --------------
keyword ::= <KEYWORD>
<KEYWORD> ~ 'abstract'
          | 'as'
          | 'base'
          | 'bool'
          | 'break'
          | 'byte'
          | 'case'
          | 'catch'
          | 'char'
          | 'checked'
          | 'class'
          | 'const'
          | 'continue'
          | 'decimal'
          | 'default'
          | 'delegate'
          | 'do'
          | 'double'
          | 'else'
          | 'enum'
          | 'event'
          | 'explicit'
          | 'extern'
          | 'false'
          | 'finally'
          | 'fixed'
          | 'float'
          | 'for'
          | 'foreach'
          | 'goto'
          | 'if'
          | 'implicit'
          | 'in'
          | 'int'
          | 'interface'
          | 'internal'
          | 'is'
          | 'lock'
          | 'long'
          | 'namespace'
          | 'new'
          | 'null'
          | 'object'
          | 'operator'
          | 'out'
          | 'override'
          | 'params'
          | 'private'
          | 'protected'
          | 'public'
          | 'readonly'
          | 'ref'
          | 'return'
          | 'sbyte'
          | 'sealed'
          | 'short'
          | 'sizeof'
          | 'stackalloc'
          | 'static'
          | 'string'
          | 'struct'
          | 'switch'
          | 'this'
          | 'throw'
          | 'true'
          | 'try'
          | 'typeof'
          | 'uint'
          | 'ulong'
          | 'unchecked'
          | 'unsafe'
          | 'ushort'
          | 'using'
          | 'virtual'
          | 'void'
          | 'volatile'
          | 'while'
<contextual keyword> ::= 'add'
                       | 'alias'
                       | 'ascending'
                       | 'async'
                       | 'await'
                       | 'by'
                       | 'descending'
                       | 'dynamic'
                       | 'equals'
                       | 'from'
                       | 'get'
                       | 'global'
                       | 'group'
                       | 'into'
                       | 'join'
                       | 'let'
                       | 'orderby'
                       | 'partial'
                       | 'remove'
                       | 'select'
                       | 'set'
                       | 'value'
                       | 'var'
                       | 'where'
                       | 'yield'

#
# 7.4.5. Literals
# ---------------

<literal> ::= <boolean literal>
            | <integer literal>
            | <real literal>
            | <character literal>
            | <string literal>
            | <null literal>

# 7.4.5.2 Boolean literals

<boolean literal> ::= 'true'
                    | 'false'
<TRUE OR FALSE>     ~ 'true'
                    | 'false'

# 7.4.5.3 Integer literals

<integer literal>             ::= <decimal integer literal>
                                | <hexadecimal integer literal>
<decimal integer literal>     ::= <decimal digits> <integer type suffix>
                                | <decimal digits>
<decimal digits>              ::= <decimal digit>+
<decimal digit>               ::= /[0-9]/
<integer type suffix>         ::= 'u':i
                                | 'l':i
                                | 'ul':i
                                | 'lu':i
<integer type suffix opt>     ::=
<integer type suffix opt>     ::= <integer type suffix>
<hexadecimal integer literal> ::= '0x':i <hex digits> <integer type suffix opt>
<hex digits>                  ::= <hex digit>+
<hex digit>                   ::= /[0-9A-Fa-f]/

# 7.4.5.4 Real literals

<real literal>         ::= <decimal digits> '.' <decimal digits> <exponent part opt> <real type suffix opt>
                         | '.' <decimal digits> <exponent part opt> <real type suffix opt>
                         | <decimal digits> <exponent part> <real type suffix opt>
                         | <decimal digits> <real type suffix>
<exponent part opt>    ::=
<exponent part opt>    ::= <exponent part>
<real type suffix opt> ::=
<real type suffix opt> ::= <real type suffix>
<exponent part>        ::= 'e':i <sign opt> <decimal digits>
<sign opt>             ::=
<sign opt>             ::= <sign>
<sign>                 ::= '+'
                         | '-'
<real type suffix>     ::= 'f':i
                         | 'd':i
                         | 'm':i

# 7.4.5.5 Character literals

<character literal>           ::= "'" <character> "'"
<character>                   ::= <single character>
                                | <simple escape sequence>
                                | <hexadecimal escape sequence>
                                | <unicode escape sequence>
<single character>            ::= /[^\x{0027}\x{005C}\x{000D}\x{000A}\x{000D}\x{000A}\x{085}\x{2028}\x{2029}]/u
<simple escape sequence>      ::= "\\'"
                                | '\\"'
                                | '\\\\'
                                | '\\0'
                                | '\\a'
                                | '\\b'
                                | '\\f'
                                | '\\n'
                                | '\\r'
                                | '\\t'
                                | '\\v'
<hexadecimal escape sequence> ::= '\\x' <hex digit>
                                | '\\x' <hex digit> <hex digit>
                                | '\\x' <hex digit> <hex digit> <hex digit>
                                | '\\x' <hex digit> <hex digit> <hex digit> <hex digit>

# 7.4.5.6 String literals

<string literal>                           ::= <regular string literal>
                                             | <verbatim string literal>
<regular string literal>                   ::= '"'                                     '"'
                                             | '"' <regular string literal characters> '"'
<regular string literal characters>        ::= <regular string literal character>+
<regular string literal character>         ::= <single regular string literal character>
                                             | <simple escape sequence>
                                             | <hexadecimal escape sequence>
                                             | <unicode escape sequence>
<single regular string literal character>  ::= /[^\x{0022}\x{005C}\x{000D}\x{000A}\x{000D}\x{000A}\x{085}\x{2028}\x{2029}]/u
<verbatim string literal>                  ::= '@"'                                      '"'
                                             | '@"' <verbatim string literal characters> '"'
<verbatim string literal characters>       ::= <verbatim string literal character>+
<verbatim string literal character>        ::= <single verbatim string literal character>
                                             | <quote escape sequence>
<single verbatim string literal character> ::= /[^"]/
<quote escape sequence>                    ::= '""'

# 7.4.5.7 The null literal

<null literal> ::= 'null'

#
# 7.4.6 Operators and punctuators
# -------------------------------

<operator or punctuator> ::= '{'
                           | '}'
                           | '['
                           | ']'
                           | '('
                           | ')'
                           | '.'
                           | ','
                           | ':'
                           | ';'
                           | '+'
                           | '-'
                           | '*'
                           | '/'
                           | '%'
                           | '&'
                           | '|'
                           | '^'
                           | '!'
                           | '~'
                           | '='
                           | '<'
                           | '>'
                           | '?'
                           | '??'
                           | '::'
                           | '++'
                           | '--'
                           | '&&'
                           | '||'
                           | '=='
                           | '!='
                           | '<='
                           | '>='
                           | '+='
                           | '*='
                           | '/='
                           | '%='
                           | '&='
                           | '|='
                           | '^='
                           | '<<'
                           | '<<='
<right shift> ::= '>' '>'
<right shift assignment> ::= '>' '>='

#
# 7.5 Pre-processing directives
# =============================

#
# 7.5.1 General
# -------------

<pp directive> ::= <pp declaration>
                 | <pp conditional>
                 | <pp line>
                 | <pp diagnostic>
                 | <pp region>
                 | <pp pragma>

#
# 7.5.2 Conditional compilation symbols
# -------------------------------------

<conditional symbol>                             ::= <Any identifier or keyword except true or false>
<Any identifier or keyword except true or false> ::= <IDENTIFIER OR KEYWORD> - <TRUE OR FALSE>

#
# 7.5.3 Pre-processing expressions
# --------------------------------

<whitespace opt>         ::=
<whitespace opt>         ::= <whitespace>
<pp expression>          ::= <whitespace opt> <pp or expression> <whitespace opt>
<pp or expression>       ::= <pp and expression>
                           | <pp or expression> <whitespace opt> '||' <whitespace opt> <pp and expression>
<pp and expression>      ::= <pp equality expression>
                           | <pp and expression> <whitespace opt> '&&' <whitespace opt> <pp equality expression>
<pp equality expression> ::= <pp unary expression>
                           | <pp equality expression> <whitespace opt> '==' <whitespace opt> <pp unary expression>
                           | <pp equality expression> <whitespace opt> '!=' <whitespace opt> <pp unary expression>
<pp unary expression>    ::= <pp primary expression>
                           | '!' <whitespace opt> <pp unary expression>
<pp primary expression>  ::= 'true'
                           | 'false'
                           | <conditional symbol>
                           | '(' <whitespace opt> <pp expression> <whitespace opt> ')'

#
# 7.5.4 Definition directives 
# ---------------------------

<pp declaration> ::= <whitespace opt> '#' <whitespace opt> 'define' <whitespace> <conditional symbol> <pp new line>
                   | <whitespace opt> '#' <whitespace opt> 'undef' <whitespace> <conditional symbol> <pp new line>
<pp new line>    ::= <whitespace opt>                       <new line>
                   | <whitespace opt> <single line comment> <new line>

#
# 7.5.5 Conditional compilation directives
# ----------------------------------------

<pp conditional>          ::= <pp if section> <pp elif sections opt> <pp else section opt> <pp endif>
<pp else section opt>     ::=
<pp else section opt>     ::= <pp else section>
<pp elif sections opt>    ::=
<pp elif sections opt>    ::= <pp elif sections>
<pp if section>           ::= <whitespace opt> '#' <whitespace opt> 'if' <whitespace>  <pp expression> <pp new line> <conditional section opt>
<conditional section opt> ::=
<conditional section opt> ::= <conditional section>
<pp elif sections>        ::= <pp elif section>+
<pp elif section>         ::= <whitespace opt> '#' <whitespace opt> 'elif' <whitespace> <pp expression> <pp new line> <conditional section opt>
<pp else section>         ::= <whitespace opt> '#' <whitespace opt> 'else' <pp new line> <conditional section opt>
<pp endif>                ::= <whitespace opt> '#' <whitespace opt> 'endif' <pp new line>
<conditional section>     ::= <input section>
                            | <skipped section>
<skipped section>         ::= <skipped section part>+
<skipped section part>    ::= <skipped characters opt> <new line>
                            | <pp directive>
<skipped characters opt>  ::=
<skipped characters opt>  ::= <skipped characters>
<skipped characters>      ::= <whitespace opt> <not number sign> <input characters opt>
<input characters opt>    ::=
<input characters opt>    ::= <input characters>
<not number sign>         ::= /[^#]/

#
# 7.5.6 Diagnostic directives
# ---------------------------

<pp diagnostic> ::= <whitespace opt> '#' <whitespace opt> 'error' <pp message>
                  | <whitespace opt> '#' <whitespace opt> 'warning' <pp message>
<pp message>    ::= <new line>
                  | <whitespace> <input characters opt> <new line>


#
# 7.5.7 Region directives
# -----------------------

<pp region>       ::= <pp start region> <conditional section opt> <pp end region>
<pp start region> ::= <whitespace opt> '#' <whitespace opt> 'region' <pp message>
<pp end region>   ::= <whitespace opt> '#' <whitespace opt> 'endregion' <pp message>


#
# 7.5.8 Line directives
# ---------------------

<pp line>              ::= <whitespace opt> '#' <whitespace opt> 'line' <whitespace> <line indicator> <pp new line>
<line indicator>       ::= <decimal digits> <whitespace> <file name>
                         | <decimal digits>
                         | 'default'
                         | 'hidden'
<file name>            ::= '"' <file name characters> '"'
<file name characters> ::= <file name character>+
<file name character>  ::= /[^\x{0022}\x{000D}\x{000A}\x{000D}\x{000A}\x{085}\x{2028}\x{2029}]/u

#
# 7.5.9 Pragma directives
# -----------------------

<pp pragma> ::= <whitespace opt> '#' <whitespace opt> 'pragma' <pp pragma text>
<pp pragma text> ::= <new line>
                   | <whitespace> <input characters opt> <new line>

