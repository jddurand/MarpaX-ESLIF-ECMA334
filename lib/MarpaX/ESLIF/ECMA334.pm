=for html <a href="https://travis-ci.org/jddurand/MarpaX-ESLIF-ECMA334"><img src="https://travis-ci.org/jddurand/MarpaX-ESLIF-ECMA334.svg?branch=master" alt="Travis CI build status" height="18"></a> <a href="https://badge.fury.io/gh/jddurand%2FMarpaX-ESLIF-ECMA334"><img src="https://badge.fury.io/gh/jddurand%2FMarpaX-ESLIF-ECMA334.svg" alt="GitHub version" height="18"></a> <a href="https://dev.perl.org/licenses/" rel="nofollow noreferrer"><img src="https://img.shields.io/badge/license-Perl%205-blue.svg" alt="License Perl5" height="18">

=cut

use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::ECMA334;

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
use Data::Section -setup;
use Log::Any qw/$log/;
use MarpaX::ESLIF::ECMA334::Lexical::RecognizerInterface;
use MarpaX::ESLIF::ECMA334::Lexical::ValueInterface;
use MarpaX::ESLIF 3.0.12;

my $LEXICAL_BNF = ${__PACKAGE__->section_data('lexical')};
my $SYNTACTIC_BNF = ${__PACKAGE__->section_data('syntactic')};

my $LEXICAL_GRAMMAR = MarpaX::ESLIF::Grammar->new(MarpaX::ESLIF->new($log), $LEXICAL_BNF);
my $SYNTACTIC_GRAMMAR = MarpaX::ESLIF::Grammar->new(MarpaX::ESLIF->new($log), $SYNTACTIC_BNF);

# Sub-lexical grammars
my $IDENTIFIER_OR_KEYWORD_BNF = $LEXICAL_BNF; $IDENTIFIER_OR_KEYWORD_BNF .= "\n:start ::= <identifier or keyword>\n";
my $IDENTIFIER_OR_KEYWORD_GRAMMAR = MarpaX::ESLIF::Grammar->new(MarpaX::ESLIF->new($log), $IDENTIFIER_OR_KEYWORD_BNF);

my $KEYWORD_BNF = $LEXICAL_BNF; $KEYWORD_BNF .= "\n:start ::= <keyword>\n";
my $KEYWORD_GRAMMAR = MarpaX::ESLIF::Grammar->new(MarpaX::ESLIF->new($log), $KEYWORD_BNF);

my $UNICODE_ESCAPE_SEQUENCE_BNF = $LEXICAL_BNF; $KEYWORD_BNF .= "\n:start ::= <unicode escape sequence>\n";
my $UNICODE_ESCAPE_SEQUENCE_GRAMMAR = MarpaX::ESLIF::Grammar->new(MarpaX::ESLIF->new($log), $UNICODE_ESCAPE_SEQUENCE_BNF);

sub new {
    my ($pkg, %options) = @_;

    return bless {
        grammars => {
            lexical => $LEXICAL_GRAMMAR,
            syntactic => $SYNTACTIC_GRAMMAR,
            identifier_or_keyword => $IDENTIFIER_OR_KEYWORD_GRAMMAR,
            keyword => $KEYWORD_GRAMMAR,
            unicode_escape_sequence => $UNICODE_ESCAPE_SEQUENCE_GRAMMAR
        },
        options => \%options
    }, $pkg
}

sub lexical_parse {
    my ($self, $input, $eslifRecognizer) = @_;

    return $self->_parse($input,
                         'MarpaX::ESLIF::ECMA334::Lexical::RecognizerInterface',
                         'MarpaX::ESLIF::ECMA334::Lexical::ValueInterface',
                         $eslifRecognizer);
}

sub _parse {
    my ($self, $input, $recognizerInterfaceClass, $valueInterfaceClass, $eslifRecognizer) = @_;

    if (! defined($eslifRecognizer)) {
        my $recognizerInterface = $recognizerInterfaceClass->new(options => $self->{options}, input => $input);
        $eslifRecognizer = MarpaX::ESLIF::Recognizer->new($self->{grammars}->{lexical}, $recognizerInterface);
    }

    # -----------------------------
    # Instanciate a value interface
    # -----------------------------
    my $valueInterface = $valueInterfaceClass->new();

    $eslifRecognizer->scan(1) || croak "scan() failed";   # 1 for initial events, eventually
    $self->_event_manager($recognizerInterfaceClass, $valueInterfaceClass, $eslifRecognizer);
    if ($eslifRecognizer->isCanContinue) {
        do {
            $eslifRecognizer->resume || croak 'resume() failed';
            $self->_event_manager($recognizerInterfaceClass, $valueInterfaceClass, $eslifRecognizer)
        } while ($eslifRecognizer->isCanContinue)
    }
    #
    # We configured value interface to not accept ambiguity not null parse.
    # So no need to loop on value()
    #
    MarpaX::ESLIF::Value->new($eslifRecognizer, $valueInterface)->value() || croak 'Valuation failed';

    # ------------------------
    # Return the value
    # ------------------------
    return $valueInterface->getResult
}

sub sub_lexical_parse {
    my ($self, $recognizerInterfaceClass, $grammar_name, $eslifRecognizer) = @_;
    #
    # Prepare a $grammar_name recognizer that shares its source with $eslifRecognizer
    #
    my $nextRecognizerInterface = $recognizerInterfaceClass->new(options => $self->{options});
    my $nextEslifRecognizer = MarpaX::ESLIF::Recognizer->new($self->{grammars}->{$grammar_name}, $nextRecognizerInterface);
    $nextEslifRecognizer->share($eslifRecognizer);
    
    return $self->lexical_parse(undef, $nextEslifRecognizer);
}

sub _event_manager {
    my ($self, $recognizerInterfaceClass, $valueInterfaceClass, $eslifRecognizer) = @_;

    my $events = $eslifRecognizer->events();
    my $lexemeExpected = $eslifRecognizer->lexemeExpected();
    $log->debugf('Events: %s', $events);
    $log->debugf('Expected: %s', $lexemeExpected);
    my $haveLexeme = 0;

    foreach (@{$events}) {
        my $event = $_->{event};
        next unless $event;  # Can be undef for exhaustion
        my $match;

        if ($event eq '^An_identifier_or_keyword_that_is_not_a_keyword') {
            my $identifier_or_keyword = $self->sub_lexical_parse($recognizerInterfaceClass, 'identifier_or_keyword', $eslifRecognizer);
            my $keyword = $self->sub_lexical_parse($recognizerInterfaceClass, 'keyword', $eslifRecognizer);

            $match = $identifier_or_keyword if (defined($identifier_or_keyword) && ! defined($keyword));

            $eslifRecognizer->lexemeAlternative('AN IDENTIFIER OR KEYWORD THAT IS NOT A KEYWORD', $match) if defined($match);
        }
        elsif ($event eq '^A_unicode_escape_sequence_representing_the_character_005f') {
            my $unicode_escape_sequence = $self->sub_lexical_parse($recognizerInterfaceClass, 'unicode_escape_sequence', $eslifRecognizer);

            $match = $unicode_escape_sequence if (defined($unicode_escape_sequence) && $unicode_escape_sequence eq "\x005f");

            $eslifRecognizer->lexemeAlternative('A UNICODE ESCAPE SEQUENCE REPRESENTING THE CHARACTER 005F', $match) if defined($match);
        }
        elsif ($event eq '^A_unicode_escape_sequence_representing_a_character_of_classes_Lu_Ll_Lt_Lm_Lo_or_Nl') {
            my $unicode_escape_sequence = $self->sub_lexical_parse($recognizerInterfaceClass, 'unicode_escape_sequence', $eslifRecognizer);

            $match = $unicode_escape_sequence if (defined($unicode_escape_sequence) && $unicode_escape_sequence =~ /^[\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}\p{Nl}]$/);

            $eslifRecognizer->lexemeAlternative('A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES LU LL LT LM LO OR NL', $match) if defined($match);
        }
        elsif ($event eq '^A_unicode_escape_sequence_representing_a_character_of_classes_Mn_or_Mc') {
            my $unicode_escape_sequence = $self->sub_lexical_parse($recognizerInterfaceClass, 'unicode_escape_sequence', $eslifRecognizer);

            $match = $unicode_escape_sequence if (defined($unicode_escape_sequence) && $unicode_escape_sequence =~ /^[\p{Mn}\p{Mc}]$/);

            $eslifRecognizer->lexemeAlternative('A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES MN OR MC', $match) if defined($match);
        }
        elsif ($event eq '^A_unicode_escape_sequence_representing_a_character_of_the_class_Nd') {
            my $unicode_escape_sequence = $self->sub_lexical_parse($recognizerInterfaceClass, 'unicode_escape_sequence', $eslifRecognizer);

            $match = $unicode_escape_sequence if (defined($unicode_escape_sequence) && $unicode_escape_sequence =~ /^[\p{Nd}]$/);

            $eslifRecognizer->lexemeAlternative('A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF THE CLASS ND', $match) if defined($match);
        }
        elsif ($event eq '^A_unicode_escape_sequence_representing_a_character_of_the_class_Pc') {
            my $unicode_escape_sequence = $self->sub_lexical_parse($recognizerInterfaceClass, 'unicode_escape_sequence', $eslifRecognizer);

            $match = $unicode_escape_sequence if (defined($unicode_escape_sequence) && $unicode_escape_sequence =~ /^[\p{Pc}]$/);

            $eslifRecognizer->lexemeAlternative('A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF THE CLASS PC', $match) if defined($match);
        }
        elsif ($event eq '^A_unicode_escape_sequence_representing_a_character_of_the_class_Cf') {
            my $unicode_escape_sequence = $self->sub_lexical_parse($recognizerInterfaceClass, 'unicode_escape_sequence', $eslifRecognizer);

            $match = $unicode_escape_sequence if (defined($unicode_escape_sequence) && $unicode_escape_sequence =~ /^[\p{Cf}]$/);

            $eslifRecognizer->lexemeAlternative('A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF THE CLASS CF', $match) if defined($match);
        } else {
            croak "$event unmanaged";
        }

        ++$haveLexeme if defined($match);
    }

    croak 'No Lexeme' unless $haveLexeme;
    $eslifRecognizer->lexemeComplete(1);
}

1;

__DATA__
__[ lexical ]__
#
# 7.3 Lexical analysis
# ====================

#
# 7.3.1 General
# -------------

<input>              ::= <input section opt>                                               # This is the default :start
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

<unicode escape sequence> ::= '\\u' <hex digit> <hex digit> <hex digit> <hex digit>                                                 action => u4
                            | '\\U' <hex digit> <hex digit> <hex digit> <hex digit> <hex digit> <hex digit> <hex digit> <hex digit> action => u8

#
# 7.4.3 Identifiers
# -----------------

<identifier>                                              ::= <available identifier>
                                                            | '@' <identifier or keyword>
<available identifier>                                    ::= <An identifier or keyword that is not a keyword>

<identifier or keyword>                                   ::= <identifier start character> <identifier part characters>
                                                            | <identifier start character>
<identifier start character>                              ::= <letter character>
                                                            | <underscore character>
<underscore character>                                    ::= '_'
                                                            | <A unicode escape sequence representing the character 005f>
<identifier part characters>                              ::= <identifier part character>+
<identifier part character>                               ::= <letter character>
                                                            | <decimal digit character>
                                                            | <connecting character>
                                                            | <combining character>
                                                            | <formatting character>
<letter character>                                        ::= /[\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}\p{Nl}]/
                                                            | <A unicode escape sequence representing a character of classes Lu Ll Lt Lm Lo or Nl>
<combining character>                                     ::= /[\p{Mn}\p{Mc}]/
                                                            | <A unicode escape sequence representing a character of classes Mn or Mc>
<decimal digit character>                                 ::= /[\p{Nd}]/
                                                            | <A unicode escape sequence representing a character of the class Nd>
<connecting character>                                    ::= /[\p{Pc}]/
                                                            | <A unicode escape sequence representing a character of the class Pc>
<formatting character>                                    ::= /[\p{Cf}]/
                                                            | <A unicode escape sequence representing a character of the class Cf>

#
# 7.4.4 Keywords
# --------------
keyword ::= 'abstract'
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

#
# Lexemes
#
<An identifier or keyword that is not a keyword>                                     ::= <AN IDENTIFIER OR KEYWORD THAT IS NOT A KEYWORD>
<A unicode escape sequence representing the character 005f>                          ::= <A UNICODE ESCAPE SEQUENCE REPRESENTING THE CHARACTER 005F>
<A unicode escape sequence representing a character of classes Lu Ll Lt Lm Lo or Nl> ::= <A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES LU LL LT LM LO OR NL>
<A unicode escape sequence representing a character of classes Mn or Mc>             ::= <A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES MN OR MC>
<A unicode escape sequence representing a character of the class Nd>                 ::= <A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF THE CLASS ND>
<A unicode escape sequence representing a character of the class Pc>                 ::= <A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF THE CLASS PC>
<A unicode escape sequence representing a character of the class Cf>                 ::= <A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF THE CLASS CF>
<Any identifier or keyword except true or false>                                     ::= <ANY IDENTIFIER OR KEYWORD EXCEPT TRUE OR FALSE>

<AN IDENTIFIER OR KEYWORD THAT IS NOT A KEYWORD>                                     ~ [^\s\S]     # Matches nothing
<A UNICODE ESCAPE SEQUENCE REPRESENTING THE CHARACTER 005F>                          ~ [^\s\S]     # Matches nothing
<A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES LU LL LT LM LO OR NL> ~ [^\s\S]     # Matches nothing
<A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES MN OR MC>             ~ [^\s\S]     # Matches nothing
<A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF THE CLASS ND>                 ~ [^\s\S]     # Matches nothing
<A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF THE CLASS PC>                 ~ [^\s\S]     # Matches nothing
<A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF THE CLASS CF>                 ~ [^\s\S]     # Matches nothing
<ANY IDENTIFIER OR KEYWORD EXCEPT TRUE OR FALSE>                                     ~ [^\s\S]     # Matches nothing


event ^An_identifier_or_keyword_that_is_not_a_keyword                                     = predicted <An identifier or keyword that is not a keyword>
event ^A_unicode_escape_sequence_representing_the_character_005f                          = predicted <A unicode escape sequence representing the character 005f>
event ^A_unicode_escape_sequence_representing_a_character_of_classes_Lu_Ll_Lt_Lm_Lo_or_Nl = predicted <A unicode escape sequence representing a character of classes Lu Ll Lt Lm Lo or Nl>
event ^A_unicode_escape_sequence_representing_a_character_of_classes_Mn_or_Mc             = predicted <A unicode escape sequence representing a character of classes Mn or Mc>
event ^A_unicode_escape_sequence_representing_a_character_of_the_class_Nd                 = predicted <A unicode escape sequence representing a character of the class Nd>
event ^A_unicode_escape_sequence_representing_a_character_of_the_class_Pc                 = predicted <A unicode escape sequence representing a character of the class Pc>
event ^A_unicode_escape_sequence_representing_a_character_of_the_class_Cf                 = predicted <A unicode escape sequence representing a character of the class Cf>
event ^Any_identifier_or_keyword_except_true_or_false                                     = predicted <Any identifier or keyword except true or false>

__[ syntactic ]__
todo ::= 'TO DO'
