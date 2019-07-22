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
use Encode qw/decode/;
use Log::Any qw/$log/;
use MarpaX::ESLIF::ECMA334::Lexical::RecognizerInterface;
use MarpaX::ESLIF::ECMA334::Lexical::ValueInterface;
use MarpaX::ESLIF 3.0.13; # if-action

$Log::Log4perl::Logger::APPENDER_BY_NAME{Screen}->threshold($TRACE);

my $PRE_LEXICAL_BNF           = ${__PACKAGE__->section_data('pre lexical')};
my $LEXICAL_BNF               = ${__PACKAGE__->section_data('lexical')};
my $IDENTIFIER_OR_KEYWORD_BNF = ${__PACKAGE__->section_data('identifier or keyword')};
my $KEYWORD_BNF               = ${__PACKAGE__->section_data('keyword')};

my $PRE_LEXICAL_GRAMMAR = MarpaX::ESLIF::Grammar->new(MarpaX::ESLIF->new($log), $PRE_LEXICAL_BNF);
$log->debug('pre lexical grammar compiled');
my $LEXICAL_GRAMMAR     = MarpaX::ESLIF::Grammar->new(MarpaX::ESLIF->new($log), $LEXICAL_BNF);
$log->debug('lexical grammar compiled');
my $IDENTIFIER_OR_KEYWORD_GRAMMAR = MarpaX::ESLIF::Grammar->new(MarpaX::ESLIF->new($log), $IDENTIFIER_OR_KEYWORD_BNF);
$log->debug('identifier or keyword grammar compiled');
my $KEYWORD_GRAMMAR = MarpaX::ESLIF::Grammar->new(MarpaX::ESLIF->new($log), $KEYWORD_BNF);
$log->debug('keyword grammar compiled');

# ============================================================================
# new
# ============================================================================
sub new {
    my ($pkg) = @_;

    return bless {}, $pkg
}

# ============================================================================
# parse
# ============================================================================
sub parse {
    my ($self, %options) = @_;

    #
    # Pre-parse
    #
    my $preparsedInput = $self->_preparse(%options);    

    #
    # Lexical parse
    #
    return $self->_parse($LEXICAL_GRAMMAR,
                         'MarpaX::ESLIF::ECMA334::Lexical::RecognizerInterface',
                         'MarpaX::ESLIF::ECMA334::Lexical::ValueInterface',
                         \&_lexicalEventManager,
                         %options,
                         input => $preparsedInput,
                         encoding => 'UTF-8'
        );
}

# ============================================================================
# _preparse
# ============================================================================
sub _preparse {
    my ($self, %options) = @_;

    my $input = delete($options{input}) // '';
    my $encoding = delete($options{encoding}); # Can be undef

    my $output = $input;
    if (bytes::length($input)) {
        #
        # Pre-parse: we require this is a valid source file from encoding point
        #
        my $recognizerInterface = MarpaX::ESLIF::ECMA334::Lexical::RecognizerInterface->new(input => $input, encoding => $encoding);
        my $valueInterface = MarpaX::ESLIF::ECMA334::Lexical::ValueInterface->new();
        $PRE_LEXICAL_GRAMMAR->parse($recognizerInterface, $valueInterface) || croak 'Pre-parse phase failure';
        $output = $valueInterface->getResult();

        #
        # - If the last character of the source file is a Control-Z character (U+001A), this character is deleted
        #
        if ($output =~ s/\x{001A}$//) {
            $log->debugf('Deleted Control-Z character (U+001A) at the end of the source file');
        }

        #
        # - A carriage-return character (U+000D) is added to the end of the source file if that source file is non-
        #   empty and if the last character of the source file is not a carriage return (U+000D), a line feed
        #   (U+000A), a next line character (U+0085), a line separator (U+2028), or a paragraph separator
        #   (U+2029)
        #
        if (bytes::length($output) > 0 && $output !~ /[\x{000D}\x{000A}\x{0085}\x{2028}\x{2029}]$/) {
            $output .= "\x{000D}";
            $log->debugf('Added carriage-return character (U+000D) at the end of the source file');
        }
    }

    return $output # Always UTF-8 if not empty
}

# ============================================================================
# _parse
# ============================================================================
sub _parse {
    my ($self, $grammar, $recognizerInterfaceClass, $valueInterfaceClass, $eventManager, %options) = @_;

    # -------------------------------------------
    # Instanciate recognizer and value interfaces
    # -------------------------------------------
    my $recognizerInterface = $recognizerInterfaceClass->new(%options);
    my $valueInterface = $valueInterfaceClass->new();

    # ------------------------
    # Instanciate a recognizer
    # ------------------------
    $log->debugf('MarpaX::ESLIF::Recognizer->new($grammar=%s, $recognizerInterface=%s)', $grammar, $recognizerInterface);
    my $eslifRecognizer = MarpaX::ESLIF::Recognizer->new($grammar, $recognizerInterface);

    # -------------------------------------------------------------------------
    # Make sure all data (that is recognizer interface memory) is seen by ESLIF
    # -------------------------------------------------------------------------
    croak 'read failure' unless $eslifRecognizer->read();
    #
    # Input must be defined
    #
    croak 'undefined input' unless defined $eslifRecognizer->input();

    # -----------------------------------------------------
    # Run recognizer manually so that events are accessible
    # -----------------------------------------------------
    $eslifRecognizer->scan(1) || croak 'Initial scan failed';
    $self->$eventManager($eslifRecognizer, $recognizerInterface);
    if ($eslifRecognizer->isCanContinue) {
        {
            do {
                if (! $eslifRecognizer->resume) {
                    #
                    # This is a failure unless it is a sub-grammar that has reached completion at least once
                    #
                    if ($recognizerInterface->hasCompletion && $recognizerInterface->recurseLevel) {
                        last;
                    } else {
                        croak 'resume() failed';
                    }
                }
                $self->$eventManager($eslifRecognizer, $recognizerInterface)
            } while ($eslifRecognizer->isCanContinue)
        }
    }

    # -----------------------------------------------------------------------------------------
    # Call for valuation (we configured value interface to not accept ambiguity not null parse)
    # -----------------------------------------------------------------------------------------
    my $value = MarpaX::ESLIF::Value->new($eslifRecognizer, $valueInterface);
    croak 'Valuation failure' unless $value->value();

    # ------------------------
    # Return the value
    # ------------------------
    my $rc = $valueInterface->getResult;
    return $rc;
}

sub _lexicalEventManager {
    my ($self, $eslifRecognizer, $recognizerInterface) = @_;

    foreach (@{$eslifRecognizer->events}) {
	my $event = $_->{event} // '';
        my $match = undef;
	my $lexeme = undef;

        if ($event eq 'not_a_keyword[]') {
            my ($offset, $length) = $eslifRecognizer->lastCompletedLocation('an identifier or keyword that is not a keyword');
            my $identifier_or_keyword = decode('UTF-8', bytes::substr($recognizerInterface->data, $offset, $length), Encode::FB_CROAK);
            my $keyword = eval {
                $self->_parse($KEYWORD_GRAMMAR,
                              'MarpaX::ESLIF::ECMA334::Lexical::RecognizerInterface',
                              'MarpaX::ESLIF::ECMA334::Lexical::ValueInterface',
                              \&_lexicalEventManager,
                              input => $identifier_or_keyword,
                              encoding => 'UTF-8');
            };
	    croak "$identifier_or_keyword: a keyword is not allowed" if defined($keyword);
	    $log->debugf('[%2d] $identifier_or_keyword=%s: ok, not a keyword', $recognizerInterface->recurseLevel, $identifier_or_keyword);
        }
        elsif ($event eq 'not_a_boolean_literal[]') {
            # We want to verify that <identifier or keyword> is not a 'true' or 'false'
            #
            my ($offset, $length) = $eslifRecognizer->lastCompletedLocation('any identifier or keyword except true or false');
            my $identifier_or_keyword = decode('UTF-8', bytes::substr($recognizerInterface->data, $offset, $length), Encode::FB_CROAK);
	    croak "$identifier_or_keyword: true or false is not allowed" if ($identifier_or_keyword eq 'true' or $identifier_or_keyword eq 'false');
	    $log->debugf('[%2d] $identifier_or_keyword=%s: ok, not true or false', $recognizerInterface->recurseLevel, $identifier_or_keyword);
        }
        else {
            croak "Unsupported event $event";
        }

	if (defined($match)) {
	    $log->debugf('[%2d] Lexeme read: %s: %s (%d bytes)', $recognizerInterface->recurseLevel, $lexeme, $match, bytes::length($match));
	    $eslifRecognizer->lexemeRead($lexeme, $match, bytes::length($match));
	}
    }
}

1;

__DATA__
__[ pre lexical ]__
:default ::= action => ::convert[UTF-8]
:desc ::= 'Pre lexical grammar'

<input> ::= /./su *

__[ lexical ]__
:default ::= action => ::convert[UTF-8]
:desc ::= 'Lexical grammar'
:discard ::= <comment>

#
# Note that the spec does NOT say if file-name characters disable comments. It is assume that they do.
#
:terminal ::= "'"  pause => after event => :discard[switch]           # Disable comment in character literal
:terminal ::= '"'  pause => after event => :discard[switch]           # Disable comment in regular string literal
:terminal ::= '@"' pause => after event => :discard[switch]           # Disable comment in verbatim string literal (Note that it ends with '"' character)

<input>                ::= <input section opt>
<input section opt>    ::=
<input section opt>    ::= <input section>
<input section>        ::= <input section part>+
<input section part>   ::= <input elements opt> <new line>
                          | <pp directive>
<input elements opt>   ::=
<input elements opt>   ::= <input elements>
<input elements>       ::= <input element>+
<input element>        ::= <whitespace>
                         | <token>

<new line> ::= /\x{000D}/                     # Carriage return character
             | /\x{000A}/                     # Line feed character
             | /\x{000D}\x{000A}/             # Carriage return character followed by line feed character
             | /\x{0085}/                     # Next line character
             | /\x{2028}/u                    # Line separator character
             | /\x{2029}/u                    # Paragraph separator character

<whitespace>           ::= <whitespace character>+
<whitespace character> ::= /\p{Zs}/u                      # Any character with Unicode class Zs
                         | /\x{0009}/                     # Horizontal tab character
                         | /\x{000B}/                     # Vertical tab character
                         | /\x{000C}/                     # Form feed character

<comment>                    ::= <single line comment>
                               | <delimited comment>
<single line comment>        ::= '//' <input characters opt>
<input characters opt>       ::=
<input characters opt>       ::= <input characters>
<input characters>           ::= <input character>+
<input character>            ::= /[^\x{000D}\x{000A}\x{0085}\x{2028}\x{2029}]/u   # <ANY UNICODE CHARACTER EXCEPT A NEW LINE CHARACTER>
<new line character>         ::= /[\x{000D}\x{000A}\x{0085}\x{2028}\x{2029}]/u

<delimited comment>          ::= '/*' <delimited comment text opt> <asterisks> '/'
<delimited comment text opt> ::=
<delimited comment text opt> ::= <delimited comment text>
<delimited comment text>     ::= <delimited comment section>+
<delimited comment section>  ::= '/' <asterisks opt> <not slash or asterisk>
<asterisks opt>              ::=
<asterisks opt>              ::= <asterisks>
<asterisks>                  ::= '*'+
<not slash or asterisk>      ::= /[^\/*]/u # Any Unicode character except / or *

<token> ::= <identifier>
          | <keyword>
          | <integer literal>
          | <real literal>
          | <character literal>
          | <string literal>
          | <operator or punctuator>

<unicode escape sequence> ::= '\\u' <hex digit> <hex digit> <hex digit> <hex digit>
                            | '\\U' <hex digit> <hex digit> <hex digit> <hex digit> <hex digit> <hex digit> <hex digit> <hex digit>
<identifier> ::= <available identifier>
               | '@' <identifier or keyword>

<available identifier>  ::= <an identifier or keyword that is not a keyword>
event not_a_keyword[] = nulled <not a keyword>
<an identifier or keyword that is not a keyword> ::= <identifier or keyword> <not a keyword>
<not a keyword> ::=
<identifier or keyword> ::= <identifier start character> <identifier part characters opt>
<identifier part characters opt> ::=
<identifier part characters opt> ::= <identifier part characters>
<identifier start character> ::= <letter character>
                               | <underscore character>
<underscore character>       ::= '_' # The underscore character U+005F
                               | <A UNICODE ESCAPE SEQUENCE REPRESENTING THE CHARACTER 005F>
<identifier part characters> ::= <identifier part character>+
<identifier part character>  ::= <letter character>
                               | <decimal digit character>
                               | <connecting character>
                               | <combining character>
                               | <formatting character>
<letter character>           ::= /[\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}\p{Nl}]/u
                               | <A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Lu Ll Lt Lm Lo or Nl>
<combining character>        ::= /[\p{Mn}\p{Mc}]/u
                               | <A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Mn or Mc>
<decimal digit character>    ::= /[\p{Nd}]/u
                               | <A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Nd>
<connecting character>       ::= /[\p{Pc}]/u
                               | <A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Pc>
<formatting character>       ::= /[\p{Cf}]/u
                               | <A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Cf>

<keyword> ::= 'abstract'
            | 'byte'
            | 'class'
            | 'delegate'
            | 'event'
            | 'fixed'
            | 'if'
            | 'internal'
            | 'new'
            | 'override'
            | 'readonly'
            | 'short'
            | 'struct'
            | 'try'
            | 'unsafe'
            | 'volatile'
            | 'as'
            | 'case'
            | 'const'
            | 'do'
            | 'explicit'
            | 'float'
            | 'implicit'
            | 'is'
            | 'null'
            | 'params'
            | 'ref'
            | 'sizeof'
            | 'switch'
            | 'typeof'
            | 'ushort'
            | 'while'
            | 'base'
            | 'catch'
            | 'continue'
            | 'double'
            | 'extern'
            | 'for'
            | 'in'
            | 'lock'
            | 'object'
            | 'private'
            | 'return'
            | 'stackalloc'
            | 'this'
            | 'uint'
            | 'using'
            | 'bool'
            | 'char'
            | 'decimal'
            | 'else'
            | 'false'
            | 'foreach'
            | 'int'
            | 'long'
            | 'operator'
            | 'protected'
            | 'sbyte'
            | 'static'
            | 'throw'
            | 'ulong'
            | 'virtual'
            | 'break'
            | 'checked'
            | 'default'
            | 'enum'
            | 'finally'
            | 'goto'
            | 'interface'
            | 'namespace'
            | 'out'
            | 'public'
            | 'sealed'
            | 'string'
            | 'true'
            | 'unchecked'
            | 'void'

<contextual keyword> ::= 'add'
                       | 'by'
                       | 'get'
                       | 'let'
                       | 'set'
                       | 'alias'
                       | 'descending'
                       | 'global'
                       | 'orderby'
                       | 'value'
                       | 'ascending'
                       | 'dynamic'
                       | 'group'
                       | 'partial'
                       | 'var'
                       | 'async'
                       | 'equals'
                       | 'into'
                       | 'remove'
                       | 'where'
                       | 'await'
                       | 'from'
                       | 'join'
                       | 'select'
                       | 'yield'

<literal> ::= <boolean literal>
            | <integer literal>
            | <real literal>
            | <character literal>
            | <string literal>
            | <null literal>

<boolean literal> ::= 'true'
                    | 'false'

<integer literal> ::= <decimal integer literal>
                    | <hexadecimal integer literal>

<decimal integer literal> ::= <decimal digits> <integer type suffix opt>
<integer type suffix opt> ::=
<integer type suffix opt> ::= <integer type suffix>
<decimal digits> ::= <decimal digit>+
<decimal digit> ::= /[0-9]/
<integer type suffix> ::= 'U'
                        | 'u'
                        | 'L'
                        | 'l'
                        | 'UL'
                        | 'Ul'
                        | 'uL'
                        | 'ul'
<hexadecimal integer literal> ::= '0x' <hex digits> <integer type suffix opt>
                                | '0X' <hex digits> <integer type suffix opt>
<hex digits> ::= <hex digit>+
<hex digit> ::= /[0-9A-Fa-f]/

<real literal> ::= <decimal digits> '.' <decimal digits> <exponent part opt> <real type suffix opt>
                 |                  '.' <decimal digits> <exponent part opt> <real type suffix opt>
                 |                      <decimal digits> <exponent part>     <real type suffix opt>
                 |                      <decimal digits>                     <real type suffix>
<exponent part> ::= 'e' <sign opt> <decimal digits>
                  | 'E' <sign opt> <decimal digits>
<sign opt> ::=
<sign opt> ::= <sign>
<sign> ::= '+'
         | '-'

<exponent part opt> ::=
<exponent part opt> ::= <exponent part>
<real type suffix opt> ::=
<real type suffix opt> ::= <real type suffix>

<real type suffix> ::= /[FfDdMm]/

<character literal> ::= "'" <character> "'"
<character> ::= <single character>
              | <simple escape sequence>
              | <hexadecimal escape sequence>
              | <unicode escape sequence>
<single character> ::= /[^\x{0027}\x{005C}\x{000D}\x{000A}\x{0085}\x{2028}\x{2029}]/u   # <ANY CHARACTER EXCEPT 0027 005C AND NEW LINE CHARACTER>

<simple escape sequence> ::= "\\'"
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
<hexadecimal escape sequence> ::= '\\x' <hex digit> <hex digit opt> <hex digit opt> <hex digit opt>
<hex digit opt> ::=
<hex digit opt> ::= <hex digit>

<string literal> ::= <regular string literal>
                   | <verbatim string literal>
<regular string literal> ::= '"' <regular string literal characters opt> '"'
<regular string literal characters opt> ::=
<regular string literal characters opt> ::= <regular string literal characters>
<regular string literal characters> ::= <regular string literal character>+
<regular string literal character> ::= <single regular string literal character>
                                     | <simple escape sequence>
                                     | <hexadecimal escape sequence>
                                     | <unicode escape sequence>
<single regular string literal character> ::= /[^\x{0022}\x{005C}\x{000D}\x{000A}\x{0085}\x{2028}\x{2029}]/u   # <ANY CHARACTER EXCEPT 0022 005C AND NEW LINE CHARACTER>

<verbatim string literal> ::= '@"' <verbatim string literal characters opt> '"'
<verbatim string literal characters opt> ::=
<verbatim string literal characters opt> ::= <verbatim string literal characters>
<verbatim string literal characters> ::= <verbatim string literal character>+
<verbatim string literal character> ::= <single verbatim string literal character>
                                      | <quote escape sequence>
<single verbatim string literal character> ::= /[^"]/u # Any character except "
<quote escape sequence> ::= '""'

<null literal> ::= 'null'

<operator or punctuator> ::= '{'
                           | '+'
                           | '='
                           | '->'
                           | '&='
                           | '}'
                           | '-'
                           | '<'
                           | '=='
                           | '|='
                           | '['
                           | '*'
                           | '>'
                           | '!='
                           | '^='
                           | ']'
                           | '/'
                           | '?'
                           | '<='
                           | '<<'
                           | '('
                           | '%'
                           | '??'
                           | '>='
                           | '<<='
                           | ')'
                           | '&'
                           | '::'
                           | '+='
                           | '.'
                           | '|'
                           | '++'
                           | '-='
                           | ','
                           | '^'
                           | '--'
                           | '*='
                           | ':'
                           | '!'
                           | '&&'
                           | '/='
                           | ';'
                           | '~'
                           | '||'
                           | '%='
<right shift> ::= '>' '>'
<right shift assignment> ::= '>' '>='

<pp directive> ::= <pp declaration>
                 | <pp conditional>
                 | <pp line>
                 | <pp diagnostic>
                 | <pp region>
                 | <pp pragma>

event ^conditional_symbol  = predicted <conditional symbol>
<conditional symbol> ::= <any identifier or keyword except true or false>
event not_a_boolean_literal[] = nulled <not a boolean literal>
<any identifier or keyword except true or false> ::= <identifier or keyword> <not a boolean literal>
<not a boolean literal> ::=
<pp expression> ::= <whitespace opt> <pp or expression> <whitespace opt>
<whitespace opt> ::=
<whitespace opt> ::= <whitespace>
<pp or expression> ::= <pp and expression>
                     | <pp or expression> <whitespace opt> '||' <whitespace opt> <pp and expression>
<pp and expression> ::= <pp equality expression>
                      | <pp and expression> <whitespace opt> '&&' <whitespace opt> <pp equality expression>
<pp equality expression> ::= <pp unary expression>
                           | <pp equality expression> <whitespace opt> '==' <whitespace opt> <pp unary expression>
                           | <pp equality expression> <whitespace opt> '!=' <whitespace opt> <pp unary expression>
<pp unary expression> ::= <pp primary expression>
                        | '!' <whitespace opt> <pp unary expression>
<pp primary expression> ::= 'true'
                          | 'false'
                          | <conditional symbol>
                          | '(' <whitespace opt> <pp expression> <whitespace opt> ')'
<pp declaration> ::= <whitespace opt> '#' <whitespace opt> 'define' <whitespace> <conditional symbol> <pp new line>
                   | <whitespace opt> '#' <whitespace opt> 'undef'  <whitespace> <conditional symbol> <pp new line>
<pp new line> ::= <whitespace opt> <single line comment opt> <new line>
<single line comment opt> ::=
<single line comment opt> ::= <single line comment>

<pp conditional> ::= <pp if section> <pp elif sections opt> <pp else section opt> <pp endif>
<pp elif sections opt> ::=
<pp elif sections opt> ::= <pp elif sections>
<pp else section opt> ::=
<pp else section opt> ::= <pp else section>
<pp if section> ::= <whitespace opt> '#' <whitespace opt> 'if' <whitespace> <pp expression> <pp new line> <conditional section opt>
<conditional section opt> ::=
<conditional section opt> ::= <conditional section>
<pp elif sections> ::= <pp elif section>+
<pp elif section> ::= <whitespace opt> '#' <whitespace opt> 'elif' <whitespace> <pp expression> <pp new line> <conditional section opt>
<pp else section> ::= <whitespace opt> '#' <whitespace opt> 'else' <pp new line> <conditional section opt>
<pp endif> ::= <whitespace opt> '#' <whitespace opt> 'endif' <pp new line>
<conditional section> ::= <input section>
                        | <skipped section>
<skipped section> ::= <skipped section part>+
<skipped section part> ::= <skipped characters opt> <new line>
                         | <pp directive>
<skipped characters opt> ::=
<skipped characters opt> ::= <skipped characters>
<skipped characters> ::= <whitespace opt> <not number sign> <input characters opt>
<not number sign> ::= /[^#]/u # Any input-character except #

<pp diagnostic> ::= <whitespace opt> '#' <whitespace opt> 'error' <pp message>
                  | <whitespace opt> '#' <whitespace opt> 'warning' <pp message>
<pp message> ::= <new line>
               | <whitespace> <input characters opt> <new line>

<pp region> ::= <pp start region> <conditional section opt> <pp end region>
<pp start region> ::= <whitespace opt> '#' <whitespace opt> 'region' <pp message>
<pp end region> ::= <whitespace opt> '#' <whitespace opt> 'endregion' <pp message>

<pp line> ::= <whitespace opt> '#' <whitespace opt> 'line' <whitespace> <line indicator> <pp new line>
<line indicator> ::= <decimal digits> <whitespace> <file name>
                   | <decimal digits>
                   | 'default'
                   | 'hidden'
<file name> ::= '"' <file name characters> '"'
<file name characters> ::= <file name character>+
<file name character> ::= /[^\x{0022}\x{000D}\x{000A}\x{0085}\x{2028}\x{2029}]/u   # <ANY INPUT CHARACTER EXCEPT 0022 AND NEW LINE CHARACTER>

<pp pragma> ::= <whitespace opt> '#' <whitespace opt> 'pragma' <pp pragma text>
<pp pragma text> ::= <new line>
                   | <whitespace> <input characters opt> <new line>

#
# Lexemes
#
<UNICODE ESCAPE SEQUENCE> :[2]:= /\\u[0-9A-Fa-f]{4}/
                               | /\\U[0-9A-Fa-f]{8}/
<ANY IDENTIFIER OR KEYWORD EXCEPT TRUE OR FALSE>                                     ~ /[^\s\S]/ # Matches nothing
<AN IDENTIFIER OR KEYWORD THAT IS NOT A KEYWORD>                                     ~ /[^\s\S]/ # Matches nothing

<A UNICODE ESCAPE SEQUENCE REPRESENTING THE CHARACTER 005F>                          ~ <UNICODE ESCAPE SEQUENCE>
<A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Lu Ll Lt Lm Lo or Nl> ~ <UNICODE ESCAPE SEQUENCE>
<A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Mn or Mc>             ~ <UNICODE ESCAPE SEQUENCE>
<A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Nd>                   ~ <UNICODE ESCAPE SEQUENCE>
<A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Pc>                   ~ <UNICODE ESCAPE SEQUENCE>
<A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Cf>                   ~ <UNICODE ESCAPE SEQUENCE>

:lexeme ::= <A UNICODE ESCAPE SEQUENCE REPRESENTING THE CHARACTER 005F>                          if-action => A_unicode_escape_sequence_representing_the_character_005f
:lexeme ::= <A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Lu Ll Lt Lm Lo or Nl> if-action => A_unicode_escape_sequence_representing_a_character_of_classes_Lu_Ll_Lt_Lm_Lo_or_Nl
:lexeme ::= <A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Mn or Mc>             if-action => A_unicode_escape_sequence_representing_a_character_of_classes_Mn_or_Mc
:lexeme ::= <A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Nd>                   if-action => A_unicode_escape_sequence_representing_a_character_of_the_class_Nd
:lexeme ::= <A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Pc>                   if-action => A_unicode_escape_sequence_representing_a_character_of_the_class_Pc
:lexeme ::= <A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Cf>                   if-action => A_unicode_escape_sequence_representing_a_character_of_the_class_Cf

ERROR ~ /[^\s\S]/ # Matches nothing

__[ identifier or keyword ]__
:default ::= action => ::convert[UTF-8]
:desc ::= 'Identifier or Keyword grammar'

<identifier or keyword> ::= <identifier start character> <identifier part characters opt>
<identifier part characters opt> ::=
<identifier part characters opt> ::= <identifier part characters>
<identifier start character> ::= <letter character>
                               | <underscore character>
<underscore character>       ::= '_' # The underscore character U+005F
                               | <A UNICODE ESCAPE SEQUENCE REPRESENTING THE CHARACTER 005F>
<identifier part characters> ::= <identifier part character>+
<identifier part character>  ::= <letter character>
                               | <decimal digit character>
                               | <connecting character>
                               | <combining character>
                               | <formatting character>
<letter character>           ::= /[\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}\p{Nl}]/u
                               | <A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Lu Ll Lt Lm Lo or Nl>
<combining character>        ::= /[\p{Mn}\p{Mc}]/u
                               | <A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Mn or Mc>
<decimal digit character>    ::= /[\p{Nd}]/u
                               | <A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Nd>
<connecting character>       ::= /[\p{Pc}]/u
                               | <A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Pc>
<formatting character>       ::= /[\p{Cf}]/u
                               | <A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Cf>

<UNICODE ESCAPE SEQUENCE> :[2]:= /\\u[0-9A-Fa-f]{4}/
                               | /\\U[0-9A-Fa-f]{8}/
<A UNICODE ESCAPE SEQUENCE REPRESENTING THE CHARACTER 005F>                          ~ <UNICODE ESCAPE SEQUENCE>
<A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Lu Ll Lt Lm Lo or Nl> ~ <UNICODE ESCAPE SEQUENCE>
<A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Mn or Mc>             ~ <UNICODE ESCAPE SEQUENCE>
<A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Nd>                   ~ <UNICODE ESCAPE SEQUENCE>
<A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Pc>                   ~ <UNICODE ESCAPE SEQUENCE>
<A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Cf>                   ~ <UNICODE ESCAPE SEQUENCE>

:lexeme ::= <A UNICODE ESCAPE SEQUENCE REPRESENTING THE CHARACTER 005F>                          if-action => A_unicode_escape_sequence_representing_the_character_005f
:lexeme ::= <A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Lu Ll Lt Lm Lo or Nl> if-action => A_unicode_escape_sequence_representing_a_character_of_classes_Lu_Ll_Lt_Lm_Lo_or_Nl
:lexeme ::= <A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Mn or Mc>             if-action => A_unicode_escape_sequence_representing_a_character_of_classes_Mn_or_Mc
:lexeme ::= <A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Nd>                   if-action => A_unicode_escape_sequence_representing_a_character_of_the_class_Nd
:lexeme ::= <A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Pc>                   if-action => A_unicode_escape_sequence_representing_a_character_of_the_class_Pc
:lexeme ::= <A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Cf>                   if-action => A_unicode_escape_sequence_representing_a_character_of_the_class_Cf

__[ keyword ]__
:default ::= action => ::convert[UTF-8]
:desc ::= 'Keyword grammar'

<keyword> ::= 'abstract'
            | 'byte'
            | 'class'
            | 'delegate'
            | 'event'
            | 'fixed'
            | 'if'
            | 'internal'
            | 'new'
            | 'override'
            | 'readonly'
            | 'short'
            | 'struct'
            | 'try'
            | 'unsafe'
            | 'volatile'
            | 'as'
            | 'case'
            | 'const'
            | 'do'
            | 'explicit'
            | 'float'
            | 'implicit'
            | 'is'
            | 'null'
            | 'params'
            | 'ref'
            | 'sizeof'
            | 'switch'
            | 'typeof'
            | 'ushort'
            | 'while'
            | 'base'
            | 'catch'
            | 'continue'
            | 'double'
            | 'extern'
            | 'for'
            | 'in'
            | 'lock'
            | 'object'
            | 'private'
            | 'return'
            | 'stackalloc'
            | 'this'
            | 'uint'
            | 'using'
            | 'bool'
            | 'char'
            | 'decimal'
            | 'else'
            | 'false'
            | 'foreach'
            | 'int'
            | 'long'
            | 'operator'
            | 'protected'
            | 'sbyte'
            | 'static'
            | 'throw'
            | 'ulong'
            | 'virtual'
            | 'break'
            | 'checked'
            | 'default'
            | 'enum'
            | 'finally'
            | 'goto'
            | 'interface'
            | 'namespace'
            | 'out'
            | 'public'
            | 'sealed'
            | 'string'
            | 'true'
            | 'unchecked'
            | 'void'
