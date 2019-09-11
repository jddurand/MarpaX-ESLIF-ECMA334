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
use Devel::Hexdump qw/xd/;
use Log::Any qw/$log/;
use MarpaX::ESLIF::ECMA334::Lexical::RecognizerInterface;
use MarpaX::ESLIF::ECMA334::Lexical::ValueInterface;
use MarpaX::ESLIF 3.0.15; # if-action

$Log::Log4perl::Logger::APPENDER_BY_NAME{Screen}->threshold($TRACE);

my $PRE_LEXICAL_BNF           = ${__PACKAGE__->section_data('pre lexical grammar')};
my $LEXICAL_BNF               = ${__PACKAGE__->section_data('lexical grammar')};
my $IDENTIFIER_OR_KEYWORD_BNF = ${__PACKAGE__->section_data('identifier or keyword grammar')};
my $KEYWORD_BNF               = ${__PACKAGE__->section_data('keyword grammar')};
my $PP_EXPRESSION_BNF         = ${__PACKAGE__->section_data('pp expression grammar')};

my $PRE_LEXICAL_GRAMMAR = MarpaX::ESLIF::Grammar->new(MarpaX::ESLIF->new($log), $PRE_LEXICAL_BNF);
$log->debug('pre lexical grammar compiled');
my $LEXICAL_GRAMMAR     = MarpaX::ESLIF::Grammar->new(MarpaX::ESLIF->new($log), $LEXICAL_BNF);
$log->debug('lexical grammar compiled');
my $IDENTIFIER_OR_KEYWORD_GRAMMAR = MarpaX::ESLIF::Grammar->new(MarpaX::ESLIF->new(), $IDENTIFIER_OR_KEYWORD_BNF); # No log
$log->debug('identifier or keyword grammar compiled');
my $KEYWORD_GRAMMAR = MarpaX::ESLIF::Grammar->new(MarpaX::ESLIF->new(), $KEYWORD_BNF); # No log
$log->debug('keyword grammar compiled');
my $PP_EXPRESSION_GRAMMAR = MarpaX::ESLIF::Grammar->new(MarpaX::ESLIF->new($log), $PP_EXPRESSION_BNF); # No log
$log->debug('pp expression grammar compiled');

# ============================================================================
# new
# ============================================================================
sub new {
    my ($pkg) = @_;

    return bless {
        last_pp_expression => undef,
        last_conditional_symbol => undef,
        can_next_conditional_section => [],
        has_token => 0
    }, $pkg
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
                         undef, # sharedEslifRecognizer
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
        # Pre-parse: we require this is a valid source file from encoding point of view
        #
        my $eslifRecognizerInterface = MarpaX::ESLIF::ECMA334::Lexical::RecognizerInterface->new(input => $input, encoding => $encoding);
        my $eslifValueInterface = MarpaX::ESLIF::ECMA334::Lexical::ValueInterface->new();
        $PRE_LEXICAL_GRAMMAR->parse($eslifRecognizerInterface, $eslifValueInterface) || croak 'Pre-parse phase failure';
        $output = $eslifValueInterface->getResult();

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
    my ($self, $eslifGrammar, $eslifRecognizerInterfaceClass, $eslifValueInterfaceClass, $eventManager, $sharedEslifRecognizer, %options) = @_;

    # -------------------------------------------
    # Instanciate recognizer and value interfaces
    # -------------------------------------------
    my $eslifRecognizerInterface = $eslifRecognizerInterfaceClass->new(%options);
    my $eslifValueInterface = $eslifValueInterfaceClass->new(%options);

    # ------------------------
    # Instanciate a recognizer
    # ------------------------
    my $eslifRecognizer = defined($sharedEslifRecognizer) ? $sharedEslifRecognizer->newFrom($eslifGrammar) : MarpaX::ESLIF::Recognizer->new($eslifGrammar, $eslifRecognizerInterface);

    # -----------------------------------------------------
    # Run recognizer manually so that events are accessible
    # -----------------------------------------------------
    $eslifRecognizer->scan(1) || croak 'Initial scan failed';
    $self->$eventManager($eslifRecognizer, $eslifRecognizerInterface);
    if ($eslifRecognizer->isCanContinue) {
        {
            do {
                if (! $eslifRecognizer->resume) {
                    #
                    # This is a failure unless it is a sub-grammar that has reached completion at least once
                    #
                    if ($eslifRecognizerInterface->hasCompletion && $eslifRecognizerInterface->recurseLevel) {
                        last;
                    } else {
                        croak 'resume() failed';
                    }
                }
                $self->$eventManager($eslifRecognizer, $eslifRecognizerInterface)
            } while ($eslifRecognizer->isCanContinue)
        }
    }

    # -----------------------------------------------------------------------------------------
    # Call for valuation (we configured value interface to not accept ambiguity nor null parse)
    # -----------------------------------------------------------------------------------------
    my $value = MarpaX::ESLIF::Value->new($eslifRecognizer, $eslifValueInterface);
    croak 'Valuation failure' unless $value->value();

    # ------------------------
    # Return the value
    # ------------------------
    return $eslifValueInterface->getResult;
}

sub _identifier_or_keyword {
    my ($self, $eslifRecognizer, $eslifRecognizerInterface) = @_;

    $log->debugf('[%2d] Trying grammar %s', $eslifRecognizerInterface->recurseLevel, '<identifier or keyword>');

    my $identifier_or_keyword = eval {
        $self->_parse($IDENTIFIER_OR_KEYWORD_GRAMMAR,
                      'MarpaX::ESLIF::ECMA334::Lexical::RecognizerInterface',
                      'MarpaX::ESLIF::ECMA334::Lexical::ValueInterface',
                      \&_lexicalEventManager,
                      undef, # sharedEslifRecognizer
                      input => $eslifRecognizer->input,
                      recurseLevel => $eslifRecognizerInterface->recurseLevel + 1,
                      exhaustion => 1,
                      encoding => 'UTF-8',
                      definitions => $eslifRecognizerInterface->definitions);
    };

    return $identifier_or_keyword;
}

sub _pp_expression {
    my ($self, $eslifRecognizer, $eslifRecognizerInterface) = @_;

    $log->noticef('[%2d] Trying grammar %s', $eslifRecognizerInterface->recurseLevel, '<pp expression>');

    my $pp_expression = eval {
        $self->_parse($PP_EXPRESSION_GRAMMAR,
                      'MarpaX::ESLIF::ECMA334::Lexical::RecognizerInterface',
                      'MarpaX::ESLIF::ECMA334::Lexical::ValueInterface',
                      \&_lexicalEventManager,
                      $eslifRecognizer, # sharedEslifRecognizer
                      input => $eslifRecognizer->input,
                      recurseLevel => $eslifRecognizerInterface->recurseLevel + 1,
                      exhaustion => 1,
                      encoding => 'UTF-8',
                      definitions => $eslifRecognizerInterface->definitions);
    };

    return $pp_expression;
}

sub _keyword {
    my ($self, $eslifRecognizer, $eslifRecognizerInterface) = @_;

    $log->debugf('[%2d] Trying grammar %s', $eslifRecognizerInterface->recurseLevel, '<keyword>');
    
    my $keyword = eval {
        $self->_parse($KEYWORD_GRAMMAR,
                      'MarpaX::ESLIF::ECMA334::Lexical::RecognizerInterface',
                      'MarpaX::ESLIF::ECMA334::Lexical::ValueInterface',
                      \&_lexicalEventManager,
                      undef, # sharedEslifRecognizer
                      input => $eslifRecognizer->input,
                      recurseLevel => $eslifRecognizerInterface->recurseLevel + 1,
                      exhaustion => 1,
                      encoding => 'UTF-8',
                      definitions => $eslifRecognizerInterface->definitions);
    };

    return $keyword;
}

sub _hexdump
{
    my ($self, $eslifRecognizer, $eslifRecognizerInterface) = @_;

    my $output = '';
    my $offset = 0;
		
    foreach my $chunk (unpack "(a16)*", $eslifRecognizer->input)
    {
        use bytes;
        my $hex = unpack "H*", $chunk; # hexadecimal magic
        $chunk =~ tr/ -~/./c;          # replace unprintables
        $hex   =~ s/(.{1,8})/$1 /gs;   # insert spaces
        $log->noticef('[%2d] 0x%08x (%05u)  %-*s %s', $eslifRecognizerInterface->recurseLevel, $offset, $offset, 36, $hex, $chunk);
        $offset += 16;
    }

    return $output
}

sub _error {
    my ($self, $fmt, @args) = @_;

    $log->errorf($fmt, @args);
    croak sprintf($fmt, @args);
}

sub _lexicalEventManager {
    my ($self, $eslifRecognizer, $eslifRecognizerInterface) = @_;

    my @events = grep { defined } map { $_->{event} } @{$eslifRecognizer->events};
    $log->noticef('[%2d] Events  : %s', $eslifRecognizerInterface->recurseLevel, \@events);
    my @expected = @{$eslifRecognizer->lexemeExpected};
    $log->noticef('[%2d] Expected: %s', $eslifRecognizerInterface->recurseLevel, \@expected);
    # $self->_hexdump($eslifRecognizer, $eslifRecognizerInterface);

    #
    # At any predicted event, we have two possible sub-grammars
    #
    my @matches;
    my $latm = -1;

    foreach my $event (@events) {
        my ($identifier_or_keyword, $keyword, $match, $name, $pp_expression) = (undef, undef, undef, undef, undef);

        if ($event eq "'exhausted'") {
	    $log->noticef('[%2d] Exhaustion event', $eslifRecognizerInterface->recurseLevel);
            $eslifRecognizerInterface->hasCompletion(1);
        }
        elsif ($event eq 'pp_expression$') {
	    $log->noticef('[%2d] <pp expression> completion event', $eslifRecognizerInterface->recurseLevel);
            $eslifRecognizerInterface->hasCompletion(1);
        }
        elsif ($event eq 'token$') {
            $self->{has_token} = 1;
        }
        elsif ($event eq 'pp_declaration_define$') {
            croak 'A #define declaration is allowed only before any token' if $self->{has_token};
	    $log->noticef('[%2d] #define %s', $eslifRecognizerInterface->recurseLevel, $self->{last_conditional_symbol});
            $eslifRecognizerInterface->definitions->{$self->{last_conditional_symbol}} = $MarpaX::ESLIF::true;
        }
        elsif ($event eq 'pp_declaration_undef$') {
            croak 'An #undef declaration is allowed only before any token' if $self->{has_token};
	    $log->noticef('[%2d] #undef %s', $eslifRecognizerInterface->recurseLevel, $self->{last_conditional_symbol});
            $eslifRecognizerInterface->definitions->{$self->{last_conditional_symbol}} = $MarpaX::ESLIF::false;
        }
        elsif ($event eq "pp_if_context[]") {
            if ($self->{last_pp_expression}) {
                $log->noticef('[%2d] #if context: CONDITIONAL SECTION OK match', $eslifRecognizerInterface->recurseLevel);
                $match = '';
                $name = 'CONDITIONAL SECTION OK';
                push(@{$self->{can_next_conditional_section}}, 0);
            } else {
                $log->noticef('[%2d] #if context: CONDITIONAL SECTION KO match', $eslifRecognizerInterface->recurseLevel);
                $match = '';
                $name = 'CONDITIONAL SECTION KO';
                push(@{$self->{can_next_conditional_section}}, 1);
            }
        }
        elsif ($event eq "pp_elif_context[]") {
            #
            # We depend on the previous 'if' of 'elif' use of conditional section
            #
            if ($self->{can_next_conditional_section}->[-1]) {
                if ($self->{last_pp_expression}) {
                    $log->noticef('[%2d] #elif context: CONDITIONAL SECTION OK match', $eslifRecognizerInterface->recurseLevel);
                    $match = '';
                    $name = 'CONDITIONAL SECTION OK';
                    $self->{can_next_conditional_section}->[-1] = 0;
                } else {
                    $log->noticef('[%2d] #elif context: CONDITIONAL SECTION KO match', $eslifRecognizerInterface->recurseLevel);
                    $match = '';
                    $name = 'CONDITIONAL SECTION KO';
                }
            }
        }
        elsif ($event eq "pp_else_context[]") {
            #
            # We depend on the previous 'if' of 'elif' use of conditional section
            #
            if ($self->{can_next_conditional_section}->[-1]) {
                if ($self->{last_pp_expression}) {
                    $log->noticef('[%2d] #else context: CONDITIONAL SECTION OK match', $eslifRecognizerInterface->recurseLevel);
                    $match = '';
                    $name = 'CONDITIONAL SECTION OK';
                } else {
                    $log->noticef('[%2d] #else context: CONDITIONAL SECTION KO match', $eslifRecognizerInterface->recurseLevel);
                    $match = '';
                    $name = 'CONDITIONAL SECTION KO';
                }
            }
        }
        elsif ($event eq "pp_endif_context[]") {
            $log->noticef('[%2d] #endif context', $eslifRecognizerInterface->recurseLevel);
            pop(@{$self->{can_next_conditional_section}});
        }
        elsif ($event eq '^available_identifier') {
            $identifier_or_keyword //= $self->_identifier_or_keyword($eslifRecognizer, $eslifRecognizerInterface);
            if (defined($identifier_or_keyword)) {
                $keyword //= $self->_keyword($eslifRecognizer, $eslifRecognizerInterface);
                if (! defined($keyword)) {
                    $match = $identifier_or_keyword;
                    $name = 'AN IDENTIFIER OR KEYWORD THAT IS NOT A KEYWORD';
                }
            }
        }
        elsif ($event eq '^keyword') {
            $keyword //= $self->_keyword($eslifRecognizer, $eslifRecognizerInterface);
            if (defined($keyword)) {
                $match = $keyword;
                $name = 'KEYWORD';
            }
        }
        elsif ($event eq '^identifier_or_keyword') {
            $identifier_or_keyword //= $self->_identifier_or_keyword($eslifRecognizer, $eslifRecognizerInterface);
            if (defined($identifier_or_keyword)) {
                $match = $identifier_or_keyword;
                $name = 'IDENTIFIER OR KEYWORD';
            }
        }
        elsif ($event eq '^conditional_symbol') {
            $identifier_or_keyword //= $self->_identifier_or_keyword($eslifRecognizer, $eslifRecognizerInterface);
            if (defined($identifier_or_keyword)) {
                if ($identifier_or_keyword ne 'true' && $identifier_or_keyword ne 'false') {
                    $match = $identifier_or_keyword;
                    $name = 'ANY IDENTIFIER OR KEYWORD EXCEPT TRUE OR FALSE';
                    $self->{last_conditional_symbol} = $match;
                }
            }
        }
        elsif ($event eq '^pp_expression') {
            $pp_expression //= $self->_pp_expression($eslifRecognizer, $eslifRecognizerInterface);
	    $log->noticef('[%2d] Last <pp expression> is: %s', $eslifRecognizerInterface->recurseLevel, $pp_expression);
            #
            # Remember the last expression, and send a zero-length token into <PP EXPRESSION>
            # because _pp_expression() shared to stream with our recognizer: our position is already
            # advanced.
            #
            $self->{last_pp_expression} = $pp_expression;
            $match = '';
            $name = 'PP EXPRESSION';
        }
        else {
	    $self->_error('[%2d] Unsupported event %s', $eslifRecognizerInterface->recurseLevel, $event);
        }

	if (defined($match)) {
            # $log->noticef('[%2d] Event %s matches lexeme %s: %s', $eslifRecognizerInterface->recurseLevel, $event, $name, $match);
            my $length = bytes::length($match);
            if ($length >= $latm) {
                if ($length > $latm) {
                    @matches = ();
                    $latm = $length;
                }
                push(@matches, { match => $match, name => $name });
            }
        } else {
            # $log->noticef('[%2d] Event %s matches no lexeme', $eslifRecognizerInterface->recurseLevel, $event);
        }
    }

    if ($latm >= 0) {
        foreach (@matches) {
	    # $log->noticef('[%2d] Lexeme alternative: %s: %s', $eslifRecognizerInterface->recurseLevel, $_->{name}, $_->{match});
            $self->_error('%s alternative failure', $_->{name}) unless $eslifRecognizer->lexemeAlternative($_->{name}, $_->{match})
        }
        # $log->noticef('[%2d] Lexeme complete on %d bytes', $eslifRecognizerInterface->recurseLevel, $latm);
        $self->_error('lexeme complete failure') unless $eslifRecognizer->lexemeComplete($latm);
    } else {
        # $log->noticef('[%2d] Events: no lexeme', $eslifRecognizerInterface->recurseLevel, \@events);
    }

    # $log->noticef('[%2d] Events: %d matches', $eslifRecognizerInterface->recurseLevel, \@events, scalar(@matches));
}

1;

__DATA__
__[ pre lexical grammar ]__
:default ::= action => ::convert[UTF-8]
:desc ::= 'Pre lexical grammar'

<input> ::= /./su *

__[ lexical grammar ]__
:default ::= action => ::ast symbol-action => ::convert[UTF-8]
:desc ::= 'Lexical grammar'
:discard ::= <comment>

#
# Note that the spec does NOT say if file-name characters disable comments. We assume they do so.
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
event token$ = completed <token>

<new line> ::= /(?:\x{000D}|\x{000A}|\x{000D}\x{000A}|\x{0085}|\x{2028}|\x{2029})/u

<whitespace>           ::= /[\p{Zs}\x{0009}\x{000B}\x{000C}]+/u

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

event ^identifier_or_keyword = predicted <identifier or keyword>
<identifier or keyword> ::= <IDENTIFIER OR KEYWORD>

event ^available_identifier = predicted <available identifier>
<available identifier> ::= /\w+/
<available identifier>  ::= <AN IDENTIFIER OR KEYWORD THAT IS NOT A KEYWORD>

event ^keyword = predicted <keyword>
<keyword> ::= <KEYWORD>

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
<decimal digits> ::= /[0-9]+/
<integer type suffix> ::= /U|L|UL/i
<hexadecimal integer literal> ::= '0x':i <hex digits> <integer type suffix opt>
<hex digits> ::= <hex digit>+
<hex digit> ::= /[0-9A-Fa-f]/

<real literal> ::= <decimal digits> '.' <decimal digits> <exponent part opt> <real type suffix opt>
                 |                  '.' <decimal digits> <exponent part opt> <real type suffix opt>
                 |                      <decimal digits> <exponent part>     <real type suffix opt>
                 |                      <decimal digits>                     <real type suffix>
<exponent part> ::= 'E':i <sign opt> <decimal digits>
<sign opt> ::=
<sign opt> ::= <sign>
<sign> ::= /[+-]/

<exponent part opt> ::=
<exponent part opt> ::= <exponent part>
<real type suffix opt> ::=
<real type suffix opt> ::= <real type suffix>

<real type suffix> ::= /[FDM]/i

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

# A pre-processing directive always occupies a separate line of source code and always begins with a
# '#' character and a pre-processing directive name. White space may occur before the '#' character and
# between the '#' character and the directive name.
#
# A source line containing a #define , #undef , #if , #elif , #else , #endif , #line , or #endregion
# directive can end with a single-line comment. Delimited comments (the /* */ style of comments) are not
# permitted on source lines containing pre-processing directives.
#
# => This is why  #define , #undef , #if , #elif , #else , #endif and #line have <pp new line> that contains
#    <single line comment opt>
# => For #endregion this is <pp message> instead, which I believe is an error. For #endregion <pp message>
#    is changed to <pp endregion message> that uses <pp new line> instead of <new line>
# => :discard is switched off when a pp directive is found
# => :discard is switch on at the end of <pp directive>

:lexeme ::= <PP DEFINE> pause => after event => :discard[switch]
:lexeme ::= <PP UNDEF> pause => after event => :discard[switch]
:lexeme ::= <PP IF> pause => after event => :discard[switch]
:lexeme ::= <PP ELIF> pause => after event => :discard[switch]
:lexeme ::= <PP ELSE> pause => after event => :discard[switch]
:lexeme ::= <PP ENDIF> pause => after event => :discard[switch]
:lexeme ::= <PP LINE> pause => after event => :discard[switch]
:lexeme ::= <PP ERROR> pause => after event => :discard[switch]
:lexeme ::= <PP WARNING> pause => after event => :discard[switch]
:lexeme ::= <PP REGION> pause => after event => :discard[switch]
:lexeme ::= <PP ENDREGION> pause => after event => :discard[switch]
:lexeme ::= <PP PRAGMA> pause => after event => :discard[switch]
event :discard[switch] = completed <pp directive>

<pp directive> ::= <pp declaration>
                 | <pp conditional>
                 | <pp line>
                 | <pp diagnostic>
                 | <pp region>
                 | <pp pragma>

event ^conditional_symbol = predicted <conditional symbol>
<conditional symbol> ::= <ANY IDENTIFIER OR KEYWORD EXCEPT TRUE OR FALSE>
<whitespace opt> ::=
<whitespace opt> ::= <whitespace>
<pp declaration define> ::= <PP DEFINE> <whitespace> <conditional symbol> <pp new line>
<pp declaration undef> ::= <PP UNDEF> <whitespace> <conditional symbol> <pp new line>
<pp declaration> ::= <pp declaration define>
                   | <pp declaration undef>
event pp_declaration_define$ = completed <pp declaration define>
event pp_declaration_undef$ = completed <pp declaration undef>
<pp new line> ::= <whitespace opt> <single line comment opt> <new line>
<single line comment opt> ::=
<single line comment opt> ::= <single line comment>

event ^pp_expression = predicted <pp expression>
<pp expression> ::= <PP EXPRESSION>
<pp conditional> ::= <pp if section> <pp elif sections opt> <pp else section opt> <pp endif>
<pp elif sections opt> ::=
<pp elif sections opt> ::= <pp elif sections>
<pp else section opt> ::=
<pp else section opt> ::= <pp else section>
<conditional section opt> ::=
<conditional section opt> ::= <conditional section>

event pp_if_context[] = nulled <pp if context>
<pp if context> ::=
<pp if section> ::= <PP IF> <whitespace> <pp expression> <pp new line> (- <pp if context> -) <conditional section opt>

<pp elif sections> ::= <pp elif section>+
event pp_elif_context[] = nulled <pp elif context>
<pp elif context> ::=
<pp elif section> ::= <PP ELIF> <whitespace> <pp expression> <pp new line> (- <pp elif context> -) <conditional section opt>

event pp_else_context[] = nulled <pp else context>
<pp else context> ::=
<pp else section> ::= <PP ELSE> <pp new line> (- <pp else context> -) <conditional section opt>

event pp_endif_context[] = nulled <pp endif context>
<pp endif context> ::=
<pp endif> ::= <PP ENDIF> (- <pp endif context> -) <pp new line>

<conditional section> ::= (- <CONDITIONAL SECTION OK> -) <input section>
                        | (- <CONDITIONAL SECTION KO> -) <skipped section>
<skipped section> ::= <skipped section part>+
<skipped section part> ::= <skipped characters opt> <new line>
                         | <pp directive>
<skipped characters opt> ::=
<skipped characters opt> ::= <skipped characters>
<skipped characters> ::= <whitespace opt> <not number sign> <input characters opt>
<not number sign> ::= /[^#]/u # Any input-character except #

<pp diagnostic> ::= <PP ERROR> <pp message>
                  | <PP WARNING> <pp message>
<pp message> ::= <new line>
               | <whitespace> <input characters opt> <new line>

# The lexical processing of a region:
# #region
# ...
# #endregion
# corresponds exactly to the lexical processing of a conditional compilation directive of the form:
# #if true
# ...
# #endif
#
# ==> This is an <input section opt> instead of <conditional section opt>
#
# <pp region> ::= <pp start region> <conditional section opt> <pp end region>
<pp region> ::= <pp start region> <input section opt> <pp end region>
<pp start region> ::= <PP REGION> <pp message>
# WAS: <pp end region> ::= <whitespace opt> '#' <whitespace opt> 'endregion' <pp message>
<pp end region> ::= <PP ENDREGION> <pp endregion message>
<pp endregion message> ::= <pp new line>
                         | <whitespace> <input characters opt> <pp new line>

<pp line> ::= <PP LINE> <whitespace> <line indicator> <pp new line>
<line indicator> ::= <decimal digits> <whitespace> <file name>
                   | <decimal digits>
                   | 'default'
                   | 'hidden'
<file name> ::= '"' <file name characters> '"'
<file name characters> ::= <file name character>+
<file name character> ::= /[^\x{0022}\x{000D}\x{000A}\x{0085}\x{2028}\x{2029}]/u   # <ANY INPUT CHARACTER EXCEPT 0022 AND NEW LINE CHARACTER>

<pp pragma> ::= <PP PRAGMA> <pp pragma text>
<pp pragma text> ::= <new line>
                   | <whitespace> <input characters opt> <new line>

#
# Lexemes
#
<KEYWORD>                                        ~ /[^\s\S]/ # Matches nothing
<IDENTIFIER OR KEYWORD>                          ~ /[^\s\S]/ # Matches nothing
<ANY IDENTIFIER OR KEYWORD EXCEPT TRUE OR FALSE> ~ /[^\s\S]/ # Matches nothing
<AN IDENTIFIER OR KEYWORD THAT IS NOT A KEYWORD> ~ /[^\s\S]/ # Matches nothing
<PP DEFINE>                                      ~ /[\p{Zs}\x{0009}\x{000B}\x{000C}]*#[\p{Zs}\x{0009}\x{000B}\x{000C}]*define/u
<PP UNDEF>                                       ~ /[\p{Zs}\x{0009}\x{000B}\x{000C}]*#[\p{Zs}\x{0009}\x{000B}\x{000C}]*undef/u
<PP IF>                                          ~ /[\p{Zs}\x{0009}\x{000B}\x{000C}]*#[\p{Zs}\x{0009}\x{000B}\x{000C}]*if/u
<PP ELIF>                                        ~ /[\p{Zs}\x{0009}\x{000B}\x{000C}]*#[\p{Zs}\x{0009}\x{000B}\x{000C}]*elif/u
<PP ELSE>                                        ~ /[\p{Zs}\x{0009}\x{000B}\x{000C}]*#[\p{Zs}\x{0009}\x{000B}\x{000C}]*else/u
<PP ENDIF>                                       ~ /[\p{Zs}\x{0009}\x{000B}\x{000C}]*#[\p{Zs}\x{0009}\x{000B}\x{000C}]*endif/u
<PP LINE>                                        ~ /[\p{Zs}\x{0009}\x{000B}\x{000C}]*#[\p{Zs}\x{0009}\x{000B}\x{000C}]*line/u
<PP ERROR>                                       ~ /[\p{Zs}\x{0009}\x{000B}\x{000C}]*#[\p{Zs}\x{0009}\x{000B}\x{000C}]*error/u
<PP WARNING>                                     ~ /[\p{Zs}\x{0009}\x{000B}\x{000C}]*#[\p{Zs}\x{0009}\x{000B}\x{000C}]*warning/u
<PP REGION>                                      ~ /[\p{Zs}\x{0009}\x{000B}\x{000C}]*#[\p{Zs}\x{0009}\x{000B}\x{000C}]*region/u
<PP ENDREGION>                                   ~ /[\p{Zs}\x{0009}\x{000B}\x{000C}]*#[\p{Zs}\x{0009}\x{000B}\x{000C}]*endregion/u
<PP PRAGMA>                                      ~ /[\p{Zs}\x{0009}\x{000B}\x{000C}]*#[\p{Zs}\x{0009}\x{000B}\x{000C}]*pragma/u
<CONDITIONAL SECTION OK>                         ~ /[^\s\S]/ # Matches nothing
<CONDITIONAL SECTION KO>                         ~ /[^\s\S]/ # Matches nothing
<PP EXPRESSION>                                  ~ /[^\s\S]/ # Matches nothing

__[ identifier or keyword grammar ]__
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

__[ keyword grammar ]__
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

__[ pp expression grammar ]__
:desc    ::= 'Pre-processing expression'
:default ::= action => ::shift
:start ::= <pp expression>

event pp_expression$ = completed <pp expression>
<pp expression>                                  ::= (- <whitespace opt> -) <pp or expression> (- <whitespace opt> -)
<whitespace opt>                                 ::=
<whitespace opt>                                 ::= <whitespace>
<pp or expression>                               ::= <pp and expression>
                                                   | <pp or expression> (- <whitespace opt> '||' <whitespace opt> -) <pp and expression>         action => pp_or_expression
<pp and expression>                              ::= <pp equality expression>
                                                   | <pp and expression> (- <whitespace opt> '&&' <whitespace opt> -) <pp equality expression>   action => pp_and_expression
<pp equality expression>                         ::= <pp unary expression>
                                                   | <pp equality expression> (- <whitespace opt> '==' <whitespace opt> -) <pp unary expression> action => pp_equal_expression
                                                   | <pp equality expression> (- <whitespace opt> '!=' <whitespace opt> -) <pp unary expression> action => pp_not_equal_expression
<pp unary expression>                            ::= <pp primary expression>
                                                   | (- '!' <whitespace opt> -) <pp unary expression>                                            action => pp_not_expression
<pp primary expression>                          ::= 'true'                                                                                      action => ::true
                                                   | 'false'                                                                                     action => ::false
                                                   | <conditional symbol>                                                                        action => pp_condition_symbol
                                                   | (- '(' <whitespace opt> -) <pp expression> (- <whitespace opt> ')' -)
<whitespace>                                     ::= /[\p{Zs}\x{0009}\x{000B}\x{000C}]+/u
event ^conditional_symbol = predicted <conditional symbol>
<conditional symbol>                             ::= <ANY IDENTIFIER OR KEYWORD EXCEPT TRUE OR FALSE>

<ANY IDENTIFIER OR KEYWORD EXCEPT TRUE OR FALSE>   ~ /[^\s\S]/ # Matches nothing
__[ syntactic grammar ]__
