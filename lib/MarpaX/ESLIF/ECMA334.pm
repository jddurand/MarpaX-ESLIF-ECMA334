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
my $IDENTIFIER_OR_KEYWORD_BNF = $LEXICAL_BNF;
$IDENTIFIER_OR_KEYWORD_BNF .= "\n:start ::= <identifier or keyword>\n";
my $IDENTIFIER_OR_KEYWORD_GRAMMAR = MarpaX::ESLIF::Grammar->new(MarpaX::ESLIF->new($log), $IDENTIFIER_OR_KEYWORD_BNF);

my $KEYWORD_BNF = $LEXICAL_BNF;
$KEYWORD_BNF .= "\n:start ::= <keyword>\n";
my $KEYWORD_GRAMMAR = MarpaX::ESLIF::Grammar->new(MarpaX::ESLIF->new($log), $KEYWORD_BNF);

my $UNICODE_ESCAPE_SEQUENCE_BNF = $LEXICAL_BNF;
$UNICODE_ESCAPE_SEQUENCE_BNF .= "\n:start ::= <unicode escape sequence>\n";
my $UNICODE_ESCAPE_SEQUENCE_GRAMMAR = MarpaX::ESLIF::Grammar->new(MarpaX::ESLIF->new($log), $UNICODE_ESCAPE_SEQUENCE_BNF);

my $UNICODE_ESCAPE_SEQUENCE = 'UNICODE ESCAPE SEQUENCE';
my $TRUE = 'TRUE';
my $FALSE = 'FALSE';
my $lexicalValue = MarpaX::ESLIF::ECMA334::Lexical::ValueInterface->new();

sub new {
    my ($pkg) = @_;

    return bless {
        grammars => {
            lexical => $LEXICAL_GRAMMAR,
            syntactic => $SYNTACTIC_GRAMMAR,
            identifier_or_keyword => $IDENTIFIER_OR_KEYWORD_GRAMMAR,
            keyword => $KEYWORD_GRAMMAR,
            unicode_escape_sequence => $UNICODE_ESCAPE_SEQUENCE_GRAMMAR,
            can_comment => 1,                     # At start it is true
            can_single_line_comment => 1,         # At start it is true
            can_delimited_comment => 1            # At start it is true
        }
    }, $pkg
}

sub lexicalParse {
    my ($self, %options) = @_;

    return $self->_parse($options{grammar} // $self->{grammars}->{lexical},
                         'MarpaX::ESLIF::ECMA334::Lexical::RecognizerInterface',
                         'MarpaX::ESLIF::ECMA334::Lexical::ValueInterface',
                         \&_lexicalEventManager,
                         input => $options{input}, # May be undef
                         encoding => $options{encoding},
                         recurseLevel => $options{recurseLevel}
        );
}

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
    my $eslifRecognizer = MarpaX::ESLIF::Recognizer->new($grammar, $recognizerInterface);

    # -------------------------------------------------------------------------
    # Make sure all data (that is recognizer interface memory) is seen by ESLIF
    # -------------------------------------------------------------------------
    croak 'read failure' unless $eslifRecognizer->read();
    #
    # Input must be defined
    #
    croak 'undefined input' unless defined $eslifRecognizer->input();
    $log->debugf('[%2d] Input (10st characters): %s', $recognizerInterface->recurseLevel, substr($eslifRecognizer->input, 0, 10));

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
                        $log->debugf('[%2d] resume failed, hasCompletion: %d', $recognizerInterface->recurseLevel, $recognizerInterface->hasCompletion);
                        croak 'resume failed';
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
    return $valueInterface->getResult;
}

sub _subLexicalParse {
    my ($self, $grammarName, $eslifRecognizer, $recognizerInterface) = @_;

    my $recurseLevel = $recognizerInterface->recurseLevel + 1;
    #
    # Prepare a $grammar_name recognizer that start where is current $eslifRecognizer
    #
    $log->debugf('[%2d] Sub grammar: %s', $recurseLevel, $grammarName);
    my $rc = eval {
        $self->lexicalParse(
            input => $eslifRecognizer->input,
            encoding => $recognizerInterface->encoding,
            recurseLevel => $recurseLevel,
            grammar => $self->{grammars}->{$grammarName})};
    $log->debugf('[%2d] %s', $recurseLevel, $@) if $@;
    $log->debugf('[%2d] rc=%s', $recurseLevel, $rc);

    return $rc;
}

sub _lexicalEventManager {
    my ($self, $eslifRecognizer, $recognizerInterface) = @_;

    my @lexemeExpected = @{$eslifRecognizer->lexemeExpected()};
    $log->debugf('[%2d] Expected: %s', $recognizerInterface->recurseLevel, join(', ', @lexemeExpected));

    #
    # Sub-grammar completion ?
    #
    my @events = map { $_->{event} } @{$eslifRecognizer->events()};
    if ($recognizerInterface->recurseLevel && grep {$_ eq 'sub_grammar_completion$'} @events) {
        $log->debugf('[%2d] Sub-grammar reached completion at least once', $recognizerInterface->recurseLevel);
        $recognizerInterface->hasCompletion(1);
    }

    #
    # Hash { $lexeme => $match }
    my %lexemes = ();
    foreach my $lexeme (@lexemeExpected) {
        my $match = undef;

        if ($lexeme eq 'AN IDENTIFIER OR KEYWORD THAT IS NOT A KEYWORD') {
            my $identifier_or_keyword = $self->_subLexicalParse('identifier_or_keyword', $eslifRecognizer, $recognizerInterface);
            my $keyword = defined($identifier_or_keyword) ? $self->_subLexicalParse('keyword', $eslifRecognizer, $recognizerInterface) : undef;
            $match = $identifier_or_keyword if defined($identifier_or_keyword) && ! defined($keyword);
        }
        elsif ($lexeme eq 'ANY IDENTIFIER OR KEYWORD EXCEPT TRUE OR FALSE') {
            my $identifier_or_keyword = $self->_subLexicalParse('identifier_or_keyword', $eslifRecognizer, $recognizerInterface);
            my $true_or_false = defined($identifier_or_keyword) ? ($eslifRecognizer->lexemeTry($TRUE) || $eslifRecognizer->lexemeTry($FALSE)) : undef;
            $match = $identifier_or_keyword if defined($identifier_or_keyword) && ! defined($true_or_false);
        }
        elsif ($lexeme eq 'CHARACTER 005F') {
            if ($eslifRecognizer->lexemeTry($UNICODE_ESCAPE_SEQUENCE)) {
                my $utf8bytes = $eslifRecognizer->lexemeLastTry($UNICODE_ESCAPE_SEQUENCE) || croak "$UNICODE_ESCAPE_SEQUENCE is undef";
                my $unicode_escape_sequence = $lexicalValue->unicode_escape_sequence($utf8bytes);
                $match = $unicode_escape_sequence if $unicode_escape_sequence eq "\x{005f}";
            }
        }
        elsif ($lexeme eq 'CHARACTER OF CLASSES Lu Ll Lt Lm Lo OR Nl') {
            if ($eslifRecognizer->lexemeTry($UNICODE_ESCAPE_SEQUENCE)) {
                my $utf8bytes = $eslifRecognizer->lexemeLastTry($UNICODE_ESCAPE_SEQUENCE) || croak "$UNICODE_ESCAPE_SEQUENCE is undef";
                my $unicode_escape_sequence = $lexicalValue->unicode_escape_sequence($utf8bytes);
                $match = $unicode_escape_sequence if $unicode_escape_sequence =~ /^[\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}\p{Nl}]$/;
            }
        }
        elsif ($lexeme eq 'CHARACTER OF CLASSES Mn OR Mc') {
            if ($eslifRecognizer->lexemeTry($UNICODE_ESCAPE_SEQUENCE)) {
                my $utf8bytes = $eslifRecognizer->lexemeLastTry($UNICODE_ESCAPE_SEQUENCE) || croak "$UNICODE_ESCAPE_SEQUENCE is undef";
                my $unicode_escape_sequence = $lexicalValue->unicode_escape_sequence($utf8bytes);
                $match = $unicode_escape_sequence if $unicode_escape_sequence =~ /^[\p{Mn}\p{Mc}]$/;
            }
        }
        elsif ($lexeme eq 'CHARACTER OF THE CLASS Nd') {
            if ($eslifRecognizer->lexemeTry($UNICODE_ESCAPE_SEQUENCE)) {
                my $utf8bytes = $eslifRecognizer->lexemeLastTry($UNICODE_ESCAPE_SEQUENCE) || croak "$UNICODE_ESCAPE_SEQUENCE is undef";
                my $unicode_escape_sequence = $lexicalValue->unicode_escape_sequence($utf8bytes);
                $match = $unicode_escape_sequence if $unicode_escape_sequence =~ /^[\p{Nd}]$/;
            }
        }
        elsif ($lexeme eq 'CHARACTER OF THE CLASS Pc') {
            if ($eslifRecognizer->lexemeTry($UNICODE_ESCAPE_SEQUENCE)) {
                my $utf8bytes = $eslifRecognizer->lexemeLastTry($UNICODE_ESCAPE_SEQUENCE) || croak "$UNICODE_ESCAPE_SEQUENCE is undef";
                my $unicode_escape_sequence = $lexicalValue->unicode_escape_sequence($utf8bytes);
                $match = $unicode_escape_sequence if $unicode_escape_sequence =~ /^[\p{Pc}]$/;
            }
        }
        elsif ($lexeme eq 'CHARACTER OF THE CLASS Cf') {
            if ($eslifRecognizer->lexemeTry($UNICODE_ESCAPE_SEQUENCE)) {
                my $utf8bytes = $eslifRecognizer->lexemeLastTry($UNICODE_ESCAPE_SEQUENCE) || croak "$UNICODE_ESCAPE_SEQUENCE is undef";
                my $unicode_escape_sequence = $lexicalValue->unicode_escape_sequence($utf8bytes);
                $match = $unicode_escape_sequence if $unicode_escape_sequence =~ /^[\p{Cf}]$/;
            }
        }
        elsif ($lexeme eq 'TRUE' || $lexeme eq 'FALSE') {
            if ($eslifRecognizer->lexemeTry($lexeme)) {
                my $utf8bytes = $eslifRecognizer->lexemeLastTry($lexeme) || croak "$lexeme is undef";
                $match = $utf8bytes;
            }
        }
        else {
            croak "Unsupported lexeme $lexeme";
        }

        if (defined($match)) {
            #
            # A lexeme that we have to insert ourself
            #
            $log->debugf('Match on: %s, length: %d bytes', $lexeme, bytes::length($match));
            $lexemes{$lexeme} = $match;
        }
    }

    if (%lexemes) {
        #
        # Take the longests
        #
        my $longestMatch = (sort { bytes::length($b) <=> bytes::length($a) } values %lexemes)[0];
        my $length = bytes::length($longestMatch);
        foreach my $lexeme (grep { bytes::length($lexemes{$_}) == $length} keys %lexemes) {
            my $match = $lexemes{$lexeme};
            $log->debugf('lexemeAlternative: %s: %s', $lexeme, $match);
            $eslifRecognizer->lexemeAlternative($lexeme, $match);
        }
        $log->debugf('lexemeComplete on %d bytes', $length);
        $eslifRecognizer->lexemeComplete($length);
    }
}

1;

__DATA__
__[ lexical ]__
#
# 7.3 Lexical analysis
# ====================
#
# Five basic elements make up the lexical structure of a C# source file:
# - Line terminators (7.3.2). Rules:
#   + <new line>
# - whitespace (7.3.4). Rules:
#   + <whitespace>
# - comments (7.3.3). Rules:
#   + <comment>
# - tokens (7.4). Rules:
#   + <token>
# - preprocessing directives (7.5). Rules:
#   + <pp directive>

#
# 7.3.1 General
# -------------

<input>              ::= <input section opt>                name => 'input'                                      # This is the default :start
<input section opt>  ::=                                    name => 'input section opt (nulled)'
<input section opt>  ::= <input section>                    name => 'input section opt'

<input section>      ::= <input section part>+              name => 'input section'

<input section part> ::= <input elements opt> <new line>    name => 'input section part (1)'
                       | <pp directive>                     name => 'input section part (2)'
<input elements opt> ::=                                    name => 'input elements opt (nulled)'
<input elements opt> ::= <input elements>                   name => 'input elements opt'

<input elements>     ::= <input element>+                   name => 'input elements'

<input element>      ::= <whitespace>                       name => 'input element (1)'
                       | <comment>                          name => 'input element (2)'
                       | <token>                            name => 'input element (3)'

#
# 7.3.2 Line terminators
# ----------------------

<new line> ::= /[\x{000D}\x{000A}\x{0085}\x{2028}\x{2029}]/u name => 'new line'

#
# 7.3.3 Comments
# --------------

#
# Notes:
#
# 1. Comments do not nest. The character sequences '/*' and '*/' have no special meaning within a <single line comment>,
#    and the character sequences '//' and '/*' have no special meaning within a <delimited comment>.
#
# 2. Comments are not processed within character and string literals.
#

<comment>                    ::= <single line comment>                                                    name => 'comment (1)'
                               | <delimited comment>                                                      name => 'comment (2)'
<single line comment>        ::= '//' <input characters opt>                       action => ::concat     name => 'single line comment'
# <input characters>           ::= <input character>+                              action => ::concat     name => 'input characters'
# <input character>            ::= /[^\x{000D}\x{000A}\x{0085}\x{2028}\x{2029}]/u  action => ::concat     name => 'input character'
<input characters>           ::= /[^\x{000D}\x{000A}\x{0085}\x{2028}\x{2029}]+/u   action => ::shift      name => 'input characters'   # Eat as much as possible in one go
<delimited comment>          ::= '/*' <delimited comment text opt> <asterisks> '/' action => ::concat     name => 'delimited comment'
<delimited comment text opt> ::=                                                   action => ::undef      name => 'delimited comment text opt (nulled)'
<delimited comment text opt> ::= <delimited comment text>                          action => ::shift      name => 'delimited comment text opt'
<delimited comment text>     ::= <delimited comment section>+                      action => ::concat     name => 'delimited comment text'
<delimited comment section>  ::= '/'                                               action => ::shift      name => 'delimited comment section (1)'
                               | <asterisks opt> <not slash or asterisk>           action => ::concat     name => 'delimited comment section (2)'
<asterisks opt>              ::=                                                   action => ::undef      name => 'asterisks opt (nulled)'
<asterisks opt>              ::= <asterisks>                                       action => ::shift      name => 'asterisks opt'
<asterisks>                  ::= /\*+/                                             action => ::shift      name => 'asterisks'          # Eat as much as possible askterisks in one go
<not slash or asterisk>      ::= /[^\/*]/                                          action => ::shift      name => 'not slash or asterisk'

#
# 7.3.4 White space
# -----------------
<whitespace>           ::= /[\p{Zs}\x{0009}\x{000B}\x{000C}]+/                      name => 'whitespace'

#
# 7.4 Tokens
# ==========

#
# 7.4.1 General
# -------------

<token> ::= <identifier>                                                            name => 'token (1)'
          | <keyword>                                                               name => 'token (2)'
          | <integer literal>                                                       name => 'token (3)'
          | <real literal>                                                          name => 'token (4)'
          | <character literal>                                                     name => 'token (5)'
          | <string literal>                                                        name => 'token (6)'
          | <operator or punctuator>                                                name => 'token (7)'

#
# 7.4.2 Unicode character escape sequence
# ---------------------------------------

<unicode escape sequence> ::= <UNICODE ESCAPE SEQUENCE> action => unicode_escape_sequence

#
# 7.4.3 Identifiers
# -----------------

<identifier>                                              ::= <available identifier>                                                               name => 'identifier (1)'
                                                            | '@' <identifier or keyword>                                                          name => 'identifier (2)'
<available identifier>                                    ::= <An identifier or keyword that is not a keyword>                                     name => 'available identifier'

<identifier or keyword>                                   ::= <identifier start character> <identifier part characters>                            name => 'identifier or keyword (1)'
                                                            | <identifier start character>                                                         name => 'identifier or keyword (2)'
<identifier start character>                              ::= <letter character>                                                                   name => 'identifier start character (1)'
                                                            | <underscore character>                                                               name => 'identifier start character (2)'
<underscore character>                                    ::= '_'                                                                                  name => 'underscore character (1)'
                                                            | <A unicode escape sequence representing the character 005f>                          name => 'underscore character (2)'
<identifier part characters>                              ::= <identifier part character>+                                                         name => 'identifier part characters'
<identifier part character>                               ::= <letter character>                                                                   name => 'identifier part character (1)'
                                                            | <decimal digit character>                                                            name => 'identifier part character (2)'
                                                            | <connecting character>                                                               name => 'identifier part character (3)'
                                                            | <combining character>                                                                name => 'identifier part character (4)'
                                                            | <formatting character>                                                               name => 'identifier part character (5)'
<letter character>                                        ::= /[\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}\p{Nl}]/                                             name => 'letter character (1)'
                                                            | <A unicode escape sequence representing a character of classes Lu Ll Lt Lm Lo or Nl> name => 'letter character (2)'
<combining character>                                     ::= /[\p{Mn}\p{Mc}]/                                                                     name => 'combining character (1)'
                                                            | <A unicode escape sequence representing a character of classes Mn or Mc>             name => 'combining character (2)'
<decimal digit character>                                 ::= /[\p{Nd}]/                                                                           name => 'decimal digit character (1)'
                                                            | <A unicode escape sequence representing a character of the class Nd>                 name => 'decimal digit character (2)'
<connecting character>                                    ::= /[\p{Pc}]/                                                                           name => 'connecting character (1)'
                                                            | <A unicode escape sequence representing a character of the class Pc>                 name => 'connecting character (2)'
<formatting character>                                    ::= /[\p{Cf}]/                                                                           name => 'formatting character (1)'
                                                            | <A unicode escape sequence representing a character of the class Cf>                 name => 'formatting character (2)'

#
# 7.4.4 Keywords
# --------------
keyword ::= 'abstract'                    name => 'keyword (1)'
          | 'as'                          name => 'keyword (2)'
          | 'base'                        name => 'keyword (3)'
          | 'bool'                        name => 'keyword (4)'
          | 'break'                       name => 'keyword (5)'
          | 'byte'                        name => 'keyword (6)'
          | 'case'                        name => 'keyword (7)'
          | 'catch'                       name => 'keyword (8)'
          | 'char'                        name => 'keyword (9)'
          | 'checked'                     name => 'keyword (10)'
          | 'class'                       name => 'keyword (11)'
          | 'const'                       name => 'keyword (12)'
          | 'continue'                    name => 'keyword (13)'
          | 'decimal'                     name => 'keyword (14)'
          | 'default'                     name => 'keyword (15)'
          | 'delegate'                    name => 'keyword (16)'
          | 'do'                          name => 'keyword (17)'
          | 'double'                      name => 'keyword (18)'
          | 'else'                        name => 'keyword (19)'
          | 'enum'                        name => 'keyword (20)'
          | 'event'                       name => 'keyword (21)'
          | 'explicit'                    name => 'keyword (22)'
          | 'extern'                      name => 'keyword (23)'
          | FALSE                         name => 'keyword (24)'
          | 'finally'                     name => 'keyword (25)'
          | 'fixed'                       name => 'keyword (26)'
          | 'float'                       name => 'keyword (27)'
          | 'for'                         name => 'keyword (28)'
          | 'foreach'                     name => 'keyword (29)'
          | 'goto'                        name => 'keyword (30)'
          | 'if'                          name => 'keyword (31)'
          | 'implicit'                    name => 'keyword (32)'
          | 'in'                          name => 'keyword (33)'
          | 'int'                         name => 'keyword (34)'
          | 'interface'                   name => 'keyword (35)'
          | 'internal'                    name => 'keyword (36)'
          | 'is'                          name => 'keyword (37)'
          | 'lock'                        name => 'keyword (38)'
          | 'long'                        name => 'keyword (39)'
          | 'namespace'                   name => 'keyword (40)'
          | 'new'                         name => 'keyword (41)'
          | 'null'                        name => 'keyword (42)'
          | 'object'                      name => 'keyword (43)'
          | 'operator'                    name => 'keyword (44)'
          | 'out'                         name => 'keyword (45)'
          | 'override'                    name => 'keyword (46)'
          | 'params'                      name => 'keyword (47)'
          | 'private'                     name => 'keyword (48)'
          | 'protected'                   name => 'keyword (49)'
          | 'public'                      name => 'keyword (50)'
          | 'readonly'                    name => 'keyword (51)'
          | 'ref'                         name => 'keyword (52)'
          | 'return'                      name => 'keyword (53)'
          | 'sbyte'                       name => 'keyword (54)'
          | 'sealed'                      name => 'keyword (55)'
          | 'short'                       name => 'keyword (56)'
          | 'sizeof'                      name => 'keyword (57)'
          | 'stackalloc'                  name => 'keyword (58)'
          | 'static'                      name => 'keyword (59)'
          | 'string'                      name => 'keyword (60)'
          | 'struct'                      name => 'keyword (61)'
          | 'switch'                      name => 'keyword (62)'
          | 'this'                        name => 'keyword (63)'
          | 'throw'                       name => 'keyword (64)'
          | TRUE                          name => 'keyword (65)'
          | 'try'                         name => 'keyword (66)'
          | 'typeof'                      name => 'keyword (67)'
          | 'uint'                        name => 'keyword (68)'
          | 'ulong'                       name => 'keyword (69)'
          | 'unchecked'                   name => 'keyword (70)'
          | 'unsafe'                      name => 'keyword (71)'
          | 'ushort'                      name => 'keyword (72)'
          | 'using'                       name => 'keyword (73)'
          | 'virtual'                     name => 'keyword (74)'
          | 'void'                        name => 'keyword (75)'
          | 'volatile'                    name => 'keyword (76)'
          | 'while'                       name => 'keyword (77)'

<contextual keyword> ::= 'add'            name => 'contextual keyword (1)'
                       | 'alias'          name => 'contextual keyword (2)'
                       | 'ascending'      name => 'contextual keyword (3)'
                       | 'async'          name => 'contextual keyword (4)'
                       | 'await'          name => 'contextual keyword (5)'
                       | 'by'             name => 'contextual keyword (6)'
                       | 'descending'     name => 'contextual keyword (7)'
                       | 'dynamic'        name => 'contextual keyword (8)'
                       | 'equals'         name => 'contextual keyword (9)'
                       | 'from'           name => 'contextual keyword (10)'
                       | 'get'            name => 'contextual keyword (11)'
                       | 'global'         name => 'contextual keyword (12)'
                       | 'group'          name => 'contextual keyword (13)'
                       | 'into'           name => 'contextual keyword (14)'
                       | 'join'           name => 'contextual keyword (15)'
                       | 'let'            name => 'contextual keyword (16)'
                       | 'orderby'        name => 'contextual keyword (17)'
                       | 'partial'        name => 'contextual keyword (18)'
                       | 'remove'         name => 'contextual keyword (19)'
                       | 'select'         name => 'contextual keyword (20)'
                       | 'set'            name => 'contextual keyword (21)'
                       | 'value'          name => 'contextual keyword (22)'
                       | 'var'            name => 'contextual keyword (23)'
                       | 'where'          name => 'contextual keyword (24)'
                       | 'yield'          name => 'contextual keyword (25)'

#
# 7.4.5. Literals
# ---------------

<literal> ::= <boolean literal>           name => 'literal (1)'
            | <integer literal>           name => 'literal (2)'
            | <real literal>              name => 'literal (3)'
            | <character literal>         name => 'literal (4)'
            | <string literal>            name => 'literal (5)'
            | <null literal>              name => 'literal (6)'

# 7.4.5.2 Boolean literals

<boolean literal> ::= TRUE                name => 'boolean literal (1)'
                    | FALSE               name => 'boolean literal (2)'

# 7.4.5.3 Integer literals

<integer literal>             ::= <decimal integer literal>                     name => 'integer literal (1)'
                                | <hexadecimal integer literal>                 name => 'integer literal (2)'
<decimal integer literal>     ::= <decimal digits> <integer type suffix>        name => 'decimal integer literal (1)'
                                | <decimal digits>                              name => 'decimal integer literal (2)'
<decimal digits>              ::= <decimal digit>+                              name => 'decimal digits'
<decimal digit>               ::= /[0-9]/                                       name => 'decimal digit'
<integer type suffix>         ::= 'u':i                                         name => 'integer type suffix (1)'
                                | 'l':i                                         name => 'integer type suffix (2)'
                                | 'ul':i                                        name => 'integer type suffix (3)'
                                | 'lu':i                                        name => 'integer type suffix (4)'
<integer type suffix opt>     ::=                                               name => 'integer type suffix (nulled)'
<integer type suffix opt>     ::= <integer type suffix>                         name => 'integer type suffix'
<hexadecimal integer literal> ::= '0x':i <hex digits> <integer type suffix opt> name => 'hexadecimal integer literal'
<hex digits>                  ::= <hex digit>+                                  name => 'hex digits'
<hex digit>                   ::= /[0-9A-Fa-f]/                                 name => 'hex digit'

# 7.4.5.4 Real literals

<real literal>         ::= <decimal digits> '.' <decimal digits> <exponent part opt> <real type suffix opt> name => 'real literal (1)'
                         | '.' <decimal digits> <exponent part opt> <real type suffix opt>                  name => 'real literal (2)'
                         | <decimal digits> <exponent part> <real type suffix opt>                          name => 'real literal (3)'
                         | <decimal digits> <real type suffix>                                              name => 'real literal (4)'
<exponent part opt>    ::=                                                                                  name => 'exponent part opt (nulled)'
<exponent part opt>    ::= <exponent part>                                                                  name => 'exponent part opt'
<real type suffix opt> ::=                                                                                  name => 'real type suffix opt (nulled)'
<real type suffix opt> ::= <real type suffix>                                                               name => 'real type suffix opt'
<exponent part>        ::= 'e':i <sign opt> <decimal digits>                                                name => 'exponent part'
<sign opt>             ::=                                                                                  name => 'sign opt (nulled)'
<sign opt>             ::= <sign>                                                                           name => 'sign opt'
<sign>                 ::= '+'                                                                              name => 'sign (1)'
                         | '-'                                                                              name => 'sign (2)'
<real type suffix>     ::= 'f':i                                                                            name => 'real type suffix (1)'
                         | 'd':i                                                                            name => 'real type suffix (2)'
                         | 'm':i                                                                            name => 'real type suffix (3)'

# 7.4.5.5 Character literals

<character literal>           ::= "'" <character> "'"                                                           name => 'character literal'
<character>                   ::= <single character>                                                            name => 'character (1)'
                                | <simple escape sequence>                                                      name => 'character (2)'
                                | <hexadecimal escape sequence>                                                 name => 'character (3)'
                                | <unicode escape sequence>                                                     name => 'character (4)'
<single character>            ::= /[^\x{0027}\x{005C}\x{000D}\x{000A}\x{000D}\x{000A}\x{085}\x{2028}\x{2029}]/u name => 'single character'
<simple escape sequence>      ::= "\\'"                                                                         name => 'simple escape sequence (1)'
                                | '\\"'                                                                         name => 'simple escape sequence (2)'
                                | '\\\\'                                                                        name => 'simple escape sequence (3)'
                                | '\\0'                                                                         name => 'simple escape sequence (4)'
                                | '\\a'                                                                         name => 'simple escape sequence (5)'
                                | '\\b'                                                                         name => 'simple escape sequence (6)'
                                | '\\f'                                                                         name => 'simple escape sequence (7)'
                                | '\\n'                                                                         name => 'simple escape sequence (8)'
                                | '\\r'                                                                         name => 'simple escape sequence (9)'
                                | '\\t'                                                                         name => 'simple escape sequence (10)'
                                | '\\v'                                                                         name => 'simple escape sequence (11)'
<hexadecimal escape sequence> ::= '\\x' <hex digit>                                                             name => 'hexadecimal escape sequence (1)'
                                | '\\x' <hex digit> <hex digit>                                                 name => 'hexadecimal escape sequence (2)'
                                | '\\x' <hex digit> <hex digit> <hex digit>                                     name => 'hexadecimal escape sequence (3)'
                                | '\\x' <hex digit> <hex digit> <hex digit> <hex digit>                         name => 'hexadecimal escape sequence (4)'

# 7.4.5.6 String literals

<string literal>                           ::= <regular string literal>                                                       name => 'string literal (1)'
                                             | <verbatim string literal>                                                      name => 'string literal (2)'
<regular string literal>                   ::= '"'                                     '"'                                    name => 'regular string literal (1)'
                                             | '"' <regular string literal characters> '"'                                    name => 'regular string literal (2)'
<regular string literal characters>        ::= <regular string literal character>+                                            name => 'regular string literal characters'
<regular string literal character>         ::= <single regular string literal character>                                      name => 'regular string literal character (1)'
                                             | <simple escape sequence>                                                       name => 'regular string literal character (2)'
                                             | <hexadecimal escape sequence>                                                  name => 'regular string literal character (3)'
                                             | <unicode escape sequence>                                                      name => 'regular string literal character (4)'
<single regular string literal character>  ::= /[^\x{0022}\x{005C}\x{000D}\x{000A}\x{000D}\x{000A}\x{085}\x{2028}\x{2029}]/u  name => 'single regular string literal character'
<verbatim string literal>                  ::= '@"'                                      '"'                                  name => 'verbatim string literal (1)'
                                             | '@"' <verbatim string literal characters> '"'                                  name => 'verbatim string literal (2)'
<verbatim string literal characters>       ::= <verbatim string literal character>+                                           name => 'verbatim string literal characters'
<verbatim string literal character>        ::= <single verbatim string literal character>                                     name => 'verbatim string literal character (1)'
                                             | <quote escape sequence>                                                        name => 'verbatim string literal character (2)'
<single verbatim string literal character> ::= /[^"]/                                                                         name => 'single verbatim string literal character'
<quote escape sequence>                    ::= '""'                                                                           name => 'quote escape sequence'

# 7.4.5.7 The null literal

<null literal> ::= 'null' name => 'null literal'

#
# 7.4.6 Operators and punctuators
# -------------------------------

<operator or punctuator> ::= '{'      name => 'operator or punctuator (1)'
                           | '}'      name => 'operator or punctuator (2)'
                           | '['      name => 'operator or punctuator (3)'
                           | ']'      name => 'operator or punctuator (4)'
                           | '('      name => 'operator or punctuator (5)'
                           | ')'      name => 'operator or punctuator (6)'
                           | '.'      name => 'operator or punctuator (7)'
                           | ','      name => 'operator or punctuator (8)'
                           | ':'      name => 'operator or punctuator (9)'
                           | ';'      name => 'operator or punctuator (10)'
                           | '+'      name => 'operator or punctuator (11)'
                           | '-'      name => 'operator or punctuator (12)'
                           | '*'      name => 'operator or punctuator (13)'
                           | '/'      name => 'operator or punctuator (14)'
                           | '%'      name => 'operator or punctuator (15)'
                           | '&'      name => 'operator or punctuator (16)'
                           | '|'      name => 'operator or punctuator (17)'
                           | '^'      name => 'operator or punctuator (18)'
                           | '!'      name => 'operator or punctuator (19)'
                           | '~'      name => 'operator or punctuator (20)'
                           | '='      name => 'operator or punctuator (21)'
                           | '<'      name => 'operator or punctuator (22)'
                           | '>'      name => 'operator or punctuator (23)'
                           | '?'      name => 'operator or punctuator (24)'
                           | '??'     name => 'operator or punctuator (25)'
                           | '::'     name => 'operator or punctuator (26)'
                           | '++'     name => 'operator or punctuator (27)'
                           | '--'     name => 'operator or punctuator (28)'
                           | '&&'     name => 'operator or punctuator (29)'
                           | '||'     name => 'operator or punctuator (30)'
                           | '=='     name => 'operator or punctuator (31)'
                           | '!='     name => 'operator or punctuator (32)'
                           | '<='     name => 'operator or punctuator (33)'
                           | '>='     name => 'operator or punctuator (34)'
                           | '+='     name => 'operator or punctuator (35)'
                           | '*='     name => 'operator or punctuator (36)'
                           | '/='     name => 'operator or punctuator (37)'
                           | '%='     name => 'operator or punctuator (38)'
                           | '&='     name => 'operator or punctuator (39)'
                           | '|='     name => 'operator or punctuator (40)'
                           | '^='     name => 'operator or punctuator (41)'
                           | '<<'     name => 'operator or punctuator (42)'
                           | '<<='    name => 'operator or punctuator (43)'
<right shift> ::= '>' '>'             name => 'right shift'
<right shift assignment> ::= '>' '>=' name => 'right shift assignment'

#
# 7.5 Pre-processing directives
# =============================

#
# 7.5.1 General
# -------------

<pp directive> ::= <pp declaration>   name => 'pp directive (1)'
                 | <pp conditional>   name => 'pp directive (2)'
                 | <pp line>          name => 'pp directive (3)'
                 | <pp diagnostic>    name => 'pp directive (4)'
                 | <pp region>        name => 'pp directive (5)'
                 | <pp pragma>        name => 'pp directive (6)'

#
# 7.5.2 Conditional compilation symbols
# -------------------------------------

<conditional symbol> ::= <Any identifier or keyword except true or false> name => 'conditional symbol'

#
# 7.5.3 Pre-processing expressions
# --------------------------------

<whitespace opt>         ::=                                                                                       name => 'whitespace opt (nulled)'
<whitespace opt>         ::= <whitespace>                                                                          name => 'whitespace opt'
<pp expression>          ::= <whitespace opt> <pp or expression> <whitespace opt>                                  name => 'pp expression'
<pp or expression>       ::= <pp and expression>                                                                   name => 'pp or expression (1)'
                           | <pp or expression> <whitespace opt> '||' <whitespace opt> <pp and expression>         name => 'pp or expression (2)'
<pp and expression>      ::= <pp equality expression>                                                              name => 'pp and expression (1)'
                           | <pp and expression> <whitespace opt> '&&' <whitespace opt> <pp equality expression>   name => 'pp and expression (2)'
<pp equality expression> ::= <pp unary expression>                                                                 name => 'pp equality expression (1)'
                           | <pp equality expression> <whitespace opt> '==' <whitespace opt> <pp unary expression> name => 'pp equality expression (2)'
                           | <pp equality expression> <whitespace opt> '!=' <whitespace opt> <pp unary expression> name => 'pp equality expression (3)'
<pp unary expression>    ::= <pp primary expression>                                                               name => 'pp unary expression (1)'
                           | '!' <whitespace opt> <pp unary expression>                                            name => 'pp unary expression (2)'
<pp primary expression>  ::= TRUE                                                                                  name => 'pp primary expression (1)'
                           | FALSE                                                                                 name => 'pp primary expression (2)'
                           | <conditional symbol>                                                                  name => 'pp primary expression (3)'
                           | '(' <whitespace opt> <pp expression> <whitespace opt> ')'                             name => 'pp primary expression (4)'

#
# 7.5.4 Definition directives 
# ---------------------------

<pp declaration> ::= <whitespace opt> '#' <whitespace opt> 'define' <whitespace> <conditional symbol> <pp new line> name => 'pp declaration (1)'
                   | <whitespace opt> '#' <whitespace opt> 'undef' <whitespace> <conditional symbol> <pp new line>  name => 'pp declaration (2)'
<pp new line>    ::= <whitespace opt>                       <new line>                                              name => 'pp new line (1)'
                   | <whitespace opt> <single line comment> <new line>                                              name => 'pp new line (2)'

#
# 7.5.5 Conditional compilation directives
# ----------------------------------------

<pp conditional>          ::= <pp if section> <pp elif sections opt> <pp else section opt> <pp endif>                                            name => 'pp conditional'
<pp else section opt>     ::=                                                                                                                    name => 'pp else section opt (nulled)'
<pp else section opt>     ::= <pp else section>                                                                                                  name => 'pp else section opt'
<pp elif sections opt>    ::=                                                                                                                    name => 'pp elif section opt (nulled)'
<pp elif sections opt>    ::= <pp elif sections>                                                                                                 name => 'pp elif section opt'
<pp if section>           ::= <whitespace opt> '#' <whitespace opt> 'if' <whitespace>  <pp expression> <pp new line> <conditional section opt>   name => 'pp if section'
<conditional section opt> ::=                                                                                                                    name => 'conditional section opt (nulled)'
<conditional section opt> ::= <conditional section>                                                                                              name => 'conditional section opt'
<pp elif sections>        ::= <pp elif section>+                                                                                                 name => 'pp elif sections'
<pp elif section>         ::= <whitespace opt> '#' <whitespace opt> 'elif' <whitespace> <pp expression> <pp new line> <conditional section opt>  name => 'pp elif section'
<pp else section>         ::= <whitespace opt> '#' <whitespace opt> 'else' <pp new line> <conditional section opt>                               name => 'pp else section'
<pp endif>                ::= <whitespace opt> '#' <whitespace opt> 'endif' <pp new line>                                                        name => 'pp endif'
<conditional section>     ::= <input section>                                                                                                    name => 'conditional section (1)'
                            | <skipped section>                                                                                                  name => 'conditional section (2)'
<skipped section>         ::= <skipped section part>+                                                                                            name => 'skipped section'
<skipped section part>    ::= <skipped characters opt> <new line>                                                                                name => 'skipped section part (1)'
                            | <pp directive>                                                                                                     name => 'skipped section part (2)'
<skipped characters opt>  ::=                                                                                                                    name => 'skipped characters opt (nulled)'
<skipped characters opt>  ::= <skipped characters>                                                                                               name => 'skipped characters opt'
<skipped characters>      ::= <whitespace opt> <not number sign> <input characters opt>                                                          name => 'skipped characters'
<input characters opt>    ::=                                                                                                                    name => 'input characters opt (nulled)'
<input characters opt>    ::= <input characters>                                                                                                 name => 'input characters'
<not number sign>         ::= /[^#]/                                                                                                             name => 'not number sign'

#
# 7.5.6 Diagnostic directives
# ---------------------------

<pp diagnostic> ::= <whitespace opt> '#' <whitespace opt> 'error' <pp message>   name => 'pp diagnostic (1)'
                  | <whitespace opt> '#' <whitespace opt> 'warning' <pp message> name => 'pp diagnostic (2)'
<pp message>    ::= <new line>                                                   name => 'pp message (1)'
                  | <whitespace> <input characters opt> <new line>               name => 'pp message (2)'


#
# 7.5.7 Region directives
# -----------------------

<pp region>       ::= <pp start region> <conditional section opt> <pp end region>    name => 'pp region'
<pp start region> ::= <whitespace opt> '#' <whitespace opt> 'region' <pp message>    name => 'pp start region'
<pp end region>   ::= <whitespace opt> '#' <whitespace opt> 'endregion' <pp message> name => 'pp end region'


#
# 7.5.8 Line directives
# ---------------------

<pp line>              ::= <whitespace opt> '#' <whitespace opt> 'line' <whitespace> <line indicator> <pp new line> name => 'pp line'
<line indicator>       ::= <decimal digits> <whitespace> <file name>                                                name => 'line indicator (1)'
                         | <decimal digits>                                                                         name => 'line indicator (2)'
                         | 'default'                                                                                name => 'line indicator (3)'
                         | 'hidden'                                                                                 name => 'line indicator (4)'
<file name>            ::= '"' <file name characters> '"'                                                           name => 'file name'
<file name characters> ::= <file name character>+                                                                   name => 'file name characters'
<file name character>  ::= /[^\x{0022}\x{000D}\x{000A}\x{000D}\x{000A}\x{085}\x{2028}\x{2029}]/u                    name => 'file name character'

#
# 7.5.9 Pragma directives
# -----------------------

<pp pragma> ::= <whitespace opt> '#' <whitespace opt> 'pragma' <pp pragma text>                                     name => 'pp pragma'
<pp pragma text> ::= <new line>                                                                                     name => 'pp pragma text (1)'
                   | <whitespace> <input characters opt> <new line>                                                 name => 'pp pragma text (2)'

#
# Lexemes
#
<An identifier or keyword that is not a keyword>                                     ::= <AN IDENTIFIER OR KEYWORD THAT IS NOT A KEYWORD>
<A unicode escape sequence representing the character 005f>                          ::= <CHARACTER 005F>
<A unicode escape sequence representing a character of classes Lu Ll Lt Lm Lo or Nl> ::= <CHARACTER OF CLASSES Lu Ll Lt Lm Lo OR Nl>
<A unicode escape sequence representing a character of classes Mn or Mc>             ::= <CHARACTER OF CLASSES Mn OR Mc>
<A unicode escape sequence representing a character of the class Nd>                 ::= <CHARACTER OF THE CLASS Nd>
<A unicode escape sequence representing a character of the class Pc>                 ::= <CHARACTER OF THE CLASS Pc>
<A unicode escape sequence representing a character of the class Cf>                 ::= <CHARACTER OF THE CLASS Cf>
<Any identifier or keyword except true or false>                                     ::= <ANY IDENTIFIER OR KEYWORD EXCEPT TRUE OR FALSE>

TRUE                                                                                 ~ 'true'
FALSE                                                                                ~ 'false'
<AN IDENTIFIER OR KEYWORD THAT IS NOT A KEYWORD>                                     ~ [^\s\S]     # Matches nothing
<CHARACTER 005F>                                                                     ~ [^\s\S]     # Matches nothing
<CHARACTER OF CLASSES Lu Ll Lt Lm Lo OR Nl>                                          ~ [^\s\S]     # Matches nothing
<CHARACTER OF CLASSES Mn OR Mc>                                                      ~ [^\s\S]     # Matches nothing
<CHARACTER OF THE CLASS Nd>                                                          ~ [^\s\S]     # Matches nothing
<CHARACTER OF THE CLASS Pc>                                                          ~ [^\s\S]     # Matches nothing
<CHARACTER OF THE CLASS Cf>                                                          ~ [^\s\S]     # Matches nothing
<ANY IDENTIFIER OR KEYWORD EXCEPT TRUE OR FALSE>                                     ~ [^\s\S]     # Matches nothing

#
# Events
#
event ^An_identifier_or_keyword_that_is_not_a_keyword                                     = predicted <An identifier or keyword that is not a keyword>
event ^A_unicode_escape_sequence_representing_the_character_005f                          = predicted <A unicode escape sequence representing the character 005f>
event ^A_unicode_escape_sequence_representing_a_character_of_classes_Lu_Ll_Lt_Lm_Lo_or_Nl = predicted <A unicode escape sequence representing a character of classes Lu Ll Lt Lm Lo or Nl>
event ^A_unicode_escape_sequence_representing_a_character_of_classes_Mn_or_Mc             = predicted <A unicode escape sequence representing a character of classes Mn or Mc>
event ^A_unicode_escape_sequence_representing_a_character_of_the_class_Nd                 = predicted <A unicode escape sequence representing a character of the class Nd>
event ^A_unicode_escape_sequence_representing_a_character_of_the_class_Pc                 = predicted <A unicode escape sequence representing a character of the class Pc>
event ^A_unicode_escape_sequence_representing_a_character_of_the_class_Cf                 = predicted <A unicode escape sequence representing a character of the class Cf>
event ^Any_identifier_or_keyword_except_true_or_false                                     = predicted <Any identifier or keyword except true or false>
event sub_grammar_completion$                                                             = completed <identifier or keyword>
event sub_grammar_completion$                                                             = completed <keyword>

<UNICODE ESCAPE SEQUENCE>   ~ /\\u[0-9A-Fa-f]{4}/
                            | /\\u[0-9A-Fa-f]{8}/

__[ syntactic ]__
todo ::= 'TO DO'
