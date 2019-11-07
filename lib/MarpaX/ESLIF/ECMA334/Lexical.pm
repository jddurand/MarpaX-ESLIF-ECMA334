use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::ECMA334::Lexical;

# ABSTRACT: C# lexical parse as per Standard ECMA-334 5th Edition

# VERSION

# AUTHORITY

=head1 DESCRIPTION

This module parses lexically the C# language as per Standard ECMA-334 5th Edition.

=head1 SYNOPSIS

    use MarpaX::ESLIF::ECMA334::Lexical;

    my $lexical      = MarpaX::ESLIF::ECMA334::Lexical->new();
    my $input        = "public interface Test { bool MyTest(); }"
    my $lexicalValue = $lexical->parse(input => input, encoding => 'UTF-16', definitions => { 'TRUE' => $MarpaX::ESLIF::true });

=cut

use Data::Section -setup;
use MarpaX::ESLIF::ECMA334::Lexical::RecognizerInterface;
use MarpaX::ESLIF::ECMA334::Lexical::ValueInterface;
use MarpaX::ESLIF 3.0.19; # if-action, discardLast()
use Log::Any qw/$log/;

my $PRE_LEXICAL_BNF           = ${__PACKAGE__->section_data('pre lexical grammar')};
my $LEXICAL_BNF               = ${__PACKAGE__->section_data('lexical grammar')};
my $IDENTIFIER_OR_KEYWORD_BNF = ${__PACKAGE__->section_data('identifier or keyword grammar')};
my $KEYWORD_BNF               = ${__PACKAGE__->section_data('keyword grammar')};
my $PP_EXPRESSION_BNF         = ${__PACKAGE__->section_data('pp expression grammar')};

my $PRE_LEXICAL_GRAMMAR           = MarpaX::ESLIF::Grammar->new(MarpaX::ESLIF->new(), $PRE_LEXICAL_BNF);
my $LEXICAL_GRAMMAR               = MarpaX::ESLIF::Grammar->new(MarpaX::ESLIF->new(), $LEXICAL_BNF);
my $IDENTIFIER_OR_KEYWORD_GRAMMAR = MarpaX::ESLIF::Grammar->new(MarpaX::ESLIF->new(), $IDENTIFIER_OR_KEYWORD_BNF); # No log
my $KEYWORD_GRAMMAR               = MarpaX::ESLIF::Grammar->new(MarpaX::ESLIF->new(), $KEYWORD_BNF); # No log
my $PP_EXPRESSION_GRAMMAR         = MarpaX::ESLIF::Grammar->new(MarpaX::ESLIF->new(), $PP_EXPRESSION_BNF);

my %USERFRIENDLYLEXEMES = (
    'LINE INDICATOR DEFAULT'                           => 'default',
    'LINE INDICATOR HIDDEN'                            => 'hidden',
    # <NEW LINE>                                         => /(?:\x{000D}|\x{000A}|\x{000D}\x{000A}|\x{0085}|\x{2028}|\x{2029})/u
    # <SINGLE CHARACTER>                                 => /[^\x{0027}\x{005C}\x{000D}\x{000A}\x{0085}\x{2028}\x{2029}]/u   # <ANY CHARACTER EXCEPT 0027 005C AND NEW LINE CHARACTER>
    # <SINGLE REGULAR STRING LITERAL CHARACTER>          => /[^\x{0022}\x{005C}\x{000D}\x{000A}\x{0085}\x{2028}\x{2029}]/u   # <ANY CHARACTER EXCEPT 0022 005C AND NEW LINE CHARACTER>
    'KEYWORD'                                          =>
    [
     'abstract'
     , 'byte'
     , 'class'
     , 'delegate'
     , 'event'
     , 'fixed'
     , 'if'
     , 'internal'
     , 'new'
     , 'override'
     , 'readonly'
     , 'short'
     , 'struct'
     , 'try'
     , 'unsafe'
     , 'volatile'
     , 'as'
     , 'case'
     , 'const'
     , 'do'
     , 'explicit'
     , 'float'
     , 'implicit'
     , 'is'
     , 'null'
     , 'params'
     , 'ref'
     , 'sizeof'
     , 'switch'
     , 'typeof'
     , 'ushort'
     , 'while'
     , 'base'
     , 'catch'
     , 'continue'
     , 'double'
     , 'extern'
     , 'for'
     , 'in'
     , 'lock'
     , 'object'
     , 'private'
     , 'return'
     , 'stackalloc'
     , 'this'
     , 'uint'
     , 'using'
     , 'bool'
     , 'char'
     , 'decimal'
     , 'else'
     , 'false'
     , 'foreach'
     , 'int'
     , 'long'
     , 'operator'
     , 'protected'
     , 'sbyte'
     , 'static'
     , 'throw'
     , 'ulong'
     , 'virtual'
     , 'break'
     , 'checked'
     , 'default'
     , 'enum'
     , 'finally'
     , 'goto'
     , 'interface'
     , 'namespace'
     , 'out'
     , 'public'
     , 'sealed'
     , 'string'
     , 'true'
     , 'unchecked'
     , 'void'
    ],
    # <IDENTIFIER OR KEYWORD>                            => /[^\s\S]/ # Matches nothing
    # <ANY IDENTIFIER OR KEYWORD EXCEPT TRUE OR FALSE>   => /[^\s\S]/ # Matches nothing
    # <AN IDENTIFIER OR KEYWORD THAT IS NOT A KEYWORD>   => /[^\s\S]/ # Matches nothing
    'PP DEFINE'                                        => '#define',
    'PP UNDEF'                                         => '#undef',
    'PP IF'                                            => '#if',
    'PP ELIF'                                          => '#elif',
    'PP ELSE'                                          => '#else',
    'PP ENDIF'                                         => '#endif',
    'PP LINE'                                          => '#line',
    'PP ERROR'                                         => '#error',
    'PP WARNING'                                       => '#warning',
    'PP REGION'                                        => '#region',
    'PP ENDREGION'                                     => '#endregion',
    'PP PRAGMA'                                        => '#pragma',
    # <CONDITIONAL SECTION OK>                           ~ /[^\s\S]/ # Matches nothing
    # <CONDITIONAL SECTION KO>                           ~ /[^\s\S]/ # Matches nothing
    # <PP EXPRESSION>                                    ~ /[^\s\S]/ # Matches nothing
    );

#
# Exceptions definitions
#
use Exception::Class (
    'MarpaX::ESLIF::ECMA334::Lexical::Exception' =>
    {
        description => 'Lexical exception',
        fields      => [ 'file', 'line', '_line', 'column', 'expected' ],
        alias       => 'throw_exception'
    },

    'MarpaX::ESLIF::ECMA334::Lexical::Exception::Internal' => {
        isa         => 'MarpaX::ESLIF::ECMA334::Lexical::Exception',
        description => 'Internal error',
        alias       => 'throw_internal_exception'
    },

    'MarpaX::ESLIF::ECMA334::Lexical::Exception::PP::Error' => {
        isa         => 'MarpaX::ESLIF::ECMA334::Lexical::Exception',
        description => '#error directive',
        alias       => 'throw_pp_exception'
    }
);

=head1 SUBROUTINES/METHODS

=cut

# ============================================================================
# new
# ============================================================================

=head2 new($class)

Instantiate a new lexical object.

=cut

sub new {
    my ($pkg) = @_;

    return bless
        (
         {
             last_pp_expression => undef,             # Last <pp expression>
             last_pp_message => undef,                # Last <pp message>
             last_conditional_symbol => undef,        # Last <conditional symbol>
             last_identifier => undef,                # Last <identifier>
             last_keyword => undef,                   # Last <keyword>
             can_next_conditional_section => [],      # Booleans to drive the grammar using <CONDITIONAL SECTION OK> or <CONDITIONAL SECTION KO>
             has_token => 0,                          # Boolean to say if at least one token is seen
             line_hidden => 0,                        # Current line information is hidden or not (for debuggers)
             line => 1,                               # Current line with respect of #line directives
             _line => 1,                              # Current line without respect of #line directives
             line_directive_filename => undef,        # file name in #line directive, but before the end of #line directive
             line_directive_line => undef,            # line number in #line directive, but before the end of #line directive
             filename => undef,                       # Current file name with respect of #line directives
             elements => []                           # Flat list of all elements, including :discard
         },
         $pkg)
}

# ============================================================================
# parse
# ============================================================================

=head2 parse($self, %options)

Parser method. C<%options> obeys to the same constraints as L<MarpaX::ESLIF::ECMA334::Lexical::RecognizerInterface>.

Output is an AST of the lexical parse.

=cut

sub parse {
    my ($self, %options) = @_;

    #
    # Lexical parse
    #
    my ($result, $match) =  $self->_parse
        (
         $LEXICAL_GRAMMAR,
         'MarpaX::ESLIF::ECMA334::Lexical::RecognizerInterface',
         'MarpaX::ESLIF::ECMA334::Lexical::ValueInterface',
         \&_lexicalEventManager,
         undef, # sharedEslifRecognizer
         %options,
         input => $self->_preparse(%options),
         encoding => 'UTF-8'
        );

    return $self->{elements}
}

# ============================================================================
# _preparse
# ============================================================================
sub _preparse {
    my ($self, %options) = @_;

    my $input = delete($options{input}) // '';
    my $encoding = delete($options{encoding}); # Can be undef

    my $output = $input;
    my $inputLength = bytes::length($input);
    if ($inputLength) {
        #
        # Pre-parse: we require this is a valid source file from encoding point of view
        #
        my $eslifRecognizerInterface = MarpaX::ESLIF::ECMA334::Lexical::RecognizerInterface->new(input => $input, encoding => $encoding);
        my $eslifValueInterface = MarpaX::ESLIF::ECMA334::Lexical::ValueInterface->new();
        $PRE_LEXICAL_GRAMMAR->parse($eslifRecognizerInterface, $eslifValueInterface) || $self->_exception($PRE_LEXICAL_GRAMMAR, $eslifRecognizerInterface, undef, 'Pre-parse phase failure');
        $output = $eslifValueInterface->getResult();

        #
        # - If the last character of the source file is a Control-Z character (U+001A), this character is deleted
        #
        $output =~ s/\x{001A}$//;

        #
        # - A carriage-return character (U+000D) is added to the end of the source file if that source file is non-
        #   empty and if the last character of the source file is not a carriage return (U+000D), a line feed
        #   (U+000A), a next line character (U+0085), a line separator (U+2028), or a paragraph separator
        #   (U+2029)
        #
        $output .= "\x{000D}" if (bytes::length($output) > 0 && $output !~ /[\x{000D}\x{000A}\x{0085}\x{2028}\x{2029}]$/)
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

    $log->tracef("[%d] %s: Start", $eslifRecognizerInterface->recurseLevel, $eslifGrammar->currentDescription);

    # ------------------------
    # Instanciate a recognizer
    # ------------------------
    my $eslifRecognizer = defined($sharedEslifRecognizer) ?
        $sharedEslifRecognizer->newFrom($eslifGrammar)
        :
        MarpaX::ESLIF::Recognizer->new($eslifGrammar, $eslifRecognizerInterface);

    # --------------------------------------------------------------------
    # Because we may have prediction events even before the first read
    # the input may be undef. If that is the case, we read it. This can
    # happen only once in the whole life of parsing, including sub parses.
    # --------------------------------------------------------------------
    if (! defined($eslifRecognizer->input)) {
        $eslifRecognizer->read || $self->_exception($eslifGrammar, $eslifRecognizerInterface, $eslifRecognizer, 'Initial read failed')
    }
    my $initialLength = bytes::length($eslifRecognizer->input) // 0;
    $log->tracef("[%d] %s: initialLength=%s", $eslifRecognizerInterface->recurseLevel, $eslifGrammar->currentDescription, $initialLength);

    # -----------------------------------------------------
    # Run recognizer manually so that events are accessible
    # -----------------------------------------------------
    $self->_scan($eslifGrammar, $eslifRecognizerInterface, $eslifRecognizer, $eventManager);

    while ($eslifRecognizer->isCanContinue) {
        if (! $self->_resume($eslifGrammar, $eslifRecognizerInterface, $eslifRecognizer, $eventManager)) {
            #
            # This is a failure unless it is a sub-grammar that has reached completion at least once
            #
            if ($eslifRecognizerInterface->hasCompletion && $eslifRecognizerInterface->recurseLevel) {
                last
            } else {
                $self->_exception($eslifGrammar, $eslifRecognizerInterface, $eslifRecognizer, 'resume() failed')
            }
        }
    }

    # -----------------------------------------------------------------------------------------
    # Call for valuation (we configured value interface to not accept ambiguity nor null parse)
    # -----------------------------------------------------------------------------------------
    my $eslifValueInterface = $eslifValueInterfaceClass->new(%options);

    $self->_exception($eslifGrammar, $eslifRecognizerInterface, undef, 'Valuation failure') unless MarpaX::ESLIF::Value->new($eslifRecognizer, $eslifValueInterface)->value();

    # -------------------------------
    # Return the length and the value
    # -------------------------------
    my $finalLength = bytes::length($eslifRecognizer->input) // 0;
    my $length = $initialLength - $finalLength;
    my $match = bytes::substr($eslifRecognizerInterface->data, 0, $length);
    my $value = $eslifValueInterface->getResult;

    $log->tracef("[%d] %s: Success: Remains %d bytes", $eslifRecognizerInterface->recurseLevel, $eslifGrammar->currentDescription, $finalLength);

    return ($eslifValueInterface->getResult, $match)
}

# ============================================================================
# _scan
# ============================================================================
sub _scan {
    my ($self, $eslifGrammar, $eslifRecognizerInterface, $eslifRecognizer, $eventManager) = @_;

    $log->tracef("[%d] %s: Scan on %d bytes", $eslifRecognizerInterface->recurseLevel, $eslifGrammar->currentDescription, bytes::length($eslifRecognizer->input // ''));
    $eslifRecognizer->scan(1) || $self->_exception($eslifGrammar, $eslifRecognizerInterface, $eslifRecognizer, 'Initial scan failed');
    #
    # Scan can generate events
    #
    while ($self->$eventManager($eslifGrammar, $eslifRecognizer, $eslifRecognizerInterface)) {
    }
}

# ============================================================================
# _resume
# ============================================================================
sub _resume {
    my ($self, $eslifGrammar, $eslifRecognizerInterface, $eslifRecognizer, $eventManager) = @_;

    $log->tracef("[%d] %s: Resume on %d bytes", $eslifRecognizerInterface->recurseLevel, $eslifGrammar->currentDescription, bytes::length($eslifRecognizer->input // ''));
    my $rc = $eslifRecognizer->resume;
    if ($rc) {
        #
        # Resume can generate events, even in case of failure
        #
        while ($self->$eventManager($eslifGrammar, $eslifRecognizer, $eslifRecognizerInterface)) {
        }
    }

    return $rc
}

# ============================================================================
# _identifier_or_keyword
# ============================================================================
sub _identifier_or_keyword {
    my ($self, $eslifRecognizer, $eslifRecognizerInterface) = @_;

    return eval
    {
        $self->_parse(
            $IDENTIFIER_OR_KEYWORD_GRAMMAR,
            'MarpaX::ESLIF::ECMA334::Lexical::RecognizerInterface',
            'MarpaX::ESLIF::ECMA334::Lexical::ValueInterface',
            \&_lexicalEventManager,
            undef, # sharedEslifRecognizer
            input => $eslifRecognizer->input,
            recurseLevel => $eslifRecognizerInterface->recurseLevel + 1,
            exhaustion => 1,
            encoding => 'UTF-8',
            definitions => $eslifRecognizerInterface->definitions
            )
    }
}

# ============================================================================
# _pp_expression
# ============================================================================
sub _pp_expression {
    my ($self, $eslifRecognizer, $eslifRecognizerInterface) = @_;

    return eval
    {
        $self->_parse(
            $PP_EXPRESSION_GRAMMAR,
            'MarpaX::ESLIF::ECMA334::Lexical::RecognizerInterface',
            'MarpaX::ESLIF::ECMA334::Lexical::ValueInterface',
            \&_lexicalEventManager,
            $eslifRecognizer, # sharedEslifRecognizer
            input => $eslifRecognizer->input,
            recurseLevel => $eslifRecognizerInterface->recurseLevel + 1,
            exhaustion => 1,
            encoding => 'UTF-8',
            definitions => $eslifRecognizerInterface->definitions
            )
    }
}

# ============================================================================
# _keyword
# ============================================================================
sub _keyword {
    my ($self, $eslifRecognizer, $eslifRecognizerInterface) = @_;

    return eval
    {
        $self->_parse(
            $KEYWORD_GRAMMAR,
            'MarpaX::ESLIF::ECMA334::Lexical::RecognizerInterface',
            'MarpaX::ESLIF::ECMA334::Lexical::ValueInterface',
            \&_lexicalEventManager,
            undef, # sharedEslifRecognizer
            input => $eslifRecognizer->input,
            recurseLevel => $eslifRecognizerInterface->recurseLevel + 1,
            exhaustion => 1,
            encoding => 'UTF-8',
            definitions => $eslifRecognizerInterface->definitions
            )
    }
}

# ============================================================================
# _exception
# ============================================================================
sub _exception {
    my ($self, $eslifGrammar, $eslifRecognizerInterface, $eslifRecognizer, $fmt, @args) = @_;

    $self->_error(\&throw_exception, $eslifGrammar, $eslifRecognizerInterface, $eslifRecognizer, $fmt, @args)
}

# ============================================================================
# _internal_exception
# ============================================================================
sub _internal_exception {
    my ($self, $eslifGrammar, $eslifRecognizerInterface, $eslifRecognizer, $fmt, @args) = @_;

    $self->_error(\&throw_internal_exception, $eslifGrammar, $eslifRecognizerInterface, $eslifRecognizer, $fmt, @args)
}

# ============================================================================
# _pp_exception
# ============================================================================
sub _pp_exception {
    my ($self, $eslifGrammar, $eslifRecognizerInterface, $eslifRecognizer, $fmt, @args) = @_;

    $self->_error(\&throw_pp_exception, $eslifGrammar, $eslifRecognizerInterface, $eslifRecognizer, $fmt, @args)
}

# ============================================================================
# _error
# ============================================================================
sub _error {
    my ($self, $exceptionCallback, $eslifGrammar, $eslifRecognizerInterface, $eslifRecognizer, $fmt, @args) = @_;

    my ($_line, $column, $expected);

    if (defined($eslifRecognizer)) {
        ($_line, $column) = $eslifRecognizer->location();
        #
        # We know what lexemes are - so we rearrange
        # the list of expected lexemes to something
        # more user-friendly. Auto-vivification, if any, is ok.
        #
        my @expected = map
        {
            defined($USERFRIENDLYLEXEMES{$_})
                ?
                (
                 (ref($USERFRIENDLYLEXEMES{$_}) // '') eq 'ARRAY' ? @{$USERFRIENDLYLEXEMES{$_}} : $USERFRIENDLYLEXEMES{$_}
                )
                :
                $_
        } @{$eslifRecognizer->lexemeExpected};
        $expected = \@expected;
    }

    my $error = @args ? ($fmt ? sprintf($fmt, @args) : '') : ($fmt // '');

    $log->tracef("[%d] %s: Exception: %s", $eslifRecognizerInterface->recurseLevel, $eslifGrammar->currentDescription, $error);

    my @exception_args = (
        error => $error,
        file => $self->{filename},
        line => $self->{line},
        _line => $_line,
        column => $column,
        expected => $expected
        );

    $exceptionCallback->(@exception_args);
}

# ============================================================================
# _lexicalEventManager
# ============================================================================
#
# The event manager must always return a true value if it has performed a lexeme complete
#
sub _lexicalEventManager {
    my ($self, $eslifGrammar, $eslifRecognizer, $eslifRecognizerInterface) = @_;

    my $rc = 0;

    my @events = grep { defined } map { $_->{event} } @{$eslifRecognizer->events};

    #
    # At any predicted event, we have two possible sub-grammars
    #
    my @alternatives;
    my $latm = -1;

    my $have_pp_line_completion = grep {$_ eq 'pp_line$'} @events;
    my $have_NEW_LINE_completion = grep {$_ eq 'NEW_LINE$'} @events;
    my $have_diagnostics = grep {$_ eq '^trigger_pp_error' || $_ eq '^trigger_pp_warning' } @events;
    my $have_last_pp_message_empty = grep {$_ eq 'last_pp_message_empty[]' } @events;
    my $have_last_pp_message_not_empty = grep {$_ eq 'last_pp_message_not_empty[]' } @events;

    #
    # Always process last_pp_message_empty[] and last_pp_message_not_empty[] before any event
    #
    if ($have_last_pp_message_empty) {
        $self->{last_pp_message} = '';
    } elsif ($have_last_pp_message_not_empty) {
        my ($offset, $bytes_length) = $eslifRecognizer->lastCompletedLocation('input characters');
        my $input_characters = bytes::substr($eslifRecognizerInterface->data, $offset, $bytes_length);
        utf8::decode($input_characters);
        $self->{last_pp_message} = $input_characters;
    }
    #
    # Always process pp_line$ and NEW_LINE$ before any event,
    # unless there is an #error or #warning event
    #
    if (! $have_diagnostics) {
        if ($have_pp_line_completion) {
            #
            # Commit <pp line> indicator
            #
            $self->{filename} = $self->{line_directive_filename};
            if ($self->{line_directive_line} eq 'default') {
                $self->{line_hidden} = 0;
            } elsif ($self->{line_directive_line} eq 'hidden') {
                $self->{line_hidden} = 1;
            } else {
                $self->{line} = $self->{line_directive_line};
            }
        } elsif ($have_NEW_LINE_completion) {
            $self->{line}++;
            $self->{_line}++;
        }
    }

    $log->tracef("[%d] %s: Events: %s", $eslifRecognizerInterface->recurseLevel, $eslifGrammar->currentDescription, \@events);
    my ($identifier_or_keyword, $identifier_or_keyword_match, $identifier_or_keyword_done) = (undef, undef, 0);
    my ($keyword, $keyword_match, $keyword_done) = (undef, undef, 0);
    foreach my $event (@events) {
        my ($match, $name, $value, $length, $pp_expression) = (undef, undef, undef, undef, undef);

        if ($event eq "'exhausted'") {
            $eslifRecognizerInterface->hasCompletion(1)
        }
        elsif ($event eq 'token$') {
            $self->{has_token} = 1;
        }
        elsif ($event eq 'identifier$') {
            $self->_pushElement($eslifRecognizerInterface, $eslifRecognizer, 1, 'identifier', $self->{last_identifier});
        }
        elsif ($event eq 'keyword$') {
            $self->_pushElement($eslifRecognizerInterface, $eslifRecognizer, 1, 'keyword', $self->{last_keyword});
        }
        elsif ($event eq 'integer_literal$') {
            $self->_pushElement($eslifRecognizerInterface, $eslifRecognizer, 1, 'integer literal');
        }
        elsif ($event eq 'real_literal$') {
            $self->_pushElement($eslifRecognizerInterface, $eslifRecognizer, 1, 'real literal');
        }
        elsif ($event eq 'character_literal$') {
            $self->_pushElement($eslifRecognizerInterface, $eslifRecognizer, 1, 'character literal');
        }
        elsif ($event eq 'string_literal$') {
            $self->_pushElement($eslifRecognizerInterface, $eslifRecognizer, 1, 'string literal');
        }
        elsif ($event eq 'operator_or_punctuator$') {
            $self->_pushElement($eslifRecognizerInterface, $eslifRecognizer, 1, 'operator or punctuator');
        }
        elsif ($event eq 'whitespace$') {
            $self->_pushElement($eslifRecognizerInterface, $eslifRecognizer, 0, 'whitespace');
        }
        elsif ($event eq 'pp_expression$') {
            $eslifRecognizerInterface->hasCompletion(1)
        }
        elsif ($event eq 'identifier_or_keyword$') {
            $eslifRecognizerInterface->hasCompletion(1)
        }
        elsif ($event eq 'pp_declaration_define$') {
            $eslifRecognizerInterface->definitions->{$self->{last_conditional_symbol}} = $MarpaX::ESLIF::true
        }
        elsif ($event eq 'pp_declaration_undef$') {
            $eslifRecognizerInterface->definitions->{$self->{last_conditional_symbol}} = $MarpaX::ESLIF::false
        }
        elsif ($event eq 'PP_DEFINE$') {
            $log->tracef("[%d] %s: has_token: %s", $eslifRecognizerInterface->recurseLevel, $eslifGrammar->currentDescription, $self->{has_token});
            $self->_exception($eslifGrammar, $eslifRecognizerInterface, $eslifRecognizer, 'A #define declaration must appear before any token') if $self->{has_token};
        }
        elsif ($event eq 'PP_UNDEF$') {
            $self->_exception($eslifGrammar, $eslifRecognizerInterface, $eslifRecognizer, 'A #define declaration must appear before any token') if $self->{has_token};
        }
        elsif ($event eq 'NEW_LINE$') {
            #
            # No-op, processed before any other event
            #
        }
        elsif ($event eq 'pp_line_decimal_digits$') {
            my ($offset, $bytes_length) = $eslifRecognizer->lastCompletedLocation('line decimal digits');
            $self->{line_directive_line} = int(bytes::substr($eslifRecognizerInterface->data, $offset, $bytes_length));
        }
        elsif ($event eq 'pp_line_file_name$') {
            my ($offset, $bytes_length) = $eslifRecognizer->lastCompletedLocation('file name');
            my $line_directive_filename = bytes::substr($eslifRecognizerInterface->data, $offset, $bytes_length);
            utf8::decode($line_directive_filename);
            #
            # It contains the double quotes
            #
            substr($line_directive_filename,  0, 1, '');
            substr($line_directive_filename, -1, 1, '');
            $self->{line_directive_filename} = $line_directive_filename;

        }
        elsif ($event eq 'LINE_INDICATOR_DEFAULT$') {
            $self->{line_directive_line} = 'default';
        }
        elsif ($event eq 'LINE_INDICATOR_HIDDEN$') {
            $self->{line_directive_line} = 'hidden';
        }
        elsif ($event eq 'pp_line$') {
            #
            # No-op - processed before everything in any case
            #
        }
        elsif ($event eq 'discard$') {
            $self->_pushElement($eslifRecognizerInterface, $eslifRecognizer, 0, ':discard');
        }
        elsif ($event eq "last_pp_message_empty[]") {
        }
        elsif ($event eq "last_pp_message_not_empty[]") {
        }
        elsif ($event eq "pp_pragma_with_text[]") {
            $self->_pushElement($eslifRecognizerInterface, $eslifRecognizer, 0, 'input characters', undef, '#pragma');
        }
        elsif ($event eq "^trigger_pp_error") {
            $self->_pp_exception($eslifGrammar, $eslifRecognizerInterface, undef, $self->{last_pp_message});
            #
            # Also coded for coherence, the following <TRIGGER PP ERROR> will not be inject
            # because an exception has been raised
            #
            $match = '';
            $value = undef;
            $name = 'TRIGGER PP ERROR';
        }
        elsif ($event eq "^trigger_pp_warning") {
            $log->warning($self->{last_pp_message});
            $match = '';
            $value = undef;
            $name = 'TRIGGER PP WARNING';
        }
        elsif ($event eq "pp_if_context[]") {
            if ($self->{last_pp_expression}) {
                $match = '';
                $value = undef;
                $name = 'CONDITIONAL SECTION OK';
                push(@{$self->{can_next_conditional_section}}, 0)
            } else {
                $match = '';
                $value = undef;
                $name = 'CONDITIONAL SECTION KO';
                push(@{$self->{can_next_conditional_section}}, 1)
            }
        }
        elsif ($event eq "pp_elif_context[]") {
            #
            # We depend on the previous 'if' of 'elif' use of conditional section
            #
            my $can_next_conditional_section = $self->{can_next_conditional_section}->[-1] // $self->_exception($eslifGrammar, $eslifRecognizerInterface, $eslifRecognizer, 'Unbalanced #if/#elif');
            if ($can_next_conditional_section && $self->{last_pp_expression}) {
                $match = '';
                $value = undef;
                $name = 'CONDITIONAL SECTION OK';
                $self->{can_next_conditional_section}->[-1] = 0
            } else {
                $match = '';
                $value = undef;
                $name = 'CONDITIONAL SECTION KO'
            }
        }
        elsif ($event eq "pp_else_context[]") {
            #
            # We depend on the previous 'if' of 'elif' use of conditional section
            #
            my $can_next_conditional_section = $self->{can_next_conditional_section}->[-1] // $self->_exception($eslifGrammar, $eslifRecognizerInterface, $eslifRecognizer, 'Unbalanced #if/#elif');
            if ($can_next_conditional_section && ! $self->{last_pp_expression}) {
                $match = '';
                $value = undef;
                $name = 'CONDITIONAL SECTION OK'
            } else {
                $match = '';
                $value = undef;
                $name = 'CONDITIONAL SECTION KO'
            }
        }
        elsif ($event eq "pp_endif_context[]") {
            pop(@{$self->{can_next_conditional_section}})
        }
        elsif ($event eq '^available_identifier') {
            if (! $identifier_or_keyword_done) {
                ($identifier_or_keyword, $identifier_or_keyword_match) = $self->_identifier_or_keyword($eslifRecognizer, $eslifRecognizerInterface);
                $log->tracef("[%d] %s: <%s> = %s (match on: %s)", $eslifRecognizerInterface->recurseLevel, $eslifGrammar->currentDescription, 'identifier or keyword', $identifier_or_keyword, $identifier_or_keyword_match);
                $identifier_or_keyword_done = 1;
            }
            if (defined($identifier_or_keyword)) {
                if (! $keyword_done) {
                    ($keyword, $keyword_match) = $self->_keyword($eslifRecognizer, $eslifRecognizerInterface);
                    $log->tracef("[%d] %s: <%s> = %s (match on: %s)", $eslifRecognizerInterface->recurseLevel, $eslifGrammar->currentDescription, 'keyword', $keyword, $keyword_match);
                    $keyword_done = 1;
                }
                if ((! defined($keyword)) || ($identifier_or_keyword ne $keyword)) {
                    $match = $identifier_or_keyword_match;
                    $value = $identifier_or_keyword;
                    $name = 'AN IDENTIFIER OR KEYWORD THAT IS NOT A KEYWORD';
                    $self->{last_identifier} = $value
                }
            }
        }
        elsif ($event eq '^keyword') {
            if (! $keyword_done) {
                ($keyword, $keyword_match) = $self->_keyword($eslifRecognizer, $eslifRecognizerInterface);
                $log->tracef("[%d] %s: <%s> = %s (match on: %s)", $eslifRecognizerInterface->recurseLevel, $eslifGrammar->currentDescription, 'keyword', $keyword, $keyword_match);
                $keyword_done = 1;
            }
            if (defined($keyword)) {
                $match = $keyword_match;
                $value = $keyword;
                $name = 'KEYWORD';
                $self->{last_keyword} = $value
            }
        }
        elsif ($event eq '^identifier_or_keyword') {
            if (! $identifier_or_keyword_done) {
                ($identifier_or_keyword, $identifier_or_keyword_match) = $self->_identifier_or_keyword($eslifRecognizer, $eslifRecognizerInterface);
                $log->tracef("[%d] %s: <%s> = %s (match on: %s)", $eslifRecognizerInterface->recurseLevel, $eslifGrammar->currentDescription, 'identifier or keyword', $identifier_or_keyword, $identifier_or_keyword_match);
                $identifier_or_keyword_done = 1;
            }
            if (defined($identifier_or_keyword)) {
                $match = $identifier_or_keyword_match;
                $value = $identifier_or_keyword;
                $name = 'IDENTIFIER OR KEYWORD';
                $self->{last_identifier} = $value
            }
        }
        elsif ($event eq '^conditional_symbol') {
            if (! $identifier_or_keyword_done) {
                ($identifier_or_keyword, $identifier_or_keyword_match) = $self->_identifier_or_keyword($eslifRecognizer, $eslifRecognizerInterface);
                $log->tracef("[%d] %s: <%s> = %s (match on: %s)", $eslifRecognizerInterface->recurseLevel, $eslifGrammar->currentDescription, 'identifier or keyword', $identifier_or_keyword, $identifier_or_keyword_match);
                $identifier_or_keyword_done = 1;
            }
            if (defined($identifier_or_keyword)) {
                if ($identifier_or_keyword ne 'true' && $identifier_or_keyword ne 'false') {
                    $match = $identifier_or_keyword_match;
                    $value = $identifier_or_keyword;
                    $name = 'ANY IDENTIFIER OR KEYWORD EXCEPT TRUE OR FALSE';
                    $self->{last_conditional_symbol} = $match
                }
            }
        }
        elsif ($event eq '^pp_expression') {
            my ($pp_expression, $pp_expression_match) = $self->_pp_expression($eslifRecognizer, $eslifRecognizerInterface);
            $log->tracef("[%d] %s: <%s> = %s (match on: %s)", $eslifRecognizerInterface->recurseLevel, $eslifGrammar->currentDescription, 'pp expression', $pp_expression, $pp_expression_match);
            #
            # Remember the last expression, and send a zero-length token into <PP EXPRESSION>
            # because _pp_expression() shared to stream with our recognizer: our position is already
            # advanced.
            #
            $self->{last_pp_expression} = $pp_expression;
            $match = '';
            $value = $pp_expression;
            $name = 'PP EXPRESSION'
        }
        elsif ($event eq '^pp_line_indicator') {
            $self->{line_directive_filename} = undef;
            $self->{line_directive_line} = undef;
        }
        else {
	    $self->_internal_exception($eslifGrammar, $eslifRecognizerInterface, undef, '[%2d] Unsupported event %s', $eslifRecognizerInterface->recurseLevel, $event)
        }

	if (defined($match)) {
            my $length = bytes::length($match);
            if ($latm < 0) {
                #
                # First alternative
                #
                $latm = $length;
                push(@alternatives, { match => $match, name => $name, value => $value });
                $log->tracef("[%d] %s: Pushed first alternative: %s", $eslifRecognizerInterface->recurseLevel, $eslifGrammar->currentDescription, $alternatives[-1]);
            } else {
                if ($length == $latm) {
                    #
                    # Alternative of same length
                    #
                    push(@alternatives, { match => $match, name => $name, value => $value });
                    $log->tracef("[%d] %s: Pushed alternative: %s", $eslifRecognizerInterface->recurseLevel, $eslifGrammar->currentDescription, $alternatives[-1]);
                } elsif ($length > $latm) {
                    #
                    # Alternative of bigger length
                    #
                    @alternatives = ();
                    $latm = $length;
                    push(@alternatives, { match => $match, name => $name, value => $value });
                    $log->tracef("[%d] %s: Resetted alternatives to: %s", $eslifRecognizerInterface->recurseLevel, $eslifGrammar->currentDescription, $alternatives[-1]);
                }
            }
        }
    }

    #
    # Always process pp_line$ and NEW_LINE$ after any event,
    # if there is an #error or #warning event
    #
    if ($have_diagnostics) {
        if ($have_pp_line_completion) {
            #
            # Commit <pp line> indicator
            #
            $self->{filename} = $self->{line_directive_filename};
            if ($self->{line_directive_line} eq 'default') {
                $self->{line_hidden} = 0;
            } elsif ($self->{line_directive_line} eq 'hidden') {
                $self->{line_hidden} = 1;
            } else {
                $self->{line} = $self->{line_directive_line};
            }
        } elsif ($have_NEW_LINE_completion) {
            $self->{line}++;
            $self->{_line}++;
        }
    }

    if ($latm >= 0) {
        map {
            $log->tracef("[%d] %s: Alternative: <%s> = %s", $eslifRecognizerInterface->recurseLevel, $eslifGrammar->currentDescription, $_->{name}, $_->{value});
            $self->_internal_exception($eslifGrammar, $eslifRecognizerInterface, $eslifRecognizer, '%s alternative failure', $_->{name}) unless $eslifRecognizer->lexemeAlternative($_->{name}, $_->{value})
        } @alternatives;
        $log->tracef("[%d] %s: Lexeme complete on %d bytes", $eslifRecognizerInterface->recurseLevel, $eslifGrammar->currentDescription, $latm);
        $self->_internal_exception($eslifGrammar, $eslifRecognizerInterface, $eslifRecognizer, 'lexeme complete failure') unless $eslifRecognizer->lexemeComplete($latm);
        $rc = 1
    }

    return $rc;
}

# ============================================================================
# _pushElement
# ============================================================================
sub _pushElement {
    my ($self, $eslifRecognizerInterface, $eslifRecognizer, $is_token, $name, $value, $forcedName) = @_;

    my $element = {
        name        => $forcedName // $name,
        is_token    => $is_token,
        filename    => $self->{filename},
        line_hidden => $self->{line_hidden},
        line_end    => $self->{line},
        _line_end   => $self->{_line},
        column_end  => $eslifRecognizer->column
    };
    if ($name eq ':discard') {
        #
        # Special case for :discard
        #
        my $discard = $eslifRecognizer->discardLast();
        my $discardBytesLength = bytes::length($discard);
        my $currentOffset = bytes::length($eslifRecognizerInterface->data) - bytes::length($eslifRecognizer->input);
        $element->{offset} = $currentOffset - $discardBytesLength;
        $element->{bytes_length} = $discardBytesLength;
        $element->{match} = $element->{value} = $discard;
    } else {
        ($element->{offset}, $element->{bytes_length}) = $eslifRecognizer->lastCompletedLocation($name);
        $element->{match} = bytes::substr($eslifRecognizerInterface->data, $element->{offset}, $element->{bytes_length});
        utf8::decode($element->{match});
        $element->{value} = $value // $element->{match}
    }
    $element->{length} = length($element->{string});

    push(@{$self->{elements}}, $element)
}

=head1 NOTES

This module is a L<Log::Any> consumer.

=cut

1;

__DATA__
__[ pre lexical grammar ]__
# ############################################################################################################
# Pre Lexical Grammar
# ############################################################################################################
:default                                         ::= action => ::convert[UTF-8]
:desc                                            ::= 'Pre lexical grammar'

<input>                                          ::= /./su *

__[ lexical grammar ]__
# ############################################################################################################
# Lexical Grammar
# ############################################################################################################
#
# Formally we do NOT need any value from lexical grammar, though we still execute the valuation
# to check if input is correct (in particular v.s. exhaustion)
#
:default                                         ::= action => ::undef symbol-action => ::undef
:desc                                            ::= 'Lexical grammar'
:discard                                         ::= <comment> event => discard$

#
# Note that the spec does NOT say if file-name characters disable comments. We assume they do so.
#
:terminal ::= "'"  pause => after event => :discard[switch]           # Disable comment in character literal
:terminal ::= '"'  pause => after event => :discard[switch]           # Disable comment in regular string literal
:terminal ::= '@"' pause => after event => :discard[switch]           # Disable comment in verbatim string literal (Note that it ends with '"' character)

<input>                                          ::= <input section opt>
<input section opt>                              ::=
<input section opt>                              ::= <input section>
<input section>                                  ::= <input section part>+
<input section part>                             ::= <input elements opt> <new line>
                                                    | <pp directive>
<input elements opt>                             ::=
<input elements opt>                             ::= <input elements>
<input elements>                                 ::= <input element>+
#
# We add a zero-length lexeme element everytime a <token> rule matches
#
<input element>                                  ::= <whitespace>
                                                   | <token>
event whitespace$ = completed <whitespace>
event token$ = completed <token>

:lexeme ::= <NEW LINE> pause => after event => NEW_LINE$           # Increments current line number
<new line>                                       ::= <NEW LINE>

<whitespace>                                     ::= /[\p{Zs}\x{0009}\x{000B}\x{000C}]+/u

<comment>                                        ::= <single line comment>
                                                   | <delimited comment>
<single line comment>                            ::= '//' <input characters opt>
<input characters opt>                           ::=
<input characters opt>                           ::= <input characters>
<input characters>                               ::= <input character>+
<input character>                                ::= /[^\x{000D}\x{000A}\x{0085}\x{2028}\x{2029}]/u   # <ANY UNICODE CHARACTER EXCEPT A NEW LINE CHARACTER>
<new line character>                             ::= /[\x{000D}\x{000A}\x{0085}\x{2028}\x{2029}]/u

<delimited comment>                              ::= '/*' <delimited comment text opt> <asterisks> '/'
<delimited comment text opt>                     ::=
<delimited comment text opt>                     ::= <delimited comment text>
<delimited comment text>                         ::= <delimited comment section>+
<delimited comment section>                      ::= '/'
                                                   | <asterisks opt> <not slash or asterisk>
<asterisks opt>                                  ::=
<asterisks opt>                                  ::= <asterisks>
<asterisks>                                      ::= '*'+
<not slash or asterisk>                          ::= /[^\/*]/u # Any Unicode character except / or *

event identifier$ = completed <identifier>
event keyword$ = completed <keyword>
event integer_literal$ = completed <integer literal>
event real_literal$ = completed <real literal>
event character_literal$ = completed <character literal>
event string_literal$ = completed <string literal>
event operator_or_punctuator$ = completed <operator or punctuator>
<token>                                          ::= <identifier>
                                                   | <keyword>
                                                   | <integer literal>
                                                   | <real literal>
                                                   | <character literal>
                                                   | <string literal>
                                                   | <operator or punctuator>

<unicode escape sequence>                        ::= '\\u' <hex digit> <hex digit> <hex digit> <hex digit>
                                                   | '\\U' <hex digit> <hex digit> <hex digit> <hex digit> <hex digit> <hex digit> <hex digit> <hex digit>
<identifier>                                     ::= <available identifier>
                                                   | '@' <identifier or keyword>

event ^identifier_or_keyword = predicted <identifier or keyword>
<identifier or keyword>                          ::= <IDENTIFIER OR KEYWORD>

event ^available_identifier = predicted <available identifier>
<available identifier>                           ::= <AN IDENTIFIER OR KEYWORD THAT IS NOT A KEYWORD>

event ^keyword = predicted <keyword>
<keyword>                                        ::= <KEYWORD>

<contextual keyword>                             ::= 'add'
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

<literal>                                        ::= <boolean literal>
                                                   | <integer literal>
                                                   | <real literal>
                                                   | <character literal>
                                                   | <string literal>
                                                   | <null literal>

<boolean literal>                                ::= 'true'
                                                   | 'false'

<integer literal>                                ::= <decimal integer literal>
                                                   | <hexadecimal integer literal>

<decimal integer literal>                        ::= <decimal digits> <integer type suffix opt>
<integer type suffix opt>                        ::=
<integer type suffix opt>                        ::= <integer type suffix>
<decimal digits>                                 ::= /[0-9]+/
<integer type suffix>                            ::= /U|L|UL/i
<hexadecimal integer literal>                    ::= '0x':i <hex digits> <integer type suffix opt>
<hex digits>                                     ::= <hex digit>+
<hex digit>                                      ::= /[0-9A-Fa-f]/

<real literal>                                   ::= <decimal digits> '.' <decimal digits> <exponent part opt> <real type suffix opt>       
                                                   |                  '.' <decimal digits> <exponent part opt> <real type suffix opt>
                                                   |                      <decimal digits> <exponent part>     <real type suffix opt>
                                                   |                      <decimal digits>                     <real type suffix>
<exponent part>                                  ::= 'E':i <sign opt> <decimal digits>
<sign opt>                                       ::=
<sign opt>                                       ::= <sign>
<sign>                                           ::= /[+-]/

<exponent part opt>                              ::=
<exponent part opt>                              ::= <exponent part>
<real type suffix opt>                           ::=
<real type suffix opt>                           ::= <real type suffix>

<real type suffix>                               ::= /[FDM]/i

<character literal>                              ::= "'" <character> "'"
<character>                                      ::= <single character>
                                                   | <simple escape sequence>
                                                   | <hexadecimal escape sequence>
                                                   | <unicode escape sequence>
#
# If the value represented by a character literal is greater than U+FFFF , a compile-time error occurs.
#
:lexeme ::= <SINGLE CHARACTER> if-action => single_character_is_below_0xFFFF
<single character>                               ::= <SINGLE CHARACTER>

<simple escape sequence>                         ::= "\\'"
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
<hexadecimal escape sequence>                    ::= '\\x' <hex digit> <hex digit opt> <hex digit opt> <hex digit opt>
<hex digit opt>                                  ::=
<hex digit opt>                                  ::= <hex digit>

<string literal>                                 ::= <regular string literal>
                                                   | <verbatim string literal>
<regular string literal>                         ::= '"' <regular string literal characters opt> '"'
<regular string literal characters opt>          ::=
<regular string literal characters opt>          ::= <regular string literal characters>
<regular string literal characters>              ::= <regular string literal character>+
<regular string literal character>               ::= <single regular string literal character>
                                                   | <simple escape sequence>
                                                   | <hexadecimal escape sequence>
                                                   | <unicode escape sequence>
:lexeme ::= <SINGLE REGULAR STRING LITERAL CHARACTER> if-action => single_regular_string_literal_character_is_below_0xFFFF
<single regular string literal character>        ::= <SINGLE REGULAR STRING LITERAL CHARACTER>

<verbatim string literal>                        ::= '@"' <verbatim string literal characters opt> '"'
<verbatim string literal characters opt>         ::=
<verbatim string literal characters opt>         ::= <verbatim string literal characters>
<verbatim string literal characters>             ::= <verbatim string literal character>+
<verbatim string literal character>              ::= <single verbatim string literal character>
                                                   | <quote escape sequence>
<single verbatim string literal character>       ::= /[^"]/u # Any character except "
<quote escape sequence>                          ::= '""'

<null literal>                                   ::= 'null'

<operator or punctuator>                         ::= '{'
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
<right shift>                                    ::= '>' '>'
<right shift assignment>                         ::= '>' '>='

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
# => :discard is switched off when a pp keyword is found (#if, #endif, etc...)
# => :discard is switch on at the end of <pp new line> and <pp message> (because <pp diagnostic> ends with <new line> instead of <pp new line>)
# => :discard is switch off explicitly again if we enter a skipped section

#
# Inside a <pp directive>, discard is always switched off
# Note that in case of an "ok" <conditional section>, discard
# is explicitly reswitched on, and this happens before the end of
# <pp if>, <pp elif> or <pp else> directives
#
:lexeme ::= <PP DEFINE> pause => after event => :discard[off]
:lexeme ::= <PP UNDEF> pause => after event => :discard[off]
:lexeme ::= <PP IF> pause => after event => :discard[off]
:lexeme ::= <PP ELIF> pause => after event => :discard[off]
:lexeme ::= <PP ELSE> pause => after event => :discard[off]
:lexeme ::= <PP ENDIF> pause => after event => :discard[off]
:lexeme ::= <PP LINE> pause => after event => :discard[off]
:lexeme ::= <PP ERROR> pause => after event => :discard[off]
:lexeme ::= <PP WARNING> pause => after event => :discard[off]
:lexeme ::= <PP REGION> pause => after event => :discard[off]
:lexeme ::= <PP ENDREGION> pause => after event => :discard[off]
:lexeme ::= <PP PRAGMA> pause => after event => :discard[off]
#
# Whatever happened, discard is always switched on at the end of <pp new line>
#
event :discard[on] = completed <pp new line>
event :discard[on] = completed <pp diagnostic>

<pp directive>                                   ::= <pp declaration>
                                                   | <pp conditional>
                                                   | <pp line>
                                                   | <pp diagnostic>
                                                   | <pp region>
                                                   | <pp pragma>

event ^conditional_symbol = predicted <conditional symbol>
<conditional symbol>                             ::= <ANY IDENTIFIER OR KEYWORD EXCEPT TRUE OR FALSE>
<whitespace opt>                                 ::=
<whitespace opt>                                 ::= <whitespace>
<pp declaration define>                          ::= <PP DEFINE> <whitespace> <conditional symbol> <pp new line>
<pp declaration undef>                           ::= <PP UNDEF> <whitespace> <conditional symbol> <pp new line>
<pp declaration>                                 ::= <pp declaration define>
                                                   | <pp declaration undef>
:lexeme ::= <PP DEFINE> pause => after event => PP_DEFINE$
:lexeme ::= <PP UNDEF> pause => after event => PP_UNDEF$

event pp_declaration_define$ = completed <pp declaration define>
event pp_declaration_undef$ = completed <pp declaration undef>
<pp new line>                                    ::= <whitespace opt> <single line comment opt> <new line>
<single line comment opt>                        ::=
<single line comment opt>                        ::= <single line comment>

event ^pp_expression = predicted <pp expression>
<pp expression>                                  ::= <PP EXPRESSION>
<pp conditional>                                 ::= <pp if section> <pp elif sections opt> <pp else section opt> <pp endif>
<pp elif sections opt>                           ::=
<pp elif sections opt>                           ::= <pp elif sections>
<pp else section opt>                            ::=
<pp else section opt>                            ::= <pp else section>
#
# Because we introduce the internal lexemes <CONDITIONAL SECTION OK> and <CONDITIONAL SECTION KO> in <conditional section>,
# the later is never optional. Its content always contains <CONDITIONAL SECTION OK> or <CONDITIONAL SECTION KO>, and it is
# what follows that is optional.
#

event pp_if_context[] = nulled <pp if context>
<pp if context>                                  ::=
<pp if section>                                  ::= <PP IF> <whitespace> <pp expression> <pp new line> <pp if context> <conditional section>

<pp elif sections>                               ::= <pp elif section>+
event pp_elif_context[] = nulled <pp elif context>
<pp elif context>                                ::=
<pp elif section>                                ::= <PP ELIF> <whitespace> <pp expression> <pp new line> <pp elif context> <conditional section>

event pp_else_context[] = nulled <pp else context>
<pp else context>                                ::=
<pp else section>                                ::= <PP ELSE> <pp new line> <pp else context> <conditional section>

event pp_endif_context[] = nulled <pp endif context>
<pp endif context>                               ::=
<pp endif>                                       ::= <PP ENDIF> <pp endif context> <pp new line>

<conditional section>                            ::= <CONDITIONAL SECTION OK> <input section opt>
                                                   | <CONDITIONAL SECTION KO> <skipped section opt>

<skipped section opt>                            ::= <skipped section>
<skipped section opt>                            ::=
event :discard[off] = predicted <skipped section>
<skipped section>                                ::= <skipped section part>+
<skipped section part>                           ::= <skipped characters opt> <new line>
                                                   | <pp directive>
<skipped characters opt>                         ::=
<skipped characters opt>                         ::= <skipped characters>
<skipped characters>                             ::= <whitespace opt> <not number sign> <input characters opt>
<not number sign>                                ::= /[^\x{000D}\x{000A}\x{0085}\x{2028}\x{2029}#]/u # Any <input character> except #

<pp diagnostic>                                  ::= <PP ERROR> <pp message>   <trigger pp error>
                                                   | <PP WARNING> <pp message> <trigger pp warning>
<pp message>                                     ::=                                 <new line> <last pp message empty>
                                                   | <whitespace>                    <new line> <last pp message empty>
                                                   | <whitespace> <input characters> <new line> <last pp message not empty>

event last_pp_message_empty[] = nulled <last pp message empty>
event last_pp_message_not_empty[] = nulled <last pp message not empty>
<last pp message empty>                          ::=
<last pp message not empty>                      ::=
event ^trigger_pp_error = predicted <trigger pp error>
event ^trigger_pp_warning = predicted <trigger pp warning>
<trigger pp error>                               ::= <TRIGGER PP ERROR>
<trigger pp warning>                             ::= <TRIGGER PP WARNING>

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
<pp region>                                      ::= <pp start region> <input section opt> <pp end region>
<pp start region>                                ::= <PP REGION> <pp message>
# WAS: <pp end region> ::= <whitespace opt> '#' <whitespace opt> 'endregion' <pp message>
<pp end region>                                  ::= <PP ENDREGION> <pp endregion message>
<pp endregion message>                           ::= <pp new line>
                                                   | <whitespace> <input characters opt> <pp new line>
#
# line and filename indicators are commited after the <pp line> directive, so we need
# to catch it. We use pp_line$ completion but take care, this will happen at the same
# time as ^token. This is why it is a separate thingy in lexemeEventManager.
#
event pp_line$ = completed <pp line>
<pp line>                                        ::= <PP LINE> <whitespace> <line indicator> <pp new line>
#
# We create <line decimal digits> to distinguish it from <decimal digits>
# We use event ^pp_line_indicator to reset line_directive_filename and line_directive_decimal_digits
#
<line decimal digits> ::= <decimal digits>
event pp_line_decimal_digits$ = completed <line decimal digits>
event pp_line_file_name$ = completed <file name>
:lexeme ::= <LINE INDICATOR DEFAULT>  pause => after event => LINE_INDICATOR_DEFAULT$
:lexeme ::= <LINE INDICATOR HIDDEN>  pause => after event => LINE_INDICATOR_HIDDEN$
event ^pp_line_indicator = predicted <line indicator>
<line indicator>                                 ::= <line decimal digits> <whitespace> <file name>
                                                   | <line decimal digits>
                                                   | <LINE INDICATOR DEFAULT>
                                                   | <LINE INDICATOR HIDDEN>
<file name>                                      ::= '"' <file name characters> '"'
<file name characters>                           ::= <file name character>+
<file name character>                            ::= /[^\x{0022}\x{000D}\x{000A}\x{0085}\x{2028}\x{2029}]/u   # <ANY INPUT CHARACTER EXCEPT 0022 AND NEW LINE CHARACTER>

event pp_pragma_with_text[] = nulled <pp pragma with text>
<pp pragma>                                      ::= <PP PRAGMA> <pp pragma text>
<pp pragma text>                                 ::= <new line>
                                                   | <whitespace> <input characters> <new line> <pp pragma with text>
                                                   | <whitespace> <new line>
<pp pragma with text>                            ::=

#
# Lexemes
#
<LINE INDICATOR DEFAULT>                           ~ 'default'
<LINE INDICATOR HIDDEN>                            ~ 'hidden'
<NEW LINE>                                         ~ /(?:\x{000D}\x{000A}|\x{000D}|\x{000A}|\x{0085}|\x{2028}|\x{2029})/u
<SINGLE CHARACTER>                                 ~ /[^\x{0027}\x{005C}\x{000D}\x{000A}\x{0085}\x{2028}\x{2029}]/u   # <ANY CHARACTER EXCEPT 0027 005C AND NEW LINE CHARACTER>
<SINGLE REGULAR STRING LITERAL CHARACTER>          ~ /[^\x{0022}\x{005C}\x{000D}\x{000A}\x{0085}\x{2028}\x{2029}]/u   # <ANY CHARACTER EXCEPT 0022 005C AND NEW LINE CHARACTER>
<KEYWORD>                                          ~ /[^\s\S]/ # Matches nothing
<IDENTIFIER OR KEYWORD>                            ~ /[^\s\S]/ # Matches nothing
<ANY IDENTIFIER OR KEYWORD EXCEPT TRUE OR FALSE>   ~ /[^\s\S]/ # Matches nothing
<AN IDENTIFIER OR KEYWORD THAT IS NOT A KEYWORD>   ~ /[^\s\S]/ # Matches nothing
<PP DEFINE>                                        ~ /[\p{Zs}\x{0009}\x{000B}\x{000C}]*#[\p{Zs}\x{0009}\x{000B}\x{000C}]*define/u
<PP UNDEF>                                         ~ /[\p{Zs}\x{0009}\x{000B}\x{000C}]*#[\p{Zs}\x{0009}\x{000B}\x{000C}]*undef/u
<PP IF>                                            ~ /[\p{Zs}\x{0009}\x{000B}\x{000C}]*#[\p{Zs}\x{0009}\x{000B}\x{000C}]*if/u
<PP ELIF>                                          ~ /[\p{Zs}\x{0009}\x{000B}\x{000C}]*#[\p{Zs}\x{0009}\x{000B}\x{000C}]*elif/u
<PP ELSE>                                          ~ /[\p{Zs}\x{0009}\x{000B}\x{000C}]*#[\p{Zs}\x{0009}\x{000B}\x{000C}]*else/u
<PP ENDIF>                                         ~ /[\p{Zs}\x{0009}\x{000B}\x{000C}]*#[\p{Zs}\x{0009}\x{000B}\x{000C}]*endif/u
<PP LINE>                                          ~ /[\p{Zs}\x{0009}\x{000B}\x{000C}]*#[\p{Zs}\x{0009}\x{000B}\x{000C}]*line/u
<PP ERROR>                                         ~ /[\p{Zs}\x{0009}\x{000B}\x{000C}]*#[\p{Zs}\x{0009}\x{000B}\x{000C}]*error/u
<PP WARNING>                                       ~ /[\p{Zs}\x{0009}\x{000B}\x{000C}]*#[\p{Zs}\x{0009}\x{000B}\x{000C}]*warning/u
<PP REGION>                                        ~ /[\p{Zs}\x{0009}\x{000B}\x{000C}]*#[\p{Zs}\x{0009}\x{000B}\x{000C}]*region/u
<PP ENDREGION>                                     ~ /[\p{Zs}\x{0009}\x{000B}\x{000C}]*#[\p{Zs}\x{0009}\x{000B}\x{000C}]*endregion/u
<PP PRAGMA>                                        ~ /[\p{Zs}\x{0009}\x{000B}\x{000C}]*#[\p{Zs}\x{0009}\x{000B}\x{000C}]*pragma/u
<CONDITIONAL SECTION OK>                           ~ /[^\s\S]/ # Matches nothing
<CONDITIONAL SECTION KO>                           ~ /[^\s\S]/ # Matches nothing
<PP EXPRESSION>                                    ~ /[^\s\S]/ # Matches nothing
<TRIGGER PP ERROR>                                 ~ /[^\s\S]/ # Matches nothing
<TRIGGER PP WARNING>                               ~ /[^\s\S]/ # Matches nothing

__[ identifier or keyword grammar ]__
# ############################################################################################################
# Identifier Or Keyword Grammar
# ############################################################################################################
:default                                         ::= action => ::convert[UTF-8]
:desc                                            ::= 'Identifier or Keyword grammar'

event identifier_or_keyword$ = completed <identifier or keyword>

<identifier or keyword>                          ::= <identifier start character> <identifier part characters opt>
<identifier part characters opt>                 ::=
<identifier part characters opt>                 ::= <identifier part characters>
<identifier start character>                     ::= <letter character>
                                                   | <underscore character>
<underscore character>                           ::= '_' # The underscore character U+005F
                                                   | <A UNICODE ESCAPE SEQUENCE REPRESENTING THE CHARACTER 005F> action => ::u8"_"
<identifier part characters>                     ::= <identifier part character>+
<identifier part character>                      ::= <letter character>
                                                   | <decimal digit character>
                                                   | <connecting character>
                                                   | <combining character>
                                                   | <formatting character>
<letter character>                               ::= /[\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}\p{Nl}]/u
                                                   | <A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Lu Ll Lt Lm Lo or Nl> action => unicode_unescape_sequence
<combining character>                            ::= /[\p{Mn}\p{Mc}]/u
                                                   | <A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Mn or Mc>             action => unicode_unescape_sequence
<decimal digit character>                        ::= /[\p{Nd}]/u
                                                   | <A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Nd>                   action => unicode_unescape_sequence
<connecting character>                           ::= /[\p{Pc}]/u
                                                   | <A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Pc>                   action => unicode_unescape_sequence
<formatting character>                           ::= /[\p{Cf}]/u
                                                   | <A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Cf>                   action => unicode_unescape_sequence

<UNICODE ESCAPE SEQUENCE>                     :[2]:= /\\u[0-9A-Fa-f]{4}/
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
# ############################################################################################################
# Keyword Grammar
# ############################################################################################################
:default  ::= action => ::convert[UTF-8]
:desc     ::= 'Keyword grammar'

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
# ############################################################################################################
# Pp Expression Grammar
# ############################################################################################################
:desc                                            ::= 'Pre-processing expression grammar'
:default                                         ::= action => ::shift
:start                                           ::= <pp expression>

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
