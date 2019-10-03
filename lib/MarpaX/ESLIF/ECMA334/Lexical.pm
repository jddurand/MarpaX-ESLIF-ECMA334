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
use MarpaX::ESLIF 3.0.15; # if-action

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
    # 'TOKEN MARKER'                                     => /[^\s\S]/ # Matches nothing
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
             last_pp_expression => undef,
             last_conditional_symbol => undef,
             can_next_conditional_section => [],
             has_token => 0,
             line_hidden => 0,                        # Current line information is hidden or not (for debuggers)
             line => 1,                               # Current line with respect of #line directives
             _line => 1,                              # Current line without respect of #line directives
             line_directive_filename => undef,        # file name in #line directive, but before the end of #line directive
             line_directive_line => undef,            # line number in #line directive, but before the end of #line directive
             filename => undef,                       # Current file name with respect of #line directives
             last_token_type => undef,                # Last token type
             token_value => {
                 filename => undef,                   # filename as per #line directive
                 line_hidden => undef,                # Token line information is hidden (for debuggers)
                 line_start => undef,                 # Token start line with respect of #line directives
                 _line_start => undef,                # Token start line without respect of #line directives
                 line_end => undef,                   # Token end line with respect of #line directives
                 _line_end => undef,                  # Token end line without respect of #line directives
                 column_start => undef,               # Token start column
                 column_end => undef,                 # Token end column
                 offset => undef,                     # Token start offset in bytes v.s. input data
                 bytes_length => undef,               # Token byte length
                 length => undef,                     # Token character length
                 string => undef,                     # Token content
                 type => undef                        # Token type
             }
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
    return $self->_parse
        (
         $LEXICAL_GRAMMAR,
         'MarpaX::ESLIF::ECMA334::Lexical::RecognizerInterface',
         'MarpaX::ESLIF::ECMA334::Lexical::ValueInterface',
         \&_lexicalEventManager,
         undef, # sharedEslifRecognizer
         %options,
         input => $self->_preparse(%options),
         encoding => 'UTF-8'
        )
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
        $PRE_LEXICAL_GRAMMAR->parse($eslifRecognizerInterface, $eslifValueInterface) || $self->_exception(undef, 'Pre-parse phase failure');
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
        $output .= "\x{000D}" if (bytes::length($output) > 0 && $output !~ /[\x{000D}\x{000A}\x{0085}\x{2028}\x{2029}]$/);
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
        $eslifRecognizer->read || $self->_exception($eslifRecognizer, 'Initial read failed')
    }

    # -----------------------------------------------------
    # Run recognizer manually so that events are accessible
    # -----------------------------------------------------
    $eslifRecognizer->scan(1) || $self->_exception($eslifRecognizer, 'Initial scan failed');

    if ($eslifRecognizer->isCanContinue) {
        {
            do {
                # ------------------------------------------------------------
                # Event loop can do a lexeme complete that triggers new events
                # ------------------------------------------------------------
                while ($self->$eventManager($eslifRecognizer, $eslifRecognizerInterface)) {
                }
                if (! $eslifRecognizer->resume) {
                    #
                    # This is a failure unless it is a sub-grammar that has reached completion at least once
                    #
                    if ($eslifRecognizerInterface->hasCompletion && $eslifRecognizerInterface->recurseLevel) {
                        last
                    } else {
                        $self->_exception($eslifRecognizer, 'resume() failed')
                    }
                }
            } while ($eslifRecognizer->isCanContinue)
        }
    } else {
        # ------------------------------------------------------------
        # Event loop can do a lexeme complete that triggers new events
        # ------------------------------------------------------------
        while ($self->$eventManager($eslifRecognizer, $eslifRecognizerInterface)) {
        }
    }

    # -----------------------------------------------------------------------------------------
    # Call for valuation (we configured value interface to not accept ambiguity nor null parse)
    # -----------------------------------------------------------------------------------------
    $self->_exception(undef, 'Valuation failure') unless MarpaX::ESLIF::Value->new($eslifRecognizer, $eslifValueInterface)->value();

    # ------------------------
    # Return the value
    # ------------------------
    return $eslifValueInterface->getResult
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
    my $self = shift;

    $self->_error(0, @_) # Not an internal error
}

# ============================================================================
# _internal_exception
# ============================================================================
sub _internal_exception {
    my $self = shift;

    $self->_error(1, @_) # Is an internal error
}

# ============================================================================
# _error
# ============================================================================
sub _error {
    my ($self, $isInternal, $eslifRecognizer, $fmt, @args) = @_;

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

    my @exception_args = (
        error => sprintf($fmt, @args),
        file => $self->{filename},
        line => $self->{line},
        _line => $_line,
        column => $column,
        expected => $expected
        );
    if ($isInternal) {
        throw_internal_exception @exception_args
    } else {
        throw_exception @exception_args
    }
}

# ============================================================================
# _lexicalEventManager
# ============================================================================
#
# The event manager must always return a true value if it has performed a lexeme complete
#
sub _lexicalEventManager {
    my ($self, $eslifRecognizer, $eslifRecognizerInterface) = @_;

    my $rc = 0;

    my @events = grep { defined } map { $_->{event} } @{$eslifRecognizer->events};

    #
    # At any predicted event, we have two possible sub-grammars
    #
    my @alternatives;
    my $latm = -1;

    #
    # Always process pp_line$ and NEW_LINE$ before any event
    #
    my $have_pp_line_completion = grep {$_ eq 'pp_line$'} @events;
    my $have_NEW_LINE_completion = grep {$_ eq 'NEW_LINE$'} @events;
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

    foreach my $event (@events) {
        my ($identifier_or_keyword, $keyword, $match, $name, $value, $pp_expression) = (undef, undef, undef, undef, undef, undef);

        if ($event eq "'exhausted'") {
            $eslifRecognizerInterface->hasCompletion(1)
        }
        elsif ($event eq 'pp_expression$') {
            $eslifRecognizerInterface->hasCompletion(1)
        }
        elsif ($event eq 'identifier_or_keyword$') {
            $eslifRecognizerInterface->hasCompletion(1)
        }
        elsif ($event eq 'identifier$') {
            $self->{last_token_type} = 'identifier'
        }
        elsif ($event eq 'keyword$') {
            $self->{last_token_type} = 'keyword'
        }
        elsif ($event eq 'integer_literal$') {
            $self->{last_token_type} = 'integer literal'
        }
        elsif ($event eq 'real_literal$') {
            $self->{last_token_type} = 'real literal'
        }
        elsif ($event eq 'character_literal$') {
            $self->{last_token_type} = 'character literal'
        }
        elsif ($event eq 'string_literal$') {
            $self->{last_token_type} = 'string literal'
        }
        elsif ($event eq 'operator_or_punctuator$') {
            $self->{last_token_type} = 'operator or punctuator'
        }
        elsif ($event eq 'token$') {
            $self->{has_token} = 1 if ! $eslifRecognizerInterface->recurseLevel;
            #
            # We inject a <TOKEN MARKER> that we will use for the ast
            #
            $self->{token_value}->{filename} = $self->{filename};
            $self->{token_value}->{line_hidden} = $self->{line_hidden};
            $self->{token_value}->{line_end} = $self->{line};
            $self->{token_value}->{_line_end} = $self->{_line};
            $self->{token_value}->{column_end} = $eslifRecognizer->column - 1;
            ($self->{token_value}->{offset}, $self->{token_value}->{bytes_length}) = $eslifRecognizer->lastCompletedLocation('token');
            $self->{token_value}->{string} = bytes::substr($eslifRecognizerInterface->data, $self->{token_value}->{offset}, $self->{token_value}->{bytes_length});
            utf8::decode($self->{token_value}->{string});
            $self->{token_value}->{length} = length($self->{token_value}->{string});
            $self->{token_value}->{type} = $self->{last_token_type};
            $match = '';
            $value = $self->{token_value};
            $name = 'TOKEN MARKER';
        }
        elsif ($event eq 'pp_declaration_define$') {
            $eslifRecognizerInterface->definitions->{$self->{last_conditional_symbol}} = $MarpaX::ESLIF::true
        }
        elsif ($event eq 'pp_declaration_undef$') {
            $eslifRecognizerInterface->definitions->{$self->{last_conditional_symbol}} = $MarpaX::ESLIF::false
        }
        elsif ($event eq 'PP_DEFINE$') {
            $self->_exception($eslifRecognizer, 'A #define declaration must appear before any token') if $self->{has_token};
        }
        elsif ($event eq 'PP_UNDEF$') {
            $self->_exception($eslifRecognizer, 'A #define declaration must appear before any token') if $self->{has_token};
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
            my $can_next_conditional_section = $self->{can_next_conditional_section}->[-1] // $self->_exception($eslifRecognizer, 'Unbalanced #if/#elif');
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
            my $can_next_conditional_section = $self->{can_next_conditional_section}->[-1] // $self->_exception($eslifRecognizer, 'Unbalanced #if/#elif');
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
            $identifier_or_keyword //= $self->_identifier_or_keyword($eslifRecognizer, $eslifRecognizerInterface);
            if (defined($identifier_or_keyword)) {
                $keyword //= $self->_keyword($eslifRecognizer, $eslifRecognizerInterface);
                if (! defined($keyword)) {
                    $match = $identifier_or_keyword;
                    $value = $identifier_or_keyword;
                    $name = 'AN IDENTIFIER OR KEYWORD THAT IS NOT A KEYWORD'
                }
            }
        }
        elsif ($event eq '^token') {
            $self->{token_value}->{line_start} = $self->{line};
            $self->{token_value}->{_line_start} = $self->{_line};
            $self->{token_value}->{column_start} = $eslifRecognizer->column;
        }
        elsif ($event eq '^keyword') {
            $keyword //= $self->_keyword($eslifRecognizer, $eslifRecognizerInterface);
            if (defined($keyword)) {
                $match = $keyword;
                $value = $keyword;
                $name = 'KEYWORD'
            }
        }
        elsif ($event eq '^identifier_or_keyword') {
            $identifier_or_keyword //= $self->_identifier_or_keyword($eslifRecognizer, $eslifRecognizerInterface);
            if (defined($identifier_or_keyword)) {
                $match = $identifier_or_keyword;
                $value = $identifier_or_keyword;
                $name = 'IDENTIFIER OR KEYWORD'
            }
        }
        elsif ($event eq '^conditional_symbol') {
            $identifier_or_keyword //= $self->_identifier_or_keyword($eslifRecognizer, $eslifRecognizerInterface);
            if (defined($identifier_or_keyword)) {
                if ($identifier_or_keyword ne 'true' && $identifier_or_keyword ne 'false') {
                    $match = $identifier_or_keyword;
                    $value = $identifier_or_keyword;
                    $name = 'ANY IDENTIFIER OR KEYWORD EXCEPT TRUE OR FALSE';
                    $self->{last_conditional_symbol} = $match
                }
            }
        }
        elsif ($event eq '^pp_expression') {
            $pp_expression //= $self->_pp_expression($eslifRecognizer, $eslifRecognizerInterface);
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
	    $self->_internal_exception(undef, '[%2d] Unsupported event %s', $eslifRecognizerInterface->recurseLevel, $event)
        }

	if (defined($match)) {
            my $length = bytes::length($match);
            if ($length >= $latm) {
                if ($length > $latm) {
                    @alternatives = ();
                    $latm = $length;
                }
                push(@alternatives, { match => $match, name => $name, value => $value })
            }
        }
    }

    if ($latm >= 0) {
        map {
            $self->_internal_exception($eslifRecognizer, '%s alternative failure', $_->{name}) unless $eslifRecognizer->lexemeAlternative($_->{name}, $_->{value})
        } @alternatives;
        $self->_internal_exception($eslifRecognizer, 'lexeme complete failure') unless $eslifRecognizer->lexemeComplete($latm);
        $rc = 1
    }

    return $rc;
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
:default                                         ::= action => ::convert[UTF-8] symbol-action => ::convert[UTF-8]
:desc                                            ::= 'Lexical grammar'
:discard                                         ::= <comment>

#
# Note that the spec does NOT say if file-name characters disable comments. We assume they do so.
#
:terminal ::= "'"  pause => after event => :discard[switch]           # Disable comment in character literal
:terminal ::= '"'  pause => after event => :discard[switch]           # Disable comment in regular string literal
:terminal ::= '@"' pause => after event => :discard[switch]           # Disable comment in verbatim string literal (Note that it ends with '"' character)

<input>                                          ::= <input section opt>          action => inputAction
<input section opt>                              ::=
<input section opt>                              ::= <input section>
<input section>                                  ::= <input section part>+
<input section part>                             ::= <input elements opt> <new line>
                                                    | <pp directive>
<input elements opt>                             ::=
<input elements opt>                             ::= <input elements>
<input elements>                                 ::= <input element>+
<input element>                                  ::= <whitespace>
                                                   | (- <token> -) <TOKEN MARKER>
:lexeme ::= <TOKEN MARKER> symbol-action => tokenMarkerAction
event ^token = predicted <token>
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

<token>                                          ::= <identifier>
                                                   | <keyword>
                                                   | <integer literal>
                                                   | <real literal>
                                                   | <character literal>
                                                   | <string literal>
                                                   | <operator or punctuator>

event identifier$ = completed <identifier>
event keyword$ = completed <keyword>
event integer_literal$ = completed <integer literal>
event real_literal$ = completed <real literal>
event character_literal$ = completed <character literal>
event string_literal$ = completed <string literal>
event operator_or_punctuator$ = completed <operator or punctuator>

<unicode escape sequence>                        ::= '\\u' <hex digit> <hex digit> <hex digit> <hex digit>
                                                   | '\\U' <hex digit> <hex digit> <hex digit> <hex digit> <hex digit> <hex digit> <hex digit> <hex digit>
<identifier>                                     ::= <available identifier>
                                                   | '@' <identifier or keyword>

event ^identifier_or_keyword = predicted <identifier or keyword>
<identifier or keyword>                          ::= <IDENTIFIER OR KEYWORD>

event ^available_identifier = predicted <available identifier>
<available identifier>                           ::= /\w+/
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
# => :discard is switch on at the end of <pp new line>
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
<conditional section opt>                        ::=
<conditional section opt>                        ::= <conditional section>

event pp_if_context[] = nulled <pp if context>
<pp if context>                                  ::=
<pp if section>                                  ::= <PP IF> <whitespace> <pp expression> <pp new line> (- <pp if context> -) <conditional section opt>

<pp elif sections>                               ::= <pp elif section>+
event pp_elif_context[] = nulled <pp elif context>
<pp elif context>                                ::=
<pp elif section>                                ::= <PP ELIF> <whitespace> <pp expression> <pp new line> (- <pp elif context> -) <conditional section opt>

event pp_else_context[] = nulled <pp else context>
<pp else context>                                ::=
<pp else section>                                ::= <PP ELSE> <pp new line> (- <pp else context> -) <conditional section opt>

event pp_endif_context[] = nulled <pp endif context>
<pp endif context>                               ::=
<pp endif>                                       ::= <PP ENDIF> (- <pp endif context> -) <pp new line>

<conditional section>                            ::= (- <CONDITIONAL SECTION OK> -) <input section>
                                                   | (- <CONDITIONAL SECTION KO> -) <skipped section>
event :discard[off] = predicted <skipped section>
<skipped section>                                ::= <skipped section part>+
<skipped section part>                           ::= <skipped characters opt> <new line>
                                                   | <pp directive>
<skipped characters opt>                         ::=
<skipped characters opt>                         ::= <skipped characters>
<skipped characters>                             ::= <whitespace opt> <not number sign> <input characters opt>
<not number sign>                                ::= /[^#]/u # Any input-character except #

<pp diagnostic>                                  ::= <PP ERROR> <pp message>
                                                   | <PP WARNING> <pp message>
<pp message>                                     ::= <new line>
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

<pp pragma>                                      ::= <PP PRAGMA> <pp pragma text>
<pp pragma text>                                 ::= (- <new line> -)                                           action => ppPragmaTextAction
                                                   | (- <whitespace> -) <input characters opt> (- <new line> -) action => ppPragmaTextAction

#
# Lexemes
#
<LINE INDICATOR DEFAULT>                           ~ 'default'
<LINE INDICATOR HIDDEN>                            ~ 'hidden'
<TOKEN MARKER>                                     ~ /[^\s\S]/ # Matches nothing
<NEW LINE>                                         ~ /(?:\x{000D}|\x{000A}|\x{000D}\x{000A}|\x{0085}|\x{2028}|\x{2029})/u
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
                                                   | <A UNICODE ESCAPE SEQUENCE REPRESENTING THE CHARACTER 005F>
<identifier part characters>                     ::= <identifier part character>+
<identifier part character>                      ::= <letter character>
                                                   | <decimal digit character>
                                                   | <connecting character>
                                                   | <combining character>
                                                   | <formatting character>
<letter character>                               ::= /[\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}\p{Nl}]/u
                                                   | <A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Lu Ll Lt Lm Lo or Nl>
<combining character>                            ::= /[\p{Mn}\p{Mc}]/u
                                                   | <A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Mn or Mc>
<decimal digit character>                        ::= /[\p{Nd}]/u
                                                   | <A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Nd>
<connecting character>                           ::= /[\p{Pc}]/u
                                                   | <A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Pc>
<formatting character>                           ::= /[\p{Cf}]/u
                                                   | <A UNICODE ESCAPE SEQUENCE REPRESENTING A CHARACTER OF CLASSES Cf>

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
:desc                                            ::= 'Pre-processing expression'
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
