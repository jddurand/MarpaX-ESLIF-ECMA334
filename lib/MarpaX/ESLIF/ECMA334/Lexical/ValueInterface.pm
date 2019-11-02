use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::ECMA334::Lexical::ValueInterface;
use MarpaX::ESLIF::ECMA334::Lexical::UnicodeHelper;
use Carp qw/croak/;

# ABSTRACT: MarpaX::ESLIF::ECMA334 Lexical Value Interface

# VERSION

# AUTHORITY

=head1 DESCRIPTION

MarpaX::ESLIF::ECMA334's Lexical Value Interface

=head1 SYNOPSIS

    use MarpaX::ESLIF::ECMA334::Lexical::ValueInterface;

    my $valueInterface = MarpaX::ESLIF::ECMA334::Lexical::ValueInterface->new();

=cut

=head1 SUBROUTINES/METHODS

=head2 new($class)

Instantiate a new value interface object.

=cut

# ============================================================================
# new
# ============================================================================

sub new {
    my ($pkg, %options) = @_;

    my $definitions = delete($options{definitions}) // {};
    croak 'definitions must be a HASH reference' unless ((ref($definitions) // '') eq 'HASH');
    foreach (sort keys %{$definitions}) {
        croak "$_ definition must be a MarpaX::ESLIF boolean" unless MarpaX::ESLIF::is_bool($definitions->{$_})
    }

    return bless
        (
         {
             result => undef,
             definitions => $definitions,
             lexical_ast => $options{lexical_ast} // [],
             input_elements => [],
             %options
         }, $pkg)
}

=head2 Required methods

=cut

# ============================================================================
# isWithHighRankOnly
# ============================================================================

=head3 isWithHighRankOnly($self)

Returns a true or a false value, indicating if valuation should use highest ranked rules or not, respectively. Default is a true value.

=cut

sub isWithHighRankOnly { return 1 }  # When there is the rank adverb: highest ranks only ?

# ============================================================================
# isWithOrderByRank
# ============================================================================

=head3 isWithOrderByRank($self)

Returns a true or a false value, indicating if valuation should order by rule rank or not, respectively. Default is a true value.

=cut

sub isWithOrderByRank  { return 1 }  # When there is the rank adverb: order by rank ?

# ============================================================================
# isWithAmbiguous
# ============================================================================

=head3 isWithAmbiguous($self)

Returns a true or a false value, indicating if valuation should allow ambiguous parse tree or not, respectively. Default is a false value.

=cut

sub isWithAmbiguous    { return 0 }  # Allow ambiguous parse ?

# ============================================================================
# isWithNull
# ============================================================================

=head3 isWithNull($self)

Returns a true or a false value, indicating if valuation should allow a null parse tree or not, respectively. Default is a false value.

=cut

sub isWithNull         { return 1 }  # Allow null parse ?

# ============================================================================
# maxParses
# ============================================================================

=head3 maxParses($self)

Returns the number of maximum parse tree valuations. Default is unlimited (i.e. a false value).

=cut

sub maxParses          { return 0 }  # Maximum number of parse tree values

# ============================================================================
# getResult
# ============================================================================

=head3 getResult($self)

Returns the current parse tree value.

=cut

sub getResult          { return $_[0]->{result} }

# ============================================================================
# setResult
# ============================================================================

=head3 setResult($self, $result)

Sets the current parse tree value.

=cut

sub setResult          { return $_[0]->{result} = $_[1] }

=head2 Additional methods

=cut

# ============================================================================
# pp_or_expression
# ============================================================================

=head3 pp_or_expression($self, $leftValue, $rightValue)

Computes the value of a C<||> pre-processing expression

=cut

sub pp_or_expression   { return ($_[1] // 0) || ($_[2] // 0) ? $MarpaX::ESLIF::true : $MarpaX::ESLIF::false }

# ============================================================================
# pp_and_expression
# ============================================================================

=head3 pp_and_expression($self, $leftValue, $rightValue)

Computes the value of a C<&&> pre-processing expression

=cut

sub pp_and_expression   { return ($_[1] // 0) && ($_[2] // 0) ? $MarpaX::ESLIF::true : $MarpaX::ESLIF::false }

# ============================================================================
# pp_equal_expression
# ============================================================================

=head3 pp_equal_expression($self, $leftValue, $rightValue)

Computes the value of a C<==> pre-processing expression

=cut

sub pp_equal_expression   { return ($_[1] // 0) == ($_[2] // 0) ? $MarpaX::ESLIF::true : $MarpaX::ESLIF::false }

# ============================================================================
# pp_not_equal_expression
# ============================================================================

=head3 pp_not_equal_expression($self, $leftValue, $rightValue)

Computes the value of a C<!=> pre-processing expression

=cut

sub pp_not_equal_expression   { return ($_[1] // 0) != ($_[2] // 0) ? $MarpaX::ESLIF::true : $MarpaX::ESLIF::false }

# ============================================================================
# pp_not_expression
# ============================================================================

=head3 pp_not_expression($self, $Value)

Computes the value of a C<!> pre-processing expression

=cut

sub pp_not_expression   { return ($_[1] // 0) ? $MarpaX::ESLIF::false : $MarpaX::ESLIF::true }

# ============================================================================
# pp_condition_symbol($self, $symbol)
# ============================================================================

=head3 pp_condition_symbol

Returns the value of a conditional symbol

=cut

sub pp_condition_symbol { return $_[0]->{definitions}->{$_[0]->_normalized_condition_symbol($_[1])} //= $MarpaX::ESLIF::false }

# ============================================================================
# unicode_unescape_sequence
# ============================================================================

=head3 unicode_unescape_sequence($self, $escapedSequence)

Returns the unescape character corresponding to a unicode escaped sequence

=cut

sub unicode_unescape_sequence {
    my ($self, $utf8bytes) = @_;

    return MarpaX::ESLIF::ECMA334::Lexical::UnicodeHelper::unicode_unescape_sequence($utf8bytes)
}

# ============================================================================
# token_identifier
# ============================================================================

=head3 token_identifier($self, $identifier)

Action when token is an identifier. Returns a hash C<{ type => 'identifier', value => $identifier}> reference.

=cut

sub token_identifier {
    my ($self, $identifier) = @_;

    return { type => 'identifier', value => $identifier }
}

# ============================================================================
# token_keyword
# ============================================================================

=head3 token_keyword($self, $keyword)

Action when token is an keyword. Returns a hash C<{ type => 'keyword', value => $keyword}> reference.

=cut

sub token_keyword {
    my ($self, $keyword) = @_;

    return { type => 'keyword', value => $keyword }
}

# ============================================================================
# token_integer_literal
# ============================================================================

=head3 token_integer_literal($self, $integer_literal)

Action when token is an integer_literal. Returns a hash C<{ type => 'integer_literal', value => $integer_literal}> reference.

=cut

sub token_integer_literal {
    my ($self, $integer_literal) = @_;

    return { type => 'integer_literal', value => $integer_literal }
}

# ============================================================================
# token_real_literal
# ============================================================================

=head3 token_real_literal($self, $real_literal)

Action when token is an real_literal. Returns a hash C<{ type => 'real_literal', value => $real_literal}> reference.

=cut

sub token_real_literal {
    my ($self, $real_literal) = @_;

    return { type => 'real_literal', value => $real_literal }
}

# ============================================================================
# token_character_literal
# ============================================================================

=head3 token_character_literal($self, $character_literal)

Action when token is an character_literal. Returns a hash C<{ type => 'character_literal', value => $character_literal}> reference.

=cut

sub token_character_literal {
    my ($self, $character_literal) = @_;

    return { type => 'character_literal', value => $character_literal }
}

# ============================================================================
# token_string_literal
# ============================================================================

=head3 token_string_literal($self, $string_literal)

Action when token is an string_literal. Returns a hash C<{ type => 'string_literal', value => $string_literal}> reference.

=cut

sub token_string_literal {
    my ($self, $string_literal) = @_;

    return { type => 'string_literal', value => $string_literal }
}

# ============================================================================
# token_operator_or_punctuator
# ============================================================================

=head3 token_operator_or_punctuator($self, $operator_or_punctuator)

Action when token is an operator_or_punctuator. Returns a hash C<{ type => 'operator_or_punctuator', value => $operator_or_punctuator}> reference.

=cut

sub token_operator_or_punctuator {
    my ($self, $operator_or_punctuator) = @_;

    return { type => 'operator_or_punctuator', value => $operator_or_punctuator }
}

# ============================================================================
# whitespace
# ============================================================================

=head3 whitespace($self, $whitespace)

Action for whitespace rule. Returns a hash C<{ type => 'whitespace', value => $whitespace}> reference.

=cut

sub whitespace {
    my ($self, $whitespace) = @_;

    return { type => 'whitespace', value => $whitespace }
}

# ============================================================================
# push_input_element
# ============================================================================

=head3 push_input_element($self, $input_element)

Action for input element rule. Pushes C<$input_element> it to the list of input elements. Returns undef

=cut

sub push_input_element {
    my ($self, $input_element) = @_;

    push(@{$self->{input_elements}}, $input_element);

    return
}

# ============================================================================
# input
# ============================================================================

=head3 input($self, $input_section_opt)

Action for input. Returns an array reference containing the list of input elements.

=cut

sub input {
    my ($self) = @_;

    return $self->{input_elements}
}

# ============================================================================
# _normalized_condition_symbol
# ============================================================================

sub _normalized_condition_symbol {
    #
    # - Each unicode-escape-sequence is transformed into its corresponding Unicode character.
    #     This is already the case by construction, because a conditional symbol is the
    #     lexeme <ANY IDENTIFIER OR KEYWORD EXCEPT TRUE OR FALSE> that a valuation of
    #     the <identifier or keyword> grammar
    # - Any formatting-characters are removed
    #     We have to apply that
    #
    $_[1] =~ s/\p{Cf}//g;

    return $_[1]
}

1;
