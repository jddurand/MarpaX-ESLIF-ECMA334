use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::ECMA334::Lexical::ValueInterface;
use Carp qw/croak/;
use Log::Any qw/$log/;

# ABSTRACT: MarpaX::ESLIF::ECMA334 Lexical Value Interface

# VERSION

# AUTHORITY

=head1 DESCRIPTION

MarpaX::ESLIF::ECMA334's Lexical Value Interface

=head1 SYNOPSIS

    use MarpaX::ESLIF::ECMA334::Lexical::ValueInterface;

    my $valueInterface = MarpaX::ESLIF::ECMA334::Lexical::ValueInterface->new();

=cut

# -----------
# Constructor
# -----------

=head1 SUBROUTINES/METHODS

=head2 new($class)

Instantiate a new value interface object.

=cut

sub new {
    my ($pkg, %options) = @_;

    my $definitions = delete($options{definitions}) // {};
    croak 'definitions must be a HASH reference' unless ((ref($definitions) // '') eq 'HASH');
    foreach (sort keys %{$definitions}) {
        croak "$_ definition must be a MarpaX::ESLIF boolean" unless MarpaX::ESLIF::is_bool($definitions->{$_})
    }

    return bless {
        result => undef,
        definitions => $definitions,
        %options
    }, $pkg
}

# ----------------
# Required methods
# ----------------

=head2 Required methods

=head3 isWithHighRankOnly

Returns a true or a false value, indicating if valuation should use highest ranked rules or not, respectively. Default is a true value.

=cut

sub isWithHighRankOnly { return 1 }  # When there is the rank adverb: highest ranks only ?

=head3 isWithOrderByRank

Returns a true or a false value, indicating if valuation should order by rule rank or not, respectively. Default is a true value.

=cut

sub isWithOrderByRank  { return 1 }  # When there is the rank adverb: order by rank ?

=head3 isWithAmbiguous

Returns a true or a false value, indicating if valuation should allow ambiguous parse tree or not, respectively. Default is a false value.

=cut

sub isWithAmbiguous    { return 0 }  # Allow ambiguous parse ?

=head3 isWithNull

Returns a true or a false value, indicating if valuation should allow a null parse tree or not, respectively. Default is a false value.

=cut

sub isWithNull         { return 0 }  # Allow null parse ?

=head3 maxParses

Returns the number of maximum parse tree valuations. Default is unlimited (i.e. a false value).

=cut

sub maxParses          { return 0 }  # Maximum number of parse tree values

=head3 getResult

Returns the current parse tree value.

=cut

sub getResult          { return $_[0]->{result} }

=head3 setResult

Sets the current parse tree value.

=cut

sub setResult          { return $_[0]->{result} = $_[1] }

=head3 pp_or_expression

Computes the value of a C<||> pre-processing expression

=cut

sub pp_or_expression   { return ($_[1] // 0) || ($_[2] // 0) ? $MarpaX::ESLIF::true : $MarpaX::ESLIF::false }

=head3 pp_and_expression

Computes the value of a C<&&> pre-processing expression

=cut

sub pp_and_expression   { return ($_[1] // 0) && ($_[2] // 0) ? $MarpaX::ESLIF::true : $MarpaX::ESLIF::false }

=head3 pp_equal_expression

Computes the value of a C<==> pre-processing expression

=cut

sub pp_equal_expression   { return ($_[1] // 0) == ($_[2] // 0) ? $MarpaX::ESLIF::true : $MarpaX::ESLIF::false }

=head3 pp_not_equal_expression

Computes the value of a C<!=> pre-processing expression

=cut

sub pp_not_equal_expression   { return ($_[1] // 0) != ($_[2] // 0) ? $MarpaX::ESLIF::true : $MarpaX::ESLIF::false }

=head3 pp_not_expression

Computes the value of a C<!> pre-processing expression

=cut

sub pp_not_expression   { return ($_[1] // 0) ? $MarpaX::ESLIF::true : $MarpaX::ESLIF::false }

=head3 pp_not_expression

Computes the value of a C<!> pre-processing expression

=cut

sub pp_condition_symbol {
    print STDERR "Origin: $_[1]\n";
    my $normalizedSymbol = $_[0]->_normalized_condition_symbol($_[1]);
    print STDERR "normalizedSymbol: $normalizedSymbol\n";
    my $rc = $_[0]->{definitions}->{$normalizedSymbol} //= $MarpaX::ESLIF::false;
    print STDERR "value: $rc\n";
    
    return $rc
 }

=head1 SEE ALSO

L<MarpaX::ESLIF::ECMA334>

=cut

sub my_action {
    my ($self, @args) = @_;

    return { ruleName => $MarpaX::ESLIF::Context::ruleName,
             what => [ @args ] };
}

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
