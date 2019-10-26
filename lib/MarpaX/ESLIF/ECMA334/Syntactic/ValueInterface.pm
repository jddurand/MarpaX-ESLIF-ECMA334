use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::ECMA334::Syntactic::ValueInterface;

# ABSTRACT: MarpaX::ESLIF::ECMA334 Syntactic Value Interface

# VERSION

# AUTHORITY

=head1 DESCRIPTION

MarpaX::ESLIF::ECMA334's Syntactic Value Interface

=head1 SYNOPSIS

    use MarpaX::ESLIF::ECMA334::Syntactic::ValueInterface;

    my $valueInterface = MarpaX::ESLIF::ECMA334::Syntactic::ValueInterface->new();

=cut

=head1 SUBROUTINES/METHODS

=head2 new($class)

Instantiate a new value interface object.

=cut

# ============================================================================
# new
# ============================================================================

sub new {
    my ($pkg) = @_;

    return bless({}, $pkg)
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

1;
