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

    return bless { result => undef, %options }, $pkg
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

=head1 SEE ALSO

L<MarpaX::ESLIF::ECMA334>

=cut

1;
