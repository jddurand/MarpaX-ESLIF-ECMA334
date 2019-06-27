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

    my $valueInterface = MarpaX::ESLIF::ECMA334::Lexica::ValueInterface->new();

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

=head3 u4

unicode escape sequence with four hex digits action.

=cut

sub u4 {
    my ($self, $u, $hexdigit1, $hexdigit2, $hexdigit3, $hexdigit4) = @_;

    my $codepoint = hex("${hexdigit1}${hexdigit2}${hexdigit3}${hexdigit4}");
    my $result = chr($codepoint) // croak "Invalid code point \\u${hexdigit1}${hexdigit2}${hexdigit3}${hexdigit4}";

    return $result
}

=head3 u8

unicode escape sequence with eight hex digits action.

=cut

sub u8 {
    my ($self, $u, $hexdigit1, $hexdigit2, $hexdigit3, $hexdigit4, $hexdigit5, $hexdigit6, $hexdigit7, $hexdigit8) = @_;

    my @hex = (hex("${hexdigit1}${hexdigit2}${hexdigit3}${hexdigit4}"), hex("${hexdigit5}${hexdigit6}${hexdigit7}${hexdigit8}"));

    my $result;
    while (@hex) {
        if ($#hex > 0) {
            my ($high, $low) = @hex;
            #
            # An UTF-16 surrogate pair ?
            #
            if (($high >= 0xD800) && ($high <= 0xDBFF) && ($low >= 0xDC00) && ($low <= 0xDFFF)) {
                #
                # Yes.
                # This is evaled for one reason only: some old versions of perl may croak with special characters like
                # "Unicode character 0x10ffff is illegal"
                #
                $result .= eval {chr((($high - 0xD800) * 0x400) + ($low - 0xDC00) + 0x10000)} // croak "Invalid surrogate code point \\U${hexdigit1}${hexdigit2}${hexdigit3}${hexdigit4}${hexdigit5}${hexdigit6}${hexdigit7}${hexdigit8}";
                splice(@hex, 0, 2)
            } else {
                #
                # No. Take first \uhhhh as a code point.
                # Eval returns undef in scalar context if there is a failure.
                #
                $result .= eval {chr(shift @hex) } // croak "Invalid code point \\U${hexdigit1}${hexdigit2}${hexdigit3}${hexdigit4}${hexdigit5}${hexdigit6}${hexdigit7}${hexdigit8}";
            }
        } else {
            #
            # \uhhhh taken as a code point.
            # Eval returns undef in scalar context if there is a failure.
            #
            $result .= eval {chr(shift @hex) } // croak "Invalid code point \\U${hexdigit1}${hexdigit2}${hexdigit3}${hexdigit4}${hexdigit5}${hexdigit6}${hexdigit7}${hexdigit8}";
        }
    }

    return $result
}

sub my_action {
    my ($self, @args) = @_;

    my $key = $MarpaX::ESLIF::Context::ruleName // $MarpaX::ESLIF::Context::symbolName;

    return { $key => \@args }
}

=head1 SEE ALSO

L<MarpaX::ESLIF::ECMA334>

=cut



1;
