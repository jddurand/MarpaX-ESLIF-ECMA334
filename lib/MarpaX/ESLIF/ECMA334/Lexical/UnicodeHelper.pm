use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::ECMA334::Lexical::UnicodeHelper;
use Carp qw/croak/;

# ABSTRACT: MarpaX::ESLIF::ECMA334 Lexical Unicode Helper

# VERSION

# AUTHORITY

=head1 DESCRIPTION

MarpaX::ESLIF::ECMA334's Lexical Unicode Helper

=head1 SYNOPSIS

    use MarpaX::ESLIF::ECMA334::Lexical::UnicodeHelper;

    my $unescaped_character = MarpaX::ESLIF::ECMA334::Lexical::UnicodeHelper->unicode_unescape_sequence('\\u0061');

=cut


=head1 SUBROUTINES/METHODS

=cut

# ============================================================================
# unicode_unescape_sequence
# ============================================================================

=head2 unicode_unescape_sequence($utf8bytes)

Returns unescaped character corresponding to a C<\\uxxxx> or C<\\Uxxxxxxxx> escaped sequence.

=cut

sub unicode_unescape_sequence {
    my ($utf8bytes) = @_;

    return substr($utf8bytes, 0, 2, '') eq '\\u' ? _u4(split(//, $utf8bytes)) : _u8(split(//, $utf8bytes))
}

# ============================================================================
# _u4
# ============================================================================
sub _u4 {
    my ($hexdigit1, $hexdigit2, $hexdigit3, $hexdigit4) = @_;

    my $codepoint = hex("${hexdigit1}${hexdigit2}${hexdigit3}${hexdigit4}");

    return chr($codepoint) // croak "Invalid code point \\u${hexdigit1}${hexdigit2}${hexdigit3}${hexdigit4}";
}

# ============================================================================
# _u8
# ============================================================================
sub _u8 {
    my ($hexdigit1, $hexdigit2, $hexdigit3, $hexdigit4, $hexdigit5, $hexdigit6, $hexdigit7, $hexdigit8) = @_;

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
                my $hex = shift(@hex);
                $result .= eval {chr($hex) } // croak "Invalid code point \\U" . sprintf('%4x', $hex)
            }
        } else {
            #
            # \uhhhh taken as a code point.
            # Eval returns undef in scalar context if there is a failure.
            #
            my $hex = shift(@hex);
            $result .= eval {chr($hex) } // croak "Invalid code point \\U" . sprintf('%4x', $hex)
        }
    }

    return $result
}

=head1 SEE ALSO

L<MarpaX::ESLIF::ECMA404>

=cut

1;
