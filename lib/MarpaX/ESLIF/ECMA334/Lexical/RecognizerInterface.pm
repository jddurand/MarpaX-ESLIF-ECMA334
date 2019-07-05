use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::ECMA334::Lexical::RecognizerInterface;
use Carp qw/croak/;

# ABSTRACT: MarpaX::ESLIF::ECMA334 Lexical Recognizer Interface

# VERSION

# AUTHORITY

=head1 DESCRIPTION

MarpaX::ESLIF::ECMA334's Lexical Recognizer Interface

=head1 SYNOPSIS

    use MarpaX::ESLIF::ECMA334::Lexical::RecognizerInterface;

    my $recognizerInterface = MarpaX::ESLIF::ECMA334::Lexical::RecognizerInterface->new();

=cut


# -----------
# Constructor
# -----------

=head1 SUBROUTINES/METHODS

=head2 new($class, %options)

Instantiate a new recognizer interface object. C<%options> should contain at least:

=over

=item input

The input to parse

=item encoding

The encoding of the data

=back

=cut

sub new {
    my ($pkg, %options) = @_;
    bless { hasCompletion => 0, recurseLevel => 0, %options}, $pkg
}

# ----------------
# Required methods
# ----------------

=head2 Required methods

=head3 read($self)

Returns a true or a false value, indicating if last read was successful. Default is a true value.

=cut

sub read                   {        1 } # First read callback will be ok

=head3 isEof($self)

Returns a true or a false value, indicating if end-of-data is reached. Default is a true value.

=cut

sub isEof                  {        1 } # ../. and we will say this is EOF

=head3 isCharacterStream($self)

Returns a true or a false value, indicating if last read is a stream of characters. Default is a true value.

=cut

sub isCharacterStream      {        1 } # MarpaX::ESLIF will validate the input

=head3 encoding($self)

Returns encoding information. Default is undef.

=cut

sub encoding               { $_[0]->{encoding} } # Let MarpaX::ESLIF guess eventually - undef is ok

=head3 data($self)

Returns last bunch of data. Default is the string passed in the constructor.

=cut

sub data                   { $_[0]->{input} // croak 'Undefined input' } # Data itself

=head3 isWithDisableThreshold($self)

Returns a true or a false value, indicating if threshold warning is on or off, respectively. Default is a false value.

=cut

sub isWithDisableThreshold {        0 } # Disable threshold warning ?

=head3 isWithExhaustion($self)

Returns a true or a false value, indicating if exhaustion event is on or off, respectively. Default is a false value.

=cut

sub isWithExhaustion       { $_[0]->{exhaustion} // 0 } # Exhaustion event ? Default is false.

=head3 isWithNewline($self)

Returns a true or a false value, indicating if newline count is on or off, respectively. Default is a false value.

=cut

sub isWithNewline          { $_[0]->{newline} // 1 } # Newline count ? Default is true.

=head3 isWithTrack($self)

Returns a true or a false value, indicating if absolute position tracking is on or off, respectively. Default is a false value.

=cut

sub isWithTrack            { $_[0]->{track} // 0 } # Absolute position tracking ? Default is false.

=head2 Additional methods

=head3 recurseLevel($self)

Returns recurse level.

=cut

sub recurseLevel          { $_[0]->{recurseLevel} // 0 } # Recurse level - defaulting to 0

=head3 hasCompletion($self)

Returns a boolean indicating if completion has been reached at least once.

=cut

sub hasCompletion {
    my $self = shift;

    return @_ ? $self->{hasCompletion} = $_[0] : $self->{hasCompletion}
}

=head1 SEE ALSO

L<MarpaX::ESLIF::ECMA404>

=cut

1;
