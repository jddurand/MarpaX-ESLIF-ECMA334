use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::ECMA334::Syntactic::RecognizerInterface;
use Log::Any qw/$log/;

# ABSTRACT: MarpaX::ESLIF::ECMA334 Syntactic Recognizer Interface

# VERSION

# AUTHORITY

=head1 DESCRIPTION

MarpaX::ESLIF::ECMA334's Syntactic Recognizer Interface

=head1 SYNOPSIS

    use MarpaX::ESLIF::ECMA334::Syntactic::RecognizerInterface;

    my $recognizerInterface = MarpaX::ESLIF::ECMA334::Syntactic->new();

=cut

=head1 SUBROUTINES/METHODS

=cut

# ============================================================================
# new
# ============================================================================

=head2 new($class)

Instantiate a new recognizer interface object.

=cut

sub new {
    my ($pkg, %options) = @_;

    my $elements = delete($options{elements}) // [];

    return bless {
        elements => $elements,
        currentElement => undef
    }, $pkg
}

=head2 Required methods

=cut

# ============================================================================
# read
# ============================================================================

=head3 read($self)

Returns a true or a false value, indicating if last read was successful.

=cut

sub read {
    my ($self) = @_;

    my $rc = scalar(@{$self->{elements}}) > 0 ? 1 : 0;
    $log->tracef('%s::read: return %d', __PACKAGE__, $rc);
    return $rc;
}

# ============================================================================
# isEof
# ============================================================================

=head3 isEof($self)

Returns a true or a false value, indicating if end-of-data is reached.

=cut

sub isEof {
    my ($self) = @_;

    my $rc = scalar(@{$self->{elements}}) > 0 ? 0 : 1;
    $log->tracef('%s::isEof: return %d', __PACKAGE__, $rc);
    return $rc
}

# ============================================================================
# isCharacterStream
# ============================================================================

=head3 isCharacterStream($self)

Returns a true or a false value, indicating if last read is a stream of characters.

=cut

sub isCharacterStream {
    return 1 # MarpaX::ESLIF will validate the input
}

# ============================================================================
# encoding
# ============================================================================

=head3 encoding($self)

Returns encoding information.

=cut

sub encoding {
    return 'UTF-8'
}

# ============================================================================
# data
# ============================================================================

=head3 data($self)

Returns last bunch of data. Depends on the next value from lexical AST.

=cut

sub data {
    my ($self) = @_;

    $self->{currentElement} = shift @{$self->{elements}};
    my $rc = $self->{currentElement}->{value};

    $log->tracef('%s::data: return %s', __PACKAGE__, defined($rc) ? "\"$rc\"" : 'undef');
    return $rc
}

# ============================================================================
# isWithDisableThreshold
# ============================================================================

=head3 isWithDisableThreshold($self)

Returns a true or a false value, indicating if threshold warning is on or off, respectively.

=cut

sub isWithDisableThreshold {
    return 0
}

# ============================================================================
# isWithExhaustion
# ============================================================================

=head3 isWithExhaustion($self)

Returns a true or a false value, indicating if exhaustion event is on or off, respectively.

=cut

sub isWithExhaustion {
    return 0
}

# ============================================================================
# isWithNewline
# ============================================================================

=head3 isWithNewline($self)

Returns a true or a false value, indicating if newline count is on or off, respectively.

=cut

sub isWithNewline {
    return 0
}

# ============================================================================
# isWithTrack
# ============================================================================

=head3 isWithTrack($self)

Returns a true or a false value, indicating if absolute position tracking is on or off, respectively.

=cut

sub isWithTrack {
    return 0
}

# ============================================================================
# currentElement
# ============================================================================

=head3 currentElement($self)

Returns the current structured item from lexical AST.

=cut

sub currentElement {
    my ($self) = @_;

    return $self->{currentElement}
}

# ============================================================================
# consumeCurrentElement
# ============================================================================

=head3 consumeCurrentElement($self)

Consumes the current structured item from lexical AST.

=cut

sub consumeCurrentElement {
    my ($self) = @_;

    return $self->{currentElement} = undef
}

# ============================================================================
# nextElement
# ============================================================================

=head3 nextElement($self)

Returns the next structured item from lexical AST.

=cut

sub nextElement {
    my ($self) = @_;

    return @{$self->{elements}} ? $self->{elements}->[0] : undef
}

# ============================================================================
# consumeNextElement
# ============================================================================

=head3 consumeNextElement($self)

Consumes the next structured item from lexical AST.

=cut

sub consumeNextElement {
    my ($self) = @_;

    shift @{$self->{_elements}}
}

1;
