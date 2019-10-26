use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::ECMA334::Syntactic::RecognizerInterface;
use Carp qw/croak/;
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

    my $lexicalAst = $options{lexicalAst} // croak 'Undefined lexical AST';

    return bless
    {
        _lexicalAst => $lexicalAst,
        _currentAstItem => undef,
        _eof => 0
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

    if (! @{$self->{_lexicalAst}}) {
        $self->{_eof} = 1;
        return 0
    }

    $self->{_currentAstItem} = shift @{$self->{_lexicalAst}};
    return 1;
}

# ============================================================================
# isEof
# ============================================================================

=head3 isEof($self)

Returns a true or a false value, indicating if end-of-data is reached.

=cut

sub isEof {
    my ($self) = @_;

    return $self->{_eof}
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

    $log->tracef('==> "%s"', $self->{_currentAstItem}->{string});
    return $self->{_currentAstItem}->{string} # Whatever the type, next data always have a string form
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
# currentAstItem
# ============================================================================

=head3 currentAstItem($self)

Returns the current structured item from lexical AST.

=cut

sub currentAstItem {
    my ($self) = @_;

    return $self->{_currentAstItem}
}

# ============================================================================
# nextAstItem
# ============================================================================

=head3 nextAstItem($self)

Returns the next structured item from lexical AST.

=cut

sub nextAstItem {
    my ($self) = @_;

    return $self->{_eof} ? undef : $self->{_lexicalAst}->[0]
}

# ============================================================================
# consumeNextAstItem
# ============================================================================

=head3 consumeNextAstItem($self)

Consumes the next structured item from lexical AST.

=cut

sub consumeNextAstItem {
    my ($self) = @_;

    $log->tracef('... "%s"', $self->{_lexicalAst}->[0]->{string});
    splice(@{$self->{_lexicalAst}}, 0, 1)
}

1;
