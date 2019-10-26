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
        lexicalAst => $lexicalAst,
        _nextData => undef
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

    if (! @{$self->{lexicalAst}}) {
        $self->{_nextData} = undef;
        return 0
    } else {
        $self->{_nextData} = shift @{$self->{lexicalAst}};
        return 1;
    }
}

# ============================================================================
# isEof
# ============================================================================

=head3 isEof($self)

Returns a true or a false value, indicating if end-of-data is reached.

=cut

sub isEof {
    my ($self) = @_;

    return scalar(@{$self->{lexicalAst}}) # We return true until lexical AST (an array reference) is empty
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

    my $nextData = $self->{_nextData} // croak 'Undefined next value from lexical AST';

    return $nextData->{string} # Whatever the type, next data always have a string form
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
# nextData
# ============================================================================

=head3 nextData($self)

Returns the next value from lexical AST.

=cut

sub nextData {
    my ($self) = @_;

    return $self->{_nextData}
}

1;
