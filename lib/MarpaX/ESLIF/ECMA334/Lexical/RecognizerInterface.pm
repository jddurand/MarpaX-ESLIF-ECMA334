use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::ECMA334::Lexical::RecognizerInterface;
use MarpaX::ESLIF::ECMA334::Lexical::UnicodeHelper;
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

=back

C<%options> may contain:

=over

=item encoding

The encoding of the data

=item definitions

A reference to a hash containing known definitions, where values must be a MarpaX::ESLIF boolean.

=back

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
             hasCompletion => 0,
             recurseLevel => 0,
             definitions => $definitions,
             %options
         },
         $pkg)
}

# ----------------
# Required methods
# ----------------

# ============================================================================
# read
# ============================================================================

=head2 Required methods

=head3 read($self)

Returns a true or a false value, indicating if last read was successful. Default is a true value.

=cut

sub read                   { return 1 } # First read callback will be ok

# ============================================================================
# isEof
# ============================================================================

=head3 isEof($self)

Returns a true or a false value, indicating if end-of-data is reached. Default is a true value.

=cut

sub isEof                  { return 1 } # ../. and we will say this is EOF

# ============================================================================
# isCharacterStream
# ============================================================================

=head3 isCharacterStream($self)

Returns a true or a false value, indicating if last read is a stream of characters. Default is a true value.

=cut

sub isCharacterStream      { return 1 } # MarpaX::ESLIF will validate the input

# ============================================================================
# encoding
# ============================================================================

=head3 encoding($self)

Returns encoding information. Default is undef.

=cut

sub encoding               { return $_[0]->{encoding} } # Let MarpaX::ESLIF guess eventually - undef is ok

# ============================================================================
# data
# ============================================================================

=head3 data($self)

Returns last bunch of data. Default is the string passed in the constructor.

=cut

sub data                   { return $_[0]->{input} // croak 'Undefined input' } # Data itself

# ============================================================================
# isWithDisableThreshold
# ============================================================================

=head3 isWithDisableThreshold($self)

Returns a true or a false value, indicating if threshold warning is on or off, respectively. Default is a false value.

=cut

sub isWithDisableThreshold { return 0 } # Disable threshold warning ?

# ============================================================================
# isWithExhaustion
# ============================================================================

=head3 isWithExhaustion($self)

Returns a true or a false value, indicating if exhaustion event is on or off, respectively. Default is a false value.

=cut

sub isWithExhaustion       { return $_[0]->{exhaustion} // 0 } # Exhaustion event ? Default is false.

# ============================================================================
# isWithNewline
# ============================================================================

=head3 isWithNewline($self)

Returns a true or a false value, indicating if newline count is on or off, respectively. Default is a false value.

=cut

sub isWithNewline          { return $_[0]->{newline} // 1 } # Newline count ? Default is true.

# ============================================================================
# isWithTrack
# ============================================================================

=head3 isWithTrack($self)

Returns a true or a false value, indicating if absolute position tracking is on or off, respectively. Default is a true value.

=cut

sub isWithTrack            { return $_[0]->{track} // 0 } # Absolute position tracking ? Default is true.

=head2 Additional methods

# ============================================================================
# recurseLevel
# ============================================================================

=head3 recurseLevel($self)

Returns recurse level.

=cut

sub recurseLevel          { return $_[0]->{recurseLevel} // 0 } # Recurse level - defaulting to 0

# ============================================================================
# hasCompletion
# ============================================================================

=head3 hasCompletion($self)

Returns a boolean indicating if completion has been reached at least once.

=cut

sub hasCompletion {
    my $self = shift;

    return @_ ? $self->{hasCompletion} = $_[0] : $self->{hasCompletion}
}

# ============================================================================
# A_unicode_escape_sequence_representing_the_character_005f
# ============================================================================

=head3 A_unicode_escape_sequence_representing_the_character_005f($self, $lexeme)

Returns a true or a false value, indicating if C<$lexeme> represents the character x005F

=cut

sub A_unicode_escape_sequence_representing_the_character_005f {
    my ($self, $lexeme) = @_;

    return MarpaX::ESLIF::ECMA334::Lexical::UnicodeHelper->unicode_escape_sequence($lexeme) eq "\x{005F}"
}

# ============================================================================
# A_unicode_escape_sequence_representing_a_character_of_classes_Lu_Ll_Lt_Lm_Lo_or_Nl
# ============================================================================

=head3 A_unicode_escape_sequence_representing_a_character_of_classes_Lu_Ll_Lt_Lm_Lo_or_Nl($self, $lexeme)

Returns a true or a false value, indicating if C<$lexeme> represent a character of classes Lu, Ll Lt, Lm, Lo, or Nl

=cut

sub A_unicode_escape_sequence_representing_a_character_of_classes_Lu_Ll_Lt_Lm_Lo_or_Nl {
    my ($self, $lexeme) = @_;

    return MarpaX::ESLIF::ECMA334::Lexical::UnicodeHelper->unicode_escape_sequence($lexeme) =~ /[\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}\p{Nl}]/
}

# ============================================================================
# A_unicode_escape_sequence_representing_a_character_of_classes_Mn_or_Mc
# ============================================================================

=head3 A_unicode_escape_sequence_representing_a_character_of_classes_Mn_or_Mc($self, $lexeme)

Returns a true or a false value, indicating if C<$lexeme> represent a character of classes Mn or Mc

=cut

sub A_unicode_escape_sequence_representing_a_character_of_classes_Mn_or_Mc {
    my ($self, $lexeme) = @_;

    return MarpaX::ESLIF::ECMA334::Lexical::UnicodeHelper->unicode_escape_sequence($lexeme) =~ /[\p{Mn}\p{Mc}]/
}

# ============================================================================
# A_unicode_escape_sequence_representing_a_character_of_the_class_Nd
# ============================================================================

=head3 A_unicode_escape_sequence_representing_a_character_of_the_class_Nd($self, $lexeme)

Returns a true or a false value, indicating if C<$lexeme> represent a character of the class Nd

=cut

sub A_unicode_escape_sequence_representing_a_character_of_the_class_Nd {
    my ($self, $lexeme) = @_;

    return MarpaX::ESLIF::ECMA334::Lexical::UnicodeHelper->unicode_escape_sequence($lexeme) =~ /\p{Nd}/
}

# ============================================================================
# A_unicode_escape_sequence_representing_a_character_of_the_class_Pc
# ============================================================================

=head3 A_unicode_escape_sequence_representing_a_character_of_the_class_Pc($self, $lexeme)

Returns a true or a false value, indicating if C<$lexeme> represent a character of the class Pc

=cut

sub A_unicode_escape_sequence_representing_a_character_of_the_class_Pc {
    my ($self, $lexeme) = @_;

    return MarpaX::ESLIF::ECMA334::Lexical::UnicodeHelper->unicode_escape_sequence($lexeme) =~ /\p{Pc}/
}

# ============================================================================
# A_unicode_escape_sequence_representing_a_character_of_the_class_Cf
# ============================================================================

=head3 A_unicode_escape_sequence_representing_a_character_of_the_class_Cf($self, $lexeme)

Returns a true or a false value, indicating if C<$lexeme> represent a character of the class Cf

=cut

sub A_unicode_escape_sequence_representing_a_character_of_the_class_Cf {
    my ($self, $lexeme) = @_;

    return MarpaX::ESLIF::ECMA334::Lexical::UnicodeHelper->unicode_escape_sequence($lexeme) =~ /\p{Cf}/
}

# ============================================================================
# definitions
# ============================================================================

=head3 definitions($self)

Returns current definitions as a HASH reference, where key is the name and value is a MarpaX::ESLIF bBoolean

=cut

sub definitions {
    my ($self) = @_;

    return $self->{definitions}
}

# ============================================================================
# single_character_is_below_0xFFFF
# ============================================================================

sub single_character_is_below_0xFFFF {
    my ($self, $lexeme) = @_;

    utf8::decode($lexeme);

    return ord($lexeme) < 0xFFFF
}

# ============================================================================
# single_regular_string_literal_character_is_below_0xFFFF
# ============================================================================

sub single_regular_string_literal_character_is_below_0xFFFF {
    my ($self, $lexeme) = @_;

    utf8::decode($lexeme);

    return ord($lexeme) < 0xFFFF
}

=head1 SEE ALSO

L<MarpaX::ESLIF::ECMA404>

=cut

1;
