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

=head1 SUBROUTINES/METHODS

=cut

# ============================================================================
# new
# ============================================================================

=head2 new($class, %options)

Instantiate a new recognizer interface object. C<%options> should contain at least:

=over

=item input

The input to parse

=back

C<%options> may contain:

=over

=item input

The input

=item encoding

The encoding of the data

=item definitions

A reference to a hash containing known definitions, where values must be a MarpaX::ESLIF boolean.

=back

=cut

sub new {
    my ($pkg, %options) = @_;

    my $definitions = delete($options{definitions}) // {};
    my $input = delete($options{input}) // '';
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
             input => $input,
             %options
         },
         $pkg)
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
    return 1 # First read callback will be ok
}

# ============================================================================
# isEof
# ============================================================================

=head3 isEof($self)

Returns a true or a false value, indicating if end-of-data is reached.

=cut

sub isEof {
    return 1 # ../. and we will say this is EOF
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
    return $_[0]->{encoding} # Let MarpaX::ESLIF guess eventually - undef is ok
}

# ============================================================================
# data
# ============================================================================

=head3 data($self)

Returns last bunch of data. Default is the string passed in the constructor.

=cut

sub data {
    return $_[0]->{input} // croak 'Undefined input' # Data itself
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
    return $_[0]->{exhaustion} // 0
}

# ============================================================================
# isWithNewline
# ============================================================================

=head3 isWithNewline($self)

Returns a true or a false value, indicating if newline count is on or off, respectively.

=cut

sub isWithNewline {
    return 1
}

# ============================================================================
# isWithTrack
# ============================================================================

=head3 isWithTrack($self)

Returns a true or a false value, indicating if absolute position tracking is on or off, respectively.

=cut

sub isWithTrack {
    return 1
}

=head2 Additional methods

=cut

# ============================================================================
# recurseLevel
# ============================================================================

=head3 recurseLevel($self)

Returns current recurse level.

=cut

sub recurseLevel {
    return $_[0]->{recurseLevel} // 0 # Recurse level - defaulting to 0
}

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
# unicode_unescape_sequence
# ============================================================================

=head3 unicode_unescape_sequence($self, $lexeme)

Returns the unescaped unicode character corresponding to C<$lexeme>

=cut

sub unicode_unescape_sequence {
    return MarpaX::ESLIF::ECMA334::Lexical::UnicodeHelper::unicode_unescape_sequence($_[1])
}

# ============================================================================
# A_unicode_escape_sequence_representing_the_character_005f
# ============================================================================

=head3 A_unicode_escape_sequence_representing_the_character_005f($self, $lexeme)

Returns a true or a false value, indicating if C<$lexeme> represents the character x005F

=cut

sub A_unicode_escape_sequence_representing_the_character_005f {
    return MarpaX::ESLIF::ECMA334::Lexical::UnicodeHelper::unicode_unescape_sequence($_[1]) eq "\x{005F}"
}

# ============================================================================
# A_unicode_escape_sequence_representing_a_character_of_classes_Lu_Ll_Lt_Lm_Lo_or_Nl
# ============================================================================

=head3 A_unicode_escape_sequence_representing_a_character_of_classes_Lu_Ll_Lt_Lm_Lo_or_Nl($self, $lexeme)

Returns a true or a false value, indicating if C<$lexeme> represent a character of classes Lu, Ll Lt, Lm, Lo, or Nl

=cut

sub A_unicode_escape_sequence_representing_a_character_of_classes_Lu_Ll_Lt_Lm_Lo_or_Nl {
    return MarpaX::ESLIF::ECMA334::Lexical::UnicodeHelper::unicode_unescape_sequence($_[1]) =~ /[\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}\p{Nl}]/
}

# ============================================================================
# A_unicode_escape_sequence_representing_a_character_of_classes_Mn_or_Mc
# ============================================================================

=head3 A_unicode_escape_sequence_representing_a_character_of_classes_Mn_or_Mc($self, $lexeme)

Returns a true or a false value, indicating if C<$lexeme> represent a character of classes Mn or Mc

=cut

sub A_unicode_escape_sequence_representing_a_character_of_classes_Mn_or_Mc {
    return MarpaX::ESLIF::ECMA334::Lexical::UnicodeHelper::unicode_unescape_sequence($_[1]) =~ /[\p{Mn}\p{Mc}]/
}

# ============================================================================
# A_unicode_escape_sequence_representing_a_character_of_the_class_Nd
# ============================================================================

=head3 A_unicode_escape_sequence_representing_a_character_of_the_class_Nd($self, $lexeme)

Returns a true or a false value, indicating if C<$lexeme> represent a character of the class Nd

=cut

sub A_unicode_escape_sequence_representing_a_character_of_the_class_Nd {
    return MarpaX::ESLIF::ECMA334::Lexical::UnicodeHelper::unicode_unescape_sequence($_[1]) =~ /\p{Nd}/
}

# ============================================================================
# A_unicode_escape_sequence_representing_a_character_of_the_class_Pc
# ============================================================================

=head3 A_unicode_escape_sequence_representing_a_character_of_the_class_Pc($self, $lexeme)

Returns a true or a false value, indicating if C<$lexeme> represent a character of the class Pc

=cut

sub A_unicode_escape_sequence_representing_a_character_of_the_class_Pc {
    return MarpaX::ESLIF::ECMA334::Lexical::UnicodeHelper::unicode_unescape_sequence($_[1]) =~ /\p{Pc}/
}

# ============================================================================
# A_unicode_escape_sequence_representing_a_character_of_the_class_Cf
# ============================================================================

=head3 A_unicode_escape_sequence_representing_a_character_of_the_class_Cf($self, $lexeme)

Returns a true or a false value, indicating if C<$lexeme> represent a character of the class Cf

=cut

sub A_unicode_escape_sequence_representing_a_character_of_the_class_Cf {
    return MarpaX::ESLIF::ECMA334::Lexical::UnicodeHelper::unicode_unescape_sequence($_[1]) =~ /\p{Cf}/
}

# ============================================================================
# definitions
# ============================================================================

=head3 definitions($self)

Returns current definitions as a HASH reference, where key is the name and value is a MarpaX::ESLIF bBoolean

=cut

sub definitions {
    return $_[0]->{definitions}
}

# ============================================================================
# single_character_is_below_0xFFFF
# ============================================================================

=head3 single_character_is_below_0xFFFF($self, $lexeme)

Returns a boolean saying if current $lexeme represents a character below C<0xFFFF>

=cut

sub single_character_is_below_0xFFFF {
    my ($self, $lexeme) = @_;

    utf8::decode($lexeme);

    return ord($lexeme) < 0xFFFF
}

# ============================================================================
# single_regular_string_literal_character_is_below_0xFFFF
# ============================================================================

=head3 single_regular_string_literal_character_is_below_0xFFFF($self, $lexeme)

Returns a boolean saying if current $lexeme, when part of regular string literal, represents a character below C<0xFFFF>

=cut

sub single_regular_string_literal_character_is_below_0xFFFF {
    my ($self, $lexeme) = @_;

    utf8::decode($lexeme);

    return ord($lexeme) < 0xFFFF
}

1;
