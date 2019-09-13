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

sub new {
    my ($pkg, %options) = @_;

    my $definitions = delete($options{definitions}) // {};
    croak 'definitions must be a HASH reference' unless ((ref($definitions) // '') eq 'HASH');
    foreach (sort keys %{$definitions}) {
        croak "$_ definition must be a MarpaX::ESLIF boolean" unless MarpaX::ESLIF::is_bool($definitions->{$_})
    }

    my $pp_expression_values = delete($options{pp_expressions_values}) // [];
    croak 'pp_expression_values must be an ARRAY reference' unless ((ref($pp_expression_values) // '') eq 'ARRAY');
    foreach (@{$pp_expression_values}) {
        croak "$_ pp expression value must be a MarpaX::ESLIF boolean" unless MarpaX::ESLIF::is_bool($_)
    }

    return bless {
	hasCompletion => 0,
	recurseLevel => 0,
        definitions => $definitions,
        pp_expression_values => $pp_expression_values,
	%options}, $pkg
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

Returns a true or a false value, indicating if absolute position tracking is on or off, respectively. Default is a true value.

=cut

sub isWithTrack            { $_[0]->{track} // 0 } # Absolute position tracking ? Default is true.

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

sub A_unicode_escape_sequence_representing_the_character_005f {
    my ($self, $lexeme) = @_;

    return MarpaX::ESLIF::ECMA334::Lexical::UnicodeHelper->unicode_escape_sequence($lexeme) eq "\x{005F}"
}

sub A_unicode_escape_sequence_representing_a_character_of_classes_Lu_Ll_Lt_Lm_Lo_or_Nl {
    my ($self, $lexeme) = @_;

    return MarpaX::ESLIF::ECMA334::Lexical::UnicodeHelper->unicode_escape_sequence($lexeme) =~ /[\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}\p{Nl}]/
}

sub A_unicode_escape_sequence_representing_a_character_of_classes_Mn_or_Mc {
    my ($self, $lexeme) = @_;

    return MarpaX::ESLIF::ECMA334::Lexical::UnicodeHelper->unicode_escape_sequence($lexeme) =~ /[\p{Mn}\p{Mc}]/
}

sub A_unicode_escape_sequence_representing_a_character_of_the_class_Nd {
    my ($self, $lexeme) = @_;

    return MarpaX::ESLIF::ECMA334::Lexical::UnicodeHelper->unicode_escape_sequence($lexeme) =~ /\p{Nd}/
}

sub A_unicode_escape_sequence_representing_a_character_of_the_class_Pc {
    my ($self, $lexeme) = @_;

    return MarpaX::ESLIF::ECMA334::Lexical::UnicodeHelper->unicode_escape_sequence($lexeme) =~ /\p{Pc}/
}

sub A_unicode_escape_sequence_representing_a_character_of_the_class_Cf {
    my ($self, $lexeme) = @_;

    return MarpaX::ESLIF::ECMA334::Lexical::UnicodeHelper->unicode_escape_sequence($lexeme) =~ /\p{Cf}/
}

sub definitions {
    my ($self) = @_;

    return $self->{definitions}
}

sub pp_expression_values {
    my ($self) = @_;

    return $self->{pp_expression_values}
}

sub pp_expression_values_push {
    my ($self, $pp_expression_value) = @_;

    croak "$pp_expression_value must be a MarpaX::ESLIF boolean" unless MarpaX::ESLIF::is_bool($pp_expression_value);

    return push(@{$self->{pp_expression_values}}, $pp_expression_value)
}

sub pp_expression_values_pop {
    my ($self) = @_;

    return pop(@{$self->{pp_expression_values}}) // croak 'Unbalanced pp expression_values'
}

=head1 SEE ALSO

L<MarpaX::ESLIF::ECMA404>

=cut

1;
