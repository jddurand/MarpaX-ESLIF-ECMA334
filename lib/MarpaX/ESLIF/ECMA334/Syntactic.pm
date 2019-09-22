use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::ECMA334::Syntactic;

use Log::Any qw/$log/;

# ABSTRACT: C# syntactic parse as per Standard ECMA-334 5th Edition

# VERSION

# AUTHORITY

=head1 DESCRIPTION

This module parses syntactically the C# language as per Standard ECMA-334 5th Edition.

=head1 SYNOPSIS

    use MarpaX::ESLIF::ECMA334::Syntactic;
    #
    # The input to syntactic grammar must be the lexical output from lexical grammar
    #
    my $lexical      = MarpaX::ESLIF::ECMA334::Lexical->new();
    my $input        = "public interface Test { bool MyTest(); }"
    my $lexicalValue = $lexical->parse(input => input, encoding => 'UTF-16', definitions => { 'TRUE' => $MarpaX::ESLIF::true });

    my $syntactic    = MarpaX::ESLIF::ECMA334::Syntactic->new();
    my $ast          = $syntactic->parse(lexicalValue => $lexicalValue);

=cut

use Carp qw/croak/;
use Data::Section -setup;
use Log::Any qw/$log/;
use MarpaX::ESLIF 3.0.15; # if-action

my $SYNTACTIC_BNF             = ${__PACKAGE__->section_data('syntactic grammar')};
my $SYNTACTIC_GRAMMAR         = MarpaX::ESLIF::Grammar->new(MarpaX::ESLIF->new($log), $SYNTACTIC_BNF);

=head1 SUBROUTINES/METHODS

=cut

# ============================================================================
# new
# ============================================================================

=head2 new($class)

Instantiate a new lexical object.

=cut

sub new {
    my ($pkg) = @_;

    return bless
        (
         {
         },
         $pkg)
}

# ============================================================================
# parse
# ============================================================================

=head2 parse($self, %options)

Parser method. C<%options> is hash containing:

=over

=item lexicalValue

Output from lexical parse.

=back

Output is an AST of the syntactic parse.

=cut

sub parse {
    my ($self, %options) = @_;

}

=head1 NOTES

This module is a L<Log::Any> consumer.

=cut

1;

__DATA__
__[ syntactic grammar ]__
:default ::= action => ::ast symbol-action => ::shift # And not ::convert[UTF-8] because we will always inject values from lexical parse, that we know are UTF-8 strings
:desc ::= 'Syntactic grammar'

#  --------------
## Basic concepts
#  --------------
<namespace name>          ::= <namespace or type name>
<type name>               ::= <namespace or type name>
<namespace or type name>  ::= <identifier> <type argument list opt>
                            | <namespace or type name> '.' <identifier> <type argument list opt>
                            | <qualified alias member>

# -----
# Types
# -----
type                      ::= <reference type>
                            | <value type>
                            | <type parameter>
<value type>              ::= <struct type>
                            | <enum type>
<struct type>             ::= <type name>
                              <simple type>
                              <nullable value type>
<simple type>             ::= <numeric type>
                            | 'bool'
<numeric type>            ::= <integral type>
                              <floating point type>
                              'decimal'
<integral type>           ::= 'sbyte'
                            | 'byte'
                            | 'short'
                            | 'ushort'
                            | 'int'
                            | 'uint'
                            | 'long'
                            | 'ulong'
                            | 'char
<nullable type>  ::= <non nullable value type> '?'
<non nullable value type> ::= 'type'
<floating point type>     ::= 'float'
                            | 'double'
<enum type>               ::= <type name>
<type argument list>      ::= '<' <type arguments> '>'
<type arguments>          ::= <type argument>+ separator => ','
<type argument>           ::= <type>
<type parameter>          ::= <identifier>
