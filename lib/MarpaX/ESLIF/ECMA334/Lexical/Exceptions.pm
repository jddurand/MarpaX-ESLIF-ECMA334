use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::ECMA334::Lexical::Exceptions;

# ABSTRACT: C# lexical parse exception class

# VERSION

# AUTHORITY

=head1 DESCRIPTION

This module prodives C# lexical parse exception definitions.

=head1 SYNOPSIS

    use MarpaX::ESLIF::ECMA334::Lexical::Exceptions;

=cut

use Exception::Class (
    Foo::Bar::Exception::Senses =>
    { description => 'sense-related exception' },

    Foo::Bar::Exception::Smell => {
        isa         => 'Foo::Bar::Exception::Senses',
        fields      => 'odor',
        description => 'stinky!'
    },

    Foo::Bar::Exception::Taste => {
        isa         => 'Foo::Bar::Exception::Senses',
        fields      => [ 'taste', 'bitterness' ],
        description => 'like, gag me with a spoon!'
    }
);

1;
