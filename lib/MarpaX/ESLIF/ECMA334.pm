=for html <a href="https://travis-ci.org/jddurand/MarpaX-ESLIF-ECMA334"><img src="https://travis-ci.org/jddurand/MarpaX-ESLIF-ECMA334.svg?branch=master" alt="Travis CI build status" height="18"></a> <a href="https://badge.fury.io/gh/jddurand%2FMarpaX-ESLIF-ECMA334"><img src="https://badge.fury.io/gh/jddurand%2FMarpaX-ESLIF-ECMA334.svg" alt="GitHub version" height="18"></a> <a href="https://dev.perl.org/licenses/" rel="nofollow noreferrer"><img src="https://img.shields.io/badge/license-Perl%205-blue.svg" alt="License Perl5" height="18">

=cut

use strict;
use warnings FATAL => 'all';

package MarpaX::ESLIF::ECMA334;

use MarpaX::ESLIF::ECMA334::Lexical;

# ABSTRACT: C# parser as per Standard ECMA-334 5th Edition

# VERSION

# AUTHORITY

=head1 DESCRIPTION

This module parses C# language as per Standard ECMA-334 5th Edition.

=head1 SYNOPSIS

    use MarpaX::ESLIF::ECMA334;

    my $input = "public interface Test { bool MyTest(); }"

    #
    # Lexical parse
    #
    my $lexicalAst = MarpaX::ESLIF::ECMA334::Lexical->new->parse
                       (
                         input => $input,                 # The input
                         encoding => 'UTF-8',             # The eventual input encoding - optional though recommended
                         definitions => {                 # The eventual definitions - none by default
                           'TRUE' => $MarpaX::ESLIF::true # Definitions must be a MarpaX::ESLIF boolean
                         }
                       );

    #
    # Syntactic parse
    #
    my $syntacticAst = MarpaX::ESLIF::ECMA334::Syntactic->new->parse(lexicalAst);

=head1 SEE ALSO

L<MarpaX::ESLIF::ECMA404::Lexical>, L<MarpaX::ESLIF::ECMA404::Syntactic>

=cut

1;
