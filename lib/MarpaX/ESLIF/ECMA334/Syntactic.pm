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
<namespace name>                       ::= <namespace or type name>
<type name>                            ::= <namespace or type name>
<namespace or type name>               ::= <identifier> <type argument list opt>
                                         | <namespace or type name> '.' <identifier> <type argument list opt>
                                         | <qualified alias member>

# -----
# Types
# -----
type                                   ::= <reference type>
                                         | <value type>
                                         | <type parameter>
<value type>                           ::= <struct type>
                                         | <enum type>
<struct type>                          ::= <type name>
                                           <simple type>
                                           <nullable value type>
<simple type>                          ::= <numeric type>
                                         | 'bool'
<numeric type>                         ::= <integral type>
                                           <floating point type>
                                           'decimal'
<integral type>                        ::= 'sbyte'
                                         | 'byte'
                                         | 'short'
                                         | 'ushort'
                                         | 'int'
                                         | 'uint'
                                         | 'long'
                                         | 'ulong'
                                         | 'char
<nullable type>                        ::= <non nullable value type> '?'
<non nullable value type>              ::= 'type'
<floating point type>                  ::= 'float'
                                         | 'double'
<enum type>                            ::= <type name>
<type argument list>                   ::= '<' <type arguments> '>'
<type arguments>                       ::= <type argument>+ separator => ',' proper => 1 hide-separator => 1
<type argument>                        ::= <type>
<type parameter>                       ::= <identifier>

# ---------
# Variables
# ---------
<variable reference>                   ::= <expression>

# -----------
# Expressions
# -----------
<argument list>                        ::= <argument>+ separator => ',' proper => 1 hide-separator => 1
<argument>                             ::= <argument name opt> <argument value>
<argument name opt>                    ::= <argument name>
<argument name opt>                    ::=
<argument name>                        ::= <identifier> ':'
<argument value>                       ::= <expression>
                                         | 'ref' <variable reference>
                                         | 'out' <variable reference>
<primary expression>                   ::= <primary no array creation expression>
                                           <array creation expression>
<primary no array creation expression> ::= <literal>
                                         | <simple name>
                                         | <parenthesized expression>
                                         | <member access>
                                         | <invocation expression>
                                         | <element access>
                                         | <this access>
                                         | <base access>
                                         | <post increment expression>
                                         | <post decrement expression>
                                         | <object creation expression>
                                         | <delegate creation expression>
                                         | <anonymous object creation expression>
                                         | <typeof expression>
                                         | <sizeof expression>
                                         | <checked expression>
                                         | <unchecked expression>
                                         | <default value expression>
                                         | <anonymous method expression>
<simple name>                          ::= <identifier> <type argument list opt>
<type argument list opt>               ::= <type argument list>
<type argument list opt>               ::=
<parenthesized expression>             ::= '(' <expression> ')'
<member access>                        ::= <primary expression> '.' <identifier> <type argument list opt>
                                         | <predefined type> '.' <identifier> <type argument list opt>
                                         | <qualified alias member> '.' <identifier> <type argument list opt>
<predefined type>                      ::= 'bool'
                                         | 'byte'
                                         | 'char'
                                         | 'decimal'
                                         | 'double'
                                         | 'float'
                                         | 'int'
                                         | 'long'
                                         | 'object'
                                         | 'sbyte'
                                         | 'short'
                                         | 'string'
                                         | 'uint'
                                         | 'ulong'
<invocation expression>                ::= <primary expression> '(' <argument list opt> ')'
<argument list opt>                    ::= <argument list>
<argument list opt>                    ::=
<element access>                       ::= <primary no array creation expression> '[' <argument list> ']'
<expression list>                      ::= <expression>+ separator => ',' proper => 1 hide-separator => 1
<this access>                          ::= 'this'
<base access>                          ::= 'base' '.' <identifier> <type argument list opt>
                                         | 'base' '[' <argument list> ']'
<post increment expression>            ::= <primary expression> '++'
<post decrement expression>            ::= <primary expression> '--'
<object creation expression>           ::= 'new' <type> '(' <argument list opt> ')' <object or collection initializer opt>
                                         | 'new' <type> <object or collection initializer>
<object or collection initializer opt> ::= <object or collection initializer>
<object or collection initializer opt> ::=
<object or collection initializer>     ::= <object initializer>
                                         | <collection initializer>
<object initializer>                   ::= '{' <member initializer list opt> '}'
                                         | '{' <member initializer list> ',' '}'
<member initializer list opt>          ::= <member initializer list>
<member initializer list opt>          ::=
<member initializer list>              ::= <member initializer> separator => ','  proper => 1 hide-separator => 1
<member initializer>                   ::= <identifier> '=' <initializer value>
<initializer value>                    ::= <expression>
                                         | <object or collection initializer>
<collection initializer>               ::= '{' <element initializer list> '}'
                                         | '{' <element initializer list> ',' '}'
<element initializer list>             ::= <element initializer>+  separator => ','  proper => 1 hide-separator => 1
<element initializer>                  ::= <non assignment expression>
                                         | '{' <expression list> '}'
<array creation expression>            ::= 'new' <non array type> '[' <expression list> ']' <rank specifiers opt> <array initializer opt>
                                         | 'new' <array type> <array initializer>
                                         | 'new' <rank specifier> <array initializer>
<rank specifiers opt>                  ::= <rank specifiers>
<rank specifiers opt>                  ::=
<array initializer opt>                ::= <array initializer>
<array initializer opt>                ::=
<delegate creation expression>         ::= 'new' <delegate type> '(' <expression> ')'
<anonymous object creation expression> ::= 'new' <anonymous object initializer>
<anonymous object initializer>         ::= '{' <member declarator list opt> '}'
                                         | '{' <member declarator list> ',' '}'
<member declarator list opt>           ::= <member declarator list>
<member declarator list opt>           ::=
<member declarator list>               ::= <member declarator>+ separator => ','  proper => 1 hide-separator => 1
<member declarator>                    ::= <simple name>
                                         | <member access>
                                         | <base access>
                                         | <identifier> '=' <expression>
<typeof expression>                    ::= 'typeof' '(' <type> ')'
                                         | 'typeof' '(' <unbound type name> ')'
                                         | 'typeof' '(' 'void' ')'
<unbound type name>                    ::= <identifier> <generic dimension specifier opt>
                                         | <identifier> '::' <identifier> <generic dimension specifier opt>
                                         | <unbound type name> '.' <identifier> <generic dimension specifier opt>
<generic dimension specifier opt>      ::= <generic dimension specifier opt>
<generic dimension specifier opt>      ::=
<generic dimension specifier>          ::= '<' <commas opt> '>'
<commas opt>                           ::= <commas>
<commas opt>                           ::=
<commas>                               ::= ','+
<checked expression>                   ::= 'checked' '(' <expression> ')'
<unchecked expression>                 ::= 'unchecked' '(' <expression> ')'
<default value expression>             ::= 'default' '(' <type> ')'
<unary expression>                     ::= <primary expression>
                                         | '+' <unary expression>
                                         | '-' <unary expression>
                                         | '!' <unary expression>
                                         | '~' <unary expression>
                                         | <pre increment expression>
                                         | <pre decrement expression>
                                         | <cast expression>
                                         | <await expression>
<pre increment expression>             ::= '++' <unary expression>
<pre decrement expression>             ::= '--' <unary expression>
<cast expression>                      ::= '(' <type> ')' <unary expression>
<await expression>                     ::= 'await' <unary expression>
<multiplicative expression>            ::= <unary expression>
                                         | <multiplicative expression> '*' <unary expression>
                                         | <multiplicative expression> '/' <unary expression>
                                         | <multiplicative expression> '%' <unary expression>
<additive expression>                  ::= <multiplicative expression>
                                         | <additive expression> '+' <multiplicative expression>
                                         | <additive expression> '–' <multiplicative expression>
<shift expression>                     ::= <additive expression>
                                         | <shift expression> '<<' <additive expression>
                                         | <shift expression> <right shift> <additive expression>
<relational expression>                ::= <shift expression>
                                         | <relational-expression> '<' <shift expression>
                                         | <relational-expression> '>' <shift expression>
                                         | <relational-expression> '<=' <shift expression>
                                         | <relational-expression> '>=' <shift expression>
                                         | <relational-expression> 'is' <type>
                                         | <relational-expression> 'as' <type>
<equality expression>                  ::= <relational expression>
                                         | <equality expression> '==' <relational expression>
                                         | <equality expression> '!=' <relational expression>
<and expression>                       ::= <equality expression>
                                         | <and expression> '&' <equality expression>
<exclusive or expression>              ::= <and expression>
                                         | <exclusive or expression> '^' <and expression>
<inclusive or expression>              ::= <exclusive or expression>
                                         | <inclusive or expression> '|' <exclusive or expression>
<conditional and expression>           ::= <inclusive or expression>
                                         | <conditional and expression> '&&' <inclusive or expression>
<conditional or expression>            ::= <conditional and expression>
                                         | <conditional or expression> '||' <conditional and expression>
<null coalescing expression>           ::= <conditional or expression>
                                         | <conditional or expression> '??' <null coalescing expression>
<conditional expression>               ::= <null coalescing expression>
                                         | <null coalescing expression> '?' <expression> ':' <expression>
<lambda expression>                    ::= <async opt> <anonymous function signature> '=>' <anonymous function body>
<async opt>                            ::= 'async'
<async opt>                            ::=
<anonymous method expression>          ::= <async opt> 'delegate' <explicit anonymous function signature opt> <block>
<explicit anonymous function signature opt> ::= <explicit anonymous function signature>
<explicit anonymous function signature opt> ::=
<anonymous-function-signature>         ::= <explicit anonymous function signature>
                                         | <implicit anonymous function signature>
<explicit anonymous function signature>::= '(' <explicit anonymous function parameter list opt> ')'
<explicit anonymous function parameter list opt> ::= <explicit anonymous function parameter list>
<explicit anonymous function parameter list opt> ::=
<explicit anonymous function parameter list> ::= <explicit anonymous function parameter>+ separator => ','  proper => 1 hide-separator => 1
<explicit anonymous function parameter>::= <anonymous function parameter modifier opt> <type> <identifier>
<anonymous function parameter modifier opt> ::= <anonymous function parameter modifier>
<anonymous function parameter modifier opt> ::=
<anonymous function parameter modifier>::= 'ref'
                                         | 'out'
<implicit anonymous function signature>::= '(' <implicit anonymous function parameter list opt> ')'
                                         | <implicit anonymous function parameter>
<implicit anonymous function parameter list opt> ::= <implicit anonymous function parameter list opt>
<implicit anonymous function parameter list opt> ::=
<implicit anonymous function parameter list> ::= <implicit anonymous function parameter>+ separator => ','  proper => 1 hide-separator => 1
<implicit anonymous function parameter>::= <identifier>
<anonymous function body>              ::= <expression>
                                         | <block>
<query expression>                     ::= <from clause> <query body>
<from clause>                          ::= 'from' <type opt> <identifier> 'in' <expression>
<type opt>                             ::= <type>
<type opt>                             ::=
<query body>                           ::= <query body clauses opt> <select or group clause> <query continuation opt>
<query body clauses opt>               ::= <query body clauses>
<query body clauses opt>               ::=
<query continuation opt>               ::= <query continuation>
<query continuation opt>               ::=
<query body clauses>                   ::= <query body clause>+
<query body clause>                    ::= <from clause>
                                         | <let clause>
                                         | <where clause>
                                         | <join clause>
                                         | <join into clause>
                                         | <orderby clause>
<let clause>                           ::= 'let' <identifier> '=' <expression>
<where clause>                         ::= 'where' <boolean expression>
<join clause>                          ::= 'join' <type opt> <identifier> 'in' <expression> 'on' <expression> 'equals' <expression>
<join into clause>                     ::= 'join' <type opt> <identifier> 'in' <expression> 'on' <expression> 'equals' <expression> 'into' <identifier>
<orderby clause>                       ::= 'orderby' <orderings>
<orderings>                            ::= <ordering>+ separator => ','  proper => 1 hide-separator => 1
<ordering>                             ::= <expression> <ordering direction opt>
<ordering direction opt>               ::= <ordering direction>
<ordering direction opt>               ::=
<ordering direction>                   ::= 'ascending'
                                         | 'descending'
<select or group clause>               ::= <select clause>
                                         | <group clause>
<select clause>                        ::= 'select' <expression>
<group clause>                         ::= 'group' <expression> 'by' <expression>
<query continuation>                   ::= 'into' <identifier> <query body>
<assignment>                           ::= <unary expression> <assignment operator> <expression>
<assignment operator>                  ::= '='
                                         | '+='
                                         | '-='
                                         | '*='
                                         | '/='
                                         | '%='
                                         | '&='
                                         | '|='
                                         | '^='
                                         | '<<='
                                         | <right shift assignment>
<expression>                           ::= <non assignment expression>
                                         | <assignment>
<non assignment expression>            ::= <conditional expression>
                                         | <lambda expression>
                                         | <query expression>
<constant expression>                  ::= <expression>
<boolean expression>                   ::= <expression>

# ----------
# Statements
# ----------
<statement>                            ::= <labeled statement>
                                         | <declaration statement>
                                         | <embedded statement>
<embedded statement>                   ::= <block>
                                         | <empty statement>
                                         | <expression statement>
                                         | <selection statement>
                                         | <iteration statement>
                                         | <jump statement>
                                         | <try statement>
                                         | <checked statement>
                                         | <unchecked statement>
                                         | <lock statement>
                                         | <using statement>
                                         | <yield statement>
<block>                                ::= '{' <statement list opt> '}'
<statement list opt>                   ::= <statement list>
<statement list opt>                   ::=
<statement list>                       ::= <statement>+
<empty statement>                      ::= ';'
<labeled statement>                    ::= <identifier> ':' <statement>
<declaration statement>                ::= <local variable declaration> ';'
                                         | <local constant declaration> ';'
<local variable declaration>           ::= <local variable type> <local variable declarators>
<local variable type>                  ::= <type>
                                         | 'var'
<local variable declarators>           ::= <local variable declarator>+ separator => ','  proper => 1 hide-separator => 1
<local variable declarator>            ::= <identifier>
                                         | <identifier> '=' <local variable initializer>
<local variable initializer>           ::= <expression>
                                         | <array initializer>
<local constant declaration>           ::= 'const' <type> <constant declarators>
<constant declarators>                 ::= <constant declarator>+ separator => ','  proper => 1 hide-separator => 1
<constant declarator>                  ::= <identifier> '=' <constant expression>
<expression statement>                 ::= <statement expression> ';'
<statement expression>                 ::= <invocation expression>
                                         | <object creation expression>
                                         | <assignment>
                                         | <post increment expression>
                                         | <post decrement expression>
                                         | <pre increment expression>
                                         | <pre decrement expression>
                                         | <await expression>
<selection statement>                  ::= <if statement>
                                         | <switch statement>
<if statement>                         ::= 'if' '(' <boolean expression> ')' <embedded statement>
                                         | 'if' '(' <boolean expression> ')' <embedded statement> 'else' <embedded statement>
<switch statement>                     ::= 'switch' '(' <expression> ')' <switch block>
<switch block>                         ::= '{' <switch sections opt> '}'
<switch sections opt>                  ::= <switch sections>
<switch sections opt>                  ::=
<switch sections>                      ::= <switch section>+
<switch section>                       ::= <switch labels> <statement list>
<switch labels>                        ::= <switch label>+
<switch label>                         ::= 'case' <constant expression> ':'
                                         | 'default' ':'
<iteration statement>                  ::= <while statement>
                                         | <do statement>
                                         | <for statement>
                                         | <foreach statement>
<while statement>                      ::= 'while' '(' <boolean expression> )' <embedded statement>
<do statement>                         ::= 'do' <embedded statement> 'while' '(' <boolean expression> ')' ';'
<for statement>                        ::= 'for' '(' <for initializer opt> ';' <for condition opt> ';' <for iterator opt> ')' <embedded statement>
<for initializer opt>                  ::= <for initializer>
<for initializer opt>                  ::=
<for condition opt>                    ::= <for condition>
<for condition opt>                    ::=
<for iterator opt>                     ::= <for iterator>
<for iterator opt>                     ::=
<for initializer>                      ::= <local variable declaration>
                                         | <statement expression list>
<for condition>                        ::= <boolean expression>
<for iterator>                         ::= <statement expression list>
<statement expression list>            ::= <statement expression>+ separator => ','  proper => 1 hide-separator => 1
<foreach statement>                    ::= 'foreach' '(' <local variable type> <identifier> 'in' <expression> ')' <embedded statement>
<jump statement>                       ::= <break statement>
                                         | <continue statement>
                                         | <goto statement>
                                         | <return statement>
                                         | <throw statement>
<break statement>                      ::= 'break' ';'
<continue statement>                   ::= 'continue' ';'
<goto statement>                       ::= 'goto' <identifier> ';'
                                         | 'goto' 'case' <constant expression> ';'
                                         | 'goto' 'default' ';
<return statement>                     ::= 'return' <expression opt> ';'
<expression opt>                       ::= <expression>
<expression opt>                       ::=
<throw statement>                      ::= 'throw' <expression opt> ';'
<try statement>                        ::= 'try' <block> <catch clauses>
                                         | 'try' <block> <catch clauses opt> <finally clause>
<catch clauses opt>                    ::= <catch clauses>
<catch clauses opt>                    ::=
<catch clauses>                        ::= <specific catch clauses>
                                         | <specific catch clauses opt> <general catch clause>
<specific catch clauses opt>           ::= <specific catch clauses>
<specific catch clauses opt>           ::=
<specific catch clauses>               ::= <specific catch clause>+
<specific catch clause>                ::= 'catch' '(' <type> <identifier opt> ')' <block>
<general catch clause>                 ::= 'catch' <block>
<finally clause>                       ::= 'finally' <block>
<checked statement>                    ::= 'checked' <block>
<unchecked statement>                  ::= 'unchecked' <block>
<lock statement>                       ::= 'lock' '(' <expression> ')' <embedded statement>
<using statement>                      ::= 'using' '(' <resource acquisition> ')' <embedded statement>
<resource acquisition>                 ::= <local variable declaration>
                                         | <expression>
<yield statement>                      ::= 'yield' 'return' <expression> ';'
                                         | 'yield' 'break' ';'

# ----------
# Namespaces
# ----------
<compilation unit>                     ::= <extern alias directives opt> <using directives opt> <global attributes opt> <namespace member declarations opt>
<namespace declaration>                ::= 'namespace' <qualified identifier> <namespace body> ';'
                                         | 'namespace' <qualified identifier> <namespace body>
<qualified identifier>                 ::= <identifier>+  separator => ',' proper => 1 # No hide-separator
<namespace body>                       ::= '{' <extern alias directives opt> <using directives opt> namespace member declarations opt> '}'
<extern alias directives opt>          ::= <extern alias directives>
<extern alias directives opt>          ::=
<using directives opt>                 ::= <using directives>
<using directives opt>                 ::=
<namespace member declarations opt>    ::= <namespace member declarations>
<namespace member declarations opt>    ::=
<extern alias directives>              ::= <extern alias directive>+
<extern alias directive>               ::= 'extern' 'alias' <identifier> ';'
<using directives>                     ::= <using directive>+
<using directive>                      ::= <using alias directive>
                                         | <using namespace directive>
<using alias directive>                ::= 'using' <identifier> '=' <namespace or type name> ';'
<using namespace directive>            ::= 'using' <namespace name> ';'
<namespace member declarations>        ::= <namespace member declaration>+
<namespace member declaration>         ::= <namespace declaration>
                                         | <type declaration>
<type declaration>                     ::= <class declaration>
                                         | <struct declaration>
                                         | <interface declaration>
                                         | <enum declaration>
                                         | <delegate declaration>
<qualified alias member>               ::= <identifier> '::' <identifier> <type argument list opt>
<type argument list opt>               ::= <type argument list>
<type argument list opt>               ::=

# -------
# Classes
# -------
<class declaration>                    ::= <attributes opt> <class modifiers opt> <partial opt> 'class' <identifier> <type parameter list opt> <class base opt> <type parameter constraints clauses opt> <class body> ';'
                                         | <attributes opt> <class modifiers opt> <partial opt> 'class' <identifier> <type parameter list opt> <class base opt> <type parameter constraints clauses opt> <class body>
<attributes opt>                       ::= <attributes>
<attributes opt>                       ::=
<class modifiers opt>                  ::= <class modifiers opt>
<class modifiers opt>                  ::=
<partial opt>                          ::= 'partial'
<partial opt>                          ::=
<type parameter list opt>              ::= <type parameter list>
<type parameter list opt>              ::=
<class base opt>                       ::= <class base>
<class base opt>                       ::=
<type parameter constraints clauses opt> ::= <type parameter constraints clauses>
<type parameter constraints clauses opt> ::=
<class modifiers>                      ::= <class modifier>+
<class modifier>                       ::= 'new'
                                         | 'public'
                                         | 'protected'
                                         | 'internal'
                                         | 'private'
                                         | 'abstract'
                                         | 'sealed'
                                         | 'static'
<type parameter list>                  ::= '<' <type parameters> '>'
<type parameters>                      ::= <attributes opt> <type parameter>
                                         | <type parameters> ',' <attributes opt> <type parameter>
<class base>                           ::= ':' <class type>
                                         | ':' <interface type list>
                                         | ':' <class type> ',' <interface type list>
<interface type list>                  ::= <interface type>+ separator => ','  proper => 1 hide-separator => 1
<type parameter constraints clauses>   ::= <type parameter constraints clause>+
<type parameter constraints clause>    ::= 'where' <type parameter> ':' <type parameter constraints>
<type parameter constraints>           ::= <primary constraint>
                                         | <secondary constraints>
                                         | <constructor constraint>
                                         | <primary constraint> ',' <secondary constraints>
                                         | <primary constraint> ',' <constructor constraint>
                                         | <secondary constraints> ',' <constructor constraint>
                                         | <primary constraint> ',' <secondary constraints> ',' <constructor constraint>
<primary constraint>                   ::= <class type>
                                         | 'class'
                                         | 'struct'
<secondary constraints>                ::= <interface type>
                                         | <type parameter>
                                         | <secondary constraints ',' <interface type>
                                         | <secondary constraints> ',' <type parameter>
<constructor constraint>               ::= 'new' '(' ')'
<class body>                           ::= '{' <class member declarations opt> '}'
<class member declarations opt>        ::= <class member declarations>
<class member declarations opt>        ::=
<class member declarations>            ::= <class member declaration>+
<class member declaration>             ::= <constant declaration>
                                         | <field declaration>
                                         | <method declaration>
                                         | <property declaration>
                                         | <event declaration>
                                         | <indexer declaration>
                                         | <operator declaration>
                                         | <constructor declaration>
                                         | <finalizer declaration>
                                         | <static constructor declaration>
                                         | <type declaration>
<constant declaration>                 ::= <attributes opt> <onstant modifiers opt> 'const' <type> <constant declarators> ';'
<constant modifiers>                   ::= <constant modifier>+
<constant modifier>                    ::= 'new'
                                         | 'public'
                                         | 'protected'
                                         | 'internal'
                                         | 'private'
<constant declarators>                 ::= <constant declarator>+ separator => ','  proper => 1 hide-separator => 1
<constant declarator>                  ::= <identifier> '=' <constant expression>
<field declaration>                    ::= <attributes opt> <field modifiers opt> <type> <variable declarators> ';'
<field modifiers>                      ::= <field modifier>+
<field modifier>                       ::= 'new'
                                         | 'public'
                                         | 'protected'
                                         | 'internal'
                                         | 'private'
                                         | 'static'
                                         | 'readonly'
                                         | 'volatile'
<variable declarators>                 ::= <variable declarator>+ separator => ','  proper => 1 hide-separator => 1
<variable declarator>                  ::= <identifier>
                                         | <identifier> '=' <variable initializer>
<variable initializer>                 ::= <expression>
                                         | <array initializer>
<method declaration>                   ::= <method header> <method body>
<method header>                        ::= <attributes opt> <method modifiers opt> <partial opt> <return type> <member name> <type parameter list opt> '(' <formal parameter list opt> ')' <type parameter constraints clauses opt>
<method modifiers>                     ::= <method modifier>+
<method modifier>                      ::= 'new'
                                         | 'public'
                                         | 'protected'
                                         | 'internal'
                                         | 'private'
                                         | 'static'
                                         | 'virtual'
                                         | 'sealed'
                                         | 'override'
                                         | 'abstract'
                                         | 'extern'
                                         | 'async'
<return type>                          ::= <type>
                                         | 'void'
<method body>                          ::= <block>
                                         | ';'
<formal parameter list>                ::= <fixed parameters>
                                         | <fixed parameters> ',' <parameter array>
                                         | <parameter array>
<fixed parameters>                     ::= <fixed parameter>+ separator => ','  proper => 1 hide-separator => 1
<fixed parameter>                      ::= <attributes opt> <parameter modifier opt> <type> <identifier> <default argument opt>
<default argument>                     ::= '=' <expression>
<parameter modifier>                   ::= <parameter mode modifier>
                                         | 'this'
<parameter mode modifier>              ::= 'ref'
                                         | 'out'
<parameter array>                      ::= <attributes opt> 'params' <array type> <identifier>
<property declaration>                 ::= <attributes opt> <property modifiers opt> <type> <member name> '{' <accessor declarations> '}'
<property modifiers>                   ::= <property modifier>+
<property modifier>                    ::= 'new'
                                         | 'public'
                                         | 'protected'
                                         | 'internal'
                                         | 'private'
                                         | 'static'
                                         | 'virtual'
                                         | 'sealed'
                                         | 'override'
                                         | 'abstract'
                                         | 'extern'
<accessor declarations>                ::= <get accessor declaration> <set accessor declaration opt>
                                         | <set accessor declaration> <get accessor declaration opt>
<get accessor declaration>             ::= <attributes opt> <accessor modifier opt> 'get' <accessor body>
<set accessor declaration>             ::= <attributes opt> <accessor modifier opt> 'set' <accessor body>
<accessor modifier>                    ::= 'protected'
                                         | 'internal'
                                         | 'private'
                                         | 'protected' 'internal'
                                         | 'internal' 'protected'
<accessor body>                        ::= <block>
                                         | ';'
<event declaration>                    ::= <attributes opt> <event modifiers opt> 'event' <type> <variable declarators> ';'
                                         | <attributes opt> <event modifiers opt> 'event' <type> <member name> '{' <event accessor declarations> '}'
<event modifiers>                      ::= <event modifier>+
<event modifier>                       ::= 'new'
                                         | 'public'
                                         | 'protected'
                                         | 'internal'
                                         | 'private'
                                         | 'static'
                                         | 'virtual'
                                         | 'sealed'
                                         | 'override'
                                         | 'abstract'
                                         | 'extern'
<event accessor declarations>          ::= <add accessor declaration> <remove accessor declaration>
                                         | <remove accessor declaration> <add accessor declaration>
<add accessor declaration>             ::= <attributes opt> 'add' <block>
<remove accessor declaration>          ::= <attributes opt> 'remove' <block>
<indexer declaration>                  ::= <attributes opt> <indexer modifiers opt> <indexer declarator> '{' <accessor declarations> '}'
<indexer modifiers>                    ::= <indexer modifier>+
<indexer modifier>                     ::= 'new'
                                         | 'public'
                                         | 'protected'
                                         | 'internal'
                                         | 'private'
                                         | 'virtual'
                                         | 'sealed'
                                         | 'override'
                                         | 'abstract'
                                         | 'extern'
<indexer declarator>                   ::= <type> 'this' '[' <formal parameter list> ']'
                                         | <type> <interface type> '.' 'this' '[' <formal parameter list> ']'
<operator declaration>                 ::= <attributes opt> <operator modifiers> <operator declarator> <operator body>
<operator modifiers>                   ::= <operator modifier>+
<operator modifier>                    ::= 'public'
                                         | 'static'
                                         | 'extern'
<operator declarator>                  ::= <unary operator declarator>
                                         | <binary operator declarator>
                                         | <conversion operator declarator>
<unary operator declarator>            ::= <type> 'operator' <overloadable unary operator> '(' <fixed parameter> ')'
<overloadable unary operator>          ::= '+'
                                         | '-'
                                         | '!'
                                         | '~'
                                         | '++'
                                         | '--'
                                         | 'true'
                                         | 'false'
<binary operator declarator>           ::= <type> 'operator' <overloadable binary operator> '(' <fixed parameter> ',' <fixed parameter> ')'
<overloadable binary operator>         ::= '+'
                                         | '=='
                                         | '-'
                                         | '!='
                                         | '*'
                                         | '>'
                                         | '/'
                                         | '<'
                                         | '%'
                                         | '>='
                                         | '&'
                                         | '<='
                                         | '|'
                                         | '^'
                                         | '<<'
                                         | <right shift>
<conversion operator declarator>       ::= 'implicit' 'operator' <type> '(' <fixed parameter> ')'
                                         | 'explicit' 'operator' <type> '(' <fixed parameter> ')'
<operator body>                        ::= <block>
                                         | ';'
<constructor declaration>              ::= <attributes opt> <constructor modifiers opt> <constructor declarator> <constructor body>
<constructor modifiers>                ::= <constructor modifier>+
<constructor modifier>                 ::= 'public'
                                         | 'protected'
                                         | 'internal'
                                         | 'private'
                                         | 'extern'
<constructor declarator>               ::= <identifier> '(' <formal parameter list opt> ')' <constructor initializer opt>
<constructor initializer>              ::= ':' 'base' '(' <argument list opt> ')'
                                         | ':' 'this' '(' <argument list opt> ')'
<constructor body>                     ::= <block>
                                         | ';'
<static constructor declaration>       ::= <attributes opt> <static constructor modifiers> <identifier> '(' ')' <static constructor body>
<static constructor modifiers>         ::= <extern opt> 'static'
                                         | 'static' <extern opt>
<extern opt>                           ::= 'extern'
<extern opt>                           ::=
<static constructor body>              ::= <block>
                                         | ';'
<finalizer declaration>                ::= <attributes opt> <extern opt> '~' <identifier> '(' ')' <finalizer body>
<finalizer body>                       ::= <block>
                                         | ';'

# -------
# Structs
# -------
