#!perl -T

use Test::More tests => 1;

BEGIN {
    use_ok( 'Bot::BasicBot::Pluggable::Module::Hangman' ) || print "Bail out!\n";
}

diag( "Testing Bot::BasicBot::Pluggable::Module::Hangman $Bot::BasicBot::Pluggable::Module::Hangman::VERSION, Perl $], $^X" );
