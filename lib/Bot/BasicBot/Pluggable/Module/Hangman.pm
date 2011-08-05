package Bot::BasicBot::Pluggable::Module::Hangman;

use strict;
no warnings;
use base 'Bot::BasicBot::Pluggable::Module';

use Carp;

=head1 NAME

Bot::BasicBot::Pluggable::Module::Hangman - IRC Hangman game

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

my $dictionary_file = '/usr/share/dict/words';
my $namespace = 'Reaper';

=head1 SYNOPSIS

A silly pointless IRC bot that provides a text version of Hangman for anyone
that's bored enough to play it :-)

    use Bot::BasicBot::Pluggable::Module::Hangman;
    my $hangman = Bot::BasicBot::Pluggable::Module::Hangman->new();

    # IRC
    !hangman       # Start a new game, uses a word from standard dictionary.
    !hangman-solo  # Like above, but exclusively for you.

    # Start a new PvP game using a specific word, if the optional nick is 
    # supplied then you challenge that person to a 1v1 game.
    /msg <bot-name> hangman <word> [<nick_of_player>] 

=head1 DESCRIPTION

Bored? Play Hangman!  Challenge the bot to guess it's word or play against 
others using you're own words.

=head2 Modes of Play

=over 4

=item General Play

The general game, invoked by saying !hangman in a channel where the bot resides,
is commenced with an underscored representation of the word to be guessed. The
word is selected from the dictionary file by default but a custom file can be
specified. 

Players make their guesses by addressing the bot with a single letter.  The bot
will then reveal if that letter was correct or not.  If it was correct, all 
occurrences of that letter will appear in place in the word.  On an incorrect 
guess however, a life is removed.  A guess of the complete word can be made at 
any time by addressing the bot with the complete word.  

The object of the game is simple, the players must get the word correct before 
they run out of lives.  The players can win by guessing the correct letters to 
complete the word or by directly guessing the word.  

All players can participate in this mode of play.

=item Solo Play

The solo game follows the same format as the general game, with the exception
that the bot will only accept guesses from the player that started the game.
The solo game is invoked by saying !hangman-solo.

=item Challenge Mode

Challenge mode presents the opportunity for players to go head to head.  A 
player can specify their own word(s) to challenge others.  This can be in both
the general play format or against a specific player.

To begin a challenge game, a player sends a private message to the bot
containing the chosen word(s), and optionally the nick of a player to challenge.

=head1 WISHLIST

Eventually I want to add the following:

=over 4

=item Rules access method

A !command to effect the game parameters, such as word length, number of lives
etc.


=back

=head1 METHODS

=head2 send

Overrides send from Bot::BasicBot::Pluggable

=cut

sub said {
    my ($self, $message, $priority) = @_;

    return unless $priority == 2;

    my $player    = $message->{who};
    my $address   = $message->{address};
    my $body      = $message->{body};
    my $games     = $self->bot->store->get($namespace, 'games') // {};
    my $game_name 
        = (defined $games->{$player}) ? $message->{who} : $message->{channel};
    my $game_data = $games->{$game_name};

    # If we've been privately messaged, or it's a public message then we need 
    # to look for a command. 
    if (   !$address
        ||  $address eq 'msg') 
    {

        # First off, look for a command to start the game.
        # FIXME: the private message for challenge games doesn't work. Replies
        # to the person, not the channel....
        # !hangman command...
        my $pling_optional = ($address) ? '?' : ''; 
        if ($body =~ /^
                !$pling_optional
                (?<command>hangman(?:-solo)?)
                (?:
                    \s+
                    (?:challenge \s+ )?
                    (?<channel> \#\S+ ):(?<nick> \S+ )
                    \s+
                    (?:with \s+ )?
                    (?<words> .+)
                )?
            /xi)
        {
            my %named_matches = %+;
            return $self->_begin_game(
                player  => $player,
                message => $message,
                command => \%named_matches,
            );
        }

        # !state command, outputs the current wordstate.
        if ($body =~ /^!state/) {
            return $self->_game_state($game_name);
        }

        # !guesses command, outputs the current failed attempts.
        if ($body =~ /^!guesses/) {
            my $letters = join ", ", @{ $game_data->{guesses} };
            my $words   = join ", ", @{ $game_data->{guessedwords} }; 
            return  "Guessed letters: $letters\nGuessed Words: $words";
        }

        # !games command, lists the games in progress.
        if ($body =~ /^!games/) {
            my $games = join ", ", keys %$games;
            return "Current games: $games";
        }

        if ($body =~ /^!endgame \s (?<game> \#?\S+ )/xi) {
            my $deleted = delete $games->{$+{game}} 
                if exists( $games->{$+{game}} );
            $self->bot->store->set($namespace, 'games', $games);
            return ($deleted) ? "Game ended: $+{game}" : "No game: $+{game}";
        }

        # Add the ability to take a single unaddressed letter to be a guess.
        if (length $body == 1) {
            return $self->_process_guess($message);
        }

    }

    if ($address) {
        # If we've been addressed then someone is making a guess.
        return $self->_process_guess($message);
    }

    # Failing that, the message was nothing to do with us.
    return undef;
}

# create a game hashref in the %games hash, keyed on the challenged player; 
# storing the word, the current word state, guessed letters and the current 
# number of lives.  This hash is used by _process_guess() to keep track and 
# advance the game.

sub _begin_game {
    my ($self, %params) = @_;

    my $bot     = $self->bot;
    my $player  = $params{player};
    my $command = $params{command};
    my $message = $params{message};

    # Construct the game.
    my $game_data = {};
    my $game_name =
        ( $command->{command} eq 'hangman-solo' ) ? $player
      : ( $command->{nick} ) ? $command->{nick}
      :                        $message->{channel};
    my $command_channel = $command->{channel};

    if (!$game_name) {
        return "Internal Fail... no game name...";
    }

    my $games = $bot->store->get( $namespace, 'games' ) // {};
    if ( exists $games->{$game_name} ) {
        return "A game is already in progress!";
    }

    $game_data = {
        guesses      => [],
        guessedwords => [],
        lives        => 10,
        word         => ($command->{words})
            ? lc $command->{words}
            : lc _get_dict_word(),
    };
    ($game_data->{wordstate} = $game_data->{word}) =~ s/\w/_ /g;

    # Now add the game data to the games store.
    $games->{$game_name} = $game_data;
    $bot->store->set($namespace, 'games', $games);

    # If this was a challenge game, it needs to return to the channel, else it
    # can respond to the sender.
    if ($command->{channel}) {
        $self->bot->say(
            who     => $command->{nick},
            channel => $command->{channel} || $message->{channel},
            body    => "You have been challenged by $player\n"
                      .$self->_game_state($game_name),
            address => $command->{nick},
        );
    } else {
        return $self->_game_state($game_name);
    }
}


# Process a guess and progress the game.

sub _process_guess {
    my ($self, $message) = @_;

    my $player    = $message->{who};
    my $addressby = ($message->{address}) ? '' : "$player: ";
    my $games     = $self->bot->store->get($namespace, 'games');
    my $game_name = ( exists( $games->{$player} ) ) 
        ? $player 
        : $message->{channel};
    my $data      = $games->{$game_name};
    my $guess     = lc $message->{body};

    # Make sure there is actually a game!
    if (!$data) {
        return "${addressby}There is no game currently in progress!";
    }

    # If they guess a letter, look for it.
    if (length $guess == 1) {

        if ($guess ~~ @{ $data->{guesses} }) {
            return "${addressby}That letter has already been guessed";
        }

        # Add the guess to the guesses
        push @{ $data->{guesses} }, $guess;

        # It's good! Update the state...
        if ($data->{word} =~ m/$guess/) {
            my @word  = split //, $data->{word};
            my @state = split / /, $data->{wordstate};

            for (my $i = 0; $i < length $data->{word}; $i++) {
                if (   $state[$i] eq "_" 
                    && $word[$i]  eq $guess) 
                {
                    $state[$i] = $word[$i];    
                }
            }
            $data->{wordstate} = join " ", @state;

        # Failing that, it's not good, decrement the lives.
        } else {
            $data->{lives}--;
        }
    
    # Else, assume it's a word.
    } else {
        my $word = $data->{word};
        chomp $word;

        if ($word ~~ @{ $data->{guessedwords} }) {
            return "That word has been guessed!";
        }

        push @{ $data->{guessedwords} }, $guess;

        if ($guess eq $word) {
            ($data->{wordstate} = $word) =~ s/(\w)/\1 /;
        } else {
            $data->{lives}--;
        }
    }

    $self->bot->store->set($namespace, 'games', $games );
    return $self->_game_state($game_name);
}


# Gets a word from the dict file, if the file is missing, sets the word
# to NODICT.  This should be irritating enough to make the admin fix it.

sub _get_dict_word {
    my ($self) = @_;

    if ( ! -e $dictionary_file) {
        return "NODICT";
    }

    open my $dict_fh, '<', $dictionary_file;
    my @words = <$dict_fh>;
    close $dict_fh;

    my $word = $words[ sprintf("%2d", rand($#words)) ];
    $word =~ s/'s$//;

    chomp($word);

    return $word;
}


# Prints the game state line "public: _ _ _ _ 10/10"

sub _game_state {
    my ($self, $game_name) = @_;

    # Get the game data.
    my $games = $self->bot->store->get($namespace, 'games') // {};
    my $data = $games->{$game_name}; 
    if (!$data) {
        return "There is currently no game in progess :-(";
    }
    
    # Check for teh win
    if ($data->{wordstate} !~ m/_/) {
        delete $games->{$game_name};
        $self->bot->store->set($namespace, 'games', $games);
        return "\\o/ Congratulations, You win! Word: $data->{word}";
    }
    
    # Check for teh lose.
    if ($data->{lives} == 0) {
        delete $games->{$game_name};
        $self->bot->store->set($namespace, 'games', $games);
        return ":-( You lose! Word: $data->{word}";
    }

    # Build a state line and return it to be said to the channel.
    my $wordstate = $data->{wordstate};
    chomp $wordstate;
    return "$game_name:   $wordstate    $data->{lives}/10";
}


=head1 AUTHOR

James Ronan, C<< <james at ronanweb.co.uk> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-bot-basicbot-pluggable-module-hangman at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Bot-BasicBot-Pluggable-Module-Hangman>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Bot::BasicBot::Pluggable::Module::Hangman


You can also look for information at:

=over 4

=item * RT: CPAN's request tracker (report bugs here)

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Bot-BasicBot-Pluggable-Module-Hangman>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Bot-BasicBot-Pluggable-Module-Hangman>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Bot-BasicBot-Pluggable-Module-Hangman>

=item * Search CPAN

L<http://search.cpan.org/dist/Bot-BasicBot-Pluggable-Module-Hangman/>

=back


=head1 ACKNOWLEDGEMENTS


=head1 LICENSE AND COPYRIGHT

Copyright 2011 James Ronan.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; version 2 dated June, 1991 or at your option
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

A copy of the GNU General Public License is available in the source tree;
if not, write to the Free Software Foundation, Inc.,
59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.


=cut

1; # End of Bot::BasicBot::Pluggable::Module::Hangman
