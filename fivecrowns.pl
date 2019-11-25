fivecrowns(_):- 
    write("Welcome to Five Crowns!"),nl,
    write("Would you like to load a game?(y/n)"),
    read(Choice),
    validateYesNoChoice(Choice),
    startGame(Choice).

fivecrowns(_):- 
    fivecrowns(_).

startGame(y):- 
    getGameFromFile(Game),
    loadGame(Game).

startGame(n):- 
    newGame(_).

getGameFromFile(Game):- 
    write("Enter Saved Game Name: "),
    read(FileName),
    with_output_to(atom(AtomFileName), write(FileName)),
    string_concat("./", AtomFileName, FullPath),
    exists_file(FullPath),
    open(FullPath, read, File),
    read(File, State),
    close(File),
    Game=State,
    write("Game loaded "), write(FullPath), nl, nl.

getGameFromFile(Game) :-
    getGameFromFile(NewGame),
    Game = NewGame.

newGame(_):-
    coinToss(NextPlayer),
    

coinToss(NextPlayer):-
    random_between(0, 1, R),
    nl, write("Heads (1) or Tails (0): "),
    read(C),
    C = R,
    write("You won the toss! Human plays first."), nl, nl,
    NextPlayer = human.

coinToss(NextPlayer):-
    write("You lost the toss! Computer plays first."), nl, nl,
    NextPlayer = computer.

loadGame(Game):- 
    displayRoundStatus(Game).

displayRoundStatus(State):-
    getRoundNumber(State, RoundNumber),
    getComputerScore(State, CompScore),
    getComputerHand(State, CompHand),
    getHumanScore(State, HumanScore),
    getHumanHand(State, HumanHand),
    getDrawPile(State, DrawPile),
    getDiscardPile(State, DiscardPile),

    write("=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"),
    nl,
    write("Round: "), write(RoundNumber), nl,
    write("Computer: "), nl,
    write("\tScore: "), write(CompScore), nl,
    write("\tHand: "), write(CompHand), nl,
    write("Human: "), nl,
    write("\tScore: "), write(HumanScore), nl,
    write("\tHand: "), write(HumanHand), nl,
    nl,
    write("Draw Pile: "), write(DrawPile), nl, nl,
    write("Discard Pile: "), write(DiscardPile), nl,
    write("=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"),
    nl.

getRoundNumber(State, Number):- nth0(0, State, Number).

getComputerScore(State, Score):- nth0(1, State, Score).

getComputerHand(State, Hand):- nth0(2, State, Hand).

getHumanScore(State, Score):- nth0(3, State, Score).

getHumanHand(State, Hand):- nth0(4, State, Hand).

getDrawPile(State, Pile):- nth0(5, State, Pile).

getDiscardPile(State, Pile):- nth0(6, State, Pile).

getNextPlayer(State, NextPlayer):- nth0(7, State, NextPlayer).

validateYesNoChoice(y).
validateYesNoChoice(n).

printChoice(Choice):- write(Choice).