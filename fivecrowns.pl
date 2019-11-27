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
    Game = State,
    write("Game loaded "), write(FullPath), nl, nl.

getGameFromFile(Game) :-
    getGameFromFile(NewGame),
    Game = NewGame.

newGame(_):-
    coinToss(NextPlayer),
    playGame(1, [], 0, [], 0, NextPlayer).
    
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
    

playGame(RoundNum, HumanHand, HumanScore, CompHand, CompScore, NextPlayer):- 
    generateNewRound(RoundNum, HumanHand, HumanScore, CompHand, CompScore, NextPlayer, GameState),
    loadGame(GameState).

generateNewRound(RoundNum, HumanHand, HumanScore, CompHand, CompScore, NextPlayer, GameState):-
    UnshuffledDeck = ['j1', 'j2', 'j3', 't3', 't4', 't5', 't6', 't7', 't8', 't9', 'tx', 'tj', 'tq', 'tk', 'c3', 'c4', 'c5', 'c6', 'c7', 'c8', 'c9', 'cx', 'cj', 'cq', 'ck', 's3', 's4', 's5', 's6', 's7', 's8', 's9', 'sx', 'sj', 'sq', 'sk', 'd3', 'd4', 'd5', 'd6', 'd7', 'd8', 'd9', 'dx', 'dj', 'dq', 'dk', 'h3', 'h4', 'h5', 'h6', 'h7', 'h8', 'h9', 'hx', 'hj', 'hq', 'hk',
                        'j1', 'j2', 'j3', 't3', 't4', 't5', 't6', 't7', 't8', 't9', 'tx', 'tj', 'tq', 'tk', 'c3', 'c4', 'c5', 'c6', 'c7', 'c8', 'c9', 'cx', 'cj', 'cq', 'ck', 's3', 's4', 's5', 's6', 's7', 's8', 's9', 'sx', 'sj', 'sq', 'sk', 'd3', 'd4', 'd5', 'd6', 'd7', 'd8', 'd9', 'dx', 'dj', 'dq', 'dk', 'h3', 'h4', 'h5', 'h6', 'h7', 'h8', 'h9', 'hx', 'hj', 'hq', 'hk'],
    random_permutation(UnshuffledDeck, Deck),
    Num is RoundNum + 2,
    distributeHand(Num, Deck, NHumanHand, NewDeck),
    distributeHand(Num, NewDeck, NCompHand, NewNewDeck),
    popTopCard(NewNewDeck, DrawPile, Top),
    discardToPile(Top, [], DiscardPile),
    GameState = [RoundNum, CompScore, NCompHand, HumanScore, NHumanHand, DrawPile, DiscardPile, NextPlayer].

distributeHand(0, OldDeck, [], NewDeck):- 
    NewDeck = OldDeck.

distributeHand(NumCards, [DeckFirst|DeckRest], Hand, RemainingDeck):-
    NewNumCards is NumCards - 1,
    distributeHand(NewNumCards, DeckRest, NewHand, NewDeck),
    [DeckFirst | NewHand] = Hand,
    NewDeck = RemainingDeck.

popTopCard(From, Returning, Card):-
    [Top | Rest] = From,
    Card = Top,
    Returning = Rest.

distributeCards(0, CardsToDistribute, [], CardsAfterDistribution):- CardsAfterDistribution = CardsToDistribute.

distributeCards(Num, CardsToDistribute, Hand, CardsAfterDistribution):-
    write(Num),nl,
    NewNum is Num - 1,
    [H | T] = CardsToDistribute,
    distributeCards(NewNum, T, NewHand, NewCardsDistribution),
    [H | NewHand] = Hand,
    NewCardsDistribution = CardsAfterDistribution.

discardToPile(Card, DiscardPile, NewDiscardPile):- 
    [Card | DiscardPile] = NewDiscardPile.

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