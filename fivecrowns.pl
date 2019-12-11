%                   ************************************************************
%                   * Name: Bibhash Mulmi                                      *
%                   * Project: Project 4, Five Crowns Prolog                   *
%                   * Class: Fall 2019 OPL                                     *
%                   * Date: 12/11/2019                                         *
%                   ************************************************************

%---------------------------------------------------------------------------------------------------------
%------------------------------------ Game Handling Predicates -------------------------------------------
%---------------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------------
% Predicate: fivecrowns
% Purpose: To start the game and load serialized files
% Parameters: none
% Local Variables: Choice
%---------------------------------------------------------------------------------------------------------
fivecrowns(_):- 
    write("Welcome to Five Crowns!"),nl,
    write("Would you like to load a game?(y/n)"),
    read(Choice),
    validateYesNoChoice(Choice),
    startGame(Choice).

fivecrowns(_):- 
    fivecrowns(_).

%---------------------------------------------------------------------------------------------------------
% Predicate: startGame
% Purpose: To start a new game or load a game from file
% Parameters: y and n, choices for starting a new game
% Local Variables: Game
%---------------------------------------------------------------------------------------------------------
startGame(y):- 
    getGameFromFile(Game),
    loadGame(Game).
startGame(n):- 
    newGame(_).

%---------------------------------------------------------------------------------------------------------
% Predicate: getGameFromFile
% Purpose: to read the serialized file 
% Parameters: Game, to return the loaded data from serialized file
% Local Variables: 
%       FileName, to get the filename of the file to load
%       FullPath, to store the path of the file
%       State, to store the loaded data from FileName
%---------------------------------------------------------------------------------------------------------
getGameFromFile(Game):- 
    write("Enter Saved Game Name: "),
    read(FileName),
    with_output_to(atom(AtomFileName), write(FileName)),
    string_concat("./", AtomFileName, FullPath),
    exists_file(FullPath),
    open(FullPath, read, File),
    read(File, State),
    close(File),
    Game = State.
getGameFromFile(Game) :-
    getGameFromFile(NewGame),
    Game = NewGame.

%---------------------------------------------------------------------------------------------------------
% Predicate: newGame
% Purpose: to start a fresh game 
% Parameters: none
% Local Variables: NextPlayer, the player to go first
%---------------------------------------------------------------------------------------------------------
newGame(_):-
    coinToss(NextPlayer),
    playGame(1, 0, 0, NextPlayer, false).
    
%---------------------------------------------------------------------------------------------------------
% Predicate: coinToss
% Purpose: to toss a coin and declare the next player
% Parameters: NextPlayer, to store the next player to play
% Local Variables: 
%       C, the user's choice
%       R, the random number generated            
%---------------------------------------------------------------------------------------------------------
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

%---------------------------------------------------------------------------------------------------------
% Predicate: loadGame
% Purpose: to load the game into the program and play the rounds
% Parameters: Game, the state to be played
% Local Variables: choice, stores y and n values for save and quit
%---------------------------------------------------------------------------------------------------------
loadGame(Game):- 
    askIfSaveAndQuit(Choice),
    runRound(Game, [CScore, HScore, StartingPlayer | _], Choice),
    Game = [RoundNum, CompScore, _, HumanScore, _, _, _, _],
    NewRoundNum is RoundNum + 1,
    NewCompScore is CompScore + CScore,
    NewHumanScore is HumanScore + HScore,
    playGame(NewRoundNum, NewHumanScore, NewCompScore, StartingPlayer, Choice).

%---------------------------------------------------------------------------------------------------------
% Predicate: playGame
% Purpose: to play the game
% Parameters: RoundNum, HumanScore, CompScore, NextPlayer, true/false
% Local Variables: none
%---------------------------------------------------------------------------------------------------------
% when player decides to save and quit
playGame(_, _, _, _, true).

% when the game ends
playGame(12, HumanScore, CompScore, _, _):- 
    write("**********************************GAME OVER**********************************"),nl,
    getWinner(HumanScore, CompScore, Winner, Loser, WinnerScore, LoserScore),
    write(Winner), write(" is the winner!"),nl,
    write(Winner), write(" Score: "), write(WinnerScore),nl,
    write(Loser), write(" Score: "), write(LoserScore),nl.

% new game
playGame(RoundNum, HumanScore, CompScore, NextPlayer, _):- 
    generateNewRound(RoundNum, HumanScore, CompScore, NextPlayer, GameState),
    loadGame(GameState).

%---------------------------------------------------------------------------------------------------------
% Predicate: getWinner
% Purpose: To decide the winner of the game
% Parameters: HumanScore, CompScore
% Local Variables: none
%---------------------------------------------------------------------------------------------------------
getWinner(HumanScore, CompScore, human, computer, HumanScore, CompScore):-
    HumanScore < CompScore.

getWinner(HumanScore, CompScore, computer, human, CompScore, HumanScore):-
    CompScore < HumanScore.

%---------------------------------------------------------------------------------------------------------
% Predicate: runRound
% Purpose: to play the round, alternate player turns until round ends
% Parameters: current game state, new game state, y/n/last for flagging save and quit and last round
% Local Variables: none
%---------------------------------------------------------------------------------------------------------
% when player decides to save and quit
runRound(OldGameState, OldGameState, y):-
    open("./save.txt", write, Stream),
    write(Stream, OldGameState),
    write(Stream, "."),
    close(Stream),
    write("Game saved to ./save.txt"),nl,
    write("Exiting the game..."), nl,
    halt(0).

% when the round ends
runRound(OldGameState, RoundResults, last):-
    OldGameState = [RoundNum, _, CompHand, _, HumanHand, _, _, Loser],
    write("The round has ended"), nl,
    write(Loser), write(" lost the round."),nl,

    getLowestScore(CompHand, RoundNum, [], CompAssembled, CompHandScr),
    getLowestScore(HumanHand, RoundNum, [], HumanAssembled, HumanHandScr),

    write("Human Hand: "), write(HumanAssembled), write(" Score: "), write(HumanHandScr), nl,
    write("Computer Hand: "), write(CompAssembled), write(" Score: "), write(CompHandScr), nl,

    changePlayer(OldGameState, NewState),
    getNextPlayer(NewState, StartingPlayer),
    RoundResults = [CompHandScr, HumanHandScr, StartingPlayer].

% regular flow
runRound(OldGameState, NewGameState, n):-
    getNextPlayer(OldGameState, Player),
    \+ checkIfPlayerCanGoOut(OldGameState, Player),
    displayRoundStatus(OldGameState),
    playRound(OldGameState, NewState),
    %\+ checkIfPlayerCanGoOut(NewState, Player),
    %write(Player), write(" cannot go out"),nl,
    askIfSaveAndQuit(Choice),
    changePlayer(NewState, NewerState),
    runRound(NewerState, NewestState, Choice),
    NewGameState = NewestState.

% when the player who just played went out, play last round
runRound(GameState, NewGameState, n):-
    getNextPlayer(GameState, Player),
    checkIfPlayerCanGoOut(GameState, Player),
    
    changePlayer(GameState, NewState2),
    getNextPlayer(NewState2, CurrPlayer),

    write("~^~^~^~^~^~^~^~^~^~^~^~"), nl,
    write(CurrPlayer), write(" has gone out."), nl,
    write("~^~^~^~^~^~^~^~^~^~^~^~"), nl,

    displayRoundStatus(GameState),
    playRound(GameState, NewerState),
    runRound(NewerState, NewestState, last),
    NewGameState = NewestState.

%---------------------------------------------------------------------------------------------------------
% Predicate: changePlayer
% Purpose: to change the player of the current state
% Parameters: current game state, new game state
% Local Variables: NextPlayer, stores the current player
%---------------------------------------------------------------------------------------------------------
changePlayer(GameState, NewGameState):-
    GameState = [RoundNum, CompScore, CompHand, HumanScore, HumanHand, DrawPile, DiscardPile, NextPlayer],
    NextPlayer = human,
    NewGameState = [RoundNum, CompScore, CompHand, HumanScore, HumanHand, DrawPile, DiscardPile, computer].

changePlayer(GameState, NewGameState):-
    GameState = [RoundNum, CompScore, CompHand, HumanScore, HumanHand, DrawPile, DiscardPile, NextPlayer],
    NextPlayer = computer,
    NewGameState = [RoundNum, CompScore, CompHand, HumanScore, HumanHand, DrawPile, DiscardPile, human].

%---------------------------------------------------------------------------------------------------------
% Predicate: checkIfPlayerCanGoOut
% Purpose: to check if the current player can go out
% Parameters: current game state, current player
% Local Variables: none
%---------------------------------------------------------------------------------------------------------
checkIfPlayerCanGoOut(GameState, computer):-
    getHumanHand(GameState, Hand),
    getRoundNumber(GameState, RoundNum),
    getLowestScore(Hand, RoundNum, [], _, Score),
    Score = 0.

checkIfPlayerCanGoOut(GameState, human):-
    getComputerHand(GameState, Hand),
    getRoundNumber(GameState, RoundNum),
    getLowestScore(Hand, RoundNum, [], _, Score),
    Score = 0.

%---------------------------------------------------------------------------------------------------------
% Predicate: playRound
% Purpose: to let the player play their turns
% Parameters: current game state, new game state
% Local Variables: NewState, the new state after player plays its turn
%---------------------------------------------------------------------------------------------------------
playRound(OldGameState, NewGameState):- 
    getNextPlayer(OldGameState, Turn),
    Turn = human,
    getHumanMenuAction(OldGameState, NewState),
    NewGameState = NewState.

playRound(OldGameState, NewGameState):-
    getNextPlayer(OldGameState, Turn),
    Turn = computer,
    getComputerMove(OldGameState, NewState),
    NewGameState = NewState.

%---------------------------------------------------------------------------------------------------------
% Predicate: generateNewRound
% Purpose: to generate a new round with fresh deck of cards
% Parameters: RoundNum, HumanScore, CompScore, NextPlayer, GameState
% Local Variables: Num, to store the number of cards to be dealt
%---------------------------------------------------------------------------------------------------------
generateNewRound(RoundNum, HumanScore, CompScore, NextPlayer, GameState):-
    UnshuffledDeck = ['j1', 'j2', 'j3', 't3', 't4', 't5', 't6', 't7', 't8', 't9', 'tx', 'tj', 'tq', 'tk', 'c3', 'c4', 'c5', 'c6', 'c7', 'c8', 'c9', 'cx', 'cj', 'cq', 'ck', 's3', 's4', 's5', 's6', 's7', 's8', 's9', 'sx', 'sj', 'sq', 'sk', 'd3', 'd4', 'd5', 'd6', 'd7', 'd8', 'd9', 'dx', 'dj', 'dq', 'dk', 'h3', 'h4', 'h5', 'h6', 'h7', 'h8', 'h9', 'hx', 'hj', 'hq', 'hk',
                        'j1', 'j2', 'j3', 't3', 't4', 't5', 't6', 't7', 't8', 't9', 'tx', 'tj', 'tq', 'tk', 'c3', 'c4', 'c5', 'c6', 'c7', 'c8', 'c9', 'cx', 'cj', 'cq', 'ck', 's3', 's4', 's5', 's6', 's7', 's8', 's9', 'sx', 'sj', 'sq', 'sk', 'd3', 'd4', 'd5', 'd6', 'd7', 'd8', 'd9', 'dx', 'dj', 'dq', 'dk', 'h3', 'h4', 'h5', 'h6', 'h7', 'h8', 'h9', 'hx', 'hj', 'hq', 'hk'],
    random_permutation(UnshuffledDeck, Deck),
    Num is RoundNum + 2,
    distributeHand(Num, Deck, HumanHand, NewDeck),
    distributeHand(Num, NewDeck, CompHand, NewNewDeck),
    popTopCard(NewNewDeck, DrawPile, Top),
    discardToPile(Top, [], DiscardPile),
    GameState = [RoundNum, CompScore, CompHand, HumanScore, HumanHand, DrawPile, DiscardPile, NextPlayer].

%---------------------------------------------------------------------------------------------------------
%-------------------------------- Computer's Move Predicates ---------------------------------------------
%---------------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------------
% Predicate: getComputerMove 
% Purpose: to pick and discard cards for computer using strategy
% Parameters: current game state, new game state
% Local Variables: Hint, to store the strategy computer used 
%---------------------------------------------------------------------------------------------------------
getComputerMove(GameState, NewGameState):-
    getWhichPileHint(GameState, Hint),
    Hint = discard,
    write("Computer chose >> discard pile << to pick the card because it helped in making more runs or books."),nl,
    computerChooseDiscardPile(GameState, NewState),
    NewGameState = NewState.

getComputerMove(GameState, NewGameState):-
    getWhichPileHint(GameState, Hint),
    Hint = draw,
    write("Computer chose >> draw pile << to pick the card because discard pile did not help in making more runs or books"),nl,
    computerChooseDrawPile(GameState, NewState),
    NewGameState = NewState.

%---------------------------------------------------------------------------------------------------------
% Predicate: computerChooseDrawPile
% Purpose: to let computer pick from draw pile and discard a card
% Parameters: current game state, new game state
% Local Variables: 
%       Card, to store the draw pile top card
%       TempHand, to store the intermediate hand
%       CardToDiscard, to store the card to discard
%---------------------------------------------------------------------------------------------------------
computerChooseDrawPile(GameState, NewGameState):-
    getDrawPile(GameState, DrawPile),
    getDiscardPile(GameState, DiscardPile),
    getComputerHand(GameState, Hand),
    getRoundNumber(GameState, RoundNum),

    popTopCard(DrawPile, NewDrawPile, Card),
    [Card | Hand] = TempHand,

    getWhichCardHint(RoundNum, TempHand, CardToDiscard),
    write("Computer discarded >> "), write(CardToDiscard), write(" << because it made the score lower."), nl, nl,

    removeCardFromHand(CardToDiscard, TempHand, NewHand),
    discardToPile(CardToDiscard, DiscardPile, NewDiscardPile),

    getLowestScore(NewHand, RoundNum, [], CurrAssembled, CurrScore),
    write("Computer Hand: "), write(CurrAssembled), write(" Score: "), write(CurrScore),nl,

    GameState = [_, CompScore, _, HumanScr, HumanHand, _, _, NextPlayer],
    NewGameState = [RoundNum, CompScore, NewHand, HumanScr, HumanHand, NewDrawPile, NewDiscardPile, NextPlayer].

%---------------------------------------------------------------------------------------------------------
% Predicate: computerChooseDiscardPile
% Purpose: to let computer pick from discard pile and discard a card
% Parameters: current game state, new game state
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
computerChooseDiscardPile(GameState, NewGameState):-
    getDiscardPile(GameState, DiscardPile),
    getComputerHand(GameState, Hand),
    getRoundNumber(GameState, RoundNum),

    popTopCard(DiscardPile, NewDiscardPile, Card),
    [Card | Hand] = TempHand,

    getWhichCardHint(RoundNum, TempHand, CardToDiscard),
    write("Computer discarded >> "), write(CardToDiscard), write(" << because it made the score lower."), nl, nl,

    removeCardFromHand(CardToDiscard, TempHand, NewHand),

    getLowestScore(NewHand, RoundNum, [], CurrAssembled, CurrScore),
    write("Computer Hand: "), write(CurrAssembled), write(" Score: "), write(CurrScore),nl,
    
    discardToPile(CardToDiscard, NewDiscardPile, NewerDiscardPile),
    
    GameState = [_, CompScore, _, HumanScr, HumanHand, DrawPile, _, NextPlayer],
    NewGameState = [RoundNum, CompScore, NewHand, HumanScr, HumanHand, DrawPile, NewerDiscardPile, NextPlayer].

%---------------------------------------------------------------------------------------------------------
%------------------------------- Predicates for Hints ----------------------------------------------------
%---------------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------------
% Predicate: getWhichPileHint
% Purpose: to get the pile to choose hint
% Parameters: current GameState, Hint of the pile to be returned
% Local Variables: none
%---------------------------------------------------------------------------------------------------------
% for human player
getWhichPileHint(GameState, Hint):-
    getNextPlayer(GameState, Player),
    Player = human,
    getRoundNumber(GameState, RoundNum),
    getHumanHand(GameState, Hand),
    getLowestScore(Hand, RoundNum, [], CurrAssembled, CurrScore),
    
    getDiscardPile(GameState, DiscardPile),
    popTopCard(DiscardPile, _, TopCard),
    NewHand = [TopCard | Hand],

    getListOfEachElementRemoved(NewHand, NewHand, ListHands),
    [_ | WithDCard] = ListHands,

    getWhichPileToChoose(WithDCard, RoundNum, CurrScore, CurrAssembled, Pile),
    Hint = Pile.

% for computer player
getWhichPileHint(GameState, Hint):-
    getNextPlayer(GameState, Player),
    Player = computer,
    getRoundNumber(GameState, RoundNum),
    getComputerHand(GameState, Hand),
    getLowestScore(Hand, RoundNum, [], CurrAssembled, CurrScore),

    getDiscardPile(GameState, DiscardPile),
    popTopCard(DiscardPile, _, TopCard),
    NewHand = [TopCard | Hand],

    getListOfEachElementRemoved(NewHand, NewHand, ListHands),
    [_ | WithDCard] = ListHands,

    getWhichPileToChoose(WithDCard, RoundNum, CurrScore, CurrAssembled, Pile),
    Hint = Pile.

%---------------------------------------------------------------------------------------------------------
% Predicate: getWhichPileToChoose
% Purpose: to check every possible hand with discard card and see if it helps in getting more books and runs
% Parameters: List of all possible hands, round number, score of current hand, assembled hand, pile to choose
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
getWhichPileToChoose([], _, _, _, draw).
getWhichPileToChoose(ListHands, RoundNum, Score, Assembled, Pile):-
    [First | _] = ListHands,
    getLowestScore(First, RoundNum, [], Assembled1, Score1),
    Score1 < Score,
    hasAddedToBooksOrRuns(Assembled, Assembled1, Score1),
    Pile = discard.
getWhichPileToChoose(ListHands, RoundNum, Score, Assembled, Pile):-
    [_ | Rest] = ListHands,
    getWhichPileToChoose(Rest, RoundNum, Score, Assembled, NewPile),
    Pile = NewPile.

%---------------------------------------------------------------------------------------------------------
% Predicate: hasAddedToBooksOrRuns
% Purpose: to compare assembled hands and check if it has less unaccounted cards
% Parameters: Previous assembled hand, current assembled hand, current score of hand
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
hasAddedToBooksOrRuns(_, _, 0).
hasAddedToBooksOrRuns(PrevAssembled, CurrAssembled, _):-
    getLastElement(PrevAssembled, PrevLast),
    getLastElement(CurrAssembled, CurrLast),
    length(PrevLast, PrevLen),
    length(CurrLast, CurrLen),
    CurrLen < PrevLen,
    write(PrevLast), write(" <to> "), write(CurrLast),nl.

%---------------------------------------------------------------------------------------------------------
% Predicate: getLastElement
% Purpose: to get the last element of the list
% Parameters: list, last element of the list
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
getLastElement([Last], Last).
getLastElement([_|Rest], Last):-
    getLastElement(Rest, NewLast),
    Last = NewLast.

%---------------------------------------------------------------------------------------------------------
% Predicate: getWhichCardHint
% Purpose: to get which card to discard hint
% Parameters: Round number, hand of the player, hint to be returned
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
getWhichCardHint(RoundNum, Hand, Hint):-
    getListOfEachElementRemoved(Hand, Hand, ListHands),
    getWhichCardToDiscard(Hand, ListHands, RoundNum, _, Card),
    Hint = Card.

%---------------------------------------------------------------------------------------------------------
% Predicate: getWhichCardToDiscard
% Purpose: to get the card to be discarded for a lower hand score
% Parameters: Hand of the player, list of possible hands without each card removed, round number, score of 
%               hand, card to be discarded. 
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
getWhichCardToDiscard(_, [], _, 9999, garbage).

% do not discard the joker cards
getWhichCardToDiscard(Hand, ListHands, RoundNum, Score, Card):-
    [First | Rest] = ListHands,
    subtract(Hand, First, TempCard),
    [DiscardedCard1|_] = TempCard,
    isJoker(DiscardedCard1),
    getWhichCardToDiscard(Hand, Rest, RoundNum, Score, Card).

% do not discard the wild cards
getWhichCardToDiscard(Hand, ListHands, RoundNum, Score, Card):-
    [First | Rest] = ListHands,
    subtract(Hand, First, TempCard),
    [DiscardedCard1|_] = TempCard,
    isWildCard(DiscardedCard1, RoundNum),
    getWhichCardToDiscard(Hand, Rest, RoundNum, Score, Card).

% check for regular cards
getWhichCardToDiscard(Hand, ListHands, RoundNum, Score, Card):-
    [First | Rest] = ListHands,
    getLowestScore(First, RoundNum, [], _, Score1),
    subtract(Hand, First, TempCard),
    [DiscardedCard1|_] = TempCard,
    getWhichCardToDiscard(Hand, Rest, RoundNum, Score2, DiscardedCard2),
    getTheLowerScoreCombo(DiscardedCard1, Score1, DiscardedCard2, Score2, DCard, DScore),
    Score = DScore,
    Card = DCard.

%---------------------------------------------------------------------------------------------------------
% Predicate: addAtomToEachInList
% Purpose: to add an atom to each element in the list 
% Parameters: Atom to be added, list to be appended, new list to be returned
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
addAtomToEachInList(_, [], []).
addAtomToEachInList(Atom, Lists, NewLists):-
    [First | Rest] = Lists,
    [Atom | First] = NewFirst,
    addAtomToEachInList(Atom, Rest, NewRests),
    NewLists = [NewFirst | NewRests].

%---------------------------------------------------------------------------------------------------------
% Predicate: getListOfEachElementRemoved
% Purpose: to get a list of lists where each element of the list is removed
% Parameters: List from which atoms are to be removed,
%               RemList the remaining atoms of List,
%               NewList to be returned.
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
getListOfEachElementRemoved(_, [], []).
getListOfEachElementRemoved(List, RemList, NewList):-
    [First | Rest] = RemList,
    delete(List, First, NewerList),
    getListOfEachElementRemoved(List, Rest, NewestList),
    NewList = [NewerList | NewestList].

%---------------------------------------------------------------------------------------------------------
% Predicate: getLowestScoreCombos
% Purpose: to get the best books and runs of the hand that results in lowest hand score
% Parameters: Hand, list of Books and runs, round number, assembled hand and score to be returned
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
getLowestScoreCombos(_, [], _, [], 9999).
getLowestScoreCombos(Hand, BnR, RoundNum, AssembledHand, Score):-
    [FirstBnR | RestBnR] = BnR,
    removeCardCollectionFromHand(FirstBnR, Hand, NewHand),
    getLowestScore(NewHand, RoundNum, FirstBnR, NewAssembled, NewScore),
    getLowestScoreCombos(Hand, RestBnR, RoundNum, NewerAssembled, NewerScore),
    getTheLowerScoreCombo(NewAssembled, NewScore, NewerAssembled, NewerScore, RetAssembled, RetScore),
    AssembledHand = RetAssembled,
    Score = RetScore.

%---------------------------------------------------------------------------------------------------------
% Predicate: getTheLoweScoreCombo
% Purpose: to compare two scores of the special card combination and return the one with lower score
% Parameters: assembled hand, score, second assembled hand, second score, 
%               assembled hand and score with lower score to be returned
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
getTheLowerScoreCombo(Assembled1, Score1, _, Score2, Assembled1, Score1):-
    Score1 < Score2.

getTheLowerScoreCombo(_, Score1, Assembled2, Score2, Assembled2, Score2):-
    Score1 >= Score2.

%---------------------------------------------------------------------------------------------------------
% Predicate: getLowestScore
% Purpose: to get the lowest score of the hand
% Parameters: Hand, round number, the book or run combination removed from hand, assembled hand, score of hand
% Local Variables: 
%           Len1, to store the length of books and runs generated
%---------------------------------------------------------------------------------------------------------
getLowestScore([], _, RemovedNode, AssembledHand, Score):-
    AssembledHand = [RemovedNode],
    Score = 0.

getLowestScore(Hand, RoundNum, RemovedNode, AssembledHand, Score):-
    sortCards(Hand, SortedHand),
    getBooksAndRuns(SortedHand, RoundNum, BooksAndRuns),
    length(BooksAndRuns, Len1),
    Len1 = 0,
    calculateScore(SortedHand, RoundNum, NewScore),
    AssembledHand = [RemovedNode | [Hand]],
    Score = NewScore.

getLowestScore(Hand, RoundNum, RemovedNode, AssembledHand, Score):-
    sortCards(Hand, SortedHand),
    getBooksAndRuns(SortedHand, RoundNum, BooksAndRuns),
    length(BooksAndRuns, Len1),
    Len1 > 0,
    getLowestScoreCombos(Hand, BooksAndRuns, RoundNum, NewAssembledHand, Score),
    AssembledHand = [RemovedNode | NewAssembledHand].

%---------------------------------------------------------------------------------------------------------
% Predicate: calculateScore
% Purpose: to calculate the score of the hand
% Parameters: Hand, Round number, score 
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
calculateScore([], _, 0).
calculateScore(Hand, RoundNum, Score):-
    [First|Rest] = Hand,
    isJoker(First),
    calculateScore(Rest, RoundNum, NewScore),
    Score is NewScore + 50.

calculateScore(Hand, RoundNum, Score):-
    [First|Rest] = Hand,
    isWildCard(First, RoundNum),
    calculateScore(Rest, RoundNum, NewScore),
    Score is NewScore + 20.

calculateScore(Hand, RoundNum, Score):-
    [First|Rest] = Hand,
    getSuiteFace(First, _, Face),
    faceValue(Face, Val),
    calculateScore(Rest, RoundNum, NewScore),
    Score is NewScore + Val.

%---------------------------------------------------------------------------------------------------------
%------------------------------- Predicates to get Books and Runs-----------------------------------------
%---------------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------------
% Predicate: getBooksAndRuns
% Purpose: to get the list of books and runs of the hand
% Parameters: Hand, Round number, list of books and runs 
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
getBooksAndRuns(Hand, RoundNum, BooksAndRuns):-
    getRuns(Hand, RoundNum, Runs),
    getBooks(Hand, RoundNum, Books),
    append(Runs, Books, BooksAndRuns).

%---------------------------------------------------------------------------------------------------------
% Predicate: getRuns
% Purpose: to get the runs of the current hand
% Parameters: Hand, Round number, list of runs
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
getRuns(Hand, RoundNum, Runs):-
    sortCards(Hand, SortedHand),
    extractSpecialCards(SortedHand, RoundNum, SpecialCards, NormalCards),
    
    getSameSuiteCards(s, NormalCards, Spades),
    getSameSuiteCards(t, NormalCards, Tridents),
    getSameSuiteCards(d, NormalCards, Diamonds),
    getSameSuiteCards(c, NormalCards, Clubs),
    getSameSuiteCards(h, NormalCards, Hearts),
    append(Spades, Tridents, Temp),
    append(Diamonds, Temp, Temp2),
    append(Clubs, Temp2, Temp3),
    append(Hearts, Temp3, Temp4),
    
    getAllCardCombos(Temp4, NormalCombos),
    getAllCardCombos(SpecialCards, SpecialCombos),

    getRunCombos(NormalCombos, RoundNum, NormalRuns),

    addSpecialToNormalCombos(NormalCombos, SpecialCombos, CombinedCombos),
    getRunCombos(CombinedCombos, RoundNum, CombinedRuns),

    append(NormalRuns, CombinedRuns, Runs).

%---------------------------------------------------------------------------------------------------------
% Predicate: getRunCombos
% Purpose: to filter out the runs from the list of hand combinations
% Parameters: List of combos, round number, list of runs
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
getRunCombos([], _, []).
getRunCombos([First|Rest], RoundNum, [First|NewRuns]):-
    isRun(First, RoundNum),
    getRunCombos(Rest, RoundNum, NewRuns).

getRunCombos([First|Rest], RoundNum, NewRuns):-
    \+ isRun(First, RoundNum),
    getRunCombos(Rest, RoundNum, NewRuns).

%---------------------------------------------------------------------------------------------------------
% Predicate: isRun
% Purpose: to check if the cards make a run
% Parameters: Cards, round number
% Local Variables: CardLen1, the length of Cards list
%                   CardLen2, the length of Normal cards after extraction of special cards
%---------------------------------------------------------------------------------------------------------
% when all the cards are special cards
isRun(Cards, RoundNum):-
    length(Cards, CardLen1),
    CardLen1 > 2,
    extractSpecialCards(Cards, RoundNum, _, NormalCards),
    hasSameSuite(NormalCards),
    length(NormalCards, CardLen2),
    CardLen2 = 0.

% when there are normal cards
isRun(Cards, RoundNum):-
    length(Cards, CardLen1),
    CardLen1 > 2,
    extractSpecialCards(Cards, RoundNum, SpecialCards, NormalCards),
    hasSameSuite(NormalCards),
    length(NormalCards, CardLen2),
    CardLen2 \= 0,
    length(SpecialCards, SpecialLen),
    canBeRun(NormalCards, MissingCardCount),
    MissingCardCount =< SpecialLen.

%---------------------------------------------------------------------------------------------------------
% Predicate: canBeRun
% Purpose: to check if Cards are potential run
% Parameters: Cards, Missing Card Count 
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
% base case for canBeRun
canBeRun(Cards, 0):-
    length(Cards, Len),
    Len =< 1.

% when it is a perfect run
canBeRun(Cards, MissingCardCount):-
    [First, Second | Rest] = Cards,
    getSuiteFace(First, _, F1),
    getSuiteFace(Second, _, F2),
    faceValue(F1, V1),
    faceValue(F2, V2),
    Dif is V2 - 1,
    V1 = Dif,
    canBeRun([Second|Rest], NewCardCount),
    MissingCardCount = NewCardCount.

% when there are missing cards in between
canBeRun(Cards, MissingCardCount):-
    [First, Second | Rest] = Cards,
    getSuiteFace(First, _, F1),
    getSuiteFace(Second, _, F2),
    faceValue(F1, V1),
    faceValue(F2, V2),
    Dif is V2 - 1,
    V1 < Dif,
    CardCount is V2 - V1 - 1,
    canBeRun([Second|Rest], NewCardCount),
    MissingCardCount is CardCount + NewCardCount.

%---------------------------------------------------------------------------------------------------------
% Predicate: hasSameSuite
% Purpose: to check if the cards have same suite
% Parameters: Cards
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
hasSameSuite(Cards):-
    length(Cards, Len),
    Len =< 1.

hasSameSuite([First, Second|Rest]):-
    getSuiteFace(First, Suite1, _),
    getSuiteFace(Second, Suite2, _),
    Suite1 = Suite2,
    hasSameSuite([Second|Rest]).

%---------------------------------------------------------------------------------------------------------
% Predicate: getSameSutieCards
% Purpose: to get the list of cards of the same suite
% Parameters: S as the suite to be filtered, List of cards, Same suite cards
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
getSameSuiteCards(_, [], []).
getSameSuiteCards(S, [First|Rest], SameSuites):-
    getSuiteFace(First, Suite, _),
    S = Suite,
    getSameSuiteCards(S, Rest, NewSameSuites),
    SameSuites = [First | NewSameSuites].
getSameSuiteCards(S, [First|Rest], SameSuites):-
    getSuiteFace(First, Suite, _),
    S \= Suite,
    getSameSuiteCards(S, Rest, NewSameSuites),
    SameSuites = NewSameSuites.

%---------------------------------------------------------------------------------------------------------
% Predicate: cardCompare
% Purpose: helper predicate for predSort 
% Parameters: Compare operator, first card, second card
% Local Variables: V1, V2 as face values of the cards
%---------------------------------------------------------------------------------------------------------
cardCompare(>, C1, C2):-
    getSuiteFace(C1, _, F1),
    faceValue(F1, V1),
    getSuiteFace(C2, _, F2),
    faceValue(F2, V2),
    V1 > V2.

cardCompare(<, C1, C2):-
    getSuiteFace(C1, _, F1),
    faceValue(F1, V1),
    getSuiteFace(C2, _, F2),
    faceValue(F2, V2),
    V1 =< V2.

%---------------------------------------------------------------------------------------------------------
% Predicate: sortCards
% Purpose: to sort the cards
% Parameters: List of cards, sorted list of cards
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
sortCards(List, SortedList):-
    predsort(cardCompare, List, SortedList).

%---------------------------------------------------------------------------------------------------------
% Predicate: getBooks
% Purpose: to get list of book combination from the hand 
% Parameters: Hand, round number, list of books
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
getBooks(Hand, RoundNum, Books):-
    sortCards(Hand, SortedHand),
    extractSpecialCards(SortedHand, RoundNum, SpecialCards, NormalCards),
    
    getAllCardCombos(NormalCards, NormalCombos),
    getAllCardCombos(SpecialCards, SpecialCombos),
    
    getBookCombos(NormalCombos, RoundNum, NormalBooks),

    addSpecialToNormalCombos(NormalCombos, SpecialCombos, CombinedCombos),
    getBookCombos(CombinedCombos, RoundNum, CombinedBooks),
    
    append(NormalBooks, CombinedBooks, Books).

%---------------------------------------------------------------------------------------------------------
% Predicate: getBookCombos
% Purpose: to filter the books from the list of possible hand combinations
% Parameters: Cards, round number, list of books
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
getBookCombos([], _, []).
getBookCombos(CardList, RoundNum, Books):-
    [First|Rest] = CardList,
    isBook(First, RoundNum),
    getBookCombos(Rest, RoundNum, NewBooks),
    Books = [First | NewBooks].

getBookCombos(Cards, RoundNum, Books):-
    [First|Rest] = Cards,
    \+ isBook(First, RoundNum),
    getBookCombos(Rest, RoundNum, NewBooks),
    Books = NewBooks.

%---------------------------------------------------------------------------------------------------------
% Predicate: isBook
% Purpose: to check if the cards is a book
% Parameters: Cards, round number
% Local Variables: CardLen1, to store length of cards, CardLen2 to store length of normal cards
%---------------------------------------------------------------------------------------------------------
% when all the cards are special cards
isBook(Cards, RoundNum):-
    length(Cards, CardLen1),
    CardLen1 > 2,
    extractSpecialCards(Cards, RoundNum, _, NormalCards),
    length(NormalCards, CardLen2),
    CardLen2 = 0.

% when there are normal cards
isBook(Cards, RoundNum):-
    length(Cards, CardLen1),
    CardLen1 > 2,
    extractSpecialCards(Cards, RoundNum, _, NormalCards),
    length(NormalCards, CardLen2),
    CardLen2 \= 0,
    isSameFace(NormalCards).

%---------------------------------------------------------------------------------------------------------
% Predicate: isSameFace
% Purpose: to check if the cards are of same face
% Parameters: Cards
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
isSameFace(Cards):-
    length(Cards, Len),
    Len =< 1.

isSameFace(Cards):-
    [First, Second | Rest] = Cards,
    getSuiteFace(First, _, FirstFace),
    getSuiteFace(Second, _, SecFace),
    FirstFace = SecFace,
    isSameFace([Second|Rest]).

%---------------------------------------------------------------------------------------------------------
% Predicate: addSpecialToNormalCombos
% Purpose: to add each card from list of jokers and wild cards to the list of normal cards
% Parameters: Normal cards, Special Cards, combined list
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
addSpecialToNormalCombos([], _, []).
addSpecialToNormalCombos(Normal, Special, Combined):-
    [FirstNormal | RestNormal] = Normal,
    addEachToCombo(FirstNormal, Special, NewCombined),
    addSpecialToNormalCombos(RestNormal, Special, ReturningCombined),
    append(NewCombined, ReturningCombined, Combined).

%---------------------------------------------------------------------------------------------------------
% Predicate: addEachToCombo
% Purpose: To add each from special list to each in normal list
% Parameters: Normal cards list, special cards list, combined list
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
addEachToCombo(_, [], []).
addEachToCombo(Normal, SpecialList, Combined):-
    [First | Rest] = SpecialList,
    append(Normal, First, NewNormal),
    addEachToCombo(Normal, Rest, NewCombined),
    Combined = [NewNormal | NewCombined].

%---------------------------------------------------------------------------------------------------------
% Predicate: divide
% Purpose: to get a list of list of each combination of the list
% Parameters: X as the list, Return as the return list
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
divide([], []).
divide(X, Return):- 
    [H|T] = X,
    divide(T, Newerlist),
    Return = [H|Newerlist].

%---------------------------------------------------------------------------------------------------------
% Predicate: getCombos
% Purpose: to get all possible combinations from the list
% Parameters: X as the list, Return as the return list
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
getCombos([], []).
getCombos(X,Return):-
    [_|T] = X,
    getCombos(T, NewerRet),
    divide(X, NewRet),
    Return = [NewRet|NewerRet].

%---------------------------------------------------------------------------------------------------------
% Predicate: getAllCardCombos
% Purpose: to get all the card combination in a list
% Parameters: Card, Combination
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
getAllCardCombos([],[]).
getAllCardCombos(Card, Combinations):-
    getCombos(Card, Newlist),
    withoutLast(Card, NewHand),
    getAllCardCombos(NewHand, NewCombo),
    append(Newlist, NewCombo, Combinations).

%---------------------------------------------------------------------------------------------------------
% Predicate: withoutLast
% Purpose: to get the list without the last element of the list
% Parameters: List, List without last element
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
withoutLast([_], []).
withoutLast([First|Rest], [First|WithoutLast]) :- 
    withoutLast(Rest, WithoutLast).

%---------------------------------------------------------------------------------------------------------
% Predicate: isJokerOrWild
% Purpose: to check if card is a joker or wild card
% Parameters: Card, Round number
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
isJokerOrWild(Card, RoundNum):-
    isWildCard(Card, RoundNum).

isJokerOrWild(Card, _):-
    isJoker(Card).

%---------------------------------------------------------------------------------------------------------
% Predicate: isWildCard
% Purpose: to check if card is a wild card
% Parameters: Card, Round Number
% Local Variables: Face of the card, Val face value of card
%---------------------------------------------------------------------------------------------------------
isWildCard(Card, RoundNum):-
    Wild is RoundNum + 2,
    getSuiteFace(Card, _, Face),
    faceValue(Face, Val),
    Val = Wild.

%---------------------------------------------------------------------------------------------------------
% Predicate: isJoker
% Purpose: to check if the card is a joker
% Parameters: Card
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
isJoker(Card):-
    getSuiteFace(Card, Suite, _),
    Suite = j.

%---------------------------------------------------------------------------------------------------------
% Predicate: extractSpecialCards
% Purpose: to extract jokers and wild cards from hand
% Parameters: Hand, round number, special cards list, normal cards list
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
extractSpecialCards([], _, [], []).
extractSpecialCards([First|Rest], RoundNum, SpecialCards, NormalCards):-
    isJokerOrWild(First,RoundNum),
    extractSpecialCards(Rest, RoundNum, NewSpecialCards, NormalCards),
    [First|NewSpecialCards] = SpecialCards.

extractSpecialCards([First|Rest], RoundNum, SpecialCards, NormalCards):-
    \+ isJokerOrWild(First, RoundNum),
    extractSpecialCards(Rest, RoundNum, SpecialCards, NewNormalCards),
    [First|NewNormalCards] = NormalCards.

%---------------------------------------------------------------------------------------------------------
%------------------------------------- Human Action Predicates -------------------------------------------
%---------------------------------------------------------------------------------------------------------


%---------------------------------------------------------------------------------------------------------
% Predicate: getHumanMenuAction
% Purpose: to display options menu and get the human menu choice
% Parameters: current game state, new game state
% Local Variables: choice.
%---------------------------------------------------------------------------------------------------------
getHumanMenuAction(GameState, NewGameState):- 
    nl, 
    write("=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"),nl,
    write("1. Make a move"), nl,
    write("2. Hint"), nl,
    write("3. Quit"), nl,
    write("=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"),nl,
    read(Choice),
    number(Choice),
    getHumanChoiceAction(Choice, GameState, NewState),
    NewGameState = NewState.

getHumanMenuAction(GameState, NewGameState):-
    write("INVALID menu choice. Please enter the correct option choice."),
    getHumanMenuAction(GameState, NewState),
    NewGameState = NewState.

%---------------------------------------------------------------------------------------------------------
% Predicate: getHumanChoiceAction
% Purpose: to let the human make their move according to their choice
% Parameters: choice, current game state, new game state
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
% when human chooses to make a move
getHumanChoiceAction(1, GameState, NewGameState):-
    getChosenPileMove(GameState, NewerGameState),
    getDiscardCardMove(NewerGameState, NewestGameState),
    NewGameState = NewestGameState.

% when human asks for hint
getHumanChoiceAction(2, GameState, NewGameState):-
    getWhichPileHint(GameState, Hint),
    Hint = discard,
    write("You should pick from >> "), write(Hint), write(" pile << because it helps you to make more runs or books."),nl,
    getHumanChoiceAction(1, GameState, NewerGameState),
    NewGameState = NewerGameState.

getHumanChoiceAction(2, GameState, NewGameState):-
    write("You should pick from >> draw pile << because discard pile did not help in making more runs or books."),nl,
    getHumanChoiceAction(1, GameState, NewerGameState),
    NewGameState = NewerGameState.

% when human quits the game
getHumanChoiceAction(3, _, _):-
    write("Quitting the game..."), nl,
    halt(0).

%---------------------------------------------------------------------------------------------------------
% Predicate: getChosenPileMove
% Purpose: to get the card from the pile chosen by human
% Parameters: current game state, new game state
% Local Variables: choice.
%---------------------------------------------------------------------------------------------------------
getChosenPileMove(GameState, NewGameState):-
    write("Which Pile would you like to choose? "), nl,
    write("1. Draw Pile"),nl,
    write("2. Discard Pile"),nl,
    read(Choice),
    number(Choice),
    addPileCardToHand(Choice, GameState, NewState),
    NewGameState = NewState.

%---------------------------------------------------------------------------------------------------------
% Predicate: getDiscardCardMove
% Purpose: to ask human if they want help with discarding and then letting them make their move
% Parameters: current game state, new game state
% Local Variables: choice.
%---------------------------------------------------------------------------------------------------------
getDiscardCardMove(GameState, NewGameState):-
    write("Do you want help for discarding a card? (y/n)"),
    read(Choice),
    validateYesNoChoice(Choice),
    getDiscardAction(Choice, GameState, NewerGameState),
    NewGameState = NewerGameState.

getDiscardCardMove(GameState, NewGameState):-
    write("INVALID CHOICE. Please enter y for yes, n for no. "), nl,
    getDiscardCardMove(GameState, NewState),
    NewGameState = NewState.

%---------------------------------------------------------------------------------------------------------
% Predicate: getDiscardAction
% Purpose: to show the human which card to discard hint
% Parameters: choice, current game state, new game state
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
getDiscardAction(y, GameState, NewGameState):-  
    getRoundNumber(GameState, RoundNum),
    getHumanHand(GameState, Hand), 
    getWhichCardHint(RoundNum, Hand, H),
    write("You should discard >> "), write(H), write(" << because it makes your score lower."), nl,
    getDiscardAction(n, GameState, NewerGameState),
    NewGameState = NewerGameState.

% when human does not choose hint
getDiscardAction(n, GameState, NewGameState):-
    write("Which Card would you like to discard?"), nl,
    getHumanHand(GameState, Hand),
    write(Hand),nl,
    read(Card),
    member(Card, Hand),
    removeCardFromHand(Card, Hand, NewHand),
    getDiscardPile(GameState, Pile),
    discardToPile(Card, Pile, NewPile),
    GameState = [Round, CompScore, CompHand, HumanScore, _, DrawPile, _, NextPlayer],
    askToAssembleHand(NewHand, Round),
    NewGameState = [Round, CompScore, CompHand, HumanScore, NewHand, DrawPile, NewPile, NextPlayer].

getDiscardAction(n, GameState, NewGameState):-
    write("INVALID CARD ENTERED! Please enter the correct card."), nl,
    getDiscardAction(n, GameState, NewerGameState),
    NewGameState = NewerGameState.

%---------------------------------------------------------------------------------------------------------
% Predicate: askToAssembleHand
% Purpose: to ask human if they want to assemble hand and display it accordingly
% Parameters: Hand, Round number
% Local Variables: answer yes or no
%---------------------------------------------------------------------------------------------------------
askToAssembleHand(Hand, RoundNum):-
    askAssembleQuestion(Answer),
    Answer = y,
    getLowestScore(Hand, RoundNum, [], Assembled, Score),
    write("Hand: "), write(Assembled), nl,
    write("Score: "), write(Score), nl.

askToAssembleHand(_, _).

%---------------------------------------------------------------------------------------------------------
% Predicate: askAssembleQuestion
% Purpose: to ask human if they want to assemble their possible hand
% Parameters: Answer
% Local Variables: choice
%---------------------------------------------------------------------------------------------------------
askAssembleQuestion(Answer):-
    write("Would you like to assemble your possible books and runs? (y/n)"), nl,
    read(Choice),
    validateYesNoChoice(Choice),
    Answer = Choice.

askAssembleQuestion(Answer):-
    write("INVALID CHOICE! Please enter y for yes, n for no."), nl,
    askAssembleQuestion(NewAnswer),
    Answer = NewAnswer.

%---------------------------------------------------------------------------------------------------------
%----------------------------- Hand and Pile Manipulation Predicates--------------------------------------
%---------------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------------
% Predicate: distributeHand
% Purpose: to distribute cards to players
% Parameters: Number of cards to distribute, deck, hand, new deck
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
distributeHand(0, OldDeck, [], NewDeck):- 
    NewDeck = OldDeck.

distributeHand(NumCards, [DeckFirst|DeckRest], Hand, RemainingDeck):-
    NewNumCards is NumCards - 1,
    distributeHand(NewNumCards, DeckRest, NewHand, NewDeck),
    [DeckFirst | NewHand] = Hand,
    NewDeck = RemainingDeck.

%---------------------------------------------------------------------------------------------------------
% Predicate: popTopCard
% Purpose: to get the top card from the list
% Parameters: From list, Returning list, top Card
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
popTopCard([Top | Rest], Rest, Top).

%---------------------------------------------------------------------------------------------------------
% Predicate: discardToPile
% Purpose: to add a card to the discard pile
% Parameters: Card, DiscardPile, new discard pile
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
% when discard pile is empty
discardToPile(Card, [], NewDiscardPile):- 
    NewDiscardPile = [Card].

% when discard pile is not empty
discardToPile(Card, DiscardPile, NewDiscardPile):- 
    [Card | DiscardPile] = NewDiscardPile.

%---------------------------------------------------------------------------------------------------------
% Predicate: addPileCardToHand
% Purpose: to add chosen pile card to hand
% Parameters: pile choice, current game state, new game state
% Local Variables: 
%           TopCard of the pile
%---------------------------------------------------------------------------------------------------------
% when human chooses draw pile
addPileCardToHand(1, GameState, NewGameState):-
    getNextPlayer(GameState, Player),
    Player = human,
    getDrawPile(GameState, Pile),
    popTopCard(Pile, NewPile, TopCard),
    getHumanHand(GameState, Hand),
    [TopCard | Hand] = NewHand,
    GameState = [Round, CompScore, CompHand, HumanScore, _, _, DiscardPile, NextPlayer],
    NewGameState = [Round, CompScore, CompHand, HumanScore, NewHand, NewPile, DiscardPile, NextPlayer].

% when computer chooses draw pile
addPileCardToHand(1, GameState, NewGameState):-
    getNextPlayer(GameState, Player),
    Player = computer,
    getDrawPile(GameState, Pile),
    popTopCard(Pile, NewPile, TopCard),
    getComputerHand(GameState, Hand),
    [TopCard | Hand] = NewHand,
    GameState = [Round, CompScore, _, HumanScore, HumanHand, _, DiscardPile, NextPlayer],
    NewGameState = [Round, CompScore, NewHand, HumanScore, HumanHand, NewPile, DiscardPile, NextPlayer].

% when human chooses discard pile
addPileCardToHand(2, GameState, NewGameState):-
    getNextPlayer(GameState, Player),
    Player = human,
    getDiscardPile(GameState, Pile),
    popTopCard(Pile, NewPile, TopCard),
    getHumanHand(GameState, Hand),
    [TopCard | Hand] = NewHand,
    GameState = [Round, CompScore, CompHand, HumanScore, _, DrawPile, _, NextPlayer],
    NewGameState = [Round, CompScore, CompHand, HumanScore, NewHand, DrawPile, NewPile, NextPlayer].
    
% when computer chooses discard pile
addPileCardToHand(2, GameState, NewGameState):-
    getNextPlayer(GameState, Player),
    Player = computer,
    getDiscardPile(GameState, Pile),
    popTopCard(Pile, NewPile, TopCard),
    getComputerHand(GameState, Hand),
    [TopCard | Hand] = NewHand,
    GameState = [Round, CompScore, _, HumanScore, HumanHand, DrawPile, _, NextPlayer],
    NewGameState = [Round, CompScore, NewHand, HumanScore, HumanHand, DrawPile, NewPile, NextPlayer].

addPileCardToHand(_, GameState, NewGameState):-
    write("INVALID CHOICE! Please enter 1 for Draw Pile, 2 for Discard Pile."), nl,
    getChosenPileMove(GameState, NewState),
    NewGameState = NewState.

%---------------------------------------------------------------------------------------------------------
% Predicate: removeCardCollectionFromHand
% Purpose: to remove a list of cards from hand
% Parameters: list of cards to remove, hand, new hand
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
removeCardCollectionFromHand([], [], []).
removeCardCollectionFromHand(_, [], []).
removeCardCollectionFromHand([], Hand, Hand).
removeCardCollectionFromHand(Collection, Hand, RemovedHand):-
    [First | Rest] = Collection,
    removeCardFromHand(First, Hand, NewHand),
    removeCardCollectionFromHand(Rest, NewHand, NewerHand),
    RemovedHand = NewerHand.

%---------------------------------------------------------------------------------------------------------
% Predicate: removeCardFromHand
% Purpose: to delete card from the hand
% Parameters: Card, Hand, New hand
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
removeCardFromHand(_, [], []):-
    write("INVALID CARD BEING REMOVED!"),nl.

removeCardFromHand(Card, [First | Rest], Rest):-
    First = Card.

removeCardFromHand(Card, [First | Rest], NewHand):-
    First \= Card,
    removeCardFromHand(Card, Rest, NewerHand),
    NewHand = [First | NewerHand].

%---------------------------------------------------------------------------------------------------------
% Predicate: displayRoundStatus
% Purpose: to display the round status
% Parameters: current game state
% Local Variables: CurrPlayer, RoundNumber, CompScore, CompHand, HumanScore,
%               HumanHand, DrawPile, DiscardPile
%---------------------------------------------------------------------------------------------------------
displayRoundStatus(State):-
    getNextPlayer(State, CurrPlayer),
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
    write("Player: "), write(CurrPlayer),nl,
    write("=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"),
    nl.

%---------------------------------------------------------------------------------------------------------
% Predicate: getRoundNumber
% Purpose: to get the round number from current state
% Parameters: current State, round number
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
getRoundNumber(State, Number):- nth0(0, State, Number).

%---------------------------------------------------------------------------------------------------------
% Predicate: getComputerScore
% Purpose: to get the computer score from current state
% Parameters: current state, round number
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
getComputerScore(State, Score):- nth0(1, State, Score).

%---------------------------------------------------------------------------------------------------------
% Predicate: getComputerHand
% Purpose: to get the computer hand from current state
% Parameters: current state, hand
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
getComputerHand(State, Hand):- nth0(2, State, Hand).

%---------------------------------------------------------------------------------------------------------
% Predicate: getHumanScore
% Purpose: to get the human score
% Parameters: current state, score
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
getHumanScore(State, Score):- nth0(3, State, Score).

%---------------------------------------------------------------------------------------------------------
% Predicate: getHumanHand
% Purpose: to get the human hand
% Parameters: current state, hand
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
getHumanHand(State, Hand):- nth0(4, State, Hand).

%---------------------------------------------------------------------------------------------------------
% Predicate: getDrawPile
% Purpose: to get the draw pile from current state
% Parameters: current state, pile
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
getDrawPile(State, Pile):- nth0(5, State, Pile).

%---------------------------------------------------------------------------------------------------------
% Predicate: getDiscardPile
% Purpose: to get the discard pile from current state
% Parameters: current state, pile
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
getDiscardPile(State, Pile):- nth0(6, State, Pile).

%---------------------------------------------------------------------------------------------------------
% Predicate: getNextPlayer
% Purpose: to get the next player from current state
% Parameters: current state, next player
% Local Variables: none
%---------------------------------------------------------------------------------------------------------
getNextPlayer(State, NextPlayer):- nth0(7, State, NextPlayer).

%---------------------------------------------------------------------------------------------------------
% Predicate: getSuiteFace
% Purpose: to get the suite and face from the card
% Parameters: card, suite, face
% Local Variables: none
%---------------------------------------------------------------------------------------------------------
getSuiteFace(Card, Suite, Face):-
    atom_chars(Card, ListCard),
    [Suite | [Face|_]] = ListCard.

%---------------------------------------------------------------------------------------------------------
% Predicate: askIfSaveAndQuit
% Purpose: to ask human if they want to save and quit
% Parameters: Answer
% Local Variables: choice
%---------------------------------------------------------------------------------------------------------
askIfSaveAndQuit(Answer):- 
    write("Would you like to save and quit?(y/n)"),
    read(Choice), nl,
    validateYesNoChoice(Choice),
    Answer = Choice.

askIfSaveAndQuit(Answer):- askIfSaveAndQuit(NewAnswer),
    Answer = NewAnswer.

%---------------------------------------------------------------------------------------------------------
% Predicate: validateYesNoChoice
% Purpose: to validate choice of human
% Parameters: y, n
% Local Variables: none
%---------------------------------------------------------------------------------------------------------
validateYesNoChoice(y).
validateYesNoChoice(n).

%---------------------------------------------------------------------------------------------------------
% Predicate: faceValue
% Purpose: to get the face value of the card
% Parameters: Face, Value
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
faceValue(Face, Value):-
    atom_number(Face, Num),
    Value = Num.

faceValue(x, 10).
faceValue(j, 11).
faceValue(q, 12).
faceValue(k, 13).

%---------------------------------------------------------------------------------------------------------
% Predicate: main
% Purpose: to run the game
% Parameters: none
% Local Variables: none.
%---------------------------------------------------------------------------------------------------------
main:-
    fivecrowns(0).