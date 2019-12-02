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
    Game = State.

getGameFromFile(Game) :-
    getGameFromFile(NewGame),
    Game = NewGame.

newGame(_):-
    %coinToss(NextPlayer),
    %playGame(1, [], 0, [], 0, NextPlayer, false).
    playGame(1, [], 0, [], 0, human, false).
    
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
    askIfSaveAndQuit(Choice),
    runRound(Game, NewState,Choice).
    %start fresh round by calling playGame.


% save and quit
playGame(_, _, _, _, _, _, true).

% game ended
% playGame(11, HumanHand, HumanScore, CompHand, CompScore, NextPlayer, _):- 
%   checkIfGameEnded(HumanHand, HumanScore, CompHand, CompScore).

% new game
playGame(RoundNum, HumanHand, HumanScore, CompHand, CompScore, NextPlayer, _):- 
    generateNewRound(RoundNum, HumanHand, HumanScore, CompHand, CompScore, NextPlayer, GameState),
    loadGame(GameState).

% player decides to save and quit
runRound(OldGameState, OldGameState, y):-
    open("./save.txt", write, Stream),
    write(Stream, OldGameState),
    write(Stream, "."),
    close(Stream),
    write("Game saved to ./save.txt"),nl,
    write("Exiting the game..."), nl,
    halt(0).

% if round ended
% runRound(OldGameState, RoundResults, _):-
%    OldGameState = [_, CompScore, CompHand, HumanScore, HumanHand, _, _, _],
%    chceckIfRoundEnded(NCompScore, CompHand, NHumanScore, HumanHand),nl,
%    write("The round has ended"), nl,
%    CScore is CompScore + NCompScore,
%    HScore is HumanScore + NHumanScore,
%    RoundResults = [CScore, HScore].

% regular flow
runRound(OldGameState, NewGameState, n):-
    displayRoundStatus(OldGameState),
    playRound(OldGameState, NewState),
    askIfSaveAndQuit(Choice),
    runRound(NewState, NewerState, Choice),
    NewGameState = NewerState.

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

generateNewRound(RoundNum, _, HumanScore, _, CompScore, NextPlayer, GameState):-
    UnshuffledDeck = ['j1', 'j2', 'j3', 't3', 't4', 't5', 't6', 't7', 't8', 't9', 'tx', 'tj', 'tq', 'tk', 'c3', 'c4', 'c5', 'c6', 'c7', 'c8', 'c9', 'cx', 'cj', 'cq', 'ck', 's3', 's4', 's5', 's6', 's7', 's8', 's9', 'sx', 'sj', 'sq', 'sk', 'd3', 'd4', 'd5', 'd6', 'd7', 'd8', 'd9', 'dx', 'dj', 'dq', 'dk', 'h3', 'h4', 'h5', 'h6', 'h7', 'h8', 'h9', 'hx', 'hj', 'hq', 'hk',
                        'j1', 'j2', 'j3', 't3', 't4', 't5', 't6', 't7', 't8', 't9', 'tx', 'tj', 'tq', 'tk', 'c3', 'c4', 'c5', 'c6', 'c7', 'c8', 'c9', 'cx', 'cj', 'cq', 'ck', 's3', 's4', 's5', 's6', 's7', 's8', 's9', 'sx', 'sj', 'sq', 'sk', 'd3', 'd4', 'd5', 'd6', 'd7', 'd8', 'd9', 'dx', 'dj', 'dq', 'dk', 'h3', 'h4', 'h5', 'h6', 'h7', 'h8', 'h9', 'hx', 'hj', 'hq', 'hk'],
    random_permutation(UnshuffledDeck, Deck),
    Num is RoundNum + 2,
    distributeHand(Num, Deck, HumanHand, NewDeck),
    distributeHand(Num, NewDeck, CompHand, NewNewDeck),
    popTopCard(NewNewDeck, DrawPile, Top),
    discardToPile(Top, [], DiscardPile),
    GameState = [RoundNum, CompScore, CompHand, HumanScore, HumanHand, DrawPile, DiscardPile, NextPlayer].

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

% when discard pile is empty
discardToPile(Card, [], NewDiscardPile):- 
    NewDiscardPile = [Card].

% when discard pile is not empty
discardToPile(Card, DiscardPile, NewDiscardPile):- 
    [Card | DiscardPile] = NewDiscardPile.

getComputerMove(GameState, NewGameState):-
    getHumanMenuAction(GameState, NewState),
    NewGameState = NewState.

getRuns(Hand, RoundNum, Runs):-
    sortCards(Hand, SortedHand),
    extractSpecialCards(SortedHand, RoundNum, SpecialCards, NormalCards),
    
    getAllCardCombos(NormalCards, NormalCombos),
    getAllCardCombos(SpecialCards, SpecialCombos),

    getRunCombos(NormalCombos, RoundNum, NormalRuns),

    addSpecialToNormalCombos(NormalCombos, SpecialCombos, CombinedCombos),
    getRunCombos(CombinedCombos, RoundNum, CombinedRuns),

    append(NormalRuns, CombinedRuns, Runs).

getRunCombos([], _, []).
getRunCombos([First|Rest], RoundNum, [First|NewRuns]):-
    isRun(First, RoundNum),
    getRunCombos(Rest, RoundNum, NewRuns).

getRunCombos([First|Rest], RoundNum, NewRuns):-
    \+ isRun(First, RoundNum),
    getRunCombos(Rest, RoundNum, NewRuns).

isRun(Cards, RoundNum):-
    length(Cards, CardLen1),
    CardLen1 > 2,
    extractSpecialCards(Cards, RoundNum, _, NormalCards),
    hasSameSuite(NormalCards),
    length(NormalCards, CardLen2),
    CardLen2 = 0.

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

hasSameSuite(Cards):-
    length(Cards, Len),
    Len =< 1.

hasSameSuite([First, Second|Rest]):-
    getSuiteFace(First, Suite1, _),
    getSuiteFace(Second, Suite2, _),
    Suite1 = Suite2,
    hasSameSuite([Second|Rest]).

%base case for canBeRun
canBeRun(Cards, 0):-
    length(Cards, Len),
    Len =< 1.

%when it is a perfect run
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

%when there are missing cards in between,
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

%begin helper for isRun
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

sortCards(List, SortedList):-
    predsort(cardCompare, List, SortedList).

%end helper for isRun

%begin to get all Books
getBooks(Hand, RoundNum, Books):-
    sortCards(Hand, SortedHand),
    extractSpecialCards(SortedHand, RoundNum, SpecialCards, NormalCards),
    
    getAllCardCombos(NormalCards, NormalCombos),
    getAllCardCombos(SpecialCards, SpecialCombos),
    
    getBookCombos(NormalCombos, RoundNum, NormalBooks),

    addSpecialToNormalCombos(NormalCombos, SpecialCombos, CombinedCombos),
    getBookCombos(CombinedCombos, RoundNum, CombinedBooks),
    
    append(NormalBooks, CombinedBooks, Books).

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

isBook(Cards, RoundNum):-
    length(Cards, CardLen1),
    CardLen1 > 2,
    extractSpecialCards(Cards, RoundNum, _, NormalCards),
    length(NormalCards, CardLen2),
    CardLen2 = 0.

isBook(Cards, RoundNum):-
    length(Cards, CardLen1),
    CardLen1 > 2,
    extractSpecialCards(Cards, RoundNum, _, NormalCards),
    length(NormalCards, CardLen2),
    CardLen2 \= 0,
    isSameFace(NormalCards).

isSameFace(Cards):-
    length(Cards, Len),
    Len =< 1.

isSameFace(Cards):-
    [First, Second | Rest] = Cards,
    getSuiteFace(First, _, FirstFace),
    getSuiteFace(Second, _, SecFace),
    FirstFace = SecFace,
    isSameFace([Second|Rest]).

addSpecialToNormalCombos([], _, []).
addSpecialToNormalCombos(Normal, Special, Combined):-
    [FirstNormal | RestNormal] = Normal,
    addEachToCombo(FirstNormal, Special, NewCombined),
    addSpecialToNormalCombos(RestNormal, Special, ReturningCombined),
    append(NewCombined, ReturningCombined, Combined).

addEachToCombo(_, [], []).
addEachToCombo(Normal, SpecialList, Combined):-
    [First | Rest] = SpecialList,
    append(Normal, First, NewNormal),
    addEachToCombo(Normal, Rest, NewCombined),
    Combined = [NewNormal | NewCombined].

%end to get all Books

divide([], []).
divide(X, Return):- 
    [H|T] = X,
    divide(T, Newerlist),
    Return = [H|Newerlist].

getCombos([], []).
getCombos(X,Return):-
    [_|T] = X,
    getCombos(T, NewerRet),
    divide(X, NewRet),
    Return = [NewRet|NewerRet].

getAllCardCombos([],[]).
getAllCardCombos(Card, Combinations):-
    getCombos(Card, Newlist),
    without_last(Card, NewHand),
    getAllCardCombos(NewHand, NewCombo),
    append(Newlist, NewCombo, Combinations).

without_last([_], []).
without_last([First|Rest], [First|WithoutLast]) :- 
    without_last(Rest, WithoutLast).


%begin check special cards
isJokerOrWild(Card, RoundNum):-
    isWildCard(Card, RoundNum).

isJokerOrWild(Card, _):-
    isJoker(Card).

isWildCard(Card, RoundNum):-
    Wild is RoundNum + 2,
    getSuiteFace(Card, _, Face),
    faceValue(Face, Val),
    Val = Wild.

isJoker(Card):-
    getSuiteFace(Card, Suite, _),
    Suite = j.

%end check special cards


%begin to get jokers and wilds
extractSpecialCards([], _, [], []).
extractSpecialCards([First|Rest], RoundNum, SpecialCards, NormalCards):-
    isJokerOrWild(First,RoundNum),
    extractSpecialCards(Rest, RoundNum, NewSpecialCards, NormalCards),
    [First|NewSpecialCards] = SpecialCards.

extractSpecialCards([First|Rest], RoundNum, SpecialCards, NormalCards):-
    \+ isJokerOrWild(First, RoundNum),
    extractSpecialCards(Rest, RoundNum, SpecialCards, NewNormalCards),
    [First|NewNormalCards] = NormalCards.
%end to get jokers and wilds

getHumanMenuAction(GameState, NewGameState):- 
    nl, write("=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"),nl,
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

getHumanChoiceAction(1, GameState, NewGameState):-
    getChosenPileMove(GameState, NewerGameState),

    getDiscardCardMove(NewerGameState, NewestGameState),
    NewGameState = NewestGameState.

getHumanChoiceAction(2, GameState, NewGameState):-
    write("Hint: "), nl,
    %getWhichPileHint(GameState, Hint),
    %write(Hint),nl,
    getHumanChoiceAction(1, GameState, NewerGameState),
    NewGameState = NewerGameState.

getHumanChoiceAction(3, _, _):-
    write("Quitting the game..."), nl,
    halt(0).

getChosenPileMove(GameState, NewGameState):-
    write("Which Pile would you like to choose? "), nl,
    write("1. Draw Pile"),nl,
    write("2. Discard Pile"),nl,
    read(Choice),
    number(Choice),
    addPileCardToHand(Choice, GameState, NewState),
    NewGameState = NewState.

getDiscardCardMove(GameState, NewGameState):-
    write("Do you want help for discarding a card? (y/n)"),
    read(Choice),
    validateYesNoChoice(Choice),
    getDiscardAction(Choice, GameState, NewerGameState),
    NewGameState = NewerGameState.

getDiscardAction(y, GameState, NewGameState):-   
    %getWhichCardHint(GameState, Hint),
    write("Hint: "), nl, %write(Hint),nl.
    getDiscardAction(n, GameState, NewerGameState),
    NewGameState = NewerGameState.

getDiscardAction(n, GameState, NewGameState):-
    write("Which Card would you like to discard?"), nl,
    getHumanHand(GameState, Hand),
    write(Hand),nl,
    read(Card),
    removeCardFromHand(Card, Hand, NewHand),
    getDiscardPile(GameState, Pile),
    discardToPile(Card, Pile, NewPile),
    GameState = [Round, CompScore, CompHand, HumanScore, _, DrawPile, _, NextPlayer],
    NewGameState = [Round, CompScore, CompHand, HumanScore, NewHand, DrawPile, NewPile, NextPlayer].

getDiscardAction(n, GameState, NewGameState):-
    getDiscardAction(n, GameState, NewerGameState),
    NewGameState = NewerGameState.

addPileCardToHand(1, GameState, NewGameState):-
    getNextPlayer(GameState, Player),
    Player = human,
    getDrawPile(GameState, Pile),
    popTopCard(Pile, NewPile, TopCard),
    getHumanHand(GameState, Hand),
    [TopCard | Hand] = NewHand,
    GameState = [Round, CompScore, CompHand, HumanScore, _, _, DiscardPile, NextPlayer],
    NewGameState = [Round, CompScore, CompHand, HumanScore, NewHand, NewPile, DiscardPile, NextPlayer].
    
addPileCardToHand(1, GameState, NewGameState):-
    getNextPlayer(GameState, Player),
    Player = computer,
    getDrawPile(GameState, Pile),
    popTopCard(Pile, NewPile, TopCard),
    getComputerHand(GameState, Hand),
    [TopCard | Hand] = NewHand,
    GameState = [Round, CompScore, _, HumanScore, HumanHand, _, DiscardPile, NextPlayer],
    NewGameState = [Round, CompScore, NewHand, HumanScore, HumanHand, NewPile, DiscardPile, NextPlayer].

addPileCardToHand(2, GameState, NewGameState):-
    getNextPlayer(GameState, Player),
    Player = human,
    getDiscardPile(GameState, Pile),
    popTopCard(Pile, NewPile, TopCard),
    getHumanHand(GameState, Hand),
    write("adding.."), write(Hand), nl,
    [TopCard | Hand] = NewHand,
    write("added: "), write(NewHand), nl,
    GameState = [Round, CompScore, CompHand, HumanScore, _, DrawPile, _, NextPlayer],
    NewGameState = [Round, CompScore, CompHand, HumanScore, NewHand, DrawPile, NewPile, NextPlayer].
    
addPileCardToHand(2, GameState, NewGameState):-
    getNextPlayer(GameState, Player),
    Player = computer,
    getDiscardPile(GameState, Pile),
    popTopCard(Pile, NewPile, TopCard),
    getComputerHand(GameState, Hand),
    [TopCard | Hand] = NewHand,
    GameState = [Round, CompScore, _, HumanScore, HumanHand, DrawPile, _, NextPlayer],
    NewGameState = [Round, CompScore, NewHand, HumanScore, HumanHand, DrawPile, NewPile, NextPlayer].

removeCardFromHand(_, [], _):-
    write("Could not find card in Hand. Please enter the correct card.").

removeCardFromHand(Card, Hand, NewHand):-
    [First | Rest] = Hand,
    First = Card,
    NewHand = Rest.

removeCardFromHand(Card, Hand, NewHand):-
    [First | Rest] = Hand,
    removeCardFromHand(Card, Rest, NewerHand),
    [First | NewerHand] = NewHand.



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

getSuiteFace(Card, Suite, Face):-
    atom_chars(Card, ListCard),
    [Suite | [Face|_]] = ListCard.

askIfSaveAndQuit(Answer):- 
    write("Would you like to save and quit?(y/n)"),
    read(Choice), nl,
    validateYesNoChoice(Choice),
    Answer = Choice.

askIfSaveAndQuit(Answer):- askIfSaveAndQuit(NewAnswer),
    Answer = NewAnswer.

validateYesNoChoice(y).
validateYesNoChoice(n).

faceValue(Face, Value):-
    atom_number(Face, Num),
    Value = Num.

faceValue(x, 10).
faceValue(j, 11).
faceValue(q, 12).
faceValue(k, 13).

printChoice(Choice):- write(Choice).