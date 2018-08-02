type player = X | O;

type square = option(player);
type winner = square;

type gameBoard = array(square);

/* State declaration */
type state = {
  turn: player,
  winner: winner,
  board: gameBoard,
};

/* Action declaration */
type action =
  | Click(int);

let initialBoard : gameBoard = ArrayLabels.make(9, None);


let isNone(opt: option('a)): bool =
  switch (opt) {
    | None => true
    | Some(_a) => false
  };

let isSome(opt: option('a)): bool =
  !isNone(opt)

let winningCombos = [[0, 1, 2], [3, 4, 5], [6, 7, 8], [0, 3, 6], [1, 4, 7], [2, 5, 8], [0, 4, 8], [2, 4, 6]];

let isAWinner = (list: list(square)): square =>
  ListLabels.fold_left(
    ~f=(init: 'a, b: 'a) => {(init === b) ? init : None},
    ~init=ListLabels.hd(list),
    list
  );

let safeHd = (xs: list('a)): option('a) => ListLabels.length(xs) > 0 ? Some(ListLabels.hd(xs)) : None

let getOrElse = (default: 'a, opt: option('a)): 'a =>
  switch (opt) {
    | Some(a) => a
    | None => default
  };

let updateWinner = (currentBoard: gameBoard): winner =>
  ListLabels.map(
    ~f=(combo: list(int)) => {
      ListLabels.fold_right(
        ~f=(index: int, init: square) => {
          isSome(currentBoard[index]) && currentBoard[index] === init ? init : None
        },
        ~init=currentBoard[ListLabels.hd(combo)],
        combo
      )
    },
    winningCombos
  )
  |> ListLabels.filter(~f=isSome)
  |> safeHd
  |> getOrElse(None)

let updateBoard = (currentBoard: gameBoard, player: player, squareIndex: int): gameBoard => {
  let copy = ArrayLabels.copy(currentBoard);
  copy[squareIndex] = Some(player);
  copy
};

let switchPlayer(currentPlayer: player): player =
  switch(currentPlayer) {
    | X => O
    | O => X
  };

let playerToString(currentPlayer: player): string =
  switch(currentPlayer) {
    | X => "X"
    | O => "O"
  };

let id(i, a) = (i, a);

let zipWithIndices(arr: array('a)): array((int, 'a)) = ArrayLabels.mapi(~f=id, arr);

let arrToMatrix = (arr: array(square)): array(array((int, 'a))) => {
  let init: array(array((int, 'a))) = ArrayLabels.make_matrix(~dimx=3, ~dimy=3, (0, None));
  let zipped : array((int, 'a)) = zipWithIndices(arr);
  let f = (acc, zippedVal): array(array((int, 'a))) => {
    let (index, _a) = zippedVal;
    let x = index mod 3;
    let y = index / 3;
    acc[x][y] = zippedVal;
    acc
  };
  ArrayLabels.fold_left(~f=f, ~init=init, zipped)
};

/* Component template declaration.
   Needs to be **after** state and action declarations! */
let component = ReasonReact.reducerComponent("Example");

let availableMoves = (board: gameBoard): list(int) => {
  let zipped: array((int, square)) = zipWithIndices(board);
  let zippedList = ArrayLabels.to_list(zipped);
  let filteredZippedList = ListLabels.filter(~f=((_idx, square)) => isNone(square), zippedList);
  ListLabels.map(~f=Pervasives.fst, filteredZippedList)
}

let predictFuture = (board: gameBoard, turn: player): list((int, gameBoard)) => {
  let moves = availableMoves(board);
  ListLabels.map(~f=(idx) => (idx, updateBoard(board, turn, idx)), moves)
}

let optionMap = (f: 'a => 'b, opt: option('a)): option('b) =>
  switch(opt) {
    | Some(a) => Some(f(a))
    | None => None
  };

let optionFlatMap = (f: 'a => 'b, opt: option('a)): 'b =>
  switch(opt) {
    | Some(a) => f(a)
    | None => None
  };

/* TODO: Think one move farther ahead to figure out how the player can win on their turn */
let chooseComputerMove = (board: gameBoard): int => {
  let availableMoves = 
    predictFuture(board, O)
    |> ListLabels.map(~f=((idx, futureBoard)) => (idx, updateWinner(futureBoard)))
    |> ListLabels.filter(~f=(((idx, _future) )=> ListLabels.mem(idx, ~set=availableMoves(board))));
  let compWinners = ListLabels.filter(~f=((_idx, winner)) => winner === Some(O), availableMoves);
  let playerWinners = ListLabels.filter(~f=((_idx, winner)) => winner === Some(X), availableMoves);
  let noWinners = ListLabels.filter(~f=((_idx, winner)) => winner === None, availableMoves);

  let getIdxs = input => ArrayLabels.of_list(
    ListLabels.map(
      ~f=Pervasives.fst,
      input)
    );
  Js.log(getIdxs(availableMoves));
  Js.log(getIdxs(compWinners));
  Js.log(getIdxs(playerWinners));
  Js.log(getIdxs(noWinners));

  ListLabels.filter(~f=isSome, [safeHd(compWinners), safeHd(playerWinners), safeHd(noWinners)])
  |> ListLabels.map(~f=optionMap(Pervasives.fst))
  |> safeHd
  |> optionFlatMap(a => a)
  |> getOrElse(1)
}

/* greeting and children are props. `children` isn't used, therefore ignored.
We ignore it by prepending it with an underscore */
let make = (~greeting, _children) => {
  /* spread the other default fields of component here and override a few */
  ...component,

  initialState: () => {turn: X, board: initialBoard, winner: None},

  /* State transitions */
  reducer: (action, state) =>
    switch (action) {
    | Click(squareIndex) => {
      let boardAfterPlayer = updateBoard(state.board, state.turn, squareIndex);
      let boardAfterComputer = updateBoard(boardAfterPlayer, O, chooseComputerMove(boardAfterPlayer));

      let winnerAfterPlayer = updateWinner(boardAfterPlayer);
      let winnerAfterComp = updateWinner(boardAfterComputer);
      let winner = isSome(winnerAfterPlayer) ? winnerAfterPlayer : winnerAfterComp;

      let newState = {
        board: isSome(winnerAfterPlayer) ? boardAfterPlayer : boardAfterComputer,
        turn: X,
        winner: winner,
      };

      ReasonReact.Update(newState)
    }
  },

  render: self => {
    let message =
      switch (self.state.winner) {
        | None => "It's " ++ playerToString(self.state.turn) ++ "'s turn";
        | Some(player) => playerToString(player) ++ " wins!"
      }
    let renderSquare(square: square, index: int): ReasonReact.reactElement =
      switch (square) {
        | None =>
          <button disabled=(isSome(self.state.winner)) onClick=(_event => self.send(Click(index)))>
            (ReasonReact.string("Select"))
          </button>
        | Some(player) => <span>(ReasonReact.string(playerToString(player)))</span>
      };
    let matrix = arrToMatrix(self.state.board);
    let rows = ArrayLabels.mapi(
      ~f=(i, row) =>
        <div key=("row-key-" ++ string_of_int(i))>
          <div>
            (ReasonReact.array(
              ArrayLabels.map(
                ~f=((i: int, square: square)) => <span key=("square-key-" ++ string_of_int(i))>(renderSquare(square, i))</span>,
                row
              )
            ))
          </div>
        </div>,
        matrix
    );
    <div>
      <h1>(ReasonReact.string(greeting))</h1>
      <h2>(ReasonReact.string(message))</h2>
      <div>(ReasonReact.array(rows))</div>
    </div>;
  },
};
