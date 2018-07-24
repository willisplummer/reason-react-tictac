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

let updateWinner = (currentBoard: gameBoard): winner => {
  let winner: list(square) = ListLabels.map(
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
  |> ListLabels.filter(
    ~f=isSome
  );

  ListLabels.length(winner) > 0 ? ListLabels.hd(winner) : None
};

let updateBoard = (currentBoard: gameBoard, player: player, squareIndex: int): gameBoard => {
  currentBoard[squareIndex] = Some(player);
  currentBoard
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
      let newBoard = updateBoard(state.board, state.turn, squareIndex);
      let newState = {
        board: newBoard,
        turn: switchPlayer(state.turn),
        winner: updateWinner(newBoard),
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

/* TODO:
- X Check for a winner
- Check for a tie
- Calculate all possible moves
- Make computer select random move and play as O's
- Start/Finish gamestate
- Styling */
