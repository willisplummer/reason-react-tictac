type player = X | O;

type square = option(player);

type gameBoard = array(square);

/* State declaration */
type state = {
  turn: player,
  board: gameBoard,
};

/* Action declaration */
type action =
  | Click(int);

let initialBoard : gameBoard = ArrayLabels.make(9, None);

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

let arrToMatrix = (arr: array(square)): array(array(square)) => {
  let init: array(array(square)) = ArrayLabels.make_matrix(3, 3, None);
  let zipped : array((int, 'a)) = zipWithIndices(arr);
  let f = (acc, zippedVal): array(array('a)) => {
    let (index, a) = zippedVal;
    let x = index mod 3;
    let y = index / 3;
    acc[x][y] = a;
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

  initialState: () => {turn: X, board: initialBoard},

  /* State transitions */
  reducer: (action, state) =>
    switch (action) {
    | Click(squareIndex) => ReasonReact.Update({
      board: updateBoard(state.board, state.turn, squareIndex),
      turn: switchPlayer(state.turn)
    })
  },

  render: self => {
    let message =
      "It's " ++ playerToString(self.state.turn) ++ "'s turn";
    let renderSquare(square: square, index: int): ReasonReact.reactElement =
      switch (square) {
        | None =>
          <button onClick=(_event => self.send(Click(index)))>
            (ReasonReact.string("Select"))
          </button>
        | Some(player) => <span>(ReasonReact.string(playerToString(player)))</span>
      };
    let matrix = arrToMatrix(self.state.board);
    let rows = ArrayLabels.mapi(
      (i, row) =>
        <div key=("row-key-" ++ string_of_int(i))>
          <div>
            (ReasonReact.array(
              ArrayLabels.mapi(
                (i, square) => <span key=("square-key-" ++ string_of_int(i))>(renderSquare(square, i))</span>,
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
      <div>(ReasonReact.array(rows ))</div>
      <button onClick=(_event => self.send(Click(0)))>
        (ReasonReact.string(message))
      </button>
    </div>;
  },
};
