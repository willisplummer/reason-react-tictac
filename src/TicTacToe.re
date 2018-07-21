type player = X | O;

type gameBoard = array(option(player));

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

let renderSquare(square: option(player)) => square: 

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
    <div>
      <h1>(ReasonReact.string(greeting))</h1>
      <h1>testss</h1>
      <h2>(ReasonReact.string(message))</h2>
      (ArrayLabels.map(renderSquare, self.state.board.map)
      <button onClick=(_event => self.send(Click(0)))>
        (ReasonReact.string(message))
      </button>
    </div>;
  },
};
