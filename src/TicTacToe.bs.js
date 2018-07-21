// Generated by BUCKLESCRIPT VERSION 4.0.0, PLEASE EDIT WITH CARE
'use strict';

var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var React = require("react");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");

var initialBoard = /* array */[
  undefined,
  undefined,
  undefined,
  undefined,
  undefined,
  undefined,
  undefined,
  undefined
];

function updateBoard(currentBoard, player, squareIndex) {
  Caml_array.caml_array_set(currentBoard, squareIndex, player);
  return currentBoard;
}

function switchPlayer(currentPlayer) {
  if (currentPlayer) {
    return /* X */0;
  } else {
    return /* O */1;
  }
}

function playerToString(currentPlayer) {
  if (currentPlayer) {
    return "O";
  } else {
    return "X";
  }
}

var component = ReasonReact.reducerComponent("Example");

function make(greeting, _) {
  return /* record */[
          /* debugName */component[/* debugName */0],
          /* reactClassInternal */component[/* reactClassInternal */1],
          /* handedOffState */component[/* handedOffState */2],
          /* willReceiveProps */component[/* willReceiveProps */3],
          /* didMount */component[/* didMount */4],
          /* didUpdate */component[/* didUpdate */5],
          /* willUnmount */component[/* willUnmount */6],
          /* willUpdate */component[/* willUpdate */7],
          /* shouldUpdate */component[/* shouldUpdate */8],
          /* render */(function (self) {
              var message = "It's " + ((
                  self[/* state */1][/* turn */0] ? "O" : "X"
                ) + "'s turn");
              return React.createElement("div", undefined, React.createElement("h1", undefined, greeting), React.createElement("h2", undefined, message), React.createElement("button", {
                              onClick: (function () {
                                  return Curry._1(self[/* send */3], /* Click */[0]);
                                })
                            }, message));
            }),
          /* initialState */(function () {
              return /* record */[
                      /* turn : X */0,
                      /* board */initialBoard
                    ];
            }),
          /* retainedProps */component[/* retainedProps */11],
          /* reducer */(function (action, state) {
              return /* Update */Block.__(0, [/* record */[
                          /* turn */state[/* turn */0] ? /* X */0 : /* O */1,
                          /* board */updateBoard(state[/* board */1], state[/* turn */0], action[0])
                        ]]);
            }),
          /* subscriptions */component[/* subscriptions */13],
          /* jsElementWrapped */component[/* jsElementWrapped */14]
        ];
}

exports.initialBoard = initialBoard;
exports.updateBoard = updateBoard;
exports.switchPlayer = switchPlayer;
exports.playerToString = playerToString;
exports.component = component;
exports.make = make;
/* component Not a pure module */
