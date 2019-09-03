# Curryfish 
A simple UCI chess engine implemented in Haskell. 

Specification of the UCI-protocol: http://wbec-ridderkerk.nl/html/UCIProtocol.html

This engine supports the following commands: 
* `go`
* `go movetime 1000`
* `stop`
* `uci`
* `ucinewgame`
* `position` followed by a fen string or moves
* `quit`
* `isready`

Moves are generated with the MinMax algorithm.
