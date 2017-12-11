# Shlack
### A chat app by Robert Zajac (rzajac) and Alex Haynes (alhaynes)

## Files
* shlack/lib/Model.hs
The shared type definitions between server and client. A sketch of the ADTs needed for our system.
* shlack/lib/Server.hs
The server logic. Contains type definitions for both production server and testing server.
* shlack/lib/Client.hs
The client logic. Contains formatting for terminal.

* shlack/client/Main.hs
A driver for the client.
* shlack/server/Main.hs
A driver for the server.

* shlack/test/ServerTesting.hs
Some basic test cases for the server. Includes mock monad logic. To be fleshed out more before demo.
* shlack/test/ClientTesting.hs
Some basic test cases for the client.
* shlack/test/Spec.hs
Driver for tests.

## Compilation
cd into shlack/
run stack build
use our getbins.sh script to get the binaries. you may need to edit the directories in the script to match your directories.
run ./server for the server and ./client for the client.
if stack doesn't work we have conveniently included all code and binaries at the root of this project.

## Design Notes
We did not use a state monad for the SeverState because the ServerState is already contained in an MVar, and the MVar supports state-like functions. Therefore, it'd be an unnecessary and unuseful to add an extra abstraction.