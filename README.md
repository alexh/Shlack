# Shlack
### A chat app by Robert Zajac and Alex Haynes for CIS552 (Advanced Programming)

## Files
* shlack/lib/Model.hs
The shared type definitions between server and client.
* shlack/lib/Server.hs
Server implementation, including type definitions used in testing.
* shlack/lib/Client.hs
Client implementation.

* shlack/client/Main.hs
A driver for the client.
* shlack/server/Main.hs
A driver for the server.

* shlack/test/ServerTesting.hs
Test suite for server, including a mock server monad.
* shlack/test/ClientTesting.hs
Test suite for client.
* shlack/test/Spec.hs
Driver for tests.

## Compilation
* cd into shlack/
* run stack build
* use getbins.sh to get the binaries. you may need to edit the directories in the script to match your directories.
* run ./server and ./client
* You could also run `stack exec server` and `stack exec client` from shlack/, but we've noticed strange behavior in this case and do not recommend it.