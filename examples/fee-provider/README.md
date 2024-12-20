# External fee and collateral provider

This is an advanced example doing the following things:

1. Load the script blueprint
2. Load cost models from an API provider (koios)
3. Lock 2 ada into the script address
4. Unlock the 2 ada while asking another wallet to pay the fees and provide a collateral

## The aiken script

The aiken script is very minimalist.
Basically any validation requires a signature from the credential stored in the datum.

```sh
# Build the script -> generate the plutus.json blueprint
aiken build
```

## The elm-cardano frontend

The web frontend is composed of two independant Elm apps, compiled to a single bundle.
The first one, `src/Main.elm` contains the main logic, for the person executing the steps in the introduction.
The second one, `src/External.elm` emulates an external provider for the fees and collateral.

Both Elm apps will be connected to a different wallet.
They will communicate with each other via ports, at two occasions.

1. When the main app asks for available UTxOs in preparation for Tx building
2. When the main app asks the other one to sign the prepared transaction

This communication via ports is a good simulation of how the main app would communicate to a backend server.
But here the advantage is that all this happens in the frontend so it’s easier to set up.

```sh
# Build the frontend and start a static server
npx elm-cardano make src/Main.elm src/External.elm --output main.js && python -m http.server
```
