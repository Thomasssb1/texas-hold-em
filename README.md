# Texas-hold-em
The goal of this implementation using Haskell is to create a working game loop of poker, where each individual user can have separate strategies when deciding whether to call, fold or raise.
<a name="figure-2"><img alt="image" src="https://github.com/user-attachments/assets/f0272c9a-e4c8-4fa3-ae01-1d0a08473b42" /></a>

## Getting started
First, open `ghci` and expose `mtl` in order to load the State monad.
```bash
ghci> :set -package mtl
```
Once exposed, you can load `main.hs` and then call `main` in order to start the game.
```bash
ghci> :load main.hs
ghci> main
```

When starting the game by calling the `main` function, you are asked how many players you wish to play the game. 
Once an integer is input, you are asked for each player – what strategy you want them to follow (Aggressive, Passive, Random, Smart or Human). 
Once created the game will start and will enter gameLoop. If you have any human players then the game will wait for user input – with each input being validated to ensure that you can only enter “call”, “fold”, or “raise” as an action (these are case-insensitive). 
Once an action is selected, it will be validated or in the case of "raise” will require another input – which is the amount to raise up to. If any of these actions are considered invalid (e.g. player does not have enough chips to call), then the user will need to re-enter another action until one is valid.

## Clarifications
The following are clarifications regarding [figure 2](figure-2).
### Return type of (Bool, <\<function\>>)
The return type of (Bool, <\<function\>>) for each of the actions represents whether the action is valid or not along with the action function (e.g. call, fold or raise). This allows for the random, aggressive, passive and smart strategies to work without needing to check before compiling the list of strategies but after instead.
This allows for a ranking of each strategy which is particularly useful for the smart strategy as, for example, if the raise action is invalid then the next likely action would be call and then finally fold. The returned function is an incomplete function which requires the player to be passed to it to run and each action is recursively checked to determine whether it is a valid action before updating the state.

### State management
The state is managed using a StateT monad, which allows for monad transformation of the State and IO monad so that output can be printed to the screen. Most functions do not return a value as shown in Figure 2, but they update a state by using the execStateT function. Although the state is returned by the execStateT (or equivalent) function, it is not displayed on the diagram as it the functions themselves do not explicitly return anything.
StateT was used in order to be able to lift into the IO context to be able to create output to the screen in order to print additional information and implement the human strategy.
