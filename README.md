# Langton's Ant in Haskell


[Langton's Ant](https://en.wikipedia.org/wiki/Langton%27s_ant) implemented in
the Haskell programming language.

I made this project to help me learn Haskell. It's the first real project I've
done in the language, so it's probably not perfect, but it works well enough.

### How to Use

With Cabal you can just run `cabal run` to run the program directly with
arguments like so:

```
$ cabal run -- -a "45,45,Up 55,55,Down 45,55,Left 55,45,Right" \
    -r "TurnLeft TurnRight" \
    -h 100 \
    -w 100 \
    -n 1000
```

Alternatively build it first with:

```
cabal configure
cabal build
```

And find the executable under `dist/build/langtons-ant`. 

You can also use `cabal install` if you want to install the executable to your
system.

Alternatively you can compile and run it all in one go without leaving behind
intermediate files:

`$ runhaskell src/LangtonsAnt.hs -n 10000 -a "50,50,Up" -r "Continue TurnLeft TurnLeft" -p`

### Available Options

For a summary of available options, do `langtons-ant --help`

```
Usage: langtons-ant [OPTION]...
  -a ANTSTR   --ants=ANTSTR    specify ant string, like '20,20,Up, 15,10,Left'
  -r RULESTR  --rules=RULESTR  specify rule string, like 'TurnLeft, Continue, UTurn'
  -h N        --height=N       specify board height
  -w M        --width=M        specify board width
  -p          --wrap           ants wrap around board edges
  -n X        --number=X       specify number of iterations
  -g          --graphical      display steps in a graphical window
  -v          --version        output version information and exit
              --help           display this help and exit
```

### Default Options

If you omit any option its default will be substituted:

* -a (Ant string): "50,50,Up"
* -r (Rule string): "TurnLeft, TurnRight"
* -h (Height): 100
* -w (Width): 100
* -p (Wrap): no wrap
* -n (Iteration): 11000
