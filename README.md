# Langton's Ant in Haskell


[Langton's Ant](https://en.wikipedia.org/wiki/Langton%27s_ant) implemented in
the Haskell programming language.

I made this project to help me learn Haskell. It's the first real project I've
done in the language, so it's probably not perfect, but it works well enough.

### How to Use

With Stack you can just run `stack run` to run the program directly with
arguments like so:

```
$ stack run -- -a "45,45,Up 55,55,Down 45,55,Left 55,45,Right" \
    -r "TurnLeft TurnRight" \
    -h 100 \
    -w 100 \
    -n 1000
```

Alternatively build it first with:

```
$ stack setup
$ stack build
```

You can also use `stack install` if you want to install the executable to your
system.

### Available Options

For a summary of available options, do `langtons-ant --help`

```
Usage: langtons-ant [OPTION]...
  -a ANTSTR   --ants=ANTSTR    ant string, like '20,20,Up, 15,10,Left'
  -r RULESTR  --rules=RULESTR  rule string, like 'TurnLeft, Continue, UTurn'
  -h N        --height=N       board height
  -w M        --width=M        board width
  -p          --wrap           ants wrap around board edges
  -n X        --number=X       number of iterations for non-graphical mode
  -g          --graphical      display steps in a graphical window
  -v          --version        output version information and exit
              --help           display this help and exit
```

Normally, the result of running the simulation for N steps will be printed to
stdout. However, if the graphical mode flag (`-g`) is set, the simulation will
be run graphically in a window until all ants fall off the board.

### Default Options

If you omit any option its default will be substituted:

* -a (Ant string): "50,50,Up"
* -r (Rule string): "TurnLeft, TurnRight"
* -h (Height): 100
* -w (Width): 100
* -p (Wrap): no wrap
* -n (Iteration): 11000
* -g (Graphical): non-graphical

### Invalid Options

If you pass the program impossible options, like a negative width, you'll get an
informative error like `Width too small`. If you give the program legal but
invalid options, like having no ants starting within the bounds of the board,
you'll get no output. This is intentional.
