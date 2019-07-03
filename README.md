# Langton's Ant in Haskell


[https://en.wikipedia.org/wiki/Langton%27s_ant](Langton's Ant) implemented in
the Haskell programming language.

I made this project to help me learn Haskell. It's the first real project I've
done in the language, so it's probably not perfect, but it works well enough.

## Instructions

Compile with GHC:

`ghc LangtonsAnt.hs`

Then run it with arguments like so:

```./LangtonsAnt -a "45,45,Up 55,55,Down 45,55,Left 55,45,Right" \
  -r "TurnLeft TurnRight" \
  -h 100 \
  -w 100 \
  -n 1000```

For a summary of available options, do

`./LangtonsAnt --help`
