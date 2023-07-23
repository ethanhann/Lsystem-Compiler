# Lsystem-Compiler

## Compile

1. Run Makefile ("make")
2. ./lsystem -c (lsystem file name w/ extension)
3. java (filename)

For example, to build the dragon fractal:

```sh
./lsystem -c Demo/dragon.ls && java dragon
``` 

## Tests

To run the test shell script, use "bash test.sh".  Using "sh test.sh" will not work as the script uses some bash-exclusive commands.
