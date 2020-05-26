# goat-lang

## Build

```
$ ./_scripts/build
```

## Compile and run

```
$ stack exec -- goat-exe ./in/{FILENAME}.gt/ > ./out/{FILENAME}.oz
$ ./oz/oz ./out/{FILENAME}.oz
```

## Test and verify

```
$ ./_scripts/test
```