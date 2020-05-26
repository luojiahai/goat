# goat-lang

## Prerequisite

### The Haskell Tool Stack

```
curl -sSL https://get.haskellstack.org/ | sh
```

## Usage

### Build

```
$ ./_scripts/build
```

### Compile and run

```
$ stack exec -- goat-exe ./_data/in/{FILENAME}.gt/ > ./_data/out/{FILENAME}.oz
$ ./oz/oz ./_data/out/{FILENAME}.oz
```

### Test and verify

```
$ ./_scripts/test
```

### Clean

```
$ ./_scripts/clean
```
