shrinkray
=========

This is a work in progress. That means I don't promise that it works. :)

# Requirements

Haskell, cabal, Selenium Server

# Installation

First, clone the repo.

Then..

```
cabal sandbox init

cabal install

cabal configure

cabal build
```

You now have a binary in...

```
$ dist/build/shrinkray/shrinkray
usage: shrinkray URL element_id
```

Now start your Selenium Server...

``` 
java -jar selenium-server-standalone-2.44.0.jar
```

You can now fire shrinkrays at websites, searching for possible responsive design issues against the element if your as in the example below..

```
dist/build/shrinkray/shrinkray http://www.google.com gsr
```
