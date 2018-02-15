# autochrome

Autochrome is a program for structurally diffing and highlighting clojure source code.  It generates diffs as static HTML from
github pull requests or local git repos. For more information, some examples, and a description of how it works, please see
[the HTML readme](https://fazzone.github.io/autochrome.html)
 (generated from [readme.clj](https://github.com/ladderlife/autochrome/blob/master/src/autochrome/readme.clj)).

## Usage
```
$ lein run <owner> <repo> <pr-number> -o diff.html        # write a diff for a GitHub pull request
$ lein run --token user:123abc <owner> <repo> <pr-number> # use supplied auth token for github api
$ lein run ... --open                                     # try to open the diff in a browser
$ lein uberjar                                      # create a standalone jar in target/ directory
$ java -jar autochrome.jar <old-tree> <new-tree>          # run like git diff from your repo
```

## License

Copyright © 2018 Ladder Financial, Inc.

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
