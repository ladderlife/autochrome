# autochrome

Autochrome is a program for structurally diffing and highlighting clojure source code.  It generates diffs as static HTML from
github pull requests or local git repos. For more information, some examples, and a description of how it works, please see
[the HTML readme](https://fazzone.github.io/autochrome.html)
 (generated from [readme.clj](https://github.com/ladderlife/autochrome/blob/master/src/autochrome/readme.clj)).

## Usage
### From local git repo:
```
$ lein run --open --git-dir /path/to/repo <old-ref> [<new-ref>]
```
- `old-ref` and `new-ref` are just like the arguments to `git diff`
- `--open` tries to open the diff in a browser.  With no arguments you get HTML on stdout.
- `-o <file>` also works.

### From github:
```
$ lein run <owner> <repo> <pr-number> -o diff.html        # write a diff for a GitHub pull request
$ lein run --token user:123abc <owner> <repo> <pr-number> # use supplied auth token for github api
```
## License

Copyright © 2018 Ladder Financial, Inc.

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
