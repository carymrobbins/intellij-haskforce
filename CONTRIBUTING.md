# Contributing to HaskForce

## Filing an issue

Thanks for reporting an issue with HaskForce.  Depending on your issue, it may be a good
idea to provide your OS flavor and versions of IntelliJ and any Haskell tools which
may be involved (e.g. if you are having autocompletion issues, providing your ghc-mod
version is usually necessary).

Unfortunately, HaskForce lacks full-time developers dedicated to resolving bugs and
implementing new features.  If a bug is holding you back or you'd really like a new feature,
opening an issue is a great start, but contributing the code yourself will get it done
significantly faster.  If you don't know where to start, simply ask in your issue,
and someone should respond promptly with some guidance.

## Submitting pull requests

### Bug fix or new feature

You've done the hard work of making HaskForce better.  Thank you!

Pull requests should be reviewed and merged within a month.  To help
make the reviewer's life easier and get your awesome addition merged sooner,
please follow these guidelines -

1. Send patches via GitHub pull requests.
1. Each pull request should achieve a specific goal and have a descriptive title.
   Don't put multiple unrelated changes in a single pull request.
   (An exception can be made for multiple very simple self-contained commits,
   or for changes with a hard dependency on each other.)
1. Typically, a pull request should consist of just a few commits.
   Rewrite the history (see `git rebase`) to make commits logical, not historical.
1. Write descriptive commit messages.
1. If you want to amend a pull request, rewrite your branch and leave a comment as opposed
   to adding commits to the branch or opening a new pull request.
1. Make sure the tests pass.  You can track them via
   [travis](https://travis-ci.org/carymrobbins/intellij-haskforce).
1. Unless the change is trivial, it should be accompanied by the tests that
   show the effects of the change.

   New tests should be added in a commit separate
   from the code changes. On the other hand, if your change breaks some tests,
   the tests should be preferably updated in the same commit.

### Refactoring or architectural changes

Please contact [Cary Robbins](https://github.com/carymrobbins) to discuss before
you start refactoring to possibly save some time and effort.  There are plenty of
potential gotchas involved here.

## Other ways to contribute

### Review pull requests

You can help by reviewing pull requests submitted by others.
If you've found any issues, raise them in the comments.
If you've done a review and think that everything is fine, please say so, too - it does help!

### Fix tests

Some tests simply document the existing (but wrong) behavior.

Make sure that there is an existing issue in the tracker about such a wrong behavior.
Link to the issue in the test.

------------------

Special thanks to [haskell-src-exts](https://github.com/haskell-suite/haskell-src-exts)
as the source of this document's
[plagiarism](https://github.com/haskell-suite/haskell-src-exts/blob/master/CONTRIBUTING.md).


