# Plan B

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/plan-b.svg?style=flat)](https://hackage.haskell.org/package/plan-b)
[![Stackage Nightly](http://stackage.org/package/plan-b/badge/nightly)](http://stackage.org/nightly/package/plan-b)
[![Stackage LTS](http://stackage.org/package/plan-b/badge/lts)](http://stackage.org/lts/package/plan-b)
[![Build Status](https://travis-ci.org/mrkkrp/plan-b.svg?branch=master)](https://travis-ci.org/mrkkrp/plan-b)
[![Coverage Status](https://coveralls.io/repos/mrkkrp/plan-b/badge.svg?branch=master&service=github)](https://coveralls.io/github/mrkkrp/plan-b?branch=master)

This is a Haskell library helping perform failure-tolerant operations on
files and directories.

## Quick start

The library allows to create and/or edit files, directories, and
containers. By “container” we mean archive-like object that can contain
representation of a directory inside it. Consequently, we have six functions
available:

* `withNewFile`
* `withExstingFile`
* `withNewDir`
* `withExistingDir`
* `withNewContainer`
* `withExistingContainer`

You specify name of object to edit or create, options (more on them below),
and action that is passed `Path` argument with the same type as object you
intend to edit (we use type-safe file paths from
[`path`](https://hackage.haskell.org/package/path) package here to prevent a
certain class of potential bugs). Then, having that path, you can do all
actions you want to and if at some point during this editing an exception is
thrown, state of file system is rolled back — you get no corrupted files,
half-way edited directories, everything is intact as if nothing happened at
all. If, however, the action is executed successfully (i.e. no exceptions
thrown), all your manipulations are reflected in the file system.

This is a lightweight solution that makes it harder to corrupt sensitive
information. And since file system exists in the real world, all sorts of
bad things *can* (and *will*) happen. *You should always have plan B.*

Temporary files and back-ups are handled and deleted automatically, however
you can pass options to change default behaviors. Not all options can be
used with every function, but wrong combinations won't type-check, so it's
OK.

Collection of options is a monoid. `mempty` corresponds to default behavior,
while non-standard behavioral deviations can be `mappend`ed to it.

By default, when we want to create new object and it already exists, we get
an exception, two alternative options exist (only work when you create new
object):

* `overrideIfExist`
* `useIfExist`

There is no way to prevent exception when you want to *edit* object that
does not exist, though.

All functions make use of temporary directories. You can control certain
aspects of this business:

* `tempDir dir` — will tell the library to create temporary directories and
  files inside `dir`. By default system's standard temporary directory
  (e.g. `/tmp/` on Unix-like systems) is used.

* `nameTemplate template` — specifies template to use for generation of
  unique file and directory names. By default `"plan-b"` is used.

* `preserveCorpse` — if you add this to options, in case of failure
  (exception), temporary directory is not automatically deleted and can be
  inspected. However, if operation succeeds, temporary directory is *always*
  deleted.

* `moveByRenaming` — by default files and directories are moved by copying,
  this option enables moving by renaming.

That should be enough for a quick intro, for more information regarding
concrete functions, consult Haddocks.

## License

Copyright © 2016–2017 Mark Karpov

Distributed under BSD 3 clause license.
