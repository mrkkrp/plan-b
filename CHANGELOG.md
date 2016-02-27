## Plan B 0.2.0

* Added `moveByRenaming` option to allow move files and directories by
  renaming. This is not default because moving by renaming is not always
  possible, while moving by coping is always an option.

## Plan B 0.1.1

* Fixed the problem with moving of files and directories from `/tmp/` to
  some location not under `/tmp/` resulting in “unsupported operation”
  exception on Unix. Now they are moved by copying.

## Plan B 0.1.0

* Initial release.
