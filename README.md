# autoreload.hx

Reload open files in Helix when they change on disk.

## How it works

Polls `mtime` of every open, file-backed document (default every 700 ms) and
calls `editor-document-reload` when the file changed. Deliberately
conservative:

* never reloads a buffer with unsaved edits (shows a warning instead);
* ignores the editor's own saves via the `document-saved` hook;
* compares the file contents with the buffer, so `touch` and no-op writes do
  not cause a reload;
* waits for normal mode before reloading the focused buffer;
* a single timer chain, one check per file per tick.

This is polling, not inotify: latency equals the interval. Each tick does one
`stat` per open file and no work when nothing changed.

## Install

```scheme
;; init.scm
(require "autoreload/autoreload.scm")
```

## Config

```scheme
(autoreload-config #:interval-ms 700)
(autoreload-config #:enabled #f)
```

## Commands

| Command | Action |
|---|---|
| `:autoreload-toggle` | enable/disable |
| `:autoreload-enable` | enable |
| `:autoreload-disable` | disable |
| `:autoreload-config` | report current settings |
