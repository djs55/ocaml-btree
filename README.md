This is a work-in-progress.

The aim is to create a persistent on-disk map for use as an Irmin backing
store.

Assumptions and consequences
============================

- This will be optimised for append operations (since Irmin is history-preserving)

Append operations are efficient on SSD devices since an update to an allocated
sector involves erasing and rewriting a whole block (see
[CloudFS](https://labs.vmware.com/flings/cloudfs)).

- This will be single-reader and single-writer, like a traditional (non-clustered)
  filesystem.

We will not need to implement locking or fencing, as all locks will be stored
in the memory of the single process accessing it.

- This will be transactional: updates will either be committed or not.

We will not support grey-areas such as modes where metadata is committed but
data is not.

Design
======

The system will have 2 layers:

1. a heap supporting object allocation and deallocation. Space will be reclaimed
   by means of a garbage collector. This is similar to the OCaml heap but over
	 a `BLOCK` device.
2. a b-tree supporting functional update i.e. an update will require a new root
   to be created, referencing some new and some existing tree nodes, which will
	 be atomically switched to commit the update.
