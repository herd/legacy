herd-opam-repository is a custom `OPAM <https://opam.ocaml.org/>`__ repository for `the herd organization <http://github.com/herd>`__'s packages.

You should use this OPAM repository only if you need the latest version of some of our packages quicker than we can add them to the default OPAM repository.
Most of the time, the default OPAM repository will be up-to-date.

Quick start
===========

In your shell::

    $ opam repository add herd git@github.com:herd/herd-opam-repository.git

    $ opam install herdtools7  # For example

To update the installed packages::

    $ opam update herd

    $ opam upgrade
