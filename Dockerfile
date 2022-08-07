FROM lnl7/nix:2.0

WORKDIR /tmp/pipes-files

COPY default.nix /tmp/pipes-files
COPY pipes-files.cabal /tmp/pipes-files
COPY hierarchy /tmp/hierarchy

# Install tools needed by builtins.fetchTarball, and then install all
# dependencies into its own layer, which doesn't change.
RUN nix-env -f '<nixpkgs>' -i gnutar gzip && \
    nix-shell -Q -j2 --run true

COPY . /tmp/pipes-files
RUN nix-env -f . -i pipes-files
