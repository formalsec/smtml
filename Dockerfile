ARG BASE_OS=ubuntu:24.04
FROM ${BASE_OS}

ENV BASE=/home/smtml \
    DEBIAN_FRONTEND=noninteractive

SHELL ["/bin/bash", "-c"]

RUN apt-get update && apt-get install -y \
    curl \
    git \
    sudo \
    python3-pip \
    unzip \
    rsync \
    mercurial \
    libgmp-dev \
    pkg-config \
    libgsl-dev \
    liblapacke-dev \
    libopenblas-dev \
    libgsl-dev \
    gnuplot-x11 \
    libsqlite3-dev

RUN echo "/usr/local/bin" | bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)" \
    pip install --break-system-packages --upgrade pip setuptools

RUN useradd -m -s /bin/bash smtml \
    && echo "smtml ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

USER smtml
COPY --chown=smtml:smtml . ${BASE}/smtml
WORKDIR ${BASE}/smtml

RUN opam init --disable-sandboxing --shell-setup -y \
    && opam update \
    && opam switch create -y z3-bitwuzla 5.2.0 \
    && eval $(opam env --switch=z3-bitwuzla) \
    && opam install -y . --deps-only --with-test --with-dev-setup \
    && opam install -y z3 bitwuzla-cxx dune-glob \
    && dune build && dune install

RUN opam switch create -y cvc5 5.2.0 \
    && eval $(opam env --switch=cvc5) \
    && opam install -y . --deps-only --with-test --with-dev-setup \
    && opam install -y --confirm-level=unsafe-yes cvc5 dune-glob \
    && dune build && dune install

RUN opam switch create -y colibri2 5.2.0 \
    && eval $(opam env --switch=colibri2) \
    && opam install -y . --deps-only --with-test --with-dev-setup \
    && opam install -y --confirm-level=unsafe-yes colibri2 dune-glob \
    && dune build && dune install