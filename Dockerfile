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
    libgsl-dev

RUN echo "/usr/local/bin" | bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)" \
    pip install --break-system-packages --upgrade pip setuptools

RUN useradd -m -s /bin/bash smtml \
    && echo "smtml ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

USER smtml
WORKDIR ${BASE}

RUN opam init --disable-sandboxing --shell-setup -y \
    && opam switch create -y z3-bitwuzla 5.2.0 \
    && eval $(opam env --switch=z3-bitwuzla) \
    && opam update \
    && opam install -y dune zarith prelude cmdliner yojson rusage lwt cohttp-lwt-unix core_unix lwt owl \
    && echo "eval \$(opam env --switch=z3-bitwuzla)" >> ~/.bash_profile

RUN opam switch create -y cvc5 5.2.0 \
    && eval $(opam env --switch=cvc5) \
    && opam update \
    && opam install -y dune \
    && echo "eval \$(opam env --switch=cvc5)" >> ~/.bash_profile

RUN opam switch create -y colibri2 5.2.0 \
    && eval $(opam env --switch=colibri2) \
    && opam update \
    && opam install -y dune \
    && echo "eval \$(opam env --switch=colibri2)" >> ~/.bash_profile

RUN git clone https://github.com/formalsec/smtml.git ${BASE}/smtml

WORKDIR ${BASE}/smtml

RUN eval $(opam env --switch=z3-bitwuzla) \
    && sudo apt update \
    && opam install -y . --deps-only --confirm-level=unsafe-yes \
    && dune build \
    && dune install