#-------------------------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See https://go.microsoft.com/fwlink/?linkid=2090316 for license information.
#-------------------------------------------------------------------------------------------------------------

FROM rust:latest

EXPOSE 9228

ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=$USER_UID

COPY rustfmt.toml /.rustfmt.toml

# Configure apt and install packages
RUN apt-get update \
    && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends apt-utils dialog 2>&1 \
    #
    # Verify git, needed tools installed
    && apt-get -y install git openssh-client less iproute2 procps lsb-release \
    #
    # Install lldb, vadimcn.vscode-lldb VSCode extension dependencies
    && apt-get install -y lldb python3-minimal libpython3.7 \
    #
    # Install Rust components
    && rustup update 2>&1 \
    #
    # Create a non-root user to use if preferred - see https://aka.ms/vscode-remote/containers/non-root-user.
    && groupadd --gid $USER_GID $USERNAME \
    && useradd -s /bin/bash --uid $USER_UID --gid $USER_GID -m $USERNAME \
    # [Optional] Add sudo support for the non-root user
    && apt-get install -y sudo \
    && echo $USERNAME ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/$USERNAME\
    && chmod 0440 /etc/sudoers.d/$USERNAME

RUN rustup default nightly && rustup component add rust-src rustfmt clippy

RUN apt-get -y install libarchive-dev

RUN wget https://github.com/NixOS/nixpkgs/archive/32b46dd897ab2143a609988a04d87452f0bbef59.tar.gz -O nixpkgs.tar.gz && tar xzvf nixpkgs.tar.gz

ENV NIX_PATH nixpkgs=/nixpkgs-32b46dd897ab2143a609988a04d87452f0bbef59
ENV RUST_LOG rnix=debug
