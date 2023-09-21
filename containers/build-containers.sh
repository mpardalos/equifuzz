#!/usr/bin/env bash

revision=$(git rev-parse --short HEAD)
if [[ $(git status --short --untracked-files=no) ]]; then
    revision="$revision-dirty"
fi
project_root=$(git rev-parse --show-toplevel)
registry="ghcr.io/mpardalos"

podman build \
    --tag $registry/equifuzz-full:$revision \
    --tag $registry/equifuzz-full:latest \
    --tag equifuzz-full:$revision \
    --tag equifuzz-full:latest \
    --file containers/Containerfile-full \
    "$project_root"

podman build \
    --tag $registry/equifuzz-evaluation:$revision \
    --tag $registry/equifuzz-evaluation:latest \
    --tag equifuzz-evaluation:$revision \
    --tag equifuzz-evaluation:latest \
    --file containers/Containerfile-evaluation \
    "$project_root"
