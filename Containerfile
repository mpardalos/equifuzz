FROM haskell:9.2.7 AS BUILDER

WORKDIR /opt/equifuzz

RUN cabal update

# Add just the .cabal file to capture dependencies
COPY ./equifuzz.cabal /opt/equifuzz/equifuzz.cabal

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN cabal build --only-dependencies -j4

# Add and Install Application Code
COPY . /opt/equifuzz
RUN cabal install

FROM redhat/ubi9-minimal:9.2

RUN microdnf install -y gmp openssh-clients g++

COPY --from=BUILDER /root/.cabal/bin/equifuzz /equifuzz
