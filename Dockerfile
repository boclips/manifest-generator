FROM fpco/stack-build:lts-13.8
ADD . .
RUN stack \
    --no-terminal \
    --stack-yaml=./stack.yaml \
    build \
    --test \
    --copy-bins \
    --ghc-options=-Werror
RUN rm -rf \
    /root/.stack \
    /.stack-work
