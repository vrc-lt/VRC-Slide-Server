FROM fpco/stack-build:lts-12.26
WORKDIR /usr/lib/gcc/x86_64-linux-gnu/5.4.0
RUN cp crtbeginT.o crtbeginT.o.orig
RUN cp crtbeginS.o crtbeginT.o
ADD ./stack.yaml /work/stack.yaml
ADD ./package.yaml /work/package.yaml
WORKDIR /work
RUN stack setup
ADD ./app /work/app
RUN stack --system-ghc --local-bin-path /sbin build --ghc-options '-optl-static -fPIC -optc-Os'

FROM alpine:latest
RUN mkdir /work
COPY --from=0 /work/.stack-work /work/.stack-work
COPY --from=0 /work/.stack-work/install/x86_64-linux/lts-12.26/8.4.4/bin /work/
CMD ["/work/spock"]
