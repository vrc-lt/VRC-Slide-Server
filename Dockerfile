FROM fpco/stack-build:lts-12.26 as build
WORKDIR /usr/lib/gcc/x86_64-linux-gnu/5.4.0
RUN cp crtbeginT.o crtbeginT.o.orig
RUN cp crtbeginS.o crtbeginT.o
ADD ./stack.yaml /work/stack.yaml
ADD ./package.yaml /work/package.yaml
WORKDIR /work
ADD ./app /work/app
ADD ./src /work/src
ADD ./views /work/views
# RUN stack --system-ghc --local-bin-path /sbin build --ghc-options '-optl-static -fPIC -optc-Os'
RUN stack --system-ghc --local-bin-path /sbin build 

FROM ubuntu:18.04
RUN apt-get update && apt-get install -y \
    ca-certificates \
    libgmp-dev \
    libpq-dev
RUN mkdir /work
COPY --from=build /work/.stack-work /work/.stack-work
COPY --from=build /work/.stack-work/install/x86_64-linux/lts-12.26/8.4.4/bin /work/
CMD ["/work/vrc-slide-server"]
