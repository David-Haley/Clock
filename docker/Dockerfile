FROM arm64v8/ubuntu:latest

RUN apt-get -o APT::Sandbox::Seccomp=false update \
 && apt-get -o APT::Sandbox::Seccomp=false install -y --no-install-recommends \
    gnat gprbuild libgpiod-dev linux-libc-dev \
    libmosquitto-dev \
    python3 python3-pip make rsync \
 && pip3 install --break-system-packages websockets \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /build/Clock
CMD ["gprbuild", "-P", "/build/Clock/iot_clock.gpr", "-j0"]
