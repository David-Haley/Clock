FROM --platform=linux/arm64 arm64v8/ubuntu:24.04

RUN apt-get update && apt-get install -y --no-install-recommends \
    gnat gprbuild libgpiod-dev linux-libc-dev \
    python3 python3-pip \
 && pip3 install --break-system-packages websockets \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /build/Clock
CMD ["gprbuild", "-P", "/build/Clock/iot_clock.gpr", "-j0"]
