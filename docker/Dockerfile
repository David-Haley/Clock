FROM arm64v8/ubuntu:latest

# TZ here is just a safe default for a non-interactive tzdata install at build
# time; the actual simulator timezone is set per-run via `docker run -e TZ=...`
# (see docker/run_sim.sh).
ENV DEBIAN_FRONTEND=noninteractive TZ=Etc/UTC

RUN apt-get -o APT::Sandbox::Seccomp=false update \
 && apt-get -o APT::Sandbox::Seccomp=false install -y --no-install-recommends \
    gnat gprbuild libgpiod-dev linux-libc-dev \
    libmosquitto-dev \
    tzdata \
    python3 python3-pip make rsync \
 && pip3 install --break-system-packages websockets \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /build/Clock
CMD ["gprbuild", "-P", "/build/Clock/iot_clock.gpr", "-j0"]
