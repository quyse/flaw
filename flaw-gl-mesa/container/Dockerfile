FROM base/archlinux as builder

RUN pacman -Syu --noconfirm \
	autoconf \
	automake \
	bison \
	flex \
	gcc \
	git \
	libdrm \
	libtool \
	libx11 \
	libxcb \
	libxext \
	llvm \
	make \
	pkg-config \
	python2-mako \
	xorgproto

RUN git clone -b mesa-18.1.4 --depth 1 https://github.com/mesa3d/mesa.git /root/mesa

RUN \
	cd /root/mesa && \
	./autogen.sh --disable-driglx-direct --disable-dri --disable-egl --disable-gbm --with-gallium-drivers=swrast,swr --enable-gallium-osmesa && \
	make -j$(nproc) && \
	make install && \
	cd /root && \
	rm -rf /root/mesa

RUN tar czf /root/mesa.tar.gz /usr/local

FROM base/archlinux

RUN pacman -Syu --noconfirm llvm-libs libx11 libxext

COPY --from=builder /root/mesa.tar.gz /root/mesa.tar.gz

RUN tar xf /root/mesa.tar.gz

ENV LD_LIBRARY_PATH=/usr/local/lib
