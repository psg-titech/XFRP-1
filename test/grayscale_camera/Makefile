.PHONY: all clean
all: grayscale_cuda grayscale_cpu1 grayscale_cpu2 grayscale_cpu4 grayscale_cpu6

GCC = g++ -O2
NVCC = nvcc -O2

clean:
	rm -f grayscale_cpu* grayscale_cuda grayscale.cu grayscale*.cc

grayscale.cu : ../../main.native ../../example/grayscale.gxfrp
	(cd ../.. && ./main.native -gpu true example/grayscale.gxfrp > test/grayscale_camera/grayscale.cu && cd test/grayscale_camera)

grayscale1.cc : ../../main.native ../../example/grayscale.gxfrp
	(cd ../.. && ./main.native example/grayscale.gxfrp > test/grayscale_camera/grayscale1.cc && cd test/grayscale_camera)

grayscale2.cc : ../../main.native ../../example/grayscale.gxfrp
	(cd ../.. && ./main.native -thread 2 example/grayscale.gxfrp > test/grayscale_camera/grayscale2.cc && cd test/grayscale_camera)

grayscale4.cc : ../../main.native ../../example/grayscale.gxfrp
	(cd ../.. && ./main.native -thread 4 example/grayscale.gxfrp > test/grayscale_camera/grayscale4.cc && cd test/grayscale_camera)

grayscale6.cc : ../../main.native ../../example/grayscale.gxfrp
	(cd ../.. && ./main.native -thread 6 example/grayscale.gxfrp > test/grayscale_camera/grayscale6.cc && cd test/grayscale_camera)

grayscale_cuda : grayscale.cu setting.h
	$(NVCC) -o grayscale_cuda -I/usr/local/cuda/samples/common/inc -DXFRP_ON_PTHREAD grayscale.cu `pkg-config --cflags --libs opencv4`

grayscale_cpu1 : grayscale1.cc setting.h
	$(GCC) -o grayscale_cpu1 -DXFRP_ON_PTHREAD grayscale1.cc `pkg-config --cflags --libs opencv4`

grayscale_cpu2 : grayscale2.cc setting.h
	$(GCC) -o grayscale_cpu2 -pthread -DXFRP_ON_PTHREAD grayscale2.cc `pkg-config --cflags --libs opencv4`

grayscale_cpu4 : grayscale4.cc setting.h
	$(GCC) -o grayscale_cpu4 -pthread -DXFRP_ON_PTHREAD grayscale4.cc `pkg-config --cflags --libs opencv4`

grayscale_cpu6 : grayscale6.cc setting.h
	$(GCC) -o grayscale_cpu6 -pthread -DXFRP_ON_PTHREAD grayscale6.cc `pkg-config --cflags --libs opencv4`
