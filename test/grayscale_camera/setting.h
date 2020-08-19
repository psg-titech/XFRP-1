#define loop_name(i) loop##i

#ifdef XFRP_ON_PTHREAD
#include <pthread.h>
pthread_t th[6];
#define fork(i) pthread_create(th+i, NULL, loop_name(i), NULL)
pthread_barrier_t barrier;
#define init_barrier(thread) pthread_barrier_init(&barrier,NULL,(thread))
#define synchronization(tid) pthread_barrier_wait(&barrier);
#define LOOP_RETTYPE void *
#define LOOP_ARGS void *arg
#define LOOP_RETVAL NULL
#endif

#ifdef XFRP_ON_ESP32
#include <Arduino.h>
#include <M5Stack.h>
#define TASK0_BIT (1 << 0)
#define ALL_TASK_BIT (TASK0_BIT | TASK1_BIT)
#define fork(i) xTaskCreatePinnedToCore(loop_name(i),"Task##i",8192,NULL,1,NULL,0)
EventGroupHandle_t barrier;
#define init_barrier(thread) barrier = xEventGroupCreate();
#define synchronization(i) xEventGroupSync(barrier,TASK ## i ## _BIT ,ALL_TASK_BIT,portMAX_DELAY);
#define LOOP_RETTYPE void 
#define LOOP_ARGS
#define LOOP_RETVAL
#endif

#include "opencv2/opencv.hpp"
#include <iostream>
#include <cstdint>
#include <chrono>

cv::VideoCapture cap(0);
const int w = 1600;
const int h = 1200;

std::chrono::system_clock::time_point start,end;

void user_setup(){
	std::cout << "setup" << std::endl;
	if(!cap.isOpened()){
		std::cerr << "VideoCapture not opened" << std::endl;
		return;
	}
	if(cap.get(cv::CAP_PROP_FRAME_WIDTH) < w || cap.get(cv::CAP_PROP_FRAME_HEIGHT) < h) {
		std::cerr << "Frame is smaller than expected" << std::endl;
	}
	std::cerr << "Frame size:" << cap.get(cv::CAP_PROP_FRAME_WIDTH) << "x" << cap.get(cv::CAP_PROP_FRAME_HEIGHT) << std::endl;
}

void input(int image[]){
	//std::cout << "input()" << std::endl;
	cv::Mat frame;
	cap.read(frame);
	for(int x=0;x<h;x++){
		cv::Vec3b *ptr = frame.ptr<cv::Vec3b>(x);
		for(int y=0;y<w;y++){
			int R = ptr[y][0];
			int G = ptr[y][1];
			int B = ptr[y][2];
			image[x*w+y] = (R<<16) | (G<<8) | B;
		}
	}
	start = std::chrono::system_clock::now();
}

void output(float grayImage[]){
	end = std::chrono::system_clock::now();
	double elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(end-start).count();
	printf("%dx%d:%lf, \n", w, h, elapsed);
	//std::cout << grayImage[0] << std::endl;
	//cv::Mat out(h,w,CV_32F,grayImage);
	//cv::imshow("gray", out);
	//const int key = cv::waitKey(1);
	//if(key == 'q'){
	//	exit(0);
	//}
}
