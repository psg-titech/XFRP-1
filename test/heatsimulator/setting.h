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

#include <iostream>
#include <cstdlib>
#include <chrono>

std::chrono::system_clock::time_point start,end;

float sensor_v[5];

void user_setup(){
	std::cout << "setup" << std::endl;
	sensor_v[0] = sensor_v[1] = sensor_v[2] = sensor_v[3] = sensor_v[4] = 30.0f;
}

static float random_value() {
	return (float)(std::rand()/(double)RAND_MAX);
}

void input(float sensor[]){
	//std::cout << "input()" << std::endl;
	for(int i = 0; i < 5; ++i) {
		sensor_v[i] += random_value();
	}
	start = std::chrono::system_clock::now();
}

void output(float T[]){
	end = std::chrono::system_clock::now();
	double elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(end-start).count();
	static int count = 0;
	printf("%4d:%lf, \n", count++, elapsed);
	//std::cout << grayImage[0] << std::endl;
	//cv::Mat out(h,w,CV_32F,grayImage);
	//cv::imshow("gray", out);
	//const int key = cv::waitKey(1);
	//if(key == 'q'){
	//	exit(0);
	//}
}
