#include <openspace/interaction/externalcontrol/randomexternalcontrol.h>

#include <cstdio>
#ifndef __WIN32__
	#include <unistd.h>
#endif

namespace openspace {

typedef struct 
{
	bool *keepGoing;
	double *dx;
} parm;

void *updatedx(void * arg) {
	parm *p = (parm*) arg;
	bool *kg = p->keepGoing;
	double *dx = p->dx;
	while( *kg ) {
		//printf("Hello world!\n");
		*dx = *dx + 0.5;

		// random sleep time
		int diff = rand() % 200;
		
#ifndef __WIN32__
		usleep(10000*diff);
#endif
	}
	delete p;
	return NULL;
}

	
RandomExternalControl::RandomExternalControl() {
	/*
	inputGuard = PTHREAD_MUTEX_INITIALIZER;

	pthread_attr_t pthread_custom_attr;
	pthread_attr_init(&pthread_custom_attr);
	keepGoing_ = new bool;
	*keepGoing_ = true;
	parm *p = (parm*)malloc(sizeof(parm));
	p->keepGoing = keepGoing_;
	p->dx = &dx_;

	pthread_create(&backgroundThread, &pthread_custom_attr, updatedx, (void*)p);
	*/
}

RandomExternalControl::~RandomExternalControl() {
	*keepGoing_ = false;
	//pthread_join(backgroundThread, NULL);
	delete keepGoing_;
}

} // namespace openspace

