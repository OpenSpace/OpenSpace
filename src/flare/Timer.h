#ifndef TIMER_H_
#define TIMER_H_

/*
 * Author: Victor Sand (victor.sand@gmail.com)
 *
 */

namespace osp {

class Timer {
public:
  Timer();
  void Start();
  void Stop();
  void Print();
private:
  float elapsedTime_;
};

}

#endif;

