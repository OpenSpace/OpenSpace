#ifndef KERNELCONSTANTS_H_
#define KERNELCONSTANTS_H_

/*
Author: Victor Sand (victor.sand@gmail.com)
Simple structs to gather constants used in kernel
*/

namespace osp {

struct KernelConstants {
  int gridType_;
  float stepsize_;
  float intensity_;
  int numTimesteps_;
  int numValuesPerNode_;
  int numOTNodes_;
  int numBoxesPerAxis_;
  float temporalTolerance_;
  float spatialTolerance_;
  int rootLevel_;
  int paddedBrickDim_;
};

struct TraversalConstants {
  int gridType_;
  float stepsize_;
  int numTimesteps_;
  int numValuesPerNode_;
  int numOTNodes_;
  float temporalTolerance_;
  float spatialTolerance_;
};

}

#endif
